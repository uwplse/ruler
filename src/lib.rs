/*! `Ruler` is a framework for automatically inferring rewrite rules using equality saturation.
It uses equality saturation in two novel ways to scale the rule synthesis:
    1. to minimize the term space from which candidate rules are selected,
   and 2. to minimize the candidate rule space by removing redundant rules based on rules
   currently in the ruleset.
!*/
use clap::Parser;
use egg::*;
use rand::SeedableRng;
use rand_pcg::Pcg64;
use serde::{Deserialize, Serialize};
use std::{borrow::{Borrow, Cow}};
use std::{cmp::Ordering, hash::Hash};
use std::{
    fmt::{Debug, Display},
    time::Duration,
    time::Instant,
};
use std::{hash::BuildHasherDefault, sync::Arc};

mod bv;
mod convert_sexp;
mod derive;
mod equality;
mod util;

/// Faster hashMap implementation used in rustc
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

/// Faster hashSet implementation used in rustc
pub type HashSet<K> = rustc_hash::FxHashSet<K>;

/// IndexMap data implementation used in rustc
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<rustc_hash::FxHasher>>;

pub use bv::*;
pub use equality::*;
pub use util::*;

/// Return the `i`th letter from the English alphabet.
pub fn letter(i: usize) -> &'static str {
    let alpha = "abcdefghijklmnopqrstuvwxyz";
    &alpha[i..i + 1]
}

/// Constant folding method
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ConstantFoldMethod {
    NoFold,             // disables constant folding
    CvecMatching,       // constant folding done by cvec matching
    Lang,               // constant folding implemented by language
}

/// Properties of cvecs in `Ruler`; currently only their length.
/// cvecs are stored as [eclass analysis data](https://docs.rs/egg/0.6.0/egg/trait.Analysis.html).
#[derive(Debug, Clone)]
pub struct SynthAnalysis {
    /// Length of cvec or characteristic vector.
    /// All cvecs have the same length.    
    pub cvec_len: usize,
    pub constant_fold: ConstantFoldMethod,
    pub rule_lifting: bool,
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self { 
            cvec_len: 10,
            constant_fold: ConstantFoldMethod::CvecMatching,
            rule_lifting: false,
        }
    }
}

/// Trait for defining a language for which `Ruler` will synthesize rewrites.
///
/// Every domain defines it own `Constant` type.
/// `eval` implements an interpreter for the domain. It returns a `Cvec` of length `cvec_len`
/// where each cvec element is computed using `eval`.
pub trait SynthLanguage: egg::Language + Send + Sync + 'static {
    type Constant: Clone + Hash + Eq + Debug + Display;

    fn eval<'a, F>(&'a self, cvec_len: usize, f: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>;

    fn to_var(&self) -> Option<Symbol>;
    fn mk_var(sym: egg::Symbol) -> Self;
    fn is_var(&self) -> bool {
        self.to_var().is_some()
    }

    fn to_constant(&self) -> Option<&Self::Constant>;
    fn mk_constant(c: Self::Constant) -> Self;
    fn is_constant(&self) -> bool {
        self.to_constant().is_some()
    }

    /// Generalize a pattern
    fn generalize(expr: &RecExpr<Self>, map: &mut HashMap<Symbol, Var>) -> Pattern<Self> {
        let nodes: Vec<_> = expr
            .as_ref()
            .iter()
            .map(|n| match n.to_var() {
                Some(sym) => {
                    let var = if let Some(var) = map.get(&sym) {
                        *var
                    } else {
                        let var = format!("?{}", letter(map.len())).parse().unwrap();
                        map.insert(sym, var);
                        var
                    };
                    ENodeOrVar::Var(var)
                }
                None => ENodeOrVar::ENode(n.clone()),
            })
            .collect();

        Pattern::from(PatternAst::from(nodes))
    }

    /// Instantiate a pattern
    fn instantiate(pattern: &Pattern<Self>) -> RecExpr<Self> {
        let nodes: Vec<_> = pattern
            .ast
            .as_ref()
            .iter()
            .map(|n| match n {
                ENodeOrVar::ENode(n) => n.clone(),
                ENodeOrVar::Var(v) => {
                    let s = v.to_string();
                    assert!(s.starts_with('?'));
                    Self::mk_var(s[1..].into())
                }
            })
            .collect();

        RecExpr::from(nodes)
    }

    // Like instantiate, but takes a recexpr of pattern nodes
    fn recpat_instantiate(expr: &RecExpr<ENodeOrVar<Self>>) -> RecExpr<Self> {
        let nodes: Vec<_> = expr
            .as_ref()
            .iter()
            .map(|n| match n {
                ENodeOrVar::ENode(n) => n.clone(),
                ENodeOrVar::Var(v) => {
                    let s = v.to_string();
                    assert!(s.starts_with('?'));
                    Self::mk_var(s[1..].into())
                }
            })
            .collect();

        RecExpr::from(nodes)
    }

    /// Like generalize, but returns a recexpr
    fn emt_generalize(expr: &RecExpr<Self>) -> RecExpr<Self> {
        let map = &mut HashMap::<Symbol, Var>::default();
        let nodes: Vec<_> = expr
            .as_ref()
            .iter()
            .map(|n| match n.to_var() {
                Some(sym) => {
                    let var = if let Some(var) = map.get(&sym) {
                        *var
                    } else {
                        let var = format!("?{}", letter(map.len())).parse().unwrap();
                        map.insert(sym, var);
                        var
                    };
                    
                    let s = var.to_string();
                    Self::mk_var(s[1..].into())
                }
                None => n.clone()
            })
            .collect();

        RecExpr::from(nodes)
    }

    /// Heuristics for ranking rewrites based on number of variables,
    /// constants, size of the `lhs` and `rhs`, total size of `lhs` and `rhs`,
    /// and number of ops.
    fn score(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> [i32; 5] {
        let sz_lhs = AstSize.cost_rec(&lhs.ast) as i32;
        let sz_rhs = AstSize.cost_rec(&rhs.ast) as i32;
        // let sz_max_pattern = sz_lhs.max(sz_rhs);

        // lhs.vars() and rhs.vars() is deduping
        // examples
        //   (- x x) => 0 --- 1 b/c x only var
        //   (- x 0) => x --- 1 b/c x only var
        //   (+ x y) => (+ y x) --- 2 b/c x, y only vars
        let mut var_set: HashSet<Var> = Default::default();
        var_set.extend(lhs.vars());
        var_set.extend(rhs.vars());
        let n_vars_rule = var_set.len() as i32;

        let mut op_set: HashSet<String> = Default::default();
        for node in lhs.ast.as_ref().iter().chain(rhs.ast.as_ref()) {
            if !node.is_leaf() {
                op_set.insert(node.display_op().to_string());
            }
        }
        let n_ops = op_set.len() as i32;

        let n_consts = lhs
            .ast
            .as_ref()
            .iter()
            .chain(rhs.ast.as_ref())
            .filter(|n| match n {
                ENodeOrVar::ENode(n) => n.is_constant(),
                ENodeOrVar::Var(_) => false,
            })
            .count() as i32;

        // (-sz_max_pattern, n_vars_rule)
        [
            n_vars_rule,
            -n_consts,
            -i32::max(sz_lhs, sz_rhs),
            // -i32::min(sz_lhs, sz_rhs),
            -(sz_lhs + sz_rhs),
            -n_ops,
            // 0
        ]
    }

    /// Initialize an egraph with variables and interesting constants from the domain.
    fn init_synth(synth: &mut Synthesizer<Self>);

    /// Layer wise term enumeration in the egraph.
    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self>;

    /// Returns true if the eclass contains valid constants from the domain
    fn valid_constants(
        _synth: &Synthesizer<Self>,
        _egraph: &EGraph<Self, SynthAnalysis>,
        _id: &Id,
        _seen: &mut HashSet<Id>
    ) -> bool {
        true
    }

    /// Returns true if the rewrite is valid
    fn is_valid_rewrite(
        _egraph: &EGraph<Self, SynthAnalysis>,
        _rhs: &Pattern<Self>,
        _subst: &Subst
    ) -> bool {
        true
    }

    /// Returns true if this node does not equal another by injectivity.
    fn not_equal_injective(&self, _o: &Self) -> bool {
        return false;
    }

    /// Returns true if the node is in the current domain.
    /// Useful for rule lifting.
    fn is_in_domain(&self) -> bool {
        return true;
    }

    /// Returns true if the node is extractable
    /// Used for rule lifting.
    fn is_extractable(&self) -> bool {
        return true;
    }

    /// Returns true if every node in the recexpr is in the domain
    fn recexpr_in_domain(expr: &RecExpr<Self>) -> bool {
        expr.as_ref().iter().all(|x| x.is_in_domain())
    }

    /// Helper function for `add_domain_expr`
    fn add_domain_expr_rec(synth: &mut Synthesizer<Self>, nodes: &[Self]) -> Id {
        let n = nodes.last().unwrap().clone()
            .map_children(|i| {
                let child = &nodes[..usize::from(i) + 1];
                Self::add_domain_expr_rec(synth, child)
            });
        Self::add_domain_node(synth, n)
    }

    /// Like calling `egraph.add_expr` except enodes for
    /// lower domain equivalencies are added.
    fn add_domain_expr(synth: &mut Synthesizer<Self>, expr: &RecExpr<Self>) -> Id {
        Self::add_domain_expr_rec(synth, expr.as_ref())
    }

    /// Like calling `egraph.add` except enodes for
    /// lower domain equivalencies are added.
    fn add_domain_node(synth: &mut Synthesizer<Self>, node: Self) -> Id {
        synth.egraph.add(node)
    }

    /// Constant folding done explicitly by the language
    fn constant_fold(_egraph: &mut EGraph<Self, SynthAnalysis>, _id: Id) {
        // do nothing by default
    }

    /// Given a , `ctx`, i.e., mapping from variables to cvecs, evaluate a pattern, `pat`,
    /// on each element of the cvec.
    fn eval_pattern(
        pat: &Pattern<Self>,
        ctx: &HashMap<Var, CVec<Self>>,
        cvec_len: usize,
    ) -> CVec<Self> {
        let mut buf: Vec<Cow<CVec<Self>>> = vec![];
        for enode in pat.ast.as_ref().iter() {
            match enode {
                ENodeOrVar::ENode(enode) => {
                    let cvec = enode.eval(cvec_len, |id| buf[usize::from(*id)].borrow());
                    buf.push(Cow::Owned(cvec));
                }
                ENodeOrVar::Var(var) => {
                    buf.push(Cow::Borrowed(&ctx[var]));
                }
            }
        }
        buf.pop().unwrap().into_owned()
    }

    /// Domain specific rule validation.
    fn is_valid(synth: &mut Synthesizer<Self>, lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool;

    /// helper functions to convert CVC4 rewrites to Ruler's rule syntax.
    fn convert_parse(s: &str) -> RecExpr<Self> {
        s.parse().unwrap()
    }

    /// convert CVC4 rewrites to Ruler's rule syntax.
    fn convert_eq(s: &str) -> Option<Equality<Self>> {
        use symbolic_expressions::{parser::parse_str, Sexp};
        let sexp = parse_str(s).unwrap();
        let list = sexp.list().unwrap();
        assert!(
            list[0] == Sexp::String("candidate-rewrite".into())
                || list[0] == Sexp::String("rewrite".into())
        );
        let l = Self::convert_parse(&list[1].to_string());
        let r = Self::convert_parse(&list[2].to_string());
        Equality::new(&l, &r)
    }

    /// Entry point. Use the `synth` argument from the command line
    /// for rule synthesis.
    fn main() {
        let _ = env_logger::builder().try_init();
        match Command::parse() {
            Command::Synth(params) => {
                let outfile = params.outfile.clone();
                let syn = Synthesizer::<Self>::new(params);
                let report = syn.run();
                let file = std::fs::File::create(&outfile)
                    .unwrap_or_else(|_| panic!("Failed to open '{}'", outfile));
                serde_json::to_writer_pretty(file, &report).expect("failed to write json");
            }
            Command::Derive(params) => derive::derive::<Self>(params),
            Command::ConvertSexp(params) => convert_sexp::convert::<Self>(params),
        }
    }
}

/// A synthesizer for a given [SynthLanguage].
pub struct Synthesizer<L: SynthLanguage> {
    pub params: SynthParams,
    pub rng: Pcg64,
    pub egraph: EGraph<L, SynthAnalysis>,
    initial_egraph: EGraph<L, SynthAnalysis>,
    pub all_eqs: EqualityMap<L>,
    pub old_eqs: EqualityMap<L>,
    pub new_eqs: EqualityMap<L>,
    pub lifting_rewrites: Vec<Rewrite<L, SynthAnalysis>>,
    pub smt_unknown: usize,
}



impl<L: SynthLanguage> Synthesizer<L> {
    /// Initialize all the arguments of the [Synthesizer].
    pub fn new(params: SynthParams) -> Self {
        // add prior rules (if any) to old_eqs
        let mut olds: EqualityMap<L> = Default::default();
        if params.prior_rules.clone().is_some() {
            for (l, r) in derive::parse::<L>(params.prior_rules.as_ref().unwrap()) {
                if let Some(e) = Equality::new(&l, &r) {
                    olds.insert(e.name.clone(), e);
                }
            }
        }

        let mut synth = Self {
            rng: Pcg64::seed_from_u64(params.seed),
            egraph: Default::default(),
            initial_egraph: Default::default(),
            old_eqs: olds.clone(),
            new_eqs: Default::default(),
            all_eqs: olds, // also add prior rules to all_eqs
            lifting_rewrites: vec![],
            smt_unknown: 0,
            params,
        };

        L::init_synth(&mut synth);
        synth.initial_egraph = synth.egraph.clone();
        synth
    }

    /// Get the eclass ids for all eclasses in the egraph.
    pub fn ids(&self) -> impl '_ + Iterator<Item = Id> {
        self.egraph.classes().map(|c| c.id)
    }

    /// Create a [runner](https://docs.rs/egg/0.6.0/egg/struct.Runner.html).
    fn mk_runner(&self, mut egraph: EGraph<L, SynthAnalysis>) -> Runner<L, SynthAnalysis, ()> {
        let node_limit = self.params.eqsat_node_limit;

        let mut runner = Runner::default()
            .with_node_limit(usize::MAX)
            .with_hook(move |r| {
                let size = r.egraph.total_number_of_nodes();
                if size > node_limit {
                    Err(format!("Node limit: {}", size))
                } else {
                    Ok(())
                }
            })
            .with_node_limit(self.params.eqsat_node_limit * 2)
            .with_iter_limit(self.params.eqsat_iter_limit)
            .with_time_limit(Duration::from_secs(self.params.eqsat_time_limit))
            .with_scheduler(SimpleScheduler);
        runner = if self.params.no_conditionals {
            egraph.analysis.cvec_len = 0;
            for c in egraph.classes_mut() {
                c.data.cvec.truncate(0);
            }
            runner.with_egraph(egraph).with_hook(|r| {
                for c in r.egraph.classes_mut() {
                    if c.nodes.iter().any(|n| n.is_constant()) {
                        c.nodes.retain(|n| n.is_constant());
                    }
                }
                Ok(())
            })
        } else {
            runner.with_egraph(egraph)
        };
        runner
    }

    fn mk_cvec_less_runner(&self, egraph: EGraph<L, SynthAnalysis>) -> Runner<L, SynthAnalysis, ()> {
        let node_limit = self.params.eqsat_node_limit;

        let runner = Runner::default()
            .with_node_limit(usize::MAX)
            .with_hook(move |r| {
                let size = r.egraph.total_number_of_nodes();
                if size > node_limit {
                    Err(format!("Node limit: {}", size))
                } else {
                    Ok(())
                }
            })
            .with_node_limit(self.params.eqsat_node_limit * 2)
            .with_iter_limit(self.params.eqsat_iter_limit)
            .with_time_limit(Duration::from_secs(self.params.eqsat_time_limit))
            .with_scheduler(SimpleScheduler);
        runner.with_egraph(egraph)
    }

    /// Apply current ruleset to the term egraph to minimize the term space.
    #[inline(never)]
    fn run_rewrites(&mut self) -> EGraph<L, SynthAnalysis> {
        // run the rewrites using all_eqs
        log::info!("running eqsat with {} rules", self.all_eqs.len());
        let start = Instant::now();
        let rewrites: Vec<&Rewrite<L, SynthAnalysis>> =
            self.all_eqs.values().flat_map(|eq| &eq.rewrites).collect();

        log::info!("Rewrites: {}", rewrites.len());

        let mut runner = self.mk_runner(self.egraph.clone());
        runner = runner.run(rewrites);

        log::info!("{:?} collecting unions...", runner.stop_reason.unwrap());
        // update the clean egraph based on any unions that happened
        let mut found_unions = HashMap::default();
        for id in self.ids() {
            let id2 = runner.egraph.find(id);
            found_unions.entry(id2).or_insert(vec![]).push(id);
        }
        for ids in found_unions.values() {
            for win in ids.windows(2) {
                self.egraph.union(win[0], win[1]);
            }
        }

        runner.egraph.rebuild();
        log::info!("Ran {} rules in {:?}", self.all_eqs.len(), start.elapsed());
        runner.egraph
    }

    /// Generate potential rewrite rule candidates by cvec_matching.
    ///
    /// Note that this is a pair-wise matcher which is invoked when conditional
    /// rewrite rule inference is enabled.
    #[inline(never)]
    fn cvec_match_pair_wise(&self) -> EqualityMap<L> {
        let mut by_cvec: IndexMap<CVec<L>, Vec<Id>> = IndexMap::default();

        let not_all_nones = self
            .ids()
            .filter(|id| !&self.egraph[*id].data.cvec.iter().all(|v| v == &None));
        for id in not_all_nones {
            let class = &self.egraph[id];
            let cvec = vec![class.data.cvec[0].clone()];
            by_cvec.entry(cvec).or_default().push(class.id);
        }

        log::info!("# unique cvecs: {}", by_cvec.len());

        let mut new_eqs = EqualityMap::default();
        let mut extract = Extractor::new(&self.egraph, AstSize);

        let compare = |cvec1: &CVec<L>, cvec2: &CVec<L>| -> bool {
            let mut _count = 0;
            for tup in cvec1.iter().zip(cvec2) {
                _count += match tup {
                    (Some(a), Some(b)) if a != b => return false,
                    (None, Some(_)) | (Some(_), None) => 1,
                    _ => 0,
                };
            }
            true
        };

        let mut rejected_injectivity = 0;
        for ids in by_cvec.values() {
            let mut ids = ids.iter().copied();
            while let Some(id1) = ids.next() {
                for id2 in ids.clone() {
                    if compare(&self.egraph[id1].data.cvec, &self.egraph[id2].data.cvec) {
                        if (self.egraph[id1].nodes.len() == 1) &&
                           (self.egraph[id2].nodes.len() == 1) &&
                           (self.egraph[id1].nodes[0].not_equal_injective(&self.egraph[id2].nodes[0])) {
                            rejected_injectivity += 1;
                        } else {
                            let (_, e1) = extract.find_best(id1);
                            let (_, e2) = extract.find_best(id2);
                            if let Some(mut eq) = Equality::new(&e1, &e2) {
                                log::debug!("  Candidate {}", eq);
                                eq.ids = Some((id1, id2));
                                new_eqs.insert(eq.name.clone(), eq);
                            }
                        }
                    }
                }
            }
        }

        log::info!("{} eqs rejected by injectivity", rejected_injectivity);
        new_eqs.retain(|k, _v| !self.all_eqs.contains_key(k));
        new_eqs
    }

    /// Generate potential rewrite rule candidates by cvec_matching.
    /// This is a more efficient implementation that hashes the eclasses by their cvecs.
    ///
    /// Note that this is only used when conditional rewrite rule inference is disabled.
    #[inline(never)]
    fn cvec_match(&self) -> (EqualityMap<L>, Vec<Vec<Id>>) {
        // build the cvec matching data structure
        let mut by_cvec: IndexMap<&CVec<L>, Vec<Id>> = IndexMap::default();

        for id in self.ids() {
            let class = &self.egraph[id];
            if class.data.is_defined() {
                by_cvec.entry(&class.data.cvec).or_default().push(class.id);
            }
        }

        log::info!("# unique cvecs: {}", by_cvec.len());

        let mut new_eqs = EqualityMap::default();
        let mut extract = Extractor::new(&self.egraph, AstSize);
        for ids in by_cvec.values() {
            if self.params.linear_cvec_matching || ids.len() > 0 {
                let mut terms_ids: Vec<_> =
                    ids.iter().map(|&id| (extract.find_best(id), id)).collect();
                terms_ids.sort_by_key(|x| x.0 .0);
                let ((_c1, e1), id1) = terms_ids.remove(0);
                for ((_c2, e2), id2) in terms_ids {
                    if let Some(mut eq) = Equality::new(&e1, &e2) {
                        log::debug!("  Candidate {}", eq);
                        eq.ids = Some((id1, id2));
                        new_eqs.insert(eq.name.clone(), eq);
                    }
                }
            } else {
                let mut id_iter = ids.iter();
                while let Some(&id1) = id_iter.next() {
                    let (_, e1) = extract.find_best(id1);
                    for &id2 in id_iter.clone() {
                        let (_, e2) = extract.find_best(id2);
                        if let Some(mut eq) = Equality::new(&e1, &e2) {
                            log::debug!("  Candidate {}", eq);
                            eq.ids = Some((id1, id2));
                            new_eqs.insert(eq.name.clone(), eq);
                        }
                    }
                }
            }
        }

        new_eqs.retain(|k, _v| !self.all_eqs.contains_key(k));
        (new_eqs, by_cvec.into_iter().map(|pair| pair.1).collect())
    }

    /// Enumerates a layer and filters (EMA, constant filtering)
    fn enumerate_layer(&self, iter: usize) -> Vec<L> {
        let mut layer = L::make_layer(&self, iter);
        layer.retain(|n| !n.all(|id| self.egraph[id].data.exact));

        // no constants (if set)
        log::info!("Made layer of {} nodes", layer.len());
        if iter > self.params.no_constants_above_iter {
            let constants: HashSet<Id> = self
                .ids()
                .filter_map(|id| {
                    let expr = &self.egraph[id].data.simplest;
                    if expr.as_ref().iter().any(|n| n.is_constant()) {
                        Some(id)
                    } else {
                        None
                    }
                })
                .collect();

            layer.retain(|n| n.all(|id| !constants.contains(&id)));
        }
        
        // deduplicate and filter bad constants
        let mut cp = self.egraph.clone();
        let mut max_id = cp.number_of_classes();
        layer.retain(|node| {
            let seen = &mut HashSet::<Id>::default();
            if iter > self.params.ema_above_iter {
                let rec = node.to_recexpr(|id| self.egraph[id].data.simplest.as_ref());
                let rec2 = L::emt_generalize(&rec);
                let id = cp.add_expr(&rec2);
                if usize::from(id) < max_id {
                    return false;
                }

                max_id = usize::from(id);
                L::valid_constants(&self, &cp, &id, seen)
            } else {
                let id = cp.add(node.clone());
                if usize::from(id) < max_id {
                    return false;
                }

                max_id = usize::from(id);
                L::valid_constants(&self, &cp, &id, seen)
            }
        });

        layer
    }

    /// Rule synthesis for one domain
    fn run_one_domain(mut self) -> Report<L> {
        let mut poison_rules: HashSet<Equality<L>> = HashSet::default();
        let t = Instant::now();
        assert!(self.params.iters > 0);
        for iter in 1..=self.params.iters {
            log::info!("[[[ Iteration {} ]]]", iter);
            let layer = self.enumerate_layer(iter);
            let chunk_count = div_up(layer.len(), self.params.node_chunk_size);
            let mut chunk_num = 1;

            log::info!("Filtered layer, {} nodes remaining", layer.len());
            log::info!("Running loop with {} chunks", chunk_count);
            for chunk in layer.chunks(self.params.node_chunk_size) {
                log::info!("Chunk {} / {}", chunk_num, chunk_count);
                log::info!(
                    "egraph n={}, e={}",
                    self.egraph.total_size(),
                    self.egraph.number_of_classes(),
                );

                chunk_num += 1;
                for node in chunk {
                    if iter > self.params.ema_above_iter {
                        let rec = node.to_recexpr(|id| self.egraph[id].data.simplest.as_ref());
                        self.egraph.add_expr(&L::emt_generalize(&rec));
                    } else {
                        self.egraph.add(node.clone());
                    }
                }

                'inner: loop {
                    let run_rewrites_before = Instant::now();
                    if !self.params.no_run_rewrites {
                        self.run_rewrites();
                    }
                    let run_rewrites = run_rewrites_before.elapsed().as_secs_f64();

                    let rule_discovery_before = Instant::now();
                    log::info!("cvec matching...");
                    let (new_eqs, _) = if self.params.no_conditionals {
                        self.cvec_match()
                    } else {
                        (self.cvec_match_pair_wise(), vec![])
                    };
                    let rule_discovery = rule_discovery_before.elapsed().as_secs_f64();
                    log::info!("{} candidate eqs", new_eqs.len());

                    let mut filtered_eqs: EqualityMap<L> = new_eqs
                        .into_iter()
                        .filter(|eq| !poison_rules.contains(&eq.1))
                        .collect();

                    log::info!("Time taken in... run_rewrites: {}, rule discovery: {}",
                                run_rewrites, rule_discovery);

                    let eq_chunk_count = div_up(filtered_eqs.len(), self.params.eq_chunk_size);
                    let mut eq_chunk_num = 1;

                    log::info!("Running minimization loop with {} chunks", eq_chunk_count);
                    while !filtered_eqs.is_empty() {
                        log::info!("Chunk {} / {}", eq_chunk_num, eq_chunk_count);
                        log::info!(
                            "egraph n={}, e={}",
                            self.egraph.total_size(),
                            self.egraph.number_of_classes(),
                        );
                        eq_chunk_num += 1;

                        let mut eqs_chunk: EqualityMap<L> = EqualityMap::default();
                        while !filtered_eqs.is_empty() && eqs_chunk.len() < self.params.eq_chunk_size {
                            if let Some((k, v)) = filtered_eqs.pop() {
                                eqs_chunk.insert(k, v);
                            }
                        }

                        let rule_minimize_before = Instant::now();
                        let (eqs, bads) = self.choose_eqs(eqs_chunk);
                        let rule_minimize = rule_minimize_before.elapsed().as_secs_f64();

                        log::info!("Added {} rules to the poison set!", bads.len());
                        for bad in bads {
                            poison_rules.insert(bad.1);
                        }
                        if eqs.is_empty() {
                            log::info!("Stopping early, no eqs");
                            break 'inner;
                        }

                        // filter bad constants
                        log::info!("{} possible rules", eqs.len());
                        let mut valid_eqs = vec![];
                        for (s, eq) in eqs {
                            if !self.params.no_run_rewrites {
                                assert!(!self.all_eqs.contains_key(&eq.name));
                                if let Some((i, j)) = eq.ids {  // inserted 
                                    self.egraph.union(i, j);
                                } else {                        // extracted
                                    // let mut cp = self.egraph.clone();
                                    let mut valid_const = true;
                                    let lrec = L::instantiate(&eq.lhs);
                                    let rrec = L::instantiate(&eq.rhs);

                                    let i = self.egraph.add_expr(&lrec);
                                    let seen = &mut HashSet::<Id>::default();
                                    valid_const &= L::valid_constants(&self, &self.egraph, &i, seen);

                                    let j = self.egraph.add_expr(&rrec);
                                    seen.clear();
                                    valid_const &= L::valid_constants(&self, &self.egraph, &j, seen);

                                    self.egraph.union(i, j);
                                    if !valid_const {   // encountered a constant we don't want to see
                                        continue;
                                    }
                                }
                            }

                            log::info!("  {}", eq);
                            valid_eqs.push((s, eq));
                        }

                        // add new rewrites to both all_eqs and new_eqs
                        log::info!("Chose {} good rules", valid_eqs.len());
                        self.all_eqs.extend(valid_eqs.clone());
                        self.new_eqs.extend(valid_eqs);

                        // TODO check formatting for Learned...
                        log::info!("Time taken in... rule minimization: {}", rule_minimize);
                    }

                    // For the no-conditional case which returns
                    // a non-empty list of ids that have the same cvec,
                    // won't this cause eclasses to merge even if the rule is actually not valid?
                    if self.params.rules_to_take == usize::MAX {
                        log::info!("Stopping early, took all eqs");
                        break 'inner;
                    }
                }
            }
        }

        let time = t.elapsed().as_secs_f64();

        let mut eqs: Vec<_> = self.all_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        eqs.sort_by_key(|eq| eq.score());
        eqs.reverse();

        // final run_rewrites
        if self.params.do_final_run {
            let old = std::mem::replace(&mut self.params.no_conditionals, false);
            let rws = self.all_eqs.values().flat_map(|eq| &eq.rewrites);
            let final_runner = self.mk_runner(self.egraph.clone());
            final_runner.run(rws);
            self.params.no_conditionals = old;
        }

        let num_rules = self.new_eqs.len();
        let mut n_eqs: Vec<_> = self.new_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        n_eqs.sort_by_key(|eq| eq.score());
        n_eqs.reverse();
        let num_olds = self.old_eqs.len();
        let o_eqs: Vec<_> = self.old_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        for eq in &n_eqs {
            println!("  {:?}   {}", eq.score(), eq);
        }
        println!("Learned {} rules in {:?} using {} old rules.", num_rules, time, num_olds);
        Report {
            params: self.params,
            time,
            num_rules,
            all_eqs: eqs,
            new_eqs: n_eqs,
            old_eqs: o_eqs,
            smt_unknown: self.smt_unknown,
        }
    }
    
    /// Rule synthesis for rule lifting
    fn run_rule_lifting(mut self) -> Report<L> {
        let t = Instant::now();
        // run HL-LL rewrites (iter 0)
        log::info!("running HL-LL rewrites");
        let mut runner = self.mk_cvec_less_runner(self.egraph.clone());
        runner = runner.run(&self.lifting_rewrites);
        self.egraph = runner.egraph;
        self.egraph.rebuild();

        assert!(self.params.iters > 0);
        for iter in 1..=self.params.iters {
            log::info!("[[[ Iteration {} ]]]", iter);
            let layer = self.enumerate_layer(iter);
            let chunk_count = div_up(layer.len(), self.params.node_chunk_size);
            let mut chunk_num = 1;

            log::info!("Filtered layer, {} nodes remaining", layer.len());
            log::info!("Running loop with {} chunks", chunk_count);
            for chunk in layer.chunks(self.params.node_chunk_size) {
                log::info!("Chunk {} / {}", chunk_num, chunk_count);
                log::info!(
                    "egraph n={}, e={}",
                    self.egraph.total_size(),
                    self.egraph.number_of_classes(),
                );

                chunk_num += 1;
                for node in chunk {
                    if iter > self.params.ema_above_iter {
                        let rec = node.to_recexpr(|id| self.egraph[id].data.simplest.as_ref());
                        L::add_domain_expr(&mut self, &rec);
                    } else {
                        L::add_domain_node(&mut self, node.clone());
                    }
                }
                
                // run new eqs
                log::info!("running eqsat with {} new rules", self.new_eqs.len());
                let mut runner = self.mk_cvec_less_runner(self.egraph.clone());
                let rewrites: Vec<&Rewrite<L, SynthAnalysis>> = self.new_eqs
                    .values()
                    .flat_map(|eq| &eq.rewrites)
                    .collect();
                runner = runner.run(rewrites);
                self.egraph = runner.egraph;
                self.egraph.rebuild();
                
                // run lifting rewrites
                log::info!("running eqsat with {} rules", self.old_eqs.len() + self.lifting_rewrites.len());
                let mut runner = self.mk_cvec_less_runner(self.egraph.clone());
                runner = runner.with_iter_limit(self.params.eqsat_iter_limit * 2);
                let run_rewrites_before = Instant::now();
                let start = Instant::now();
                runner = runner.run(&self.lifting_rewrites);

                // collect any interesting unions
                log::info!("{:?} collecting unions...", runner.stop_reason.unwrap());
                let mut found_unions = HashMap::default();
                for id in self.ids() {
                    let id2 = runner.egraph.find(id);
                    found_unions.entry(id2).or_insert(vec![]).push(id);
                }

                // these unions are candidate rewrite rules
                let mut new_eqs: EqualityMap<L> = EqualityMap::default();
                for ids in found_unions.values() {
                    for win in ids.windows(2) {             
                        if win[0] != win[1] &&
                            self.egraph[win[0]].data.is_extractable &&
                            self.egraph[win[1]].data.is_extractable { 
                            let mut extract = Extractor::new(&self.egraph, ExtractableAstSize);
                            let (_, e1) = extract.find_best(win[0]);
                            let (_, e2) = extract.find_best(win[1]);
                            if let Some(mut eq) = Equality::new(&e1, &e2) {
                                if e1 != e2 &&
                                    e1.as_ref().iter().all(|x| x.is_extractable()) &&        // extractable and valid
                                    e2.as_ref().iter().all(|x| x.is_extractable()) &&
                                    L::is_valid(&mut self, &eq.lhs, &eq.rhs) {
                                    eq.ids = Some((win[0], win[1]));
                                    if !self.new_eqs.contains_key(&eq.name) {
                                        log::debug!("  Candidate {}", eq);
                                        new_eqs.insert(eq.name.clone(), eq);
                                    }
                                }
                            }
                        }
                    }
                }

                self.egraph = runner.egraph.clone();
                self.egraph.rebuild();
                
                // run old rewrites
                let mut runner = self.mk_cvec_less_runner(self.egraph.clone());
                let rewrites: Vec<&Rewrite<L, SynthAnalysis>> = self.old_eqs
                    .values()
                    .flat_map(|eq| &eq.rewrites)
                    .collect();
                runner = runner.run(rewrites); 

                // collect any interesting unions
                log::info!("{:?} collecting unions...", runner.stop_reason.unwrap());
                let mut found_unions = HashMap::default();
                for id in self.ids() {
                    let id2 = runner.egraph.find(id);
                    found_unions.entry(id2).or_insert(vec![]).push(id);
                }

                // these unions are candidate rewrite rules
                for ids in found_unions.values() {
                    for win in ids.windows(2) {             
                        if win[0] != win[1] &&
                           self.egraph[win[0]].data.is_extractable &&
                           self.egraph[win[1]].data.is_extractable { 
                            let mut extract = Extractor::new(&self.egraph, ExtractableAstSize);
                            let (_, e1) = extract.find_best(win[0]);
                            let (_, e2) = extract.find_best(win[1]);
                            if let Some(mut eq) = Equality::new(&e1, &e2) {
                                if e1 != e2 &&
                                    e1.as_ref().iter().all(|x| x.is_extractable()) &&        // extractable and valid
                                    e2.as_ref().iter().all(|x| x.is_extractable()) &&
                                    L::is_valid(&mut self, &eq.lhs, &eq.rhs) {
                                    eq.ids = Some((win[0], win[1]));
                                    if !self.new_eqs.contains_key(&eq.name) {
                                        new_eqs.insert(eq.name.clone(), eq);
                                    }
                                }
                            }
                        }

                        self.egraph.union(win[0], win[1]);
                    }
                }

                self.egraph.rebuild();
                new_eqs.retain(|k, _v| !self.all_eqs.contains_key(k));

                log::info!("Ran {} rules in {:?}", self.all_eqs.len(), start.elapsed());
                let run_rewrites = run_rewrites_before.elapsed().as_secs_f64();
                log::info!("Time taken in... run_rewrites: {}", run_rewrites);

                let rule_minimize_before = Instant::now();
                let (eqs, _) = self.choose_eqs(new_eqs);
                let rule_minimize = rule_minimize_before.elapsed().as_secs_f64();
                log::info!("Time taken in... rule minimization: {}", rule_minimize);

                log::info!("Chose {} good rules", eqs.len());
                self.all_eqs.extend(eqs.clone());
                self.new_eqs.extend(eqs);
            }
        }

        let mut eqs: Vec<_> = self.all_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        eqs.sort_by_key(|eq| eq.score());
        eqs.reverse();

        let mut ids: Vec<Id> = self.ids().collect();
        let mut extract = Extractor::new(&self.egraph, ExtractableAstSize);
        ids.sort();
        for id in ids {
            if self.egraph[id].data.in_domain {
                let (_, e) = extract.find_best(id);
                log::info!("{} [{}]: {:?} {:?}", id, self.egraph[id].data.is_extractable,
                            e.pretty(100), self.egraph[id].nodes);
            }
        }

        let time = t.elapsed().as_secs_f64();
        let num_rules = self.new_eqs.len();
        let mut n_eqs: Vec<_> = self.new_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        n_eqs.sort_by_key(|eq| eq.score());
        n_eqs.reverse();
        let num_olds = self.old_eqs.len();
        let o_eqs: Vec<_> = self.old_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        for eq in &n_eqs {
            println!("  {:?}   {}", eq.score(), eq);
        }
        println!("Learned {} rules in {:?} using {} old rules.", num_rules, time, num_olds);
        Report {
            params: self.params,
            time,
            num_rules,
            all_eqs: eqs,
            new_eqs: n_eqs,
            old_eqs: o_eqs,
            smt_unknown: self.smt_unknown
        }
    }

    /// Top level function for rule synthesis.
    /// This corresponds to `Figure 4` in the Ruler paper, where
    /// all the key components of `Ruler` (e.g., `make_layer`, `run_rewrites`, `cvec_match`, `choose_eqs`) are invoked.
    pub fn run(mut self) -> Report<L> {
        // normalize some params
        if self.params.rules_to_take == 0 {
            self.params.rules_to_take = usize::MAX;
        }
        if self.params.node_chunk_size == 0 {
            self.params.node_chunk_size = usize::MAX;
        }
        if self.params.eq_chunk_size == 0 {
            self.params.eq_chunk_size = usize::MAX;
        }

        if self.egraph.analysis.rule_lifting {
            self.run_rule_lifting()
        } else {
            self.run_one_domain()
        }
    }
}

/// Reports for each run of Ruler.
#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
pub struct Report<L: SynthLanguage> {
    pub params: SynthParams,
    pub time: f64,
    pub num_rules: usize,
    pub smt_unknown: usize,
    pub all_eqs: Vec<Equality<L>>,
    pub new_eqs: Vec<Equality<L>>,
    pub old_eqs: Vec<Equality<L>>,
}

#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
struct SlimReport<L: SynthLanguage> {
    time: f64,
    all_eqs: Vec<Equality<L>>,
    new_eqs: Vec<Equality<L>>,
    old_eqs: Vec<Equality<L>>,
    // eqs: Vec<Equality<L>>,
}

/// All parameters for rule synthesis.
#[derive(Parser, Deserialize, Serialize)]
#[clap(rename_all = "kebab-case")]
pub struct SynthParams {
    /// Seed for random number generator, used for random cvec value generation
    #[clap(long, default_value = "0")]
    pub seed: u64,
    /// How many random values to add to the cvecs
    #[clap(long, default_value = "0")]
    pub n_samples: usize,
    /// Number of variables to add to the initial egraph
    #[clap(long, default_value = "3")]
    pub variables: usize,
    /// Output file name
    #[clap(long, default_value = "out.json")]
    pub outfile: String,
    /// Constant folding
    #[clap(long)]
    pub no_constant_fold: bool,

    ///////////////////
    // search params //
    ///////////////////
    /// Number of iterations
    #[clap(long, default_value = "1")]
    pub iters: usize,
    /// 0 is unlimited
    #[clap(long, default_value = "0")]
    pub rules_to_take: usize,
    /// 0 is unlimited
    #[clap(long, default_value = "100000")]
    pub node_chunk_size: usize,
    /// 0 is unlimited
    #[clap(long, default_value = "0")]
    pub eq_chunk_size: usize,
    /// disallows enumerating terms with constants past this iteration
    #[clap(long, default_value = "999999")]
    pub no_constants_above_iter: usize,
    /// For enabling / disabling conditional rule inference
    #[clap(long)]
    pub no_conditionals: bool,
    #[clap(long)]
    /// For turning off `run_rewrites`
    pub no_run_rewrites: bool,
    #[clap(long)]
    pub linear_cvec_matching: bool,
    /// modulo alpha renaming
    #[clap(long, default_value = "999999")]
    pub ema_above_iter: usize,
    // disabled operators during enumeration
    #[clap(long)]
    pub disabled_ops: Option<String>,
    // disabled constants during enumeration
    #[clap(long)]
    pub disabled_consts: Option<String>,
    // consts allowed in final rules, empty implies no filter
    #[clap(long)]
    pub filtered_consts: Option<String>,

    ////////////////
    // eqsat args //
    ////////////////
    /// node limit for all the eqsats
    #[clap(long, default_value = "300000")]
    pub eqsat_node_limit: usize,
    /// iter limit for all the eqsats
    #[clap(long, default_value = "2")]
    pub eqsat_iter_limit: usize,
    /// time limit (seconds) for all the eqsats
    #[clap(long, default_value = "60")]
    pub eqsat_time_limit: u64,
    /// Controls the size of cvecs
    #[clap(long, default_value = "5")]
    pub important_cvec_offsets: u32,

    //////////////////////////
    // domain specific args //
    //////////////////////////
    /// (str only), the number of additional int variables
    #[clap(long, default_value = "1")]
    pub str_int_variables: usize,
    /// Only for bv, makes it do a complete cvec
    #[clap(long, conflicts_with = "important-cvec-offsets")]
    pub complete_cvec: bool,
    /// Only for bool/bv
    #[clap(long)]
    pub no_xor: bool,
    /// Only for bv
    #[clap(long)]
    pub no_shift: bool,

    ///////////////////
    // eqsat soundness params //
    ///////////////////
    // for validation approach
    /// random testing based validation
    #[clap(long, default_value = "0")]
    pub num_fuzz: usize,
    /// SMT based verification (uses Z3 for the current prototype)
    #[clap(long, conflicts_with = "num-fuzz")]
    pub use_smt: bool,
    /// For a final round of run_rewrites to remove redundant rules.
    #[clap(long)]
    pub do_final_run: bool,

    ///////////////////
    // persistence params //
    ///////////////////
    #[clap(long)]
    pub prior_rules: Option<String>,

}

/// Derivability report.
#[derive(clap::Parser)]
#[clap(rename_all = "kebab-case")]
pub struct DeriveParams {
    in1: String,
    in2: String,
    out: String,
    #[clap(long, default_value = "5")]
    iter_limit: usize,
    #[clap(long, default_value = "300000")]
    node_limit: usize,
    #[clap(long, default_value = "60")]
    time_limit: u64,
}

/// Report for rules generated by CVC4.
#[derive(Parser)]
#[clap(rename_all = "kebab-case")]
pub struct ConvertParams {
    cvc_log: String,
    out: String,
}

/// Ruler can be run to synthesize rules, compare two rulesets
/// for derivability, and convert CVC4 rewrites to patterns in Ruler.
#[derive(Parser)]
#[clap(rename_all = "kebab-case")]
pub enum Command {
    Synth(SynthParams),
    Derive(DeriveParams),
    ConvertSexp(ConvertParams),
}

/// A mapping from a name to an `Equality`.
pub type EqualityMap<L> = IndexMap<Arc<str>, Equality<L>>;

/// A CVec is a data structure that stores the result of evaluating a term on concrete inputs.
pub type CVec<L> = Vec<Option<<L as SynthLanguage>::Constant>>;

/// Simple macro for generating cvecs for unary, binary, and ternary ops.
#[macro_export]
macro_rules! map {
    ($get:ident, $a:ident => $body:expr) => {
        $get($a)
            .iter()
            .map(|a| match a {
                Some($a) => $body,
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    ($get:ident, $a:ident, $b:ident => $body:expr) => {
        $get($a)
            .iter()
            .zip($get($b).iter())
            .map(|tup| match tup {
                (Some($a), Some($b)) => $body,
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    ($get:ident, $a:ident, $b:ident, $c:ident => $body:expr) => {
        $get($a)
            .iter()
            .zip($get($b).iter())
            .zip($get($c).iter())
            .map(|tup| match tup {
                ((Some($a), Some($b)), Some($c)) => $body,
                _ => None,
            })
            .collect::<Vec<_>>()
    };
}

/// The Signature represents eclass analysis data.
#[derive(Debug, Clone)]
pub struct Signature<L: SynthLanguage> {
    pub cvec: CVec<L>,
    pub simplest: RecExpr<L>,
    pub exact: bool,
    pub in_domain: bool,
    pub is_extractable: bool,
}

impl<L: SynthLanguage> Signature<L> {
    /// A cvec is defined either it is empty or has at least one `Some` value.
    pub fn is_defined(&self) -> bool {
        self.cvec.is_empty() || self.cvec.iter().any(|v| v.is_some())
    }
}

fn ord_merge(to: &mut Option<Ordering>, from: Ordering) {
    if let Some(ord) = to.as_mut() {
        match (*ord, from) {
            (Ordering::Equal, _) => *ord = from,
            (_, Ordering::Equal) => (),
            (_, _) if *ord == from => (),
            _ => *to = None,
        }
    }
}

impl<L: SynthLanguage> egg::Analysis<L> for SynthAnalysis {
    type Data = Signature<L>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> Option<Ordering> {
        let mut ord = Some(Ordering::Equal);
        let cost_fn = |x: &RecExpr<L>| {
            if self.rule_lifting && L::recexpr_in_domain(x) {
                ExtractableAstSize.cost_rec(x)
            } else {
                AstSize.cost_rec(x)
            }
        };

        // do not merge high-level enode with low-level enode
        assert_eq!(to.in_domain, from.in_domain,
                   "trying to merge HL and LL eclass: {} != {}",
                   to.simplest.pretty(100), from.simplest.pretty(100));

        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            for i in 0..to.cvec.len() {
                match (to.cvec[i].clone(), from.cvec[i].clone()) {
                    (None, Some(_)) => {
                        to.cvec[i] = from.cvec[i].clone();
                        ord_merge(&mut ord, Ordering::Less);
                    }
                    (Some(x), Some(y)) => {
                        assert_eq!(x, y, "cvecs do not match at index {}: {} != {}", i, x, y)
                    }
                    (Some(_), None) => {
                        ord_merge(&mut ord, Ordering::Greater);
                    }
                    _ => (),
                }
            }

            ord_merge(&mut ord, to.exact.cmp(&from.exact));
        }

        to.exact |= from.exact;
        to.is_extractable |= from.is_extractable;
        if cost_fn(&from.simplest) < cost_fn(&to.simplest) {
            to.simplest = from.simplest.clone();
        }

        ord
    }

    fn make(egraph: &EGraph<L, Self>, enode: &L) -> Self::Data {
        let get_cvec = |i: &Id| &egraph[*i].data.cvec;
        let get_simplest = |i: &Id| &egraph[*i].data.simplest;
        Signature {
            cvec: enode.eval(egraph.analysis.cvec_len, get_cvec),
            simplest: if enode.is_in_domain() && (enode.is_var() || enode.is_constant()) {
                let mut rec = RecExpr::<L>::default();
                rec.add(enode.clone());
                rec
            } else {
                let mut nodes: Vec<L> = vec![];
                let mut map: HashMap<Id, Id> = HashMap::default();
                enode.for_each(|id| {
                    if map.get(&id) == None {
                        let s = get_simplest(&id);
                        let i = nodes.len();
                        for n in s.as_ref() {
                            nodes.push(n
                                .clone()
                                .map_children(|id| Id::from(usize::from(id) + i)));
                        }

                        map.insert(id, Id::from(nodes.len() - 1));
                    }
                });

                nodes.push(enode
                    .clone()
                    .map_children(|id| *map.get(&id).unwrap()));
                RecExpr::from(nodes)
            },
            exact: !enode.is_var() && enode.all(|i| egraph[i].data.exact),
            in_domain: enode.is_in_domain(),
            is_extractable: enode.is_extractable(),
        }
    }

    fn modify(egraph: &mut EGraph<L, Self>, id: Id) {
        match egraph.analysis.constant_fold {
            ConstantFoldMethod::CvecMatching => {
                let sig = &egraph[id].data;
                if sig.exact {
                    let first = sig.cvec.iter().find_map(|x| x.as_ref());
                    if let Some(first) = first {
                        let enode = L::mk_constant(first.clone());
                        let added = egraph.add(enode);
                        egraph.union(id, added);
                    }
                }
            },
            ConstantFoldMethod::Lang => {
                if egraph[id].data.exact {
                    L::constant_fold(egraph, id);
                }
            },
            _ => (),
        }
    }
}

impl<L: SynthLanguage> Synthesizer<L> {
    /// Shrink the candidate space.
    #[inline(never)]
    fn shrink(
        &mut self,
        mut new_eqs: EqualityMap<L>,
        step: usize,
        should_validate: bool
    ) -> (EqualityMap<L>, EqualityMap<L>) {
        let mut keepers = EqualityMap::default();
        let mut bads = EqualityMap::default();
        let initial_len = new_eqs.len();

        while !new_eqs.is_empty() {
            // best are last
            new_eqs.sort_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()));

            // take step valid rules from the end of new_eqs
            let mut took = 0;
            while let Some((name, eq)) = new_eqs.pop() {
                if should_validate {
                    let valid = L::is_valid(self, &eq.lhs, &eq.rhs);
                    if valid {
                        let old = keepers.insert(name, eq);
                        took += old.is_none() as usize;
                    } else {
                        bads.insert(name, eq);
                    }
                } else {
                    let old = keepers.insert(name, eq);
                    took += old.is_none() as usize;
                }

                if took >= step {
                    break;
                }
            }

            if new_eqs.is_empty() {
                break;
            }

            if keepers.len() >= self.params.rules_to_take {
                keepers.truncate(self.params.rules_to_take);
                break;
            }

            log::info!("Shrinking with {} keepers", keepers.len());

            let rewrites = self
                .all_eqs
                .values()
                .flat_map(|eq| &eq.rewrites)
                .chain(keepers.values().flat_map(|eq| &eq.rewrites));

            let mut runner = self.mk_runner(self.initial_egraph.clone());
            for candidate_eq in new_eqs.values() {
                runner = runner.with_expr(&L::instantiate(&candidate_eq.lhs));
                runner = runner.with_expr(&L::instantiate(&candidate_eq.rhs));
            }

            runner = runner.run(rewrites);
            log::info!(
                "Stopping {:?}, {:?}",
                runner.stop_reason.clone().unwrap(),
                runner.iterations.len()
            );

            let old_len = new_eqs.len();
            if self.egraph.analysis.rule_lifting {
                let mut extract = Extractor::new(&runner.egraph, ExtractableAstSize);
                new_eqs.clear();

                for ids in runner.roots.chunks(2) {
                    if runner.egraph.find(ids[0]) != runner.egraph.find(ids[1]) {
                        let left = extract.find_best(ids[0]).1;
                        let right = extract.find_best(ids[1]).1;
                        if L::recexpr_in_domain(&left) && L::recexpr_in_domain(&right) {
                            if let Some(eq) = Equality::new(&left, &right) {
                                if !self.all_eqs.contains_key(&eq.name) {
                                    new_eqs.insert(eq.name.clone(), eq);
                                }
                            }
                        }
                    }
                }
            } else {
                let mut extract = Extractor::new(&runner.egraph, AstSize);
                new_eqs.clear();

                for ids in runner.roots.chunks(2) {
                    if runner.egraph.find(ids[0]) != runner.egraph.find(ids[1]) {
                        let left = extract.find_best(ids[0]).1;
                        let right = extract.find_best(ids[1]).1;
                        if L::recexpr_in_domain(&left) && L::recexpr_in_domain(&right) {
                            if let Some(eq) = Equality::new(&left, &right) {
                                if !self.all_eqs.contains_key(&eq.name) {
                                    new_eqs.insert(eq.name.clone(), eq);
                                }
                            }
                        }
                    }
                }
            };

            log::info!(
                "Minimizing... threw away {} rules, {} / {} remain",
                old_len - new_eqs.len(),
                new_eqs.len(),
                initial_len,
            );
        }
        if self.params.rules_to_take == usize::MAX {
            assert!(new_eqs.is_empty());
        }
        (keepers, bads)
    }

    /// Apply rewrites rules as they are being inferred, to minimize the candidate space.
    #[inline(never)]
    fn choose_eqs(
        &mut self,
        mut new_eqs: EqualityMap<L>,
    ) -> (EqualityMap<L>, EqualityMap<L>) {
        let step_sizes: Vec<usize> = vec![100, 10, 1];
        let mut bads = EqualityMap::default();
        let mut should_validate = true;
        let mut step_idx = 0;

        // Idea here is to remain at a high step level as long as possible
        // Move down in step size if
        //  (a) the number of eqs is smaller than the step size
        //  (b) the number of bad eqs is smaller than the step size
        // Never move up in step level
        'inner: loop {
            let step = step_sizes[step_idx];
            if self.params.rules_to_take < step || new_eqs.len() < step {
                if step_idx + 1 == step_sizes.len() {
                    break 'inner;
                } else {
                    step_idx += 1;
                    continue;
                }
            }

            let (n, b) = self.shrink(new_eqs, step, should_validate);
            let n_bad = b.len();
            new_eqs = n;
            bads.extend(b);

            // if we taking all the rules, we don't need to validate anymore
            if self.params.rules_to_take == usize::MAX {
                should_validate = false;
            }

            if n_bad < step {           // not enough progress:
                if step_idx + 1 == step_sizes.len() {
                    if n_bad == 0 {     // break if no progress at lowest step size
                        break 'inner;
                    }                   // else loop again
                } else {                // default: decrease step size
                    step_idx += 1;
                }
            }
        }

        (new_eqs, bads)
    }
}

/// An alternate cost function that computes the number of operators in an term.
pub struct NumberOfOps;
impl<L: Language> egg::CostFunction<L> for NumberOfOps {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &L, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if enode.is_leaf() {
            0
        } else {
            enode.fold(1, |sum, id| sum + costs(id))
        }
    }
}

// Cost function only counting ops in the domain
// Penalizes ops not in the domain
pub struct NumberOfDomainOps;
impl<L: SynthLanguage> egg::CostFunction<L> for NumberOfDomainOps {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &L, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if !enode.is_in_domain() {
            usize::max_value()
        } else if enode.is_var() || enode.is_constant() {
            0
        } else {
            enode.fold(1, |sum, id| sum.saturating_add(costs(id)))
        }
    }
}

// Cost function for ast size in the domain
// Penalizes ops not in the domain
pub struct ExtractableAstSize;
impl<L: SynthLanguage> egg::CostFunction<L> for ExtractableAstSize {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &L, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if enode.is_extractable() {
            enode.fold(1, |sum, id| sum.saturating_add(costs(id)))
        } else {
            usize::max_value()
        }
    }
}
