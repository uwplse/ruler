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
use std::{
    borrow::{Borrow, Cow},
    fmt::{Debug, Display, Write},
    hash::{BuildHasherDefault, Hash},
    sync::Arc,
    time::{Duration, Instant},
};

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
    NoFold,           // disables constant folding
    CvecMatching,     // constant folding done by cvec matching
    IntervalAnalysis, // constant folding done by interval analysis
    Lang,             // constant folding implemented by language
}

/// Validation result
#[derive(Debug, Clone)]
pub enum ValidationResult {
    Valid,
    Invalid,
    Unknown,
}

impl From<bool> for ValidationResult {
    fn from(b: bool) -> Self {
        match b {
            true => ValidationResult::Valid,
            false => ValidationResult::Invalid,
        }
    }
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
pub trait SynthLanguage: egg::Language + Send + Sync + Display + FromOp + 'static {
    type Constant: Clone + Hash + Eq + Debug + Display + Ord;
    type Type: Clone + Hash + Eq + Debug + Display + Ord;

    fn get_type(&self) -> Self::Type;

    fn eval<'a, F>(&'a self, cvec_len: usize, f: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>;

    fn to_var(&self) -> Option<Symbol>;
    fn mk_var(sym: egg::Symbol) -> Self;
    fn is_var(&self) -> bool {
        self.to_var().is_some()
    }

    fn to_enode_or_var(self) -> ENodeOrVar<Self> {
        match self.to_var() {
            Some(var) => ENodeOrVar::Var(format!("?{}", var).parse().unwrap()),
            None => ENodeOrVar::ENode(self),
        }
    }

    fn to_pattern(expr: RecExpr<Self>) -> Pattern<Self> {
        let nodes: Vec<ENodeOrVar<Self>> = expr
            .as_ref()
            .iter()
            .map(|node| node.clone().to_enode_or_var())
            .collect();
        PatternAst::from(nodes).into()
    }

    /**
     * Most domains don't need a reference to the egraph to make a constant node.
     * However, Pos and Nat represent numbers recursively, so adding a new constant
     * requires adding multiple nodes to the egraph.
     */
    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self;
    fn is_constant(&self) -> bool;

    /// Generalize a pattern
    fn generalize(expr: &RecExpr<Self>, map: &mut HashMap<Symbol, Var>) -> Pattern<Self> {
        let expr = Self::alpha_rename_with(expr, map);
        Self::to_pattern(expr)
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

    fn alpha_rename_with(expr: &RecExpr<Self>, map: &mut HashMap<Symbol, Var>) -> RecExpr<Self> {
        let mut rename_node = |node: &Self| match node.to_var() {
            Some(sym) => {
                let len = map.len();
                let var = map.entry(sym).or_insert_with(|| {
                    format!("?{}{}", node.get_type(), letter(len))
                        .parse()
                        .unwrap()
                });

                let s = var.to_string();
                Self::mk_var(s[1..].into())
            }
            None => node.clone(),
        };
        let root = rename_node(expr.as_ref().last().unwrap());
        root.build_recexpr(|id| rename_node(&expr[id]))
    }

    /// Applies alpha renaming to a recexpr
    fn alpha_rename(expr: &RecExpr<Self>) -> RecExpr<Self> {
        Self::alpha_rename_with(expr, &mut Default::default())
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
                op_set.insert(node.to_string());
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

    /// Returns interval analysis data based on this node.
    fn mk_interval(&self, _egraph: &EGraph<Self, SynthAnalysis>) -> Interval<Self::Constant> {
        Interval::default()
    }

    /// Initialize an egraph with variables and interesting constants from the domain.
    fn init_synth(synth: &mut Synthesizer<Self>);

    /// Layer wise term enumeration in the egraph.
    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self>;

    /// Returns true if the rewrite is valid
    fn is_valid_rewrite(
        _egraph: &EGraph<Self, SynthAnalysis>,
        _rhs: &Pattern<Self>,
        _subst: &Subst,
    ) -> bool {
        true
    }

    /// Returns true if the rewrite is allowed.
    fn is_allowed_rewrite(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool {
        let pattern_is_extractable = |pat: &Pattern<Self>| {
            pat.ast.as_ref().iter().all(|n| match n {
                ENodeOrVar::Var(_) => true,
                ENodeOrVar::ENode(n) => n.is_extractable(),
            })
        };

        pattern_is_extractable(lhs) && pattern_is_extractable(rhs)
    }

    /// Returns true if the node is in an allowed node.
    /// Useful for rule lifting.
    fn is_allowed(&self) -> bool {
        true
    }

    /// Returns true if the node is extractable
    /// Used for rule lifting.
    fn is_extractable(&self) -> bool {
        true
    }

    /// Returns true if every node in the recexpr is extractable
    fn recexpr_is_extractable(expr: &RecExpr<Self>) -> bool {
        expr.as_ref().iter().all(|x| x.is_extractable())
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
    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult;

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
            Command::Derive(params) => {
                if params.ci {
                    derive::derive_ci::<Self>(params);
                } else {
                    derive::derive::<Self>(params);
                }
            }
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
        if params.prior_rules.is_some() {
            for eq in derive::parse::<L>(params.prior_rules.as_ref().unwrap(), false) {
                olds.insert(eq.name.clone(), eq);
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

        // initialize egraph with variables and constants
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
        let mut runner = Runner::default()
            .with_node_limit(self.params.eqsat_node_limit)
            .with_iter_limit(self.params.eqsat_iter_limit)
            .with_time_limit(Duration::from_secs(self.params.eqsat_time_limit))
            .with_scheduler(SimpleScheduler);

        if self.params.no_conditionals {
            egraph.analysis.cvec_len = 0;
            for c in egraph.classes_mut() {
                c.data.cvec.truncate(0);
            }
            runner = runner.with_hook(|r| {
                for c in r.egraph.classes_mut() {
                    if c.nodes.iter().any(|n: &L| n.is_constant()) {
                        c.nodes.retain(|n| n.is_constant());
                    }
                }
                Ok(())
            })
        }

        runner.with_egraph(egraph)
    }

    fn mk_cvec_less_runner(
        &self,
        egraph: EGraph<L, SynthAnalysis>,
    ) -> Runner<L, SynthAnalysis, ()> {
        Runner::default()
            .with_node_limit(self.params.eqsat_node_limit)
            .with_iter_limit(self.params.eqsat_iter_limit)
            .with_time_limit(Duration::from_secs(self.params.eqsat_time_limit))
            .with_scheduler(SimpleScheduler)
            .with_egraph(egraph)
    }

    /// Apply current ruleset to the term egraph to minimize the term space.
    #[inline(never)]
    fn run_rewrites(&mut self) -> EGraph<L, SynthAnalysis> {
        // run the rewrites using all_eqs
        let start = Instant::now();
        let num_rewrites = self.all_eqs.len();
        log::info!("running eqsat with {} rules", num_rewrites);

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
            found_unions.entry(id2).or_insert_with(Vec::new).push(id);
        }

        for ids in found_unions.values() {
            for win in ids.windows(2) {
                self.egraph.union(win[0], win[1]);
            }
        }

        runner.egraph.rebuild();
        log::info!("Ran {} rules in {:?}", num_rewrites, start.elapsed());
        runner.egraph
    }

    fn run_rewrites_with_unions(
        &self,
        rewrites: Vec<&Rewrite<L, SynthAnalysis>>,
        mut runner: Runner<L, SynthAnalysis>,
    ) -> (EGraph<L, SynthAnalysis>, HashMap<Id, Vec<Id>>, StopReason) {
        let ids: Vec<Id> = runner.egraph.classes().map(|c| c.id).collect();
        runner = runner.run(rewrites);
        let stop_reason = runner.stop_reason.unwrap();

        let mut found_unions = HashMap::default();

        for id in ids {
            let new_id = runner.egraph.find(id);
            found_unions.entry(new_id).or_insert_with(Vec::new).push(id);
        }

        runner.egraph.rebuild();
        (runner.egraph, found_unions, stop_reason)
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
            .filter(|id| !&self.egraph[*id].data.cvec.iter().all(|v| v.is_none()));
        for id in not_all_nones {
            let class = &self.egraph[id];
            let cvec = vec![class.data.cvec[0].clone()];
            by_cvec.entry(cvec).or_default().push(class.id);
        }

        log::info!("# unique cvecs: {}", by_cvec.len());

        let mut new_eqs = EqualityMap::default();
        let extract = Extractor::new(&self.egraph, AstSize);

        let compare = |cvec1: &CVec<L>, cvec2: &CVec<L>| -> bool {
            for tup in cvec1.iter().zip(cvec2) {
                match tup {
                    (Some(a), Some(b)) if a != b => return false,
                    _ => (),
                }
            }
            true
        };

        for ids in by_cvec.values() {
            let mut ids = ids.iter().copied();
            while let Some(id1) = ids.next() {
                for id2 in ids.clone() {
                    if compare(&self.egraph[id1].data.cvec, &self.egraph[id2].data.cvec) {
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
        let extract = Extractor::new(&self.egraph, AstSize);
        for ids in by_cvec.values() {
            if self.params.linear_cvec_matching || !ids.is_empty() {
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
    fn enumerate_layer(&self, iter: usize) -> Vec<RecExpr<L>> {
        let mut layer = L::make_layer(self, iter);
        layer.retain(|n| !n.all(|id| self.egraph[id].data.exact));

        // no constants (if set)
        log::info!("Made layer of {} nodes", layer.len());
        if iter > self.params.no_constants_above_iter {
            let constants: HashSet<Id> = if iter > self.params.ema_above_iter {
                self.ids()
                    .filter(|id| {
                        let expr = &self.egraph[*id].data.simplest;
                        expr.as_ref().iter().any(|n| n.is_constant())
                    })
                    .collect()
            } else {
                let extract = Extractor::new(&self.egraph, NumberOfOps);
                self.ids()
                    .filter(|id| {
                        let (_, best) = extract.find_best(*id);
                        best.as_ref().iter().any(|n| n.is_constant())
                    })
                    .collect()
            };

            layer.retain(|n| n.all(|id| !constants.contains(&id)));
        }

        // deduplicate
        let mut cp = self.egraph.clone();
        let mut max_id = cp.number_of_classes();
        layer
            .iter()
            .filter_map(|node| {
                let rec = node.join_recexprs(|id| self.egraph[id].data.simplest.as_ref());
                if iter > self.params.ema_above_iter {
                    let rec2 = L::alpha_rename(&rec);
                    let id = cp.add_expr(&rec2);
                    if usize::from(id) < max_id {
                        return None;
                    }
                    max_id = usize::from(id);
                    Some(rec2)
                } else {
                    let id = cp.add(node.clone());
                    if usize::from(id) < max_id {
                        return None;
                    }
                    max_id = usize::from(id);
                    Some(rec)
                }
            })
            .collect()
    }

    // Adds a slice of nodes to the egraph
    fn add_chunk(egraph: &mut EGraph<L, SynthAnalysis>, chunk: &[RecExpr<L>]) {
        for node in chunk {
            egraph.add_expr(node);
        }
    }

    // Run single chunk
    fn run_chunk(&mut self, chunk: &[RecExpr<L>], poison_rules: &mut HashSet<Equality<L>>) {
        Synthesizer::add_chunk(&mut self.egraph, chunk);

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
            filtered_eqs.sort_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()));
            log::info!(
                "Time taken in... run_rewrites: {}, rule discovery: {}",
                run_rewrites,
                rule_discovery
            );

            // break if we have nothing to minimize
            if filtered_eqs.is_empty() {
                break 'inner;
            }

            let eq_chunk_count = div_up(filtered_eqs.len(), self.params.eq_chunk_size);
            let mut eq_chunk_num = 1;
            let mut minimized_eq_count = 0;

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

                // TODO check formatting for Learned...
                log::info!("Time taken in... rule minimization: {}", rule_minimize);

                log::info!("Chose {} good rules", eqs.len());
                for (_, eq) in &eqs {
                    if !self.params.no_run_rewrites {
                        assert!(!self.all_eqs.contains_key(&eq.name));
                        if let Some((i, j)) = eq.ids {
                            // inserted
                            self.egraph.union(i, j);
                        } else {
                            // extracted
                            // let mut cp = self.egraph.clone();
                            let lrec = L::instantiate(&eq.lhs);
                            let rrec = L::instantiate(&eq.rhs);
                            let i = self.egraph.add_expr(&lrec);
                            let j = self.egraph.add_expr(&rrec);

                            self.egraph.union(i, j);
                        }
                    }

                    log::info!("  {}", eq);
                }

                minimized_eq_count += eqs.len();
                self.new_eqs.extend(eqs.clone());
                self.all_eqs.extend(eqs);
            }

            // break if we have no new eqs
            if minimized_eq_count == 0 {
                log::info!("Stopping early, no minimized eqs");
                break 'inner;
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

    fn enumerate_workload(&self, filename: &str) -> Vec<RecExpr<L>> {
        let infile = std::fs::File::open(filename).expect("can't open file");
        let reader = std::io::BufReader::new(infile);
        let mut terms = vec![];
        for line in std::io::BufRead::lines(reader) {
            let line = line.unwrap();
            let l = L::convert_parse(&line);
            terms.push(l);
        }
        terms
    }

    /// Rule synthesis for one domain, i.e., Ruler as presented at OOPSLA'21
    fn run_cvec_synth(mut self) -> Report<L> {
        let mut poison_rules: HashSet<Equality<L>> = HashSet::default();
        let t = Instant::now();

        if let Some(filename) = &self.params.workload {
            let terms = self.enumerate_workload(filename);
            for chunk in terms.chunks(self.params.node_chunk_size) {
                self.run_chunk(chunk, &mut poison_rules);
            }
        } else {
            let iters = self
                .params
                .iters
                .expect("Either iters or workload is required.");
            for iter in 1..=iters {
                log::info!("[[[ Iteration {} ]]]", iter);
                let layer = self.enumerate_layer(iter);
                for chunk in layer.chunks(self.params.node_chunk_size) {
                    self.run_chunk(chunk, &mut poison_rules);
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
        println!(
            "Learned {} rules in {:?} using {} old rules.",
            num_rules, time, num_olds
        );
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

    fn run_rule_lifting(mut self) -> Report<L> {
        let timer = Instant::now();
        if let Some(filename) = &self.params.workload {
            let terms = self.enumerate_workload(filename);
            for chunk in terms.chunks(self.params.node_chunk_size) {
                self.run_chunk_rule_lifting(chunk);
            }
        } else {
            let iters = self
                .params
                .iters
                .expect("Either iters or workload is required");
            for iter in 1..=iters {
                let layer = self.enumerate_layer(iter);
                for chunk in layer.chunks(self.params.node_chunk_size) {
                    self.run_chunk_rule_lifting(chunk);
                }
            }
        }

        let time = timer.elapsed().as_secs_f64();

        let mut all_eqs: Vec<Equality<L>> =
            self.all_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        all_eqs.sort_by_key(|eq| eq.score());
        all_eqs.reverse();
        let mut new_eqs: Vec<Equality<L>> =
            self.new_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        new_eqs.sort_by_key(|eq| eq.score());
        new_eqs.reverse();
        let mut old_eqs: Vec<Equality<L>> =
            self.old_eqs.clone().into_iter().map(|(_, eq)| eq).collect();
        old_eqs.sort_by_key(|eq| eq.score());
        old_eqs.reverse();

        for eq in &new_eqs {
            println!("  {:?}   {}", eq.score(), eq);
        }
        println!(
            "Learned {} rules in {:?} using {} old rules.",
            new_eqs.len(),
            time,
            old_eqs.len()
        );

        Report {
            params: self.params,
            time,
            num_rules: new_eqs.len(),
            all_eqs,
            new_eqs,
            old_eqs,
            smt_unknown: self.smt_unknown,
        }
    }

    fn run_chunk_rule_lifting(&mut self, chunk: &[RecExpr<L>]) {
        // 1. Add terms to egraph
        log::info!("Adding {} terms to egraph", chunk.len());
        Synthesizer::add_chunk(&mut self.egraph, chunk);

        // 2. Run allowed rules
        // Don't add terms to the egraph (run on a clone)
        // Merges are not rule candidates because they are derivable
        // from existing allowed rules.
        log::info!("Running allowed rules");
        let mut allowed: EqualityMap<L> = EqualityMap::default();
        for (name, eq) in self.all_eqs.clone() {
            if L::is_allowed_rewrite(&eq.lhs, &eq.rhs) {
                allowed.insert(name, eq);
            }
        }
        let runner = self.mk_cvec_less_runner(self.egraph.clone());
        let rewrites = allowed.values().flat_map(|eq| &eq.rewrites).collect();
        let (_, found_unions, _) = self.run_rewrites_with_unions(rewrites, runner);
        for ids in found_unions.values() {
            for win in ids.windows(2) {
                self.egraph.union(win[0], win[1]);
            }
        }
        self.egraph.rebuild();

        // 3. Run lifting rules
        // Important to fully saturate the egraph
        // No need for a clone because we want to add new terms to the egraph
        // Merges are rule candidates
        log::info!("Running lifting rules");
        let runner = self
            .mk_cvec_less_runner(self.egraph.clone())
            .with_iter_limit(usize::MAX)
            .with_time_limit(Duration::from_secs(1000))
            .with_node_limit(usize::MAX);

        let rewrites = self.lifting_rewrites.iter().collect();
        let (new_egraph, found_unions, stop_reason) =
            self.run_rewrites_with_unions(rewrites, runner);
        assert!(
            matches!(stop_reason, StopReason::Saturated),
            "lifting rules must saturate. Instead, ended due to {:?}",
            stop_reason
        );

        let mut candidates: EqualityMap<L> = EqualityMap::default();
        let clone = self.egraph.clone();
        let extract = Extractor::new(&clone, ExtractableAstSize);
        for ids in found_unions.values() {
            for id1 in ids.clone() {
                for id2 in ids.clone() {
                    let (c1, e1) = extract.find_best(id1);
                    let (c2, e2) = extract.find_best(id2);
                    if c1 == usize::MAX || c2 == usize::MAX {
                        continue;
                    }
                    if let Some(eq) = Equality::new(&e1, &e2) {
                        if e1 != e2 {
                            if let ValidationResult::Valid = L::validate(self, &eq.lhs, &eq.rhs) {
                                if !candidates.contains_key(&eq.name) {
                                    candidates.insert(eq.name.clone(), eq);
                                }
                            }
                        }
                    }
                }
            }
        }
        self.egraph = new_egraph;

        // 4. Run all rules
        // Don't add terms to the egraph (run on a clone)
        // Merges are rule candidates
        log::info!("Running all rules");
        let runner = self
            .mk_cvec_less_runner(self.egraph.clone())
            .with_node_limit(usize::MAX);
        let rewrites: Vec<&Rewrite<L, SynthAnalysis>> = self
            .all_eqs
            .values()
            .flat_map(|eq| &eq.rewrites)
            .chain(self.lifting_rewrites.iter())
            .collect();
        let (_, found_unions, _) = self.run_rewrites_with_unions(rewrites, runner);
        let clone = self.egraph.clone();
        let extract = Extractor::new(&clone, ExtractableAstSize);
        for ids in found_unions.values() {
            for id1 in ids.clone() {
                for id2 in ids.clone() {
                    let (c1, e1) = extract.find_best(id1);
                    let (c2, e2) = extract.find_best(id2);
                    if c1 == usize::MAX || c2 == usize::MAX {
                        continue;
                    }
                    if let Some(eq) = Equality::new(&e1, &e2) {
                        if e1 != e2 {
                            if let ValidationResult::Valid = L::validate(self, &eq.lhs, &eq.rhs) {
                                if !candidates.contains_key(&eq.name) {
                                    candidates.insert(eq.name.clone(), eq);
                                }
                            }
                        }
                    }
                }
            }
        }
        for ids in found_unions.values() {
            for win in ids.windows(2) {
                self.egraph.union(win[0], win[1]);
            }
        }
        self.egraph.rebuild();

        assert!(candidates
            .iter()
            .all(|(_, v)| L::is_allowed_rewrite(&v.lhs, &v.rhs)),);

        let (eqs, _) = self.choose_eqs(candidates);
        self.new_eqs.extend(eqs.clone());
        self.all_eqs.extend(eqs);
        self.egraph.rebuild();
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
            self.run_cvec_synth()
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
    #[clap(long)]
    pub iters: Option<usize>,
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
    // for rule lifting, keeps forbidden rules as well
    #[clap(long)]
    pub keep_all: bool,

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

    #[clap(long, conflicts_with = "iters")]
    pub workload: Option<String>,
}

/// Derivability report.
#[derive(clap::Parser)]
#[clap(rename_all = "kebab-case")]
pub struct DeriveParams {
    in1: String,
    in2: String,
    out: String,
    #[clap(long, default_value = "10")]
    iter_limit: usize,
    #[clap(long, default_value = "300000")]
    node_limit: usize,
    #[clap(long, default_value = "60")]
    time_limit: u64,
    #[clap(long)]
    ci: bool,
    #[clap(long)]
    only_new: bool,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interval<T> {
    pub low: Option<T>,  // None represents -inf
    pub high: Option<T>, // None represents +inf
}

impl<T: Ord + Display> Interval<T> {
    pub fn new(low: Option<T>, high: Option<T>) -> Self {
        if let (Some(a), Some(b)) = (&low, &high) {
            assert!(
                a.le(b),
                "Invalid interval: low must be less than or equal to high\n{} >= {}",
                a,
                b
            );
        }
        Self { low, high }
    }
}

impl<T> Default for Interval<T> {
    fn default() -> Self {
        Self {
            low: None,
            high: None,
        }
    }
}

/// The Signature represents eclass analysis data.
#[derive(Debug, Clone)]
pub struct Signature<L: SynthLanguage> {
    pub cvec: CVec<L>,
    pub simplest: RecExpr<L>,
    pub exact: bool,
    pub is_extractable: bool,
    pub interval: Interval<L::Constant>,
    pub class_type: L::Type,
}

impl<L: SynthLanguage> Signature<L> {
    /// A cvec is defined either it is empty or has at least one `Some` value.
    pub fn is_defined(&self) -> bool {
        self.cvec.is_empty() || self.cvec.iter().any(|v| v.is_some())
    }
}

impl<L: SynthLanguage> egg::Analysis<L> for SynthAnalysis {
    type Data = Signature<L>;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        let mut merge_a = false;
        let mut merge_b = false;
        let cost_fn = |x: &RecExpr<L>| {
            if self.rule_lifting && L::recexpr_is_extractable(x) {
                ExtractableAstSize.cost_rec(x)
            } else {
                AstSize.cost_rec(x)
            }
        };

        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            for i in 0..to.cvec.len() {
                match (to.cvec[i].clone(), from.cvec[i].clone()) {
                    (None, Some(_)) => {
                        to.cvec[i] = from.cvec[i].clone();
                        merge_a = true;
                    }
                    (Some(_), None) => {
                        merge_b = true;
                    }
                    (Some(x), Some(y)) => {
                        assert_eq!(x, y, "cvecs do not match at index {}: {} != {}", i, x, y)
                    }
                    _ => (),
                }
            }
        }

        if from.exact && !to.exact {
            to.exact = true;
            merge_a = true;
        } else if !from.exact && to.exact {
            merge_b = true;
        }

        if from.is_extractable && !to.is_extractable {
            to.is_extractable = true;
            merge_a = true;
        } else if !from.is_extractable && to.is_extractable {
            merge_b = true;
        }

        if cost_fn(&from.simplest) < cost_fn(&to.simplest) {
            to.simplest = from.simplest;
            merge_a = true;
        } else if to.simplest != from.simplest {
            merge_b = true;
        }

        // // New interval is max of mins, min of maxes
        let new_interval = Interval::new(
            match (to.interval.low.as_ref(), from.interval.low.as_ref()) {
                (Some(a), Some(b)) => Some(a.max(b)),
                (a, b) => a.or(b),
            }
            .cloned(),
            match (to.interval.high.as_ref(), from.interval.high.as_ref()) {
                (Some(a), Some(b)) => Some(a.min(b)),
                (a, b) => a.or(b),
            }
            .cloned(),
        );

        if to.interval != new_interval {
            to.interval = new_interval;
            merge_a = true;
        }

        if to.interval != from.interval {
            merge_b = true;
        }

        // conservatively just say that b changed
        DidMerge(merge_a, merge_b)
    }

    fn make(egraph: &EGraph<L, Self>, enode: &L) -> Self::Data {
        let get_cvec = |i: &Id| &egraph[*i].data.cvec;
        let get_simplest = |i: &Id| &egraph[*i].data.simplest;
        Signature {
            class_type: enode.get_type(),
            cvec: enode.eval(egraph.analysis.cvec_len, get_cvec),
            simplest: if enode.is_var() || enode.is_constant() {
                let mut rec = RecExpr::<L>::default();
                rec.add(enode.clone());
                rec
            } else {
                let mut nodes: Vec<L> = vec![];
                let mut map: HashMap<Id, Id> = HashMap::default();
                enode.for_each(|id| {
                    if map.get(&id).is_none() {
                        let s = get_simplest(&id);
                        let i = nodes.len();
                        for n in s.as_ref() {
                            nodes.push(n.clone().map_children(|id| Id::from(usize::from(id) + i)));
                        }

                        map.insert(id, Id::from(nodes.len() - 1));
                    }
                });

                nodes.push(enode.clone().map_children(|id| *map.get(&id).unwrap()));
                RecExpr::from(nodes)
            },
            exact: !enode.is_var() && enode.all(|i| egraph[i].data.exact),
            is_extractable: enode.is_extractable(),
            interval: enode.mk_interval(egraph),
        }
    }

    fn modify(egraph: &mut EGraph<L, Self>, id: Id) {
        match egraph.analysis.constant_fold {
            ConstantFoldMethod::CvecMatching => {
                let sig = &egraph[id].data;
                if sig.exact {
                    let first = sig.cvec.iter().find_map(|x| x.as_ref());
                    if let Some(first) = first {
                        let enode = L::mk_constant(first.clone(), egraph);
                        let added = egraph.add(enode);
                        egraph.union(id, added);
                    }
                }
            }
            ConstantFoldMethod::IntervalAnalysis => {
                let interval = &egraph[id].data.interval;
                if let Interval {
                    low: Some(a),
                    high: Some(b),
                } = interval
                {
                    if a == b {
                        let enode = L::mk_constant(a.clone(), egraph);
                        let added = egraph.add(enode);
                        egraph.union(id, added);
                    }
                }
            }
            ConstantFoldMethod::Lang => {
                if egraph[id].data.exact {
                    L::constant_fold(egraph, id);
                }
            }
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
        should_validate: bool,
    ) -> (EqualityMap<L>, EqualityMap<L>) {
        let mut keepers = EqualityMap::default();
        let mut bads = EqualityMap::default();
        let initial_len = new_eqs.len();
        let mut first_iter = true;

        while !new_eqs.is_empty() {
            // best are last
            new_eqs.sort_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()));

            // take step valid rules from the end of new_eqs
            let mut took = 0;
            if !first_iter {
                while let Some((name, eq)) = new_eqs.pop() {
                    if should_validate {
                        let valid = L::validate(self, &eq.lhs, &eq.rhs);
                        match valid {
                            ValidationResult::Valid => {
                                let old = keepers.insert(name, eq);
                                took += old.is_none() as usize;
                            }
                            _ => {
                                bads.insert(name, eq);
                            }
                        }
                    } else {
                        let old = keepers.insert(name, eq);
                        took += old.is_none() as usize;
                    }

                    if took >= step {
                        break;
                    }
                }
            }

            first_iter = false;
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
            let extract = Extractor::new(&runner.egraph, AstSize);
            new_eqs.clear();

            for ids in runner.roots.chunks(2) {
                if runner.egraph.find(ids[0]) != runner.egraph.find(ids[1]) {
                    let left = extract.find_best(ids[0]).1;
                    let right = extract.find_best(ids[1]).1;
                    if let Some(eq) = Equality::new(&left, &right) {
                        if !self.all_eqs.contains_key(&eq.name) {
                            new_eqs.insert(eq.name.clone(), eq);
                        }
                    }
                }
            }

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
    fn choose_eqs(&mut self, mut new_eqs: EqualityMap<L>) -> (EqualityMap<L>, EqualityMap<L>) {
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

            if n_bad < step {
                // not enough progress:
                if step_idx + 1 == step_sizes.len() {
                    if n_bad == 0 {
                        // break if no progress at lowest step size
                        break 'inner;
                    } // else loop again
                } else {
                    // default: decrease step size
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
pub struct NumberOfAllowedOps;
impl<L: SynthLanguage> egg::CostFunction<L> for NumberOfAllowedOps {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &L, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if !enode.is_allowed() {
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

pub fn assert_eqs_same<L: SynthLanguage>(actual: &[Equality<L>], expected: &[Equality<L>]) {
    let actual: HashSet<&str> = actual.iter().map(|eq| eq.name.as_ref()).collect();
    let expected: HashSet<&str> = expected.iter().map(|eq| eq.name.as_ref()).collect();

    let mut missing = String::new();
    for rule in expected.difference(&actual) {
        let _ = writeln!(&mut missing, "  {}", rule);
    }
    let mut unexpected = String::new();
    for rule in actual.difference(&expected) {
        let _ = writeln!(&mut unexpected, "  {}", rule);
    }
    if missing.len() + unexpected.len() > 0 {
        panic!(
            "Equalities aren't the same!\nMissing:\n{}Unexpected:\n{}",
            missing, unexpected
        )
    }
}

pub fn assert_eqs_same_as_strs<L: SynthLanguage + FromOp>(
    actual: &[Equality<L>],
    expected: &[&str],
) {
    let expected: Vec<Equality<L>> = expected.iter().map(|a| a.parse().unwrap()).collect();
    assert_eqs_same(actual, &expected)
}
