use clap::Clap;
use egg::*;
use rand::SeedableRng;
use rand_pcg::Pcg64;
use serde::{Deserialize, Serialize};
use std::{
    borrow::{Borrow, Cow},
    collections::VecDeque,
};
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

pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<K> = rustc_hash::FxHashSet<K>;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<rustc_hash::FxHasher>>;

pub use bv::*;
pub use equality::*;
pub use util::*;

pub fn letter(i: usize) -> &'static str {
    let alpha = "abcdefghijklmnopqrstuvwxyz";
    &alpha[i..i + 1]
}

#[derive(Debug, Clone)]
pub struct SynthAnalysis {
    pub cvec_len: usize,
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self { cvec_len: 10 }
    }
}

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

    fn score(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> [i32; 4] {
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

        // (-sz_max_pattern, n_vars_rule)
        [
            -n_ops,
            n_vars_rule,
            -sz_lhs.min(sz_rhs),
            -sz_lhs.max(sz_rhs),
        ]
    }

    fn init_synth(synth: &mut Synthesizer<Self>);
    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self>;

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

    fn is_valid(synth: &mut Synthesizer<Self>, lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool;

    fn convert_parse(s: &str) -> RecExpr<Self> {
        s.parse().unwrap()
    }

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

pub struct Synthesizer<L: SynthLanguage> {
    pub params: SynthParams,
    pub rng: Pcg64,
    pub egraph: EGraph<L, SynthAnalysis>,
    initial_egraph: EGraph<L, SynthAnalysis>,
    pub equalities: EqualityMap<L>,
    pub smt_unknown: usize,
}

impl<L: SynthLanguage> Synthesizer<L> {
    pub fn new(params: SynthParams) -> Self {
        let mut synth = Self {
            rng: Pcg64::seed_from_u64(params.seed),
            egraph: Default::default(),
            initial_egraph: Default::default(),
            equalities: Default::default(),
            smt_unknown: 0,
            params,
        };
        L::init_synth(&mut synth);
        synth.initial_egraph = synth.egraph.clone();
        synth
    }

    pub fn ids(&self) -> impl '_ + Iterator<Item = Id> {
        self.egraph.classes().map(|c| c.id)
    }

    fn mk_runner(&self, mut egraph: EGraph<L, SynthAnalysis>) -> Runner<L, SynthAnalysis, ()> {
        let mut runner = Runner::default()
            .with_node_limit(self.params.eqsat_node_limit)
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
            // for conditional rule domains like rationals
            let trunc = 3;
            egraph.analysis.cvec_len = trunc;
            for c in egraph.classes_mut() {
                c.data.cvec.truncate(trunc);
            }
            runner.with_egraph(egraph).with_hook(|r| {
                for c in r.egraph.classes_mut() {
                    if c.nodes.iter().any(|n| n.is_constant()) {
                        c.nodes.retain(|n| n.is_constant());
                    }
                }
                Ok(())
            })
            // runner.with_egraph(egraph)
        };
        runner
    }

    #[inline(never)]
    fn run_rewrites(&mut self) -> EGraph<L, SynthAnalysis> {
        // run the rewrites
        log::info!("running eqsat with {} rules", self.equalities.len());
        let start = Instant::now();
        let rewrites = self.equalities.values().flat_map(|eq| &eq.rewrites);

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
        log::info!(
            "Ran {} rules in {:?}",
            self.equalities.len(),
            start.elapsed()
        );
        runner.egraph
    }

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

        // TODO why is this needed
        new_eqs.retain(|k, _v| !self.equalities.contains_key(k));
        new_eqs
    }

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
            if self.params.linear_cvec_matching {
                let mut terms_ids: Vec<_> =
                    ids.iter().map(|&id| (extract.find_best(id), id)).collect();
                terms_ids.sort_by_key(|x| x.0 .0);
                for win in terms_ids.windows(2) {
                    let (((_c1, e1), id1), ((_c2, e2), id2)) = (&win[0], &win[1]);
                    if let Some(mut eq) = Equality::new(e1, e2) {
                        log::debug!("  Candidate {}", eq);
                        eq.ids = Some((*id1, *id2));
                        new_eqs.insert(eq.name.clone(), eq);
                    }
                }
            } else {
                let mut id_iter = ids.iter();
                while let Some(&id1) = id_iter.next() {
                    for &id2 in id_iter.clone() {
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

        // TODO why is this needed
        new_eqs.retain(|k, _v| !self.equalities.contains_key(k));
        (new_eqs, by_cvec.into_iter().map(|pair| pair.1).collect())
    }

    pub fn run(mut self) -> Report<L> {
        // normalize some params
        if self.params.rules_to_take == 0 {
            self.params.rules_to_take = usize::MAX;
        }
        if self.params.chunk_size == 0 {
            self.params.chunk_size = usize::MAX;
        }

        let mut poison_rules: HashSet<Equality<L>> = HashSet::default();
        let t = Instant::now();
        assert!(self.params.iters > 0);
        for iter in 1..=self.params.iters {
            log::info!("[[[ Iteration {} ]]]", iter);
            let mut layer = L::make_layer(&self, iter);
            layer.retain(|n| !n.all(|id| self.egraph[id].data.exact));

            if iter > self.params.no_constants_above_iter {
                let mut extract = Extractor::new(&self.egraph, NumberOfOps);

                // maps ids to n_ops
                let has_constants: HashSet<Id> = self
                    .ids()
                    .filter_map(|id| {
                        let (_cost, best) = extract.find_best(id);
                        if best.as_ref().iter().any(|n| n.is_constant()) {
                            Some(id)
                        } else {
                            None
                        }
                    })
                    .collect();

                layer.retain(|n| n.all(|id| !has_constants.contains(&id)));
            }

            log::info!("Made layer of {} nodes", layer.len());
            for chunk in layer.chunks(self.params.chunk_size) {
                log::info!(
                    "egraph n={}, e={}",
                    self.egraph.total_size(),
                    self.egraph.number_of_classes(),
                );
                for node in chunk {
                    self.egraph.add(node.clone());
                }
                'inner: loop {
                    let run_rewrites_before = Instant::now();
                    if !self.params.no_run_rewrites {
                        self.run_rewrites();
                    }
                    let run_rewrites = run_rewrites_before.elapsed().as_secs_f64();

                    let rule_discovery_before = Instant::now();
                    log::info!("cvec matching...");
                    let (new_eqs, id_groups) = if self.params.no_conditionals {
                        self.cvec_match()
                    } else {
                        (self.cvec_match_pair_wise(), vec![])
                    };
                    let rule_discovery = rule_discovery_before.elapsed().as_secs_f64();
                    log::info!("{} candidate eqs", new_eqs.len());

                    let new_eqs: EqualityMap<L> = new_eqs
                        .into_iter()
                        .filter(|eq| !poison_rules.contains(&eq.1))
                        .collect();

                    log::info!(
                        "egraph n={}, e={}",
                        self.egraph.total_size(),
                        self.egraph.number_of_classes(),
                    );

                    let rule_minimize_before = Instant::now();
                    let (eqs, bads) = if self.params.minimize {
                        self.choose_eqs(new_eqs)
                    } else {
                        self.choose_eqs_old(new_eqs)
                    };

                    let rule_minimize = rule_minimize_before.elapsed().as_secs_f64();

                    for bad in bads {
                        log::info!("adding {} to poison set", bad.0);
                        poison_rules.insert(bad.1);
                    }
                    if eqs.is_empty() {
                        log::info!("Stopping early, no eqs");
                        break 'inner;
                    }

                    log::info!("Chose {} good rules", eqs.len());
                    for eq in eqs.values() {
                        log::info!("  {}", eq);
                        if !self.params.no_run_rewrites {
                            assert!(!self.equalities.contains_key(&eq.name));
                        }
                        if let Some((i, j)) = eq.ids {
                            self.egraph.union(i, j);
                        }
                    }

                    let n_eqs = eqs.len();
                    self.equalities.extend(eqs);

                    // TODO check formatting for Learned...
                    log::info!("Time taken in... run_rewrites: {}, rule discovery: {}, rule minimization: {}",
                    run_rewrites,  rule_discovery, rule_minimize);
                    if self.params.minimize || n_eqs < self.params.rules_to_take {
                        for ids in id_groups {
                            for win in ids.windows(2) {
                                self.egraph.union(win[0], win[1]);
                            }
                        }
                        self.egraph.rebuild();

                        log::info!("Stopping early, took all eqs");
                        break 'inner;
                    }
                }
            }
        }

        let time = t.elapsed().as_secs_f64();
        let num_rules = self.equalities.len();
        let mut eqs: Vec<_> = self.equalities.into_iter().map(|(_, eq)| eq).collect();
        eqs.sort_by_key(|eq| eq.score());
        eqs.reverse();
        println!("Learned {} rules in {:?}", num_rules, time);
        for eq in &eqs {
            println!("  {:?}   {}", eq.score(), eq);
        }
        println!("Learned {} rules in {:?}", num_rules, time);

        Report {
            params: self.params,
            time,
            num_rules,
            eqs,
            smt_unknown: self.smt_unknown,
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
pub struct Report<L: SynthLanguage> {
    pub params: SynthParams,
    pub time: f64,
    pub num_rules: usize,
    pub smt_unknown: usize,
    pub eqs: Vec<Equality<L>>,
}

#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
struct SlimReport<L: SynthLanguage> {
    time: f64,
    eqs: Vec<Equality<L>>,
}

#[derive(Clap, Deserialize, Serialize)]
#[clap(rename_all = "kebab-case")]
pub struct SynthParams {
    #[clap(long, default_value = "0")]
    pub seed: u64,
    /// How many random values to add to the cvecs
    #[clap(long, default_value = "0")]
    pub n_samples: usize,
    #[clap(long, default_value = "3")]
    pub variables: usize,

    ///////////////////
    // search params //
    ///////////////////
    #[clap(long, default_value = "1")]
    pub iters: usize,
    /// 0 is unlimited
    #[clap(long, default_value = "0")]
    pub rules_to_take: usize,
    /// 0 is unlimited
    #[clap(long, default_value = "100000")]
    pub chunk_size: usize,
    #[clap(long, conflicts_with = "rules-to-take")]
    pub minimize: bool,
    /// disallows enumerating terms with constants past this iteration
    #[clap(long, default_value = "999999")]
    pub no_constants_above_iter: usize,
    #[clap(long)]
    pub no_conditionals: bool,
    #[clap(long)]
    pub no_run_rewrites: bool,
    #[clap(long)]
    pub linear_cvec_matching: bool,
    #[clap(long, default_value = "out.json")]
    pub outfile: String,

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
    /// Only for bv, makes it do a complete cvec
    #[clap(long, conflicts_with = "important_cvec_offsets")]
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
    #[clap(long, default_value = "0")]
    pub num_fuzz: usize,
    #[clap(long, conflicts_with = "num-fuzz")]
    pub use_smt: bool,
}

#[derive(Clap)]
#[clap(rename_all = "kebab-case")]
pub struct DeriveParams {
    in1: String,
    in2: String,
    out: String,
    #[clap(long, default_value = "5")]
    iter_limit: usize,
}

#[derive(Clap)]
#[clap(rename_all = "kebab-case")]
pub struct ConvertParams {
    cvc_log: String,
    out: String,
}

#[derive(Clap)]
#[clap(rename_all = "kebab-case")]
pub enum Command {
    Synth(SynthParams),
    Derive(DeriveParams),
    ConvertSexp(ConvertParams),
}

pub type EqualityMap<L> = IndexMap<Arc<str>, Equality<L>>;
pub type CVec<L> = Vec<Option<<L as SynthLanguage>::Constant>>;

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

#[derive(Debug, Clone)]
pub struct Signature<L: SynthLanguage> {
    pub cvec: CVec<L>,
    pub exact: bool,
}

impl<L: SynthLanguage> Signature<L> {
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
            to.exact |= from.exact;
        }

        ord
    }

    fn make(egraph: &EGraph<L, Self>, enode: &L) -> Self::Data {
        let get_cvec = |i: &Id| &egraph[*i].data.cvec;
        Signature {
            cvec: enode.eval(egraph.analysis.cvec_len, get_cvec),
            exact: !enode.is_var() && enode.all(|i| egraph[i].data.exact),
        }
    }

    fn modify(egraph: &mut EGraph<L, Self>, id: Id) {
        let sig = &egraph[id].data;
        if sig.exact {
            let first = sig.cvec.iter().find_map(|x| x.as_ref());
            if let Some(first) = first {
                let enode = L::mk_constant(first.clone());
                let added = egraph.add(enode);
                egraph.union(id, added);
            }
        }
    }
}

impl<L: SynthLanguage> Synthesizer<L> {
    #[inline(never)]
    fn choose_eqs_old(&mut self, mut new_eqs: EqualityMap<L>) -> (EqualityMap<L>, EqualityMap<L>) {
        assert!(!self.params.minimize);
        let n = self.params.rules_to_take;

        let t = Instant::now();
        let n_new_eqs = new_eqs.len();
        log::info!("Choosing from {} rules...", n_new_eqs);

        let mut keepers = EqualityMap::default();
        let mut bads = EqualityMap::default();
        // make the best last
        new_eqs.sort_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()));
        while let Some((name, eq)) = new_eqs.pop() {
            let rule_validation = Instant::now();
            let valid = L::is_valid(self, &eq.lhs, &eq.rhs);
            log::info!(
                "Time taken in validation: {}",
                rule_validation.elapsed().as_secs_f64()
            );

            if valid {
                keepers.insert(name, eq);
            } else {
                bads.insert(name, eq);
            }
            if new_eqs.is_empty() || keepers.len() >= n {
                log::info!(
                    "Nothing to do. new_eqs len: {}, keepers len: {}",
                    new_eqs.len(),
                    keepers.len()
                );
                break;
            }

            let rewrites = self
                .equalities
                .values()
                .flat_map(|eq| &eq.rewrites)
                .chain(keepers.values().flat_map(|eq| &eq.rewrites));

            let mut runner = self.mk_runner(self.initial_egraph.clone());

            for candidate_eq in new_eqs.values() {
                runner = runner.with_expr(&L::instantiate(&candidate_eq.lhs));
                runner = runner.with_expr(&L::instantiate(&candidate_eq.rhs));
            }

            runner = runner.run(rewrites);
            println!("{:?}, {:?}", runner.stop_reason, runner.iterations.len());

            let mut extract = Extractor::new(&runner.egraph, AstSize);
            let old_len = new_eqs.len();
            new_eqs = runner
                .roots
                .chunks(2)
                .filter(|ids| runner.egraph.find(ids[0]) != runner.egraph.find(ids[1]))
                .filter_map(|ids| {
                    let left = extract.find_best(ids[0]).1;
                    let right = extract.find_best(ids[1]).1;
                    if let Some(eq) = Equality::new(&left, &right) {
                        if !self.equalities.contains_key(&eq.name) {
                            return Some((eq.name.clone(), eq));
                        }
                    }
                    None
                })
                .collect();

            let redundant = old_len - new_eqs.len();
            new_eqs.sort_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()));

            log::info!(
                "Minimizing... threw away {} rules, {} / {} remain",
                redundant,
                new_eqs.len(),
                n_new_eqs
            );
        }

        log::info!(
            "Minimized {}->{} rules in {:?}",
            n_new_eqs,
            keepers.len(),
            t.elapsed()
        );
        (keepers, bads)
    }

    #[inline(never)]
    fn choose_eqs(&mut self, new_eqs: EqualityMap<L>) -> (EqualityMap<L>, EqualityMap<L>) {
        assert!(self.params.minimize);

        let t = Instant::now();

        let rule_validation = Instant::now();
        let (new_eqs, bads): (EqualityMap<L>, EqualityMap<L>) = new_eqs
            .into_iter()
            .partition(|(_name, eq)| L::is_valid(self, &eq.lhs, &eq.rhs));

        log::info!(
            "Time taken in validation: {}",
            rule_validation.elapsed().as_secs_f64()
        );

        let n_new_eqs = new_eqs.len();
        log::info!("Minimizing {} rules...", n_new_eqs);
        let mut flat: VecDeque<Equality<L>> = new_eqs.into_iter().map(|(_, eq)| eq).collect();
        let mut test = vec![];

        for mut n_chunks in (2..).map(|i| 1 << i) {
            if n_chunks > flat.len() {
                if n_chunks >= flat.len() * 2 {
                    break;
                }
                n_chunks = flat.len();
            }

            log::info!("n chunks {}", n_chunks);

            let mut chunk_sizes = vec![flat.len() / n_chunks; n_chunks];
            for i in 0..(flat.len() % n_chunks) {
                chunk_sizes[i] += 1;
            }
            assert_eq!(flat.len(), chunk_sizes.iter().sum::<usize>());

            for size in chunk_sizes {
                let n_new_eqs_this_loop = flat.len();
                test.clear();
                for _ in 0..size {
                    test.push(flat.pop_front().unwrap());
                }
                assert_eq!(test.len(), size);

                log::info!("chunk size {}, flat {}", size, flat.len());

                let mut rewrites: Vec<_> = self
                    .equalities
                    .values()
                    .flat_map(|eq| &eq.rewrites)
                    .chain(flat.iter().flat_map(|eq| &eq.rewrites))
                    .collect();

                rewrites.sort_by_key(|rw| rw.name());
                rewrites.dedup_by_key(|rw| rw.name());

                let mut runner = self.mk_runner(self.initial_egraph.clone());

                for candidate_eq in &test {
                    runner = runner.with_expr(&L::instantiate(&candidate_eq.lhs));
                    runner = runner.with_expr(&L::instantiate(&candidate_eq.rhs));
                }

                runner = runner.run(rewrites);

                let mut extract = Extractor::new(&runner.egraph, AstSize);
                flat.extend(runner.roots.chunks(2).zip(&test).filter_map(|(ids, eq)| {
                    if runner.egraph.find(ids[0]) == runner.egraph.find(ids[1]) {
                        None
                    } else {
                        let lhs = extract.find_best(ids[0]).1;
                        let rhs = extract.find_best(ids[1]).1;
                        // TODO get ids so they can be unioned by `run`
                        if let Some(mut eq2) = Equality::new(&lhs, &rhs) {
                            eq2.ids = eq.ids;
                            Some(eq2)
                        } else {
                            None
                        }
                    }
                }));

                log::info!(
                    "Minimizing... threw away {} rules, {} / {} remain",
                    n_new_eqs_this_loop - flat.len(),
                    flat.len(),
                    n_new_eqs
                );
            }
        }

        log::info!(
            "Minimized {}->{} rules in {:?}",
            n_new_eqs,
            flat.len(),
            t.elapsed()
        );

        (
            flat.into_iter().map(|eq| (eq.name.clone(), eq)).collect(),
            bads,
        )
    }
}

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
