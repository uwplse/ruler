use clap::Clap;
use egg::*;
use indexmap::IndexMap;
use rand::SeedableRng;
use rand_pcg::Pcg64;
use serde::{Deserialize, Serialize};
use std::rc::Rc;
use std::{
    borrow::{Borrow, Cow},
    collections::VecDeque,
};
use std::{cmp::Ordering, hash::Hash};
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    time::Duration,
    time::Instant,
};

mod equality;
pub use equality::*;

const ITER_LIMIT: usize = 2;

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

pub trait SynthLanguage: egg::Language + 'static {
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

    fn score(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> [i32; 3] {
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

        // (-sz_max_pattern, n_vars_rule)
        [n_vars_rule, -sz_lhs.min(sz_rhs), -sz_lhs.max(sz_rhs)]
    }

    fn init_synth(synth: &mut Synthesizer<Self>);
    fn make_layer(synth: &Synthesizer<Self>) -> Vec<Self>;

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

    fn is_valid(rng: &mut Pcg64, lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool;

    fn main() {
        let _ = env_logger::builder().try_init();
        let params = SynthParams::parse();
        let outfile = params.outfile.clone();
        let syn = Synthesizer::<Self>::new(params);
        let report = syn.run();
        let file = std::fs::File::create(&outfile)
            .unwrap_or_else(|_| panic!("Failed to open '{}'", outfile));
        serde_json::to_writer_pretty(file, &report).expect("failed to write json");
    }
}

pub struct Synthesizer<L: SynthLanguage> {
    pub params: SynthParams,
    pub rng: Pcg64,
    pub egraph: EGraph<L, SynthAnalysis>,
    pub equalities: EqualityMap<L>,
}

impl<L: SynthLanguage> Synthesizer<L> {
    pub fn new(params: SynthParams) -> Self {
        let mut synth = Self {
            rng: Pcg64::seed_from_u64(params.seed),
            egraph: Default::default(),
            equalities: Default::default(),
            params,
        };
        L::init_synth(&mut synth);
        synth
    }

    pub fn ids(&self) -> impl '_ + Iterator<Item = Id> {
        self.egraph.classes().map(|c| c.id)
    }

    #[inline(never)]
    fn run_rewrites(&mut self) -> EGraph<L, SynthAnalysis> {
        // run the rewrites
        log::info!("running eqsat with {} rules", self.equalities.len());
        let rewrites = self.equalities.values().flat_map(|eq| &eq.rewrites);
        let mut runner = Runner::<L, _, ()>::new(self.egraph.analysis.clone())
            .with_egraph(self.egraph.clone())
            .with_node_limit(usize::MAX)
            .with_iter_limit(ITER_LIMIT)
            .with_scheduler(SimpleScheduler)
            .run(rewrites);

        // update the clean egraph based on any unions that happened
        let mut found_unions = HashMap::new();
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
        runner.egraph
    }

    #[inline(never)]
    fn cvec_match_pair_wise(&self) -> EqualityMap<L> {
        let mut by_cvec: IndexMap<CVec<L>, Vec<Id>> = IndexMap::new();

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
    fn cvec_match(&self) -> EqualityMap<L> {
        // build the cvec matching data structure
        let mut by_cvec: IndexMap<&CVec<L>, Vec<Id>> = IndexMap::new();

        let not_all_nones = self
            .ids()
            .filter(|id| !&self.egraph[*id].data.cvec.iter().all(|v| v == &None));
        for id in not_all_nones {
            let class = &self.egraph[id];
            by_cvec.entry(&class.data.cvec).or_default().push(class.id);
        }

        log::info!("# unique cvecs: {}", by_cvec.len());

        let mut new_eqs = EqualityMap::default();
        let mut extract = Extractor::new(&self.egraph, AstSize);
        for ids in by_cvec.values() {
            if false {
                // limit id_iter so the cartesian product doesn't get too big
                let mut id_iter = ids.iter().take(50);
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
            } else {
                // try linear cvec matching
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
            }
        }

        // TODO why is this needed
        new_eqs.retain(|k, _v| !self.equalities.contains_key(k));
        new_eqs
    }

    pub fn run(mut self) -> Report<L> {
        let starting_egraph = self.egraph.clone();
        let mut poison_rules: HashSet<Equality<L>> = HashSet::new();
        let t = Instant::now();
        for _ in 0..self.params.iters {
            let layer = L::make_layer(&self);
            for chunk in layer.chunks(self.params.chunk_size) {
                for node in chunk {
                    self.egraph.add(node.clone());
                }
                loop {
                    self.run_rewrites();
                    let new_eqs = if self.params.no_conditionals {
                        self.cvec_match()
                    } else {
                        self.cvec_match_pair_wise()
                    };

                    let new_eqs: EqualityMap<L> = new_eqs
                        .into_iter()
                        .filter(|eq| !poison_rules.contains(&eq.1))
                        .collect();

                    log::info!(
                        "egraph n={}, e={}",
                        self.egraph.total_size(),
                        self.egraph.number_of_classes(),
                    );

                    let rng = &mut self.rng;
                    let (eqs, bads) = if self.params.minimize {
                        choose_eqs(
                            rng,
                            &starting_egraph,
                            &self.equalities,
                            new_eqs,
                            self.params.rules_to_take,
                        )
                    } else {
                        choose_eqs_old(rng, &self.equalities, new_eqs, self.params.rules_to_take)
                    };

                    for bad in bads {
                        log::info!("adding {} to poison set", bad.0);
                        poison_rules.insert(bad.1);
                    }
                    if eqs.is_empty() {
                        break;
                    }

                    log::info!("Chose {} good rules", eqs.len());
                    // println!("Chose {} rules", eqs.len());
                    for eq in eqs.values() {
                        println!("chosen: {}", eq);
                        log::info!("  {}", eq);
                        assert!(!self.equalities.contains_key(&eq.name));
                    }
                    self.equalities.extend(eqs);
                }
            }
        }
        let time = t.elapsed().as_secs_f64();
        let num_rules = self.equalities.len();
        let eqs: Vec<_> = self.equalities.into_iter().map(|(_, eq)| eq).collect();
        println!("Learned {} rules in {:?}", num_rules, time);
        for eq in &eqs {
            println!("  {:?}   {}", eq.score(), eq);
        }

        Report {
            time,
            num_rules,
            eqs,
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
pub struct Report<L: SynthLanguage> {
    pub time: f64,
    pub num_rules: usize,
    pub eqs: Vec<Equality<L>>,
}

#[derive(Clap)]
#[clap(rename_all = "kebab-case")]
pub struct SynthParams {
    #[clap(long, default_value = "0")]
    pub seed: u64,
    #[clap(long, default_value = "10")]
    pub n_samples: usize,
    // #[clap(long, default_value = "")]
    // pub constants: Vec<String>,
    #[clap(long, default_value = "1")]
    pub variables: usize,
    // search params
    #[clap(long, default_value = "1")]
    pub iters: usize,
    #[clap(long, default_value = "1")]
    pub rules_to_take: usize,
    #[clap(long, default_value = "999999999999")]
    pub chunk_size: usize,
    #[clap(long)]
    pub minimize: bool,
    #[clap(long)]
    pub no_conditionals: bool,
    #[clap(long, default_value = "out.json")]
    pub outfile: String,
}

pub type EqualityMap<L> = IndexMap<Rc<str>, Equality<L>>;
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
}

#[derive(Debug, Clone)]
pub struct Signature<L: SynthLanguage> {
    pub cvec: CVec<L>,
    pub exact: bool,
}

impl<L: SynthLanguage> Signature<L> {
    pub fn is_defined(&self) -> bool {
        self.cvec.iter().any(|v| v.is_some())
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

#[inline(never)]
fn choose_eqs_old<L: SynthLanguage>(
    rng: &mut Pcg64,
    old_eqs: &EqualityMap<L>,
    mut new_eqs: EqualityMap<L>,
    n: usize,
) -> (EqualityMap<L>, EqualityMap<L>) {
    let t = Instant::now();
    let n_new_eqs = new_eqs.len();
    log::info!("Choosing from {} rules...", n_new_eqs);

    let mut keepers = EqualityMap::default();
    let mut bads = EqualityMap::default();
    // make the best last
    new_eqs.sort_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()));
    while let Some((name, eq)) = new_eqs.pop() {
        if L::is_valid(rng, &eq.lhs, &eq.rhs) {
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

        let rewrites = old_eqs
            .values()
            .flat_map(|eq| &eq.rewrites)
            .chain(keepers.values().flat_map(|eq| &eq.rewrites));

        let mut runner = Runner::default()
            .with_iter_limit(ITER_LIMIT)
            .with_scheduler(SimpleScheduler)
            .with_node_limit(1_000_000);

        for candidate_eq in new_eqs.values() {
            runner = runner.with_expr(&L::instantiate(&candidate_eq.lhs));
            runner = runner.with_expr(&L::instantiate(&candidate_eq.rhs));
        }

        runner = runner.run(rewrites);

        let mut redundant = HashSet::new();
        for (eq, ids) in new_eqs.values().zip(runner.roots.chunks(2)) {
            if runner.egraph.find(ids[0]) == runner.egraph.find(ids[1]) {
                redundant.insert(eq.name.clone());
            }
        }
        // Indexmap::retain preserves the score ordering
        new_eqs.retain(|name, _eq| !redundant.contains(name));
        log::info!(
            "Minimizing... threw away {} rules, {} / {} remain",
            redundant.len(),
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
fn choose_eqs<L: SynthLanguage>(
    rng: &mut Pcg64,
    egraph: &EGraph<L, SynthAnalysis>,
    old_eqs: &EqualityMap<L>,
    new_eqs: EqualityMap<L>,
    _n: usize,
) -> (EqualityMap<L>, EqualityMap<L>) {
    let t = Instant::now();
    let (new_eqs, bads): (EqualityMap<L>, EqualityMap<L>) = new_eqs
        .into_iter()
        .partition(|(_name, eq)| L::is_valid(rng, &eq.lhs, &eq.rhs));

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

            let mut rewrites: Vec<_> = old_eqs
                .values()
                .flat_map(|eq| &eq.rewrites)
                .chain(flat.iter().flat_map(|eq| &eq.rewrites))
                .collect();

            rewrites.sort_by_key(|rw| rw.name());
            rewrites.dedup_by_key(|rw| rw.name());

            let mut runner = Runner::default()
                .with_egraph(egraph.clone())
                .with_iter_limit(ITER_LIMIT)
                .with_scheduler(SimpleScheduler)
                .with_time_limit(Duration::from_secs(60))
                .with_node_limit(10_000_000);

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
