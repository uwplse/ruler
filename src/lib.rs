use egg::*;
use indexmap::IndexMap;
use lazy_static::lazy_static;
use num::bigint::{BigInt, RandBigInt, Sign, ToBigInt};
use num::{rational::Ratio, Signed, ToPrimitive};
use rand::SeedableRng;
use rand::{prelude::SliceRandom, Rng};
use rand_pcg::Pcg64;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    fmt::Formatter,
    time::Duration,
    time::Instant,
};
use std::{convert::TryInto, rc::Rc};
use std::{hash::Hash, sync::Mutex};
type Runner = egg::Runner<Math, SynthAnalysis, ()>;
type Pattern<L = Math> = egg::Pattern<L>;
type RecExpr<L = Math> = egg::RecExpr<L>;
type Rewrite<L = Math, A = SynthAnalysis> = egg::Rewrite<L, A>;
type EGraph<L = Math, A = SynthAnalysis> = egg::EGraph<L, A>;

#[macro_export]
macro_rules! num {
    ($x:expr, $y:expr) => {
        Constant {
            val: num::rational::Ratio::new($x, $y),
        }
    };
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Constant {
    pub val: Ratio<BigInt>,
}

pub static PREC: u32 = 2048;
pub static MASK: i32 = 10;
pub static SEED: u64 = 5;
pub static VALID_SEED: u64 = 7;
pub static EPS: f32 = 1e-5;

lazy_static! {
    static ref RNG: Mutex<Pcg64> = Mutex::new(Pcg64::seed_from_u64(SEED));
}

pub type CVec = Vec<Option<Constant>>;
pub type Ctx = HashMap<&'static str, Constant>;

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} / {}", self.val.numer(), self.val.denom())
    }
}

impl std::str::FromStr for Constant {
    type Err = String;
    fn from_str(s: &str) -> Result<Constant, Self::Err> {
        // TODO: I am not sure about this.
        let nd: Vec<&str> = s.split(" ").collect();
        if nd.len() == 2 {
            let numer = nd[0].parse::<BigInt>().unwrap();
            let denom = nd[1].parse::<BigInt>().unwrap();
            Ok(Constant {
                val: Ratio::new(numer, denom),
            })
        } else {
            Err(format!("'{}' is not a valid value for Constant", s))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Signature {
    cvec: CVec,
    exact: bool,
}

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "pow" = Pow([Id; 2]),
        "abs" = Abs(Id),
        "recip" = Reciprocal(Id),
        "~" = Neg(Id),
        Num(Constant),
        Var(egg::Symbol),
    }
}

// if there is an input that is bad, add it to the cvec and recompute them.
// TODO: recompute all cvecs for egraph from there.
// Altenative: add the rule to poison set. When you pick best, dont pick from poison rule
fn is_valid(lhs: Pattern, rhs: Pattern) -> bool {
    let mut env: Ctx = HashMap::new();
    let mut rng = Pcg64::seed_from_u64(VALID_SEED);
    let mut valid = false;
    let lhs = instantiate(&lhs);
    let rhs = instantiate(&rhs);
    // add more random inputs for testing
    for _ in 0..100000 {
        let (xn, yn, zn) = (rng.gen_bigint(32), rng.gen_bigint(32), rng.gen_bigint(32));
        let (xd, yd, zd) = (rng.gen_bigint(32), rng.gen_bigint(32), rng.gen_bigint(32));
        env.insert("x", num!(xn, xd));
        env.insert("y", num!(yn, yd));
        env.insert("z", num!(zn, zd));
        let l = eval(&env.clone(), lhs.as_ref());
        let r = eval(&env, rhs.as_ref());
        match (l, r) {
            (None, None) => {
                valid = true;
            },
            (None, Some(_)) | (Some(_), None) => {
                return false
            },
            (Some(l), Some(r)) => {
                if l == r {
                    valid = true;
                } else {
                    return false
                }
            }
        }
    }
    valid
}

pub fn eval(ctx: &Ctx, expr: &[Math]) -> Option<Constant> {
    match expr.last().expect("empty expr!") {
        Math::Num(n) => Some(n.clone()),
        Math::Var(v) => {
            let x = ctx.get("x").cloned();
            let y = ctx.get("y").cloned();
            let z = ctx.get("z").cloned();
            if v.as_str() == "x" {
                Some(x.unwrap())
            } else if v.as_str() == "y" {
                Some(y.unwrap())
            } else if v.as_str() == "z" {
                Some(z.unwrap())
            } else {
                panic!("eval: currently only supports rules with 3 variables");
            }
        }
        Math::Add([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            if e1 == None || e2 == None {
                None
            } else {
                Some(Constant {
                    val: (e1.unwrap().val + e2.unwrap().val),
                })
            }
        }
        Math::Sub([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            if e1 == None || e2 == None {
                None
            } else {
                Some(Constant {
                    val: (e1.unwrap().val - e2.unwrap().val),
                })
            }
        }
        Math::Mul([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            if e1 == None || e2 == None {
                None
            } else {
                Some(Constant {
                    val: (e1.unwrap().val - e2.unwrap().val),
                })
            }
        }
        Math::Div([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            if e1 == None || e2 == None {
                None
            } else {
                Some(Constant {
                    val: (e1.unwrap().val / e2.unwrap().val),
                })
            }
        }
        Math::Abs(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            if e1 == None {
                None
            } else {
                Some(Constant {
                    val: e1.unwrap().val.abs(),
                })
            }
        }
        Math::Pow([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            let power = e2.unwrap().val.numer().to_i32();
            if e1 == None || power == None {
                None
            } else {
                Some(Constant {
                    val: e1.unwrap().val.pow(power.unwrap()),
                })
            }
        }
        Math::Reciprocal(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            if e1 == None {
                None
            } else {
                Some(Constant {
                    val: e1.unwrap().val.recip(),
                })
            }
        }
        Math::Neg(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            if e1 == None {
                None
            } else {
                Some(Constant {
                    val: -e1.unwrap().val,
                })
            }
        }
    }
}

pub fn generalize(expr: &RecExpr<Math>, map: &mut HashMap<Symbol, Var>) -> Pattern<Math> {
    let alpha = b"abcdefghijklmnopqrstuvwxyz";
    let nodes: Vec<_> = expr
        .as_ref()
        .iter()
        .map(|n| match n {
            Math::Var(sym) => {
                let var = if let Some(var) = map.get(&sym) {
                    *var
                } else {
                    let var = format!("?{}", alpha[map.len()] as char).parse().unwrap();
                    map.insert(*sym, var);
                    var
                };
                ENodeOrVar::Var(var)
            }
            n => ENodeOrVar::ENode(n.clone()),
        })
        .collect();

    Pattern::from(PatternAst::from(nodes))
}

fn instantiate(pattern: &Pattern<Math>) -> RecExpr<Math> {
    let nodes: Vec<_> = pattern
        .ast
        .as_ref()
        .iter()
        .map(|n| match n {
            ENodeOrVar::ENode(n) => n.clone(),
            ENodeOrVar::Var(v) => {
                let s = v.to_string();
                assert!(s.starts_with('?'));
                Math::Var(s[1..].into())
            }
        })
        .collect();

    RecExpr::from(nodes)
}

impl Signature {
    fn fold1(&self, mut f: impl FnMut(&Constant) -> Option<Constant>) -> Self {
        let cvec = self
            .cvec
            .iter()
            .map(|x| x.as_ref().and_then(&mut f))
            .collect();
        Self {
            cvec,
            exact: self.exact,
        }
    }
    fn fold2(
        &self,
        other: &Self,
        mut f: impl FnMut(Constant, Constant) -> Option<Constant>,
    ) -> Self {
        if !self.cvec.is_empty() && !other.cvec.is_empty() {
            assert_eq!(self.cvec.len(), other.cvec.len());
        }

        let compute = |(x, y): (&Option<Constant>, &Option<Constant>)| match (x, y) {
            (Some(n1), Some(n2)) => f(n1.clone(), n2.clone()),
            (_, _) => None,
        };
        let cvec = self.cvec.iter().zip(&other.cvec).map(compute).collect();
        Self {
            cvec,
            exact: self.exact && other.exact,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SynthAnalysis {
    cvec_len: usize,
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self { cvec_len: 10 }
    }
}

impl Analysis<Math> for SynthAnalysis {
    type Data = Signature;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        let mut to_exact_changed = false;
        let mut to_cvec_changed = false;

        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            for i in 0..to.cvec.len() {
                match (to.cvec[i].clone(), from.cvec[i].clone()) {
                    (None, Some(_)) => {
                        to.cvec[i] = from.cvec[i].clone();
                        to_cvec_changed = true;
                    }
                    (_, _) => {}
                }
            }
            if !to.exact && from.exact {
                to.exact = true;
                to_exact_changed = true;
            }
            return to_exact_changed || to_cvec_changed;
        }
        false
    }

    fn make(egraph: &EGraph<Math, Self>, enode: &Math) -> Self::Data {
        // a closure to get the cvec for an eclass
        let v = |i: &Id| &egraph[*i].data;
        let param = &egraph.analysis;
        match enode {
            Math::Neg(a) => v(a).fold1(|a| Some(Constant { val: (-a.val.clone()) })),
            Math::Add([a, b]) => v(a).fold2(v(b), |a, b| {
                let inner = a.val + b.val;
                Some(Constant { val: inner })
            }),
            Math::Sub([a, b]) => v(a).fold2(v(b), |a, b| {
                let inner = a.val - b.val;
                Some(Constant { val: inner })
            }),
            Math::Mul([a, b]) => v(a).fold2(v(b), |a, b| {
                let inner = a.val * b.val;
                Some(Constant { val: inner })
            }),
            Math::Num(n) => {
                let cv: Vec<Option<Constant>> =
                    (0..param.cvec_len).map(|_| Some(n.clone())).collect();
                Signature {
                    cvec: cv.clone(),
                    exact: true,
                }
            }
            Math::Var(_) => Signature {
                cvec: vec![],
                exact: false,
            },
            Math::Div([a, b]) => v(a).fold2(v(b), |a, b| {
                let inner = a.val / b.val;
                Some(Constant { val: inner })
            }),
            Math::Abs(a) => v(a).fold1(|a| {
                let inner = a.val.abs();
                Some(Constant { val: inner })
            }),
            Math::Pow([a, b]) => v(a).fold2(v(b), |a, b| {
                let inner = a.val.pow(b.val.to_i32().unwrap());
                Some(Constant { val: inner })
            }),
            Math::Reciprocal(a) => v(a).fold1(|a| {
                let inner = a.val.recip();
                Some(Constant { val: inner })
            }),
        }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        let sig = &egraph[id].data;
        let cv = &sig.cvec;
        let exact = sig.exact;
        if cv.is_empty() || cv.contains(&None) {
            return;
        }
        let first = cv[0].clone();
        if cv.iter().all(|x| *x == first) {
            match first {
                Some(n) => {
                    let added = egraph.add(Math::Num(n.clone()));
                    if exact {
                        egraph.union(id, added);
                    }
                }
                None => {}
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SynthParams {
    pub seed: u64,
    pub n_samples: usize,
    pub constants: Vec<Constant>,
    pub variables: Vec<egg::Symbol>,
    // search params
    pub iters: usize,
    pub rules_to_take: usize,
    pub chunk_size: usize,
}

type EqualityMap<L = Math, A = SynthAnalysis> = IndexMap<Rc<str>, Equality<L, A>>;

#[allow(dead_code)]
pub struct Synthesizer {
    params: SynthParams,
    //rng: Pcg64,
    egraph: EGraph,
    equalities: EqualityMap,
}

impl Synthesizer {
    pub fn new(params: SynthParams) -> Self {
        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: params.n_samples + params.constants.len(),
        });
        let mut var_cvec_map: IndexMap<&Symbol, Vec<Option<Constant>>> = IndexMap::new();
        let mut rng = Pcg64::seed_from_u64(params.seed);
        for var in &params.variables {
            let id = egraph.add(Math::Var(*var));
            let mut cvec: Vec<Option<Constant>> = (0..params.n_samples)
                .map(|_| Some(num!(rng.gen_bigint(32), rng.gen_bigint(32))))
                .collect();
            cvec.push(Some(num!(0.to_bigint().unwrap(), 1.to_bigint().unwrap())));
            cvec.push(Some(num!(1.to_bigint().unwrap(), 1.to_bigint().unwrap())));
            cvec.push(Some(num!(-1.to_bigint().unwrap(), 1.to_bigint().unwrap())));
            egraph[id].data.cvec = cvec.clone();
            var_cvec_map.insert(var, cvec.clone());
            println!("var: {}, cvec: {:?}", var, cvec.clone());
        }

        for n in &params.constants {
            egraph.add(Math::Num(n.clone()));
        }

        Self {
            egraph,
            params,
            equalities: Default::default(),
        }
    }

    fn ids(&self) -> impl '_ + Iterator<Item = Id> {
        self.egraph.classes().map(|c| c.id)
    }

    fn make_layer(&self) -> Vec<Math> {
        let mut to_add = vec![];
        for i in self.ids() {
            for j in self.ids() {
                if self.egraph[i].data.exact && self.egraph[j].data.exact {
                    continue;
                }
                to_add.push(Math::Add([i, j]));
                to_add.push(Math::Sub([i, j]));
                to_add.push(Math::Mul([i, j]));
                // to_add.push(Math::Div([i, j]));
                to_add.push(Math::Pow([i, j]));
            }
            if self.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Neg(i));
            to_add.push(Math::Abs(i));
            to_add.push(Math::Reciprocal(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn run_rewrites(&mut self) -> EGraph {
        // run the rewrites
        log::info!("running eqsat with {} rules", self.equalities.len());
        let rewrites = self.equalities.values().flat_map(|eq| &eq.rewrites);
        let mut runner = Runner::new(self.egraph.analysis.clone())
            .with_egraph(self.egraph.clone())
            .with_node_limit(usize::MAX)
            .with_iter_limit(2)
            .with_scheduler(SimpleScheduler)
            // .with_iter_limit(5)
            // .with_scheduler(BackoffScheduler::default().with_initial_match_limit(5_000))
            .run(rewrites);

        // update the clean egraph based on any unions that happened
        let mut found_unions = vec![];
        for id in self.ids() {
            let id2 = runner.egraph.find(id);
            if id != id2 {
                found_unions.push((id, id2))
            }
        }
        for (id, id2) in found_unions {
            self.egraph.union(id, id2);
        }

        runner.egraph.rebuild();
        runner.egraph
    }

    fn cvec_match(&self) -> EqualityMap {
        // build the cvec matching data structure
        let mut by_cvec: IndexMap<Vec<Option<Constant>>, Vec<Id>> = IndexMap::new();
        let not_all_nones = self
            .ids()
            .filter(|id| !&self.egraph[*id].data.cvec.iter().all(|v| v == &None));
        for id in not_all_nones {
            let class = &self.egraph[id];
            // key = first_n_bits(&class.clone().data.cvec);
            // by_cvec.entry(&key).or_default().push(class.id);
            by_cvec
                .entry(class.data.cvec.clone())
                .or_default()
                .push(class.id);
        }

        let mut new_eqs = EqualityMap::default();
        let mut extract = Extractor::new(&self.egraph, AstSize);
        let mut to_merge: Vec<(Id, Id)> = vec![];
        for ids in by_cvec.values() {
            let mut id_iter = ids.iter();
            while let Some(&id1) = id_iter.next() {
                for &id2 in id_iter.clone() {
                    to_merge.push((id1, id2));
                    let (_, e1) = extract.find_best(id1);
                    let (_, e2) = extract.find_best(id2);
                    if let Some(eq) = Equality::new(&e1, &e2) {
                        // log::info!("  Candidate {}", eq);
                        new_eqs.insert(eq.name.clone(), eq);
                    }
                }
            }
        }

        // TODO why is this needed
        new_eqs.retain(|k, _v| !self.equalities.contains_key(k));
        new_eqs
    }

    pub fn run(mut self) -> EqualityMap {
        let mut poison_rules: HashSet<Equality> = HashSet::new();
        let t = Instant::now();
        for _ in 0..self.params.iters {
            let layer = self.make_layer();
            for chunk in layer.chunks(self.params.chunk_size) {
                for node in chunk {
                    self.egraph.add(node.clone());
                }
                loop {
                    self.run_rewrites();
                    let new_eqs = self.cvec_match();

                    let new_eqs: EqualityMap = new_eqs
                        .into_iter()
                        .filter(|eq| !poison_rules.contains(&eq.1))
                        .collect();

                    log::info!(
                        "egraph n={}, e={}",
                        self.egraph.total_size(),
                        self.egraph.number_of_classes(),
                    );

                    log::info!("Found {} new candidate rules", new_eqs.len());
                    let eqs: EqualityMap =
                        choose_eqs(&self.equalities, new_eqs, self.params.rules_to_take);

                    log::info!("filtering {} rules to find poison rules", eqs.len());

                    let (eqs, bads): (EqualityMap, EqualityMap) = eqs
                        .into_iter()
                        .partition(|(_name, eq)| is_valid(eq.lhs.clone(), eq.rhs.clone()));

                    log::info!("{} rules are unsound", bads.len());
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
        println!(
            "Learned {} rules in {:?}",
            self.equalities.len(),
            t.elapsed()
        );
        self.equalities
    }
}

fn choose_eqs(old_eqs: &EqualityMap, mut new_eqs: EqualityMap, n: usize) -> EqualityMap {
    let t = Instant::now();
    let n_new_eqs = new_eqs.len();
    log::info!("Minimizing {} rules...", n_new_eqs);

    let mut keepers = EqualityMap::default();
    // make the best last
    new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)));
    while let Some((name, eq)) = new_eqs.pop() {
        // println!("BEST: {}, score {:?}", name, score(&eq));
        keepers.insert(name, eq);
        if new_eqs.is_empty() || keepers.len() >= n {
            log::info!(
                "Nothing to minimize. new_eqs len: {}, keepers len: {}",
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
            .with_iter_limit(2)
            .with_scheduler(SimpleScheduler)
            .with_node_limit(1_000_000);

        for candidate_eq in new_eqs.values() {
            runner = runner.with_expr(&instantiate(&candidate_eq.lhs));
            runner = runner.with_expr(&instantiate(&candidate_eq.rhs));
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
    keepers
}

fn score(eq: &Equality) -> (isize, isize) {
    let lhs = &eq.lhs.ast;
    let rhs = &eq.rhs.ast;

    let sz_lhs = AstSize.cost_rec(lhs) as isize;
    let sz_rhs = AstSize.cost_rec(rhs) as isize;
    let sz_max_pattern = isize::max(sz_lhs, sz_rhs);

    // lhs.vars() and rhs.vars() is deduping
    // examples
    //   (- x x) => 0 --- 1 b/c x only var
    //   (- x 0) => x --- 1 b/c x only var
    //   (+ x y) => (+ y x) --- 2 b/c x, y only vars
    let mut var_set: HashSet<Var> = Default::default();
    var_set.extend(eq.lhs.vars());
    var_set.extend(eq.rhs.vars());
    let n_vars_rule = var_set.len() as isize;

    (-sz_max_pattern, n_vars_rule)
}

#[derive(Clone)]
pub struct Equality<L = Math, A = SynthAnalysis> {
    pub name: Rc<str>,
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    // pub cond: Option<Pattern<L>>,
    pub rewrites: Vec<Rewrite<L, A>>,
}

impl<L: Language, A> Hash for Equality<L, A> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<L: Language, A> PartialEq for Equality<L, A> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl<L: Language, A> Eq for Equality<L, A> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl Equality<Math, SynthAnalysis> {
    pub fn new(e1: &RecExpr, e2: &RecExpr) -> Option<Self> {
        let mut forward: (String, Pattern, Pattern, Option<Rewrite>) = {
            let map = &mut HashMap::default();
            let lhs = generalize(&e1, map);
            let rhs = generalize(&e2, map);
            let name = format!("{} => {}", lhs, rhs);
            (
                name.clone(),
                lhs.clone(),
                rhs.clone(),
                Rewrite::new(name, lhs.clone(), rhs.clone()).ok(),
            )
        };

        let mut back: (String, Pattern, Pattern, Option<Rewrite>) = {
            let map = &mut HashMap::default();
            let lhs = generalize(&e2, map);
            let rhs = generalize(&e1, map);
            let name = format!("{} => {}", lhs, rhs);
            (
                name.clone(),
                lhs.clone(),
                rhs.clone(),
                Rewrite::new(name, lhs.clone(), rhs.clone()).ok(),
            )
        };

        // make sure we always do things in the same order
        if back.0 > forward.0 {
            std::mem::swap(&mut forward, &mut back);
        }

        match (forward, back) {
            ((_, _, _, None), (_, _, _, None)) => None,
            ((name, lhs, rhs, Some(rw)), (_, _, _, None))
            | ((_, _, _, None), (name, lhs, rhs, Some(rw))) => Some(Self {
                name: name.into(),
                lhs,
                rhs,
                rewrites: vec![rw],
            }),
            ((_, lhs, rhs, Some(rw1)), (_, _, _, Some(rw2))) => Some(Self {
                name: format!("{} <=> {}", lhs, rhs).into(),
                lhs,
                rhs,
                rewrites: if rw1.name() == rw2.name() {
                    vec![rw1]
                } else {
                    vec![rw1, rw2]
                },
            }),
        }
    }
}
