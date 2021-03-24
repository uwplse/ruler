use egg::*;
use indexmap::IndexMap;
use lazy_static::lazy_static;
use num::bigint::{BigInt, RandBigInt, Sign, ToBigInt};
use num::{rational::Ratio, Signed, ToPrimitive};
use rand::SeedableRng;
use rand::{prelude::SliceRandom, Rng};
use rand_pcg::Pcg64;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::rc::Rc;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    time::Duration,
    time::Instant,
};
use std::{hash::Hash, sync::Mutex};

pub type Runner = egg::Runner<Math, SynthAnalysis, ()>;
pub type Pattern<L = Math> = egg::Pattern<L>;
pub type RecExpr<L = Math> = egg::RecExpr<L>;
pub type Rewrite<L = Math, A = SynthAnalysis> = egg::Rewrite<L, A>;
pub type EGraph<L = Math, A = SynthAnalysis> = egg::EGraph<L, A>;

pub type Constant = Ratio<BigInt>;
pub type CVec = Vec<Option<Constant>>;
pub type Ctx = HashMap<&'static str, Constant>;

pub static SEED: u64 = 5;
pub static VALID_SEED: u64 = 7;
lazy_static! {
    static ref RNG: Mutex<Pcg64> = Mutex::new(Pcg64::seed_from_u64(SEED));
}

#[derive(Debug, Clone)]
pub struct Signature {
    cvec: CVec,
    exact: bool,
    constant: Option<Constant>,
}

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "pow" = Pow([Id; 2]),
        "fabs" = Abs(Id),
        "recip" = Reciprocal(Id),
        "~" = Neg(Id),
        Num(Constant),
        Var(egg::Symbol),
    }
}

pub fn mk_constant(n: &BigInt, d: &BigInt) -> Option<Constant> {
    if d == &0.to_bigint().unwrap() {
        None
    } else {
        Some(Ratio::new(n.clone(), d.clone()))
    }
}

// randomly sample denoms so that they are not 0
// Ratio::new will panic if the denom is 0
pub fn gen_denom(rng: &Pcg64, bits: u64) -> BigInt {
    let mut res: BigInt;
    loop {
        res = rng.clone().gen_bigint(bits);
        if res != 0.to_bigint().unwrap() {
            break;
        }
    }
    res
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
    for _ in 0..1000 {
        let (xn, yn, zn) = (rng.gen_bigint(32), rng.gen_bigint(32), rng.gen_bigint(32));
        let (xd, yd, zd) = (
            gen_denom(&rng, 32),
            gen_denom(&rng, 32),
            gen_denom(&rng, 32),
        );
        let a = Ratio::new(xn, xd);
        let b = Ratio::new(yn, yd);
        let c = Ratio::new(zn, zd);
        env.insert("a", a.clone());
        env.insert("b", b.clone());
        env.insert("c", c.clone());
        let l = eval(&env.clone(), lhs.as_ref());
        let r = eval(&env, rhs.as_ref());
        match (l, r) {
            (None, _) | (_, None) => {
                println!("{} => {} failed validation at {} {} {}", lhs, rhs, a, b, c);
                return false;
            }
            (Some(l), Some(r)) => {
                if l == r {
                    valid = true;
                } else {
                    return false;
                }
            }
        }
    }
    valid
}

pub fn eval(ctx: &Ctx, expr: &[Math]) -> Option<Constant> {
    match expr.last().expect("empty expr!") {
        Math::Num(n) => mk_constant(n.numer(), n.denom()),
        Math::Var(v) => {
            let a = ctx.get("a").cloned();
            let b = ctx.get("b").cloned();
            let c = ctx.get("c").cloned();
            if v.as_str() == "a" {
                let au = a.unwrap();
                mk_constant(au.numer(), au.denom())
            } else if v.as_str() == "b" {
                let bu = b.unwrap();
                mk_constant(bu.numer(), bu.denom())
            } else if v.as_str() == "c" {
                let cu = c.unwrap();
                mk_constant(cu.numer(), cu.denom())
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
                let res = e1.unwrap() + e2.unwrap();
                mk_constant(res.numer(), res.denom())
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
                let res = e1.unwrap() - e2.unwrap();
                mk_constant(res.numer(), res.denom())
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
                let res = e1.unwrap() * e2.unwrap();
                mk_constant(res.numer(), res.denom())
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
                let res = e1.unwrap() / e2.unwrap();
                mk_constant(res.numer(), res.denom())
            }
        }
        Math::Abs(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            if e1 == None {
                None
            } else {
                let res = e1.unwrap().abs();
                mk_constant(res.numer(), res.denom())
            }
        }
        Math::Pow([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            if e1 == None || e2 == None {
                None
            } else {
                match e2.unwrap().to_i32() {
                    None => None,
                    Some(v) => {
                        let res = e1.unwrap().pow(v);
                        mk_constant(res.numer(), res.denom())
                    }
                }
            }
        }
        Math::Reciprocal(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            if e1 == None {
                None
            } else {
                let res = e1.unwrap().recip();
                mk_constant(res.numer(), res.denom())
            }
        }
        Math::Neg(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            if e1 == None {
                None
            } else {
                let res = -e1.unwrap();
                mk_constant(res.numer(), res.denom())
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

pub fn instantiate(pattern: &Pattern<Math>) -> RecExpr<Math> {
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
    fn fold1(&self, f: impl Fn(Constant) -> Option<Constant>) -> Self {
        let compute = |x: &Option<Constant>| match x {
            None => None,
            Some(v) => f(v.clone()),
        };
        let cvec = self.cvec.iter().map(&compute).collect();
        Self {
            cvec,
            exact: self.exact,
            constant: compute(&self.constant),
        }
    }

    fn fold2(&self, other: &Self, f: impl Fn(Constant, Constant) -> Option<Constant>) -> Self {
        if !self.cvec.is_empty() && !other.cvec.is_empty() {
            assert_eq!(self.cvec.len(), other.cvec.len());
        }

        let compute = |(x, y): (&Option<Constant>, &Option<Constant>)| match (x, y) {
            (Some(n1), Some(n2)) => f(n1.clone(), n2.clone()),
            (_, _) => None,
        };
        let cvec: CVec = self.cvec.iter().zip(&other.cvec).map(&compute).collect();
        Self {
            cvec,
            exact: self.exact && other.exact,
            constant: compute((&self.constant, &other.constant)),
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
                    (Some(x), Some(y)) => {
                        assert_eq!(x, y, "cvecs do not match at index {}: {} != {}", i, x, y)
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
            Math::Neg(a) => v(a).fold1(|a| mk_constant(a.numer(), a.denom())),
            Math::Add([a, b]) => v(a).fold2(v(b), |a, b| {
                let res = a + b;
                mk_constant(res.numer(), res.denom())
            }),
            Math::Sub([a, b]) => v(a).fold2(v(b), |a, b| {
                let res = a - b;
                mk_constant(res.numer(), res.denom())
            }),
            Math::Mul([a, b]) => v(a).fold2(v(b), |a, b| {
                if b.denom() != &0.to_bigint().unwrap()
                && a.denom() != &0.to_bigint().unwrap()
                {
                    let res = a * b;
                    mk_constant(res.numer(), res.denom())
                } else {
                    None
                }
            }),
            Math::Num(n) => {
                let cv: Vec<Option<Constant>> = (0..param.cvec_len)
                    .map(|_| mk_constant(n.numer(), n.denom()))
                    .collect();
                Signature {
                    cvec: cv.clone(),
                    exact: true,
                    constant: Some(n.clone()),
                }
            }
            Math::Var(_) => Signature {
                cvec: vec![],
                exact: false,
                constant: None,
            },
            Math::Div([a, b]) => v(a).fold2(v(b), |a, b| {
                if b.numer() != &0.to_bigint().unwrap()
                    && b.denom() != &0.to_bigint().unwrap()
                    && a.denom() != &0.to_bigint().unwrap()
                {
                    let res = a / b;
                    mk_constant(res.numer(), res.denom())
                } else {
                    None
                }
            }),
            Math::Abs(a) => v(a).fold1(|a| {
                let res = a.abs();
                mk_constant(res.numer(), res.denom())
            }),
            Math::Pow([a, b]) => v(a).fold2(v(b), |a, b| {
                let power = b.to_i32();
                if power == None {
                    None
                } else {
                    let res = a.pow(power.unwrap());
                    mk_constant(res.numer(), res.denom())
                }
            }),
            Math::Reciprocal(a) => v(a).fold1(|a| {
                if a.denom() != &0.to_bigint().unwrap() {
                    let res = a.recip();
                    mk_constant(res.numer(), res.denom())
                } else {
                    None
                }
            }),
        }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        let c = egraph[id].data.clone().constant;
        if let Some(v) = c {
            let added = egraph.add(Math::Num(v.clone()));
            egraph.union(id, added);
        } else {
            return;
        }
    }
}

fn chain_consts(constants: Vec<Constant>, nvars: u32, i: u32) -> Vec<Option<Constant>> {
    let mut res = vec![];
    let mut consts = vec![];
    for c in constants {
        consts.push(mk_constant(c.numer(), c.denom()));
    }
    let nc = consts.len();
    let nrows = nc.pow(nvars as u32);
    while res.len() < nrows {
        for c in &consts {
            for _ in 0..nc.pow(i) {
                res.push(c.clone())
            }
        }
    }
    res
}


pub struct SynthParams {
    pub seed: u64,
    pub n_samples: usize,
    pub constants: Vec<Constant>,
    pub variables: Vec<egg::Symbol>,
    // search params
    pub iters: usize,
    pub rules_to_take: usize,
    pub chunk_size: usize,
    pub minimize: bool,
    pub outfile: String,
}

type EqualityMap<L = Math, A = SynthAnalysis> = IndexMap<Rc<str>, Equality<L, A>>;

#[allow(dead_code)]
pub struct Synthesizer {
    pub params: SynthParams,
    //rng: Pcg64,
    egraph: EGraph,
    equalities: EqualityMap,
}

impl Synthesizer {
    pub fn new(params: SynthParams) -> Self {
        let mut egraph = EGraph::new(SynthAnalysis {
            // cvec_len: params.n_samples + params.constants.len(),
            cvec_len: params.n_samples + params.constants.len().pow(params.variables.len() as u32),
        });
        let mut rng = Pcg64::seed_from_u64(params.seed);
        for var in &params.variables {
            let id = egraph.add(Math::Var(*var));

            if var.to_string() == "x" {
                egraph[id].data.cvec = (0..params.n_samples)
                    .map(|_| mk_constant(&rng.gen_bigint(32), &gen_denom(&rng, 32)))
                    .chain(chain_consts(
                        params.constants.clone(),
                        params.variables.len() as u32,
                        0,
                    ))
                    .collect();
            }
            if var.to_string() == "y" {
                egraph[id].data.cvec = (0..params.n_samples)
                    .map(|_| mk_constant(&rng.gen_bigint(32), &gen_denom(&rng, 32)))
                    .chain(chain_consts(
                        params.constants.clone(),
                        params.variables.len() as u32,
                        1,
                    ))
                    .collect();
            }
            if var.to_string() == "z" {
                egraph[id].data.cvec = (0..params.n_samples)
                    .map(|_| mk_constant(&rng.gen_bigint(32), &gen_denom(&rng, 32)))
                    .chain(chain_consts(
                        params.constants.clone(),
                        params.variables.len() as u32,
                        2,
                    ))
                    .collect();
            }
            // let mut cvec: Vec<Option<Constant>> = (0..params.n_samples)
            //     .map(|_| mk_constant(&rng.gen_bigint(32), &gen_denom(&rng, 32)))
            //     .collect();
            // for c in &params.constants {
            //     cvec.push(mk_constant(c.numer(), c.denom()));
            // }
            // egraph[id].data.cvec = cvec.clone();
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
                to_add.push(Math::Div([i, j]));
                // to_add.push(Math::Pow([i, j]));
            }
            if self.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Abs(i));
            // to_add.push(Math::Reciprocal(i));
            to_add.push(Math::Neg(i));
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
        let mut by_cvec: IndexMap<&CVec, Vec<Id>> = IndexMap::new();

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

    pub fn run(mut self) -> Report {
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

                    let (eqs, bads) = if self.params.minimize {
                        choose_eqs(&self.equalities, new_eqs, self.params.rules_to_take)
                    } else {
                        choose_eqs_old(&self.equalities, new_eqs, self.params.rules_to_take)
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
        Report {
            time,
            num_rules,
            eqs,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Report {
    pub time: f64,
    pub num_rules: usize,
    pub eqs: Vec<Equality>,
}

fn choose_eqs_old(
    old_eqs: &EqualityMap,
    mut new_eqs: EqualityMap,
    n: usize,
) -> (EqualityMap, EqualityMap) {
    let t = Instant::now();
    let n_new_eqs = new_eqs.len();
    log::info!("Minimizing {} rules...", n_new_eqs);

    let mut keepers = EqualityMap::default();
    let mut bads = EqualityMap::default();
    // make the best last
    new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)));
    while let Some((name, eq)) = new_eqs.pop() {
        if is_valid(eq.lhs.clone(), eq.rhs.clone()) {
            keepers.insert(name, eq);
        } else {
            bads.insert(name, eq);
        }
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
    (keepers, bads)
}

fn choose_eqs(old_eqs: &EqualityMap, new_eqs: EqualityMap, n: usize) -> (EqualityMap, EqualityMap) {
    let t = Instant::now();
    let (new_eqs, bads): (EqualityMap, EqualityMap) = new_eqs
        .into_iter()
        .partition(|(_name, eq)| is_valid(eq.lhs.clone(), eq.rhs.clone()));

    let n_new_eqs = new_eqs.len();
    log::info!("Minimizing {} rules...", n_new_eqs);
    let mut flat: VecDeque<Equality> = new_eqs.into_iter().map(|(_, eq)| eq).collect();
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
        assert_eq!(flat.len(), chunk_sizes.iter().sum());

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
                .with_iter_limit(2)
                .with_scheduler(SimpleScheduler)
                .with_time_limit(Duration::from_secs(60))
                .with_node_limit(10_000_000);

            for candidate_eq in &test {
                runner = runner.with_expr(&instantiate(&candidate_eq.lhs));
                runner = runner.with_expr(&instantiate(&candidate_eq.rhs));
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

#[derive(Clone, Serialize, Deserialize)]
#[serde(from = "SerializedEq")]
#[serde(into = "SerializedEq")]
#[serde(bound = "L: Clone + Language + 'static, A: Clone + Analysis<L>")]
pub struct Equality<L = Math, A = SynthAnalysis> {
    pub name: Rc<str>,
    pub lhs: Pattern<L>,
    pub ids: Option<(Id, Id)>,
    pub rhs: Pattern<L>,
    // pub cond: Option<Pattern<L>>,
    pub rewrites: Vec<Rewrite<L, A>>,
}

#[derive(Clone, Serialize, Deserialize)]
struct SerializedEq {
    lhs: String,
    rhs: String,
    bidirectional: bool,
}

impl<L: Language + 'static, A: Analysis<L>> From<SerializedEq> for Equality<L, A> {
    fn from(ser: SerializedEq) -> Self {
        let lhs: Pattern<L> = ser.lhs.parse().unwrap();
        let rhs: Pattern<L> = ser.rhs.parse().unwrap();
        let mut rewrites =
            vec![Rewrite::new(format!("{} => {}", lhs, rhs), lhs.clone(), rhs.clone()).unwrap()];
        let name = if ser.bidirectional {
            rewrites.push(
                Rewrite::new(format!("{} => {}", rhs, lhs), rhs.clone(), lhs.clone()).unwrap(),
            );
            format!("{} <=> {}", lhs, rhs)
        } else {
            format!("{} => {}", lhs, rhs)
        };
        Self {
            name: name.into(),
            lhs,
            rhs,
            rewrites,
            ids: None,
        }
    }
}

impl<L: Language, A: Analysis<L>> From<Equality<L, A>> for SerializedEq {
    fn from(eq: Equality<L, A>) -> Self {
        Self {
            lhs: eq.lhs.to_string(),
            rhs: eq.rhs.to_string(),
            bidirectional: eq.rewrites.len() > 1,
        }
    }
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
                ids: None,
                rewrites: vec![rw],
            }),
            ((_, lhs, rhs, Some(rw1)), (_, _, _, Some(rw2))) => Some(Self {
                name: format!("{} <=> {}", lhs, rhs).into(),
                lhs,
                rhs,
                ids: None,
                rewrites: if rw1.name() == rw2.name() {
                    vec![rw1]
                } else {
                    vec![rw1, rw2]
                },
            }),
        }
    }
}
