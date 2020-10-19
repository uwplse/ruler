use std::{collections::HashSet, fmt::Display, hash::Hash, rc::Rc};

use byteorder::{ByteOrder, LittleEndian};
use egg::*;
use indexmap::IndexMap;
use ordered_float::OrderedFloat;
use rand::{prelude::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;
use std::collections::HashMap;

type Runner = egg::Runner<Math, SynthAnalysis, ()>;
type Pattern<L = Math> = egg::Pattern<L>;
type RecExpr<L = Math> = egg::RecExpr<L>;
type Rewrite<L = Math, A = SynthAnalysis> = egg::Rewrite<L, A>;
type EGraph<L = Math, A = SynthAnalysis> = egg::EGraph<L, A>;

pub type Constant = OrderedFloat<f64>;
pub type CVec = Vec<Option<Constant>>;
pub type Ctx = HashMap<&'static str, Constant>;

struct AstSize;
impl CostFunction<Math> for AstSize {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &Math, mut cost: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let base = match enode {
            Math::Sub(_) => 2,
            _ => 1,
        };
        enode.fold(base, |acc, id| acc + cost(id))
    }
}

impl CostFunction<ENodeOrVar<Math>> for AstSize {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &ENodeOrVar<Math>, cost: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        match enode {
            ENodeOrVar::ENode(enode) => Self.cost(enode, cost),
            ENodeOrVar::Var(_) => 1,
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
        "~" = Neg(Id),
        "*" = Mul([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
    }
}

pub fn eval(ctx: &Ctx, expr: &[Math]) -> Option<Constant> {
    match expr.last().expect("empty expr!") {
        Math::Num(n) => Some(n.clone()),
        Math::Var(v) => {
            let a = ctx.get("a").cloned();
            let b = ctx.get("b").cloned();
            let c = ctx.get("c").cloned();
            if v.as_str() == "a" {
                Some(a.unwrap())
            } else if v.as_str() == "b" {
                Some(b.unwrap())
            } else if v.as_str() == "c" {
                Some(c.unwrap())
            } else {
                panic!("eval: currently only supports rules with 3 variables");
            }
        }
        Math::Add([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(e1.unwrap() + e2.unwrap())
        }
        Math::Sub([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(e1.unwrap() - e2.unwrap())
        }
        Math::Mul([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(e1.unwrap() * e2.unwrap())
        }
        Math::Neg(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            Some(-e1.unwrap())
        }
    }
}

fn generalize(expr: &RecExpr<Math>, map: &mut HashMap<Symbol, Var>) -> Pattern<Math> {
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

// TODO: maybe this should take just an OrderedFloat and not the entire cvec
fn first_n_bits(vs: &Vec<Option<OrderedFloat<f64>>>) -> Vec<Option<OrderedFloat<f64>>> {
    let mut ret = vec![];
    for n in vs {
        if *n != None {
            let u = n.unwrap().into_inner().to_bits();
            ret.push(Some(OrderedFloat::from(<f64>::from_bits(u & ((!0) << 10)))))
        } else {
            ret.push(None);
        }
    }
    ret
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

// sample one float uniformly (from u64, cast to float), then sample near and far from it
// maybe sample around the center
// Idea: given ulps range U
//   - sample a float, call it x
//   - sample an u64 ulp offset in [-U, U] call it u
//   - return (x as u64 + u) ass f64
fn sample_float(mut rng: Pcg64, x: f64, ulps_range: u64) -> f64 {
    let u = rng.gen_range(0, ulps_range);
    f64::from_bits(x.to_bits() + u)
}

fn rand_float_repr(mut rng: Pcg64) -> f64 {
    //f64::from_bits(Pcg64::())
    let mut rng = rand::thread_rng();
    let mut x = f64::NAN;
    while x.is_nan() {
        x = f64::from_bits(rng.gen::<u64>())
    }
    x
}

fn is_valid(rng: Pcg64, lhs: Pattern, rhs: Pattern) -> bool {
    let lhs = instantiate(&lhs);
    let rhs = instantiate(&rhs);
    let mut env: Ctx = HashMap::new();
    let mut valid = false;
    let ulp_rad_sm: u64 = 1000;
    let ulp_rad_lg: u64 = 50000000000000;
    for i in 0..1000000 {
        let mut a = rand_float_repr(rng.clone());
        let b;
        let c;
        match i % 10 {
            0 => {
                b = sample_float(rng.clone(), a, ulp_rad_sm);
                c = sample_float(rng.clone(), a, ulp_rad_sm);
            }
            1 => {
                b = sample_float(rng.clone(), a, ulp_rad_lg);
                c = sample_float(rng.clone(), a, ulp_rad_sm);
            }
            2 => {
                b = sample_float(rng.clone(), a, ulp_rad_sm);
                c = sample_float(rng.clone(), a, ulp_rad_lg);
            }
            3 => {
                b = sample_float(rng.clone(), a, ulp_rad_lg);
                c = sample_float(rng.clone(), a, ulp_rad_lg);
            }
            4 => {
                match i % 3 {
                    0 => {
                        a = -1.0;
                    }
                    1 => {
                        a = 0.0;
                    }
                    _ => {
                        a = 1.0;
                    }
                }
                b = sample_float(rng.clone(), a, ulp_rad_lg);
                c = sample_float(rng.clone(), a, ulp_rad_lg);
            }
            _ => {
                b = rand_float_repr(rng.clone());
                c = rand_float_repr(rng.clone());
            }
        }
        if a.is_nan() || b.is_nan() || c.is_nan() {
            continue;
        }
        env.insert("a", OrderedFloat::from(a));
        env.insert("b", OrderedFloat::from(b));
        env.insert("c", OrderedFloat::from(c));

        let l = eval(&env.clone(), lhs.as_ref());
        let r = eval(&env, rhs.as_ref());
        match (l, r) {
            (None, _) | (_, None) => {
                valid = false;
                println!(
                    "validation of {} => {} failed at: {} {} {}",
                    lhs, rhs, a, b, c
                );
                break;
            }
            (Some(l), Some(r)) => {
                if l == r {
                    valid = true;
                    continue;
                } else {
                    println!(
                        "vadildation of {} => {} failed at: {} {} {}",
                        lhs, rhs, a, b, c
                    );
                    valid = false;
                    break;
                }
            }
        }
    }
    valid
}

impl Signature {
    fn fold1(&self, mut f: impl FnMut(Constant) -> Option<Constant>) -> Self {
        let cvec = self.cvec.iter().map(|x| x.and_then(&mut f)).collect();
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
    pub cvx: CVec,
    pub cvy: CVec,
    pub cvz: CVec,
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self {
            cvec_len: 10,
            cvx: vec![],
            cvy: vec![],
            cvz: vec![],
        }
    }
}

impl Analysis<Math> for SynthAnalysis {
    type Data = Signature;

    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        let v = |i: &Id| &egraph[*i].data;
        let param = &egraph.analysis;
        let sig = match enode {
            Math::Neg(a) => v(a).fold1(|a| {
                if a != OrderedFloat::from(f64::NAN) {
                    Some(-a)
                } else {
                    None
                }
            }),
            Math::Add([a, b]) => v(a).fold2(v(b), |a, b| {
                if (a + b) != OrderedFloat::from(f64::NAN) {
                    Some(a + b)
                } else {
                    None
                }
            }),
            Math::Sub([a, b]) => v(a).fold2(v(b), |a, b| {
                if (a - b) != OrderedFloat::from(f64::NAN) {
                    Some(a - b)
                } else {
                    None
                }
            }),
            Math::Mul([a, b]) => v(a).fold2(v(b), |a, b| {
                if (a * b) != OrderedFloat::from(f64::NAN) {
                    Some(a * b)
                } else {
                    None
                }
            }),
            Math::Num(n) => Signature {
                cvec: (0..param.cvec_len).map(|_| Some(n.clone())).collect(),
                exact: true,
            },
            Math::Var(_) => Signature {
                cvec: vec![],
                exact: false,
            },
        };
        sig
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        let mut to_cvec_changed = false;
        let mut to_exact_changed = false;
        let pairs = to.cvec.iter().zip(from.cvec.iter());
        for (mut t, f) in pairs {
            match (t, f) {
                (None, Some(_)) => {
                    t = f;
                    to_cvec_changed = true;
                }
                (Some(a), Some(b)) => {
                    if a != b {
                        println!("cvecs do not match");
                        print!("{: >+20e} \t {: >+20e}", a.into_inner(), b.into_inner());
                        panic!("cvecs do not match");
                    } else {
                        continue;
                    }
                }
                (_, None) => continue,
            }
        }
        // if !to.cvec.is_empty() && !from.cvec.is_empty() {
        //     if to.cvec != from.cvec {
        //         println!("cvecs do not match");
        //         for i in 0..to.cvec.len() {
        //             match to.cvec[i] {
        //                 Some(of) => {
        //                     print!("{: >+20e}", of.into_inner());
        //                 }
        //                 None => {
        //                     print!("None");
        //                 }
        //             }
        //             print!("\t");
        //             match from.cvec[i] {
        //                 Some(of) => {
        //                     print!("{: >+20e}", of.into_inner());
        //                 }
        //                 None => {
        //                     print!("None");
        //                 }
        //             }
        //         }
        //         panic!("cvecs don't match");
        //     }
        // }
        if !to.exact && from.exact {
            to.exact = true;
            to_exact_changed = true;
        }

        to_cvec_changed || to_exact_changed
    }

    fn pre_union(eg: &EGraph<Math, Self>, id1: Id, id2: Id) {
        let mut extract = Extractor::new(eg, AstSize);
        println!("id1: {}", extract.find_best(id1).1);
        println!("id2: {} \n", extract.find_best(id2).1);
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
}

type EqualityMap<L = Math, A = SynthAnalysis> = IndexMap<Rc<str>, Equality<L, A>>;

#[allow(dead_code)]
pub struct Synthesizer {
    params: SynthParams,
    rng: Pcg64,
    egraph: EGraph,
    equalities: EqualityMap,
}

impl Synthesizer {
    pub fn new(mut params: SynthParams) -> Self {
        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: params.n_samples + params.constants.len(),
            cvx: vec![],
            cvy: vec![],
            cvz: vec![],
        });

        let mut rng = Pcg64::seed_from_u64(params.seed);

        // .map(|_| OrderedFloat::from(rng.gen::<f64>()))

        // initialize the variables
        for &var in &params.variables {
            let id = egraph.add(Math::Var(var));
            egraph[id].data.cvec = (0..params.n_samples)
                .map(|_| OrderedFloat::from(rand_float_repr(rng.clone())))
                .chain(params.constants.iter().cloned())
                .map(Some)
                .collect();
            egraph[id].data.cvec.shuffle(&mut rng);

            if var.to_string() == "x" {
                egraph.analysis.cvx = egraph[id].data.cvec.clone();
            }
            if var.to_string() == "y" {
                egraph.analysis.cvy = egraph[id].data.cvec.clone();
            }
            if var.to_string() == "z" {
                egraph.analysis.cvz = egraph[id].data.cvec.clone();
            }
        }

        for n in &params.constants {
            egraph.add(Math::Num(n.clone()));
        }

        Self {
            rng,
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
                to_add.push(Math::Add([i, j]));
                to_add.push(Math::Mul([i, j]));
                // to_add.push(Math::Sub([i, j]));
            }
            to_add.push(Math::Neg(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn run_rewrites(&mut self) -> EGraph {
        // run the rewrites
        let rewrites = self.equalities.values().flat_map(|eq| &eq.rewrites);
        let mut runner = Runner::new(self.egraph.analysis.clone())
            .with_egraph(self.egraph.clone())
            .with_node_limit(usize::MAX)
            .with_iter_limit(2)
            .with_scheduler(SimpleScheduler)
            // .with_iter_limit(20)
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

    fn minimize(&self, eq: Equality) -> Option<Equality> {
        // instantiate both lhs and rhs of eq
        let l_expr = instantiate(&eq.lhs);
        let r_expr = instantiate(&eq.rhs);

        // run eqsat with all current ruleset
        let rewrites = self.equalities.values().flat_map(|eq| &eq.rewrites);
        let mut runner = Runner::default()
            .with_expr(&l_expr)
            .with_node_limit(usize::MAX)
            .with_iter_limit(1)
            // .with_scheduler(SimpleScheduler)
            .run(rewrites);

        // add rhs to the egraph
        let rhs_id = runner.egraph.add_expr(&r_expr);

        // lhs and rhs are in same eclass
        if runner.egraph.find(runner.roots[0]) == runner.egraph.find(rhs_id) {
            println!("threw away: {}", eq.name);
            None
        } else {
            Some(eq)
        }
    }

    fn cvec_match(&self) -> EqualityMap {
        // build the cvec matching data structure
        let mut by_cvec: IndexMap<&CVec, Vec<Id>> = IndexMap::new();

        for id in self.ids() {
            let class = &self.egraph[id];
            // let key = first_n_bits(&class.clone().data.cvec);
            // by_cvec.entry(key).or_default().push(class.id);
            by_cvec.entry(&class.data.cvec).or_default().push(class.id);
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
                        log::debug!("  Candidate {}", eq);
                        new_eqs.insert(eq.name.clone(), eq);
                    }
                }
            }
        }

        // let id_sets: Vec<_> = by_cvec.into_iter().map(|(_k, v)| v).collect();
        // for ids in id_sets {
        //     for &id in &ids {
        //         self.egraph.union(ids[0], id);
        //     }
        // }

        // TODO why is this needed
        new_eqs.retain(|k, _v| !self.equalities.contains_key(k));
        new_eqs
    }

    pub fn run_mrat(mut self, iters: usize) -> EqualityMap {
        for _ in 0..iters {
            let layer = self.make_layer();
            for node in layer {
                self.egraph.add(node);
            }

            self.run_rewrites();
            log::info!(
                "egraph n={}, e={}",
                self.egraph.total_size(),
                self.egraph.number_of_classes()
            );

            let new_eqs = self.cvec_match();
            let new_eqs = minimize(&self.equalities, new_eqs);
            self.equalities.extend(new_eqs);
        }
        self.equalities
    }

    pub fn run_orat(mut self, iters: usize) -> EqualityMap {
        for _ in 0..iters {
            let layer = self.make_layer();
            for chunk in layer.chunks(10000) {
                for node in chunk {
                    self.egraph.add(node.clone());
                }
                loop {
                    self.run_rewrites();
                    log::info!(
                        "egraph n={}, e={}",
                        self.egraph.total_size(),
                        self.egraph.number_of_classes()
                    );
                    let new_eqs = self.cvec_match();
                    if new_eqs.is_empty() {
                        break;
                    }
                    let valid_eqs: EqualityMap = new_eqs
                        .into_iter()
                        .filter(|eq| is_valid(self.rng.clone(), eq.1.lhs.clone(), eq.1.rhs.clone()))
                        .collect();
                    if valid_eqs.is_empty() {
                        break;
                    }
                    let eq = choose_best_eq(&valid_eqs);
                    log::info!("Chose best {}", eq);
                    assert!(!self.equalities.contains_key(&eq.name));
                    self.equalities.insert(eq.name.clone(), eq);
                }
            }
        }
        self.equalities
    }
}

fn minimize(old_eqs: &EqualityMap, mut new_eqs: EqualityMap) -> EqualityMap {
    // make the best first
    new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)).reverse());

    let mut keepers = EqualityMap::default();
    for eq in new_eqs.values() {
        let l_expr = instantiate(&eq.lhs);
        let r_expr = instantiate(&eq.rhs);

        let rewrites = old_eqs
            .values()
            .flat_map(|eq| &eq.rewrites)
            .chain(keepers.values().flat_map(|eq| &eq.rewrites));
        let mut runner = Runner::default()
            .with_expr(&l_expr)
            .with_iter_limit(2)
            .with_scheduler(SimpleScheduler)
            .run(rewrites);

        let rhs_id = runner.egraph.add_expr(&r_expr);

        if runner.egraph.find(runner.roots[0]) == runner.egraph.find(rhs_id) {
            println!("threw away: {}", eq.name);
        } else {
            keepers.insert(eq.name.clone(), eq.clone());
        }
    }

    keepers
}

fn score(eq: &Equality) -> (isize, isize) {
    let mut vars: HashSet<Var> = Default::default();
    vars.extend(eq.lhs.vars());
    vars.extend(eq.rhs.vars());
    // let size = usize::add(AstSize.cost_rec(&eq.lhs.ast), AstSize.cost_rec(&eq.rhs.ast));
    let size = AstSize.cost_rec(&eq.lhs.ast) + AstSize.cost_rec(&eq.rhs.ast);
    (vars.len() as isize, -(size as isize))
    // (-(size as isize), vars.len() as isize)
    // let x = (-(size as isize), vars.len() as isize);
    // // println!("{} {:?}", eq, x);
    // x
}

fn choose_best_eq(new_eqs: &EqualityMap) -> Equality {
    new_eqs.values().max_by_key(|eq| score(eq)).unwrap().clone()
}

// TODO should probably keep things together
#[derive(Clone)]
pub struct Equality<L = Math, A = SynthAnalysis> {
    pub name: Rc<str>,
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    // pub cond: Option<Pattern<L>>,
    pub rewrites: Vec<Rewrite<L, A>>,
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Equality<Math, SynthAnalysis> {
    fn new(e1: &RecExpr, e2: &RecExpr) -> Option<Self> {
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
                rewrites: if rw1.name == rw2.name {
                    vec![rw1]
                } else {
                    vec![rw1, rw2]
                },
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_proves(eqs: &EqualityMap, a: &str, b: &str) {
        let rules = eqs.values().flat_map(|eq| &eq.rewrites);

        let runner = Runner::default()
            .with_expr(&a.parse().unwrap())
            .with_expr(&b.parse().unwrap())
            .with_hook(|runner| {
                if runner.egraph.find(runner.roots[0]) == runner.egraph.find(runner.roots[1]) {
                    Err(format!("Done"))
                } else {
                    Ok(())
                }
            })
            .run(rules);

        let id_a = runner.egraph.find(runner.roots[0]);
        let id_b = runner.egraph.find(runner.roots[1]);

        if id_a != id_b {
            panic!("Failed to simplify {} => {}", a, b)
        }
    }

    #[test]
    fn orat1() {
        let _ = env_logger::try_init();
        let syn = Synthesizer::new(SynthParams {
            seed: 5,
            n_samples: 100,
            constants: vec![
                OrderedFloat::from(-1.0),
                OrderedFloat::from(0.0),
                OrderedFloat::from(1.0),
            ],
            variables: vec!["x".into(), "y".into(), "z".into()],
        });

        let eqs = syn.run_orat(2);

        println!("CHECKING! Found {} rules", eqs.len());
        for eq in eqs.values() {
            println!("  {}", eq);
        }

        check_proves(&eqs, "(+ a b)", "(+ b a)");
        check_proves(&eqs, "(* a b)", "(* b a)");
        check_proves(&eqs, "(+ 1 1)", "2");
        check_proves(&eqs, "a", "(* 1 a)");
        check_proves(&eqs, "a", "(+ a 0)");
    }

    #[test]
    fn mrat1() {
        let _ = env_logger::try_init();
        let syn = Synthesizer::new(SynthParams {
            seed: 5,
            n_samples: 10,
            constants: vec![
                OrderedFloat::from(-1.0),
                OrderedFloat::from(0.0),
                OrderedFloat::from(1.0),
            ],
            variables: vec!["x".into(), "y".into(), "z".into()],
        });
        let eqs = syn.run_mrat(1);

        println!("CHECKING! Found {} rules", eqs.len());
        for eq in eqs.values() {
            println!("  {}", eq);
        }

        check_proves(&eqs, "(+ a b)", "(+ b a)");
        check_proves(&eqs, "(* a b)", "(* b a)");
        check_proves(&eqs, "(+ 1 1)", "2");
        check_proves(&eqs, "a", "(* 1 a)");
        check_proves(&eqs, "a", "(+ a 0)");
    }
}
