use std::ops::Not;
use std::{collections::HashSet, fmt::Display, hash::Hash, rc::Rc};

use egg::*;

use indexmap::IndexMap;
use rand::{prelude::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;
use std::collections::HashMap;

type Runner = egg::Runner<Math, SynthAnalysis, ()>;
type Pattern<L = Math> = egg::Pattern<L>;
type RecExpr<L = Math> = egg::RecExpr<L>;
type Rewrite<L = Math, A = SynthAnalysis> = egg::Rewrite<L, A>;
type EGraph<L = Math, A = SynthAnalysis> = egg::EGraph<L, A>;

pub type Constant = u8;
pub type CVec = Vec<Option<Constant>>;

#[derive(Debug, Clone)]
pub struct Signature {
    cvec: CVec,
    exact: bool,
}

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),
        "-" = Neg(Id),
        "~" = Not(Id),
        "<<" = Shl([Id; 2]),
        ">>" = Shr([Id; 2]),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
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
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self { cvec_len: 10_000 }
    }
}

// fn shl(a: u32, b: u32) -> u32 {
//     a << (b % 32)
//     // if b > 31 {
//     //     0
//     // } else {
//     //     a << b
//     // }
// }

// fn shr(a: u32, b: u32) -> u32 {
//     a >> (b % 32)
//     // if b > 31 {
//     //     0
//     // } else {
//     //     a >> b
//     // }
// }

impl Analysis<Math> for SynthAnalysis {
    type Data = Signature;

    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        let v = |i: &Id| &egraph[*i].data;
        let param = &egraph.analysis;
        let sig = match enode {
            Math::Neg(a) => v(a).fold1(|a| Some(a.wrapping_neg())),
            Math::Not(a) => v(a).fold1(|a| Some(a.not())),
            Math::Add([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_add(b))),
            Math::Mul([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_mul(b))),
            Math::Num(n) => Signature {
                cvec: (0..param.cvec_len).map(|_| Some(n.clone())).collect(),
                exact: true,
            },
            Math::Var(_) => Signature {
                cvec: vec![],
                exact: false,
            },
            Math::Shl([a, b]) => v(a).fold2(v(b), |a, b| Some(a.overflowing_shl(b.into()).0)),
            Math::Shr([a, b]) => v(a).fold2(v(b), |a, b| Some(a.overflowing_shr(b.into()).0)),
            Math::And([a, b]) => v(a).fold2(v(b), |a, b| Some(a & b)),
            Math::Or([a, b]) => v(a).fold2(v(b), |a, b| Some(a | b)),
        };
        sig
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            assert_eq!(&to.cvec, &from.cvec);
            if !to.exact && from.exact {
                to.exact = true;
                return true;
            }
        }
        false
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
    pub fn new(params: SynthParams) -> Self {
        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: params.n_samples + params.constants.len(),
        });

        let mut rng = Pcg64::seed_from_u64(params.seed);

        // initialize the variables
        for &var in &params.variables {
            let id = egraph.add(Math::Var(var));
            egraph[id].data.cvec = (0..params.n_samples)
                .map(|_| rng.gen::<Constant>())
                .chain(params.constants.iter().cloned())
                .map(Some)
                .collect();
            egraph[id].data.cvec.shuffle(&mut rng);
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
                to_add.push(Math::And([i, j]));
                to_add.push(Math::Or([i, j]));
                to_add.push(Math::Shl([i, j]));
                to_add.push(Math::Shr([i, j]));
            }
            to_add.push(Math::Neg(i));
            to_add.push(Math::Not(i));
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
                    let eq = choose_best_eq(&new_eqs);
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

fn validate(n: usize, rng: &mut impl Rng, lhs: &RecExpr, rhs: &RecExpr) -> bool {
    let mut egraph = EGraph::new(SynthAnalysis { cvec_len: n });
    let mut vars = HashSet::new();
    for node in lhs.as_ref().iter().chain(rhs.as_ref()) {
        if let Math::Var(v) = node {
            vars.insert(v);
        }
    }

    for v in vars {
        let id = egraph.add(Math::Var(*v));
        egraph[id].data.cvec = (0..n).map(|_| rng.gen::<Constant>()).map(Some).collect();
    }

    let lhs_id = egraph.add_expr(lhs);
    let rhs_id = egraph.add_expr(rhs);

    let lhs_cvec = &egraph[lhs_id].data.cvec;
    let rhs_cvec = &egraph[rhs_id].data.cvec;

    lhs_cvec == rhs_cvec
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
            n_samples: 1000,
            constants: vec![0, 1],
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
            n_samples: 1000,
            constants: vec![0, 1],
            variables: vec!["x".into(), "y".into(), "z".into()],
        });
        let eqs = syn.run_mrat(2);

        println!("CHECKING! Found {} rules", eqs.len());
        for eq in eqs.values() {
            println!("  {}", eq);
        }
        println!("CHECKED! Found {} rules", eqs.len());

        check_proves(&eqs, "(+ a b)", "(+ b a)");
        check_proves(&eqs, "(* a b)", "(* b a)");
        check_proves(&eqs, "(+ 1 1)", "2");
        check_proves(&eqs, "a", "(* 1 a)");
        check_proves(&eqs, "a", "(+ a 0)");
    }

    #[test]
    fn validate_iowa() {
        use std::fs::File;
        use std::io::{self, BufRead};
        use std::path::Path;

        let mut pairs: Vec<(RecExpr, RecExpr)> = vec![];
        let file = File::open("bv.txt").unwrap();
        let reader = io::BufReader::new(file);
        for line in reader.lines() {
            let line = line.unwrap();
            let split: Vec<&str> = line.split(" => ").collect();
            assert_eq!(split.len(), 2);
            let lhs = split[0].parse().unwrap();
            let rhs = split[1].parse().unwrap();
            pairs.push((lhs, rhs));
        }

        println!("Parsed {} rules", pairs.len());

        let n_tests = 10_000;
        let rng = &mut Pcg64::seed_from_u64(1);
        let mut n_correct = 0;
        for (lhs, rhs) in &pairs {
            if validate(n_tests, rng, lhs, rhs) {
                println!("correct: {} = {}", lhs, rhs);
                n_correct += 1;
            }
        }

        println!(
            "{} of {} ({}) were correct",
            n_correct,
            pairs.len(),
            n_correct as f64 / pairs.len() as f64
        );
    }
}
