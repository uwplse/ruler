use std::{collections::HashSet, fmt::Display, hash::Hash, rc::Rc};

use egg::*;

use indexmap::{IndexMap, IndexSet};
use rand::{prelude::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;
use std::collections::HashMap;

type Runner = egg::Runner<Math, SynthAnalysis, ()>;
type Pattern<L = Math> = egg::Pattern<L>;
type RecExpr<L = Math> = egg::RecExpr<L>;
type Rewrite<L = Math, A = SynthAnalysis> = egg::Rewrite<L, A>;
type EGraph<L = Math, A = SynthAnalysis> = egg::EGraph<L, A>;

pub type Constant = i32;
pub type CVec = Vec<Option<Constant>>;

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
        Var(egg::Symbol),
        Num(Constant),
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
        assert_eq!(self.cvec.len(), other.cvec.len());
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

#[derive(Debug, Clone, Default)]
pub struct SynthAnalysis {
    cvec_len: usize,
}

impl Analysis<Math> for SynthAnalysis {
    type Data = Signature;

    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        let v = |i: &Id| &egraph[*i].data;
        let param = &egraph.analysis;
        match enode {
            Math::Neg(a) => v(a).fold1(|a| Some(a.wrapping_neg())),
            Math::Add([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_add(b))),
            Math::Sub([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_sub(b))),
            Math::Mul([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_mul(b))),
            Math::Num(n) => Signature {
                cvec: (0..param.cvec_len).map(|_| Some(n.clone())).collect(),
                exact: true,
            },
            Math::Var(_) => Signature { cvec: vec![], exact: false },
        }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            assert_eq!(&to.cvec, &from.cvec);
            if !to.exact && from.exact {
                to.exact = true;
                return true
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

    fn add_layer(&mut self) {
        let mut to_add = vec![];
        for i in self.ids() {
            for j in self.ids() {
                to_add.push(Math::Add([i, j]));
                to_add.push(Math::Mul([i, j]));
                to_add.push(Math::Sub([i, j]));
            }
            to_add.push(Math::Neg(i));
        }
        for node in to_add {
            self.egraph.add(node);
        }
    }

    fn run_rewrites(&mut self) -> EGraph {
        // run the rewrites
        let rewrites = self.equalities.values().flat_map(|eq| &eq.rewrites);
        let runner = Runner::new(self.egraph.analysis.clone())
            .with_egraph(self.egraph.clone())
            .with_node_limit(usize::MAX)
            .with_iter_limit(2)
            .with_scheduler(SimpleScheduler)
            // .with_iter_limit(20)
            // .with_scheduler(BackoffScheduler::default())
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

        runner.egraph
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
            self.add_layer();
            self.run_rewrites();
            let new_eqs = self.cvec_match();
            self.equalities.extend(new_eqs);
            // TODO minimize
        }
        self.equalities
    }

    pub fn run_orat(mut self, iters: usize) -> EqualityMap {
        for _ in 0..iters {
            self.add_layer();
            loop {
                self.run_rewrites();
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
        self.equalities
    }
}

fn score(eq: &Equality) -> (usize, isize) {
    let mut vars: HashSet<Var> = Default::default();
    vars.extend(eq.lhs.vars());
    vars.extend(eq.rhs.vars());
    let size = usize::max(eq.lhs.ast.as_ref().len(), eq.rhs.ast.as_ref().len());
    (vars.len(), -(size as isize))
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

    // fn check_proves<L, A>(eqs: &[Equality<L, A>], a: &str, b: &str)
    // where
    //     L: Language,
    //     A: Analysis<L> + Default,
    // {
    //     let rules = eqs.iter().map(|eq| &eq.rewrite);
    //     let runner: Runner = Runner::default()
    //         .with_expr(&a.parse().unwrap())
    //         .with_expr(&b.parse().unwrap())
    //         .with_hook(|runner| {
    //             if runner.egraph.find(runner.roots[0]) == runner.egraph.find(runner.roots[1]) {
    //                 Err(format!("Done early"))
    //             } else {
    //                 Ok(())
    //             }
    //         })
    //         .run(rules);

    //     let id_a = runner.egraph.find(runner.roots[0]);
    //     let id_b = runner.egraph.find(runner.roots[1]);

    //     if id_a != id_b {
    //         panic!("Failed to simplify {} => {}", a, b)
    //     }
    // }

    #[test]
    fn test1() {
        let _ = env_logger::try_init();
        let syn = Synthesizer::new(SynthParams {
            seed: 5,
            n_samples: 10,
            constants: vec![0, 1, -1],
            variables: vec!["x".into(), "y".into(), "z".into()],
            // variables: vec!["x".into(), "y".into(), "z".into(), "w".into()],
        });
        let eqs = syn.run_orat(2);

        for eq in eqs.values() {
            println!("{}", eq);
        }
        println!("found {} rules", eqs.len());
    }
}
