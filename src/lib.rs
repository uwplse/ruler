use std::{collections::HashSet, fmt::Display};

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

pub type Constant = i32;
pub type CVec = Vec<Option<Constant>>;

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

fn fold1(a: &CVec, mut f: impl FnMut(Constant) -> Option<Constant>) -> CVec {
    a.iter().map(|x| x.and_then(&mut f)).collect()
}

fn fold2(a: &CVec, b: &CVec, mut f: impl FnMut(Constant, Constant) -> Option<Constant>) -> CVec {
    assert_eq!(a.len(), b.len());
    a.iter()
        .zip(b)
        .map(|(x, y)| match (x, y) {
            (Some(n1), Some(n2)) => f(*n1, *n2),
            (_, _) => None,
        })
        .collect()
}

#[derive(Debug, Clone, Default)]
pub struct SynthAnalysis {
    cvec_len: usize,
}

impl Analysis<Math> for SynthAnalysis {
    type Data = CVec;

    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        let v = |i: &Id| &egraph[*i].data;
        let param = &egraph.analysis;
        let cvec: CVec = match enode {
            Math::Neg(a) => fold1(v(a), |a| Some(a.wrapping_neg())),
            Math::Add([a, b]) => fold2(v(a), v(b), |a, b| Some(a.wrapping_add(b))),
            Math::Sub([a, b]) => fold2(v(a), v(b), |a, b| Some(a.wrapping_sub(b))),
            Math::Mul([a, b]) => fold2(v(a), v(b), |a, b| Some(a.wrapping_mul(b))),
            Math::Num(n) => (0..param.cvec_len).map(|_| Some(n.clone())).collect(),
            Math::Var(_) => vec![],
        };
        cvec
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        if !to.is_empty() && !from.is_empty() {
            assert_eq!(to, &from)
        }
        false
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        let cv = &egraph[id].data;
        if cv.is_empty() || cv.contains(&None) {
            return;
        }
        let first = cv[0].clone();
        if cv.iter().all(|x| *x == first) {
            match first {
                Some(n) => {
                    let added = egraph.add(Math::Num(n.clone()));
                    egraph.union(id, added);
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

pub struct Synthesizer {
    params: SynthParams,
    rng: Pcg64,
    egraph: EGraph,
    equalities: Vec<Equality<Math, SynthAnalysis>>,
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
            egraph[id].data = (0..params.n_samples)
                .map(|_| rng.gen::<Constant>())
                .chain(params.constants.iter().cloned())
                .map(Some)
                .collect();
            egraph[id].data.shuffle(&mut rng);
        }

        for n in &params.constants {
            egraph.add(Math::Num(n.clone()));
        }

        Self {
            rng,
            egraph,
            params,
            equalities: vec![],
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
        let rewrites = self.equalities.iter().map(|eq| &eq.rewrite);
        let runner = Runner::new(self.egraph.analysis.clone())
            .with_egraph(self.egraph.clone())
            .with_iter_limit(3)
            .with_node_limit(usize::MAX)
            .with_scheduler(BackoffScheduler::default())
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

    fn cvec_match(&mut self, dirty_eg: &EGraph) -> Vec<Equality> {
        // build the cvec matching data structure
        let mut by_cvec: IndexMap<&CVec, Vec<Id>> = IndexMap::new();
        for id in self.ids() {
            let class = &self.egraph[id];
            by_cvec.entry(&class.data).or_default().push(class.id);
        }

        let mut new_eqs = vec![];
        let mut extract = Extractor::new(dirty_eg, AstSize);
        let mut to_merge: Vec<(Id, Id)> = vec![];
        for ids in by_cvec.values() {
            let mut id_iter = ids.iter();
            while let Some(&id1) = id_iter.next() {
                for &id2 in id_iter.clone() {
                    to_merge.push((id1, id2));
                    let (_, e1) = extract.find_best(id1);
                    let (_, e2) = extract.find_best(id2);
                    let map = &mut HashMap::default();
                    let p1 = generalize(&e1, map);
                    let p2 = generalize(&e2, map);

                    if let Some(eq) = Equality::new(p1.clone(), p2.clone()) {
                        if !self.equalities.contains(&eq) && !new_eqs.contains(&eq) {
                            log::debug!("  Candidate {}", eq);
                            new_eqs.push(eq);
                        }
                    }

                    if let Some(eq) = Equality::new(p2, p1) {
                        if !self.equalities.contains(&eq) && !new_eqs.contains(&eq) {
                            log::debug!("  Candidate {}", eq);
                            new_eqs.push(eq);
                        }
                    }
                }
            }
        }

        new_eqs
    }

    pub fn run_mrat(mut self, iters: usize) -> Vec<Equality> {
        for _ in 0..iters {
            self.add_layer();
            let dirty = self.run_rewrites();
            let new_eqs = self.cvec_match(&dirty);
            for eq in new_eqs {
                if !self.equalities.contains(&eq) {
                    self.equalities.push(eq);
                }
            }
            // TODO minimize
        }
        self.equalities
    }

    pub fn run_orat(mut self, iters: usize) -> Vec<Equality> {
        for _ in 0..iters {
            self.add_layer();
            loop {
                let dirty = self.run_rewrites();
                let new_eqs = self.cvec_match(&dirty);
                if new_eqs.is_empty() {
                    break
                }
                let eq = choose_best_eq(new_eqs);
                assert!(!self.equalities.contains(&eq));
                log::info!("Chose best {}", eq);
                self.equalities.push(eq);
            }
        }
        self.equalities
    }
}

fn rank(eq: &Equality) -> (usize, isize) {
    let mut vars: HashSet<Var> = Default::default();
    vars.extend(eq.lhs.vars());
    vars.extend(eq.rhs.vars());
    let size = eq.lhs.ast.as_ref().len() + eq.rhs.ast.as_ref().len();
    (vars.len(), -(size as isize))
}

fn choose_best_eq(new_eqs: Vec<Equality>) -> Equality {
    new_eqs.iter().max_by_key(|eq| rank(eq)).unwrap().clone()
}

// TODO should probably keep things together
#[derive(Clone)]
pub struct Equality<L = Math, A = SynthAnalysis> {
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    // pub cond: Option<Pattern<L>>,
    pub name: String,
    pub rewrite: egg::Rewrite<L, A>,
}

impl PartialEq for Equality {
    fn eq(&self, other: &Self) -> bool {
        self.lhs == other.lhs && self.rhs == other.rhs
    }
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rewrite.name())
    }
}

impl Equality<Math, SynthAnalysis> {
    fn new(lhs: Pattern, rhs: Pattern) -> Option<Self> {
        let name = format!("{} => {}", lhs, rhs);
        let rw = egg::Rewrite::new(name.clone(), lhs.clone(), rhs.clone()).ok()?;
        Some(Self {
            lhs,
            rhs,
            name,
            rewrite: rw,
        })
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
            constants: vec![0, 1],
            variables: vec!["x".into(), "y".into(), "z".into()],
        });
        let eqs = syn.run_orat(1);

        for eq in &eqs {
            println!("{}", eq);
        }
        println!("found {} rules", eqs.len());
    }
}
