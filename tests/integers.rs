use egg::*;
use rand::{seq::SliceRandom, Rng, SeedableRng};

use rand_pcg::Pcg64;
use ruler::{SynthesisParams, Synthesizer};
use std::cell::RefCell;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
};

type EGraph = egg::EGraph<Math, ()>;
type RecExpr = egg::RecExpr<Math>;
type Pattern = egg::Pattern<Math>;
type Rewrite = egg::Rewrite<Math, ()>;
type Runner = egg::Runner<Math, (), ()>;

type Constant = i32;

define_language! {
    enum Math {
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
    }
}

struct MathSynth {
    rng: Pcg64,
    n_samples: usize,
    n_variables: usize,
    constants: Vec<Constant>,
}

impl Synthesizer<Math, ()> for MathSynth {
    type Value = Vec<Constant>;

    fn value_to_node(val: &Self::Value) -> Option<Math> {
        let n = val[0];
        if val[1..].iter().all(|x| x == &n) {
            Some(Math::Num(n))
        } else {
            None
        }
    }

    fn symbol_to_node(sym: Symbol) -> Math {
        Math::Var(sym)
    }

    fn node_to_symbol(node: &Math) -> Option<Symbol> {
        match node {
            Math::Var(sym) => Some(*sym),
            _ => None
        }
    }

    fn make_node(&mut self, egraph: &egg::EGraph<Math, ()>) -> Math {
        let classes: Vec<_> = egraph
            .classes()
            // .filter(|c| c.data.depth < 3 && c.data.constant().map_or(true, |n| -2 <= n && n <= 2))
            .collect();
        macro_rules! mk {
            () => {
                if self.rng.gen_bool(0.3) {
                    self.rng.gen_range(0, self.n_variables as Id)
                } else {
                    classes
                        .choose(&mut self.rng)
                        // .choose_weighted(rng, |c| (max_depth - c.data.depth).pow(2))
                        .unwrap()
                        .id
                }
            };
        }
        let p: f32 = self.rng.gen();
        match p {
            _ if p < 0.5 => Math::Add([mk!(), mk!()]),
            _ => Math::Mul([mk!(), mk!()]),
        }
    }
    fn initial_egraph(&mut self, params: &SynthesisParams<Math, ()>) -> EGraph {
        let mut egraph = EGraph::default();

        for i in 0..self.n_variables {
            egraph.add(Math::Var(format!("x{}", i).into()));
        }

        for n in &self.constants {
            egraph.add(Math::Num(*n));
        }

        egraph
    }
    fn eval(&mut self, enode: &Math, egraph: &EGraph, values: &HashMap<Id, Self::Value>) -> Self::Value {
        match enode {
            Math::Var(_) => {
                let mut vec = Vec::with_capacity(self.n_samples);
                vec.extend(self.constants.iter().cloned());
                vec.extend((vec.len()..self.n_samples).map(|_| self.rng.gen::<Constant>()));
                vec
            }
            n => (0..self.n_samples)
                .map(|i| eval_one(n, |id| {
                    // println!("accessing {}, {}, {:?}", id, egraph.find(*id), egraph[*id]);
                    values[&egraph.find(*id)][i]
                }))
                // .map(|i| eval_one(n, |id| values[id][i]))
                .collect(),
        }
    }
}

#[inline(always)]
fn eval_one(node: &Math, get: impl Fn(&Id) -> Constant) -> Constant {
    match node {
        Math::Add([a, b]) => get(a).wrapping_add(get(b)),
        Math::Mul([a, b]) => get(a).wrapping_mul(get(b)),
        Math::Num(n) => *n,
        Math::Var(v) => unreachable!("Shouldn't be asked to eval a var: {}", v),
    }
}

#[test]
fn synth_math() {
    env_logger::init();

    let mut synth = MathSynth {
        rng: Pcg64::seed_from_u64(0),
        n_samples: 50,
        n_variables: 3,
        constants: vec![0, 1],
    };

    let params = SynthesisParams {
        iterations: 4,
        additions_per_iteration: 100,
        eqs: vec![],
    };

    synth.run(params);
}

// #[derive(Default, Clone)]
// struct Sampler {
//     n_samples: usize,
//     vars: HashMap<egg::Symbol, Vec<Constant>>,
//     eqs: RefCell<BTreeMap<RecExpr, BTreeSet<RecExpr>>>,
// }

// impl Sampler {
//     fn submit_eq(&self, lhs: &RecExpr, rhs: &RecExpr) {
//         if lhs == rhs {
//             return;
//         }

//         // check if seen
//         if let Some(rights) = self.eqs.borrow().get(lhs) {
//             if rights.contains(rhs) {
//                 return;
//             }
//         }

//         println!("{} = {}", lhs, rhs);
//         let mut eqs = self.eqs.borrow_mut();
//         eqs.entry(lhs.clone()).or_default().insert(rhs.clone());
//     }
// }

// #[derive(Debug)]
// struct SampleData {
//     depth: usize,
//     expr: RecExpr,
//     samples: Vec<Constant>,
// }

// impl SampleData {
//     fn constant(&self) -> Option<Constant> {
//         let n = self.samples[0];
//         if self.samples.iter().all(|&m| n == m) {
//             Some(n)
//         } else {
//             None
//         }
//     }
// }

// impl Analysis<Math> for Sampler {
//     type Data = SampleData;

//     fn make(egraph: &egg::EGraph<Math, Self>, enode: &Math) -> Self::Data {
//         let samples = if let Math::Var(v) = enode {
//             egraph.analysis.vars[v].clone()
//         } else {
//             let n = egraph.analysis.n_samples;
//             (0..n)
//                 .map(|i| eval(enode, |&id| egraph[id].data.samples[i]))
//                 .collect()
//         };

//         SampleData {
//             depth: 1 + enode.fold(0, |depth, id| depth.max(egraph[id].data.depth)),
//             expr: enode.to_recexpr(|id| egraph[id].data.expr.as_ref()),
//             samples,
//         }
//     }

//     fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
//         assert_eq!(&to.samples, &from.samples);
//         // self.submit_eq(&to.expr, &from.expr);
//         if from.depth < to.depth {
//             *to = from;
//             true
//         } else {
//             false
//         }
//     }

//     fn modify(egraph: &mut egg::EGraph<Math, Self>, id: Id) {
//         let mut to_union = vec![];

//         let my_sample = &egraph[id].data.samples;
//         let first = my_sample[0];
//         if my_sample.iter().all(|&s| s == first) {
//             to_union.push(egraph.add(Math::Num(first)))
//         }

//         // let my_sample = &egraph[id].data.samples;
//         // for class in egraph.classes() {
//         //     if class.id != id && &class.data.samples == my_sample {
//         //         to_union.push(class.id)
//         //     }
//         // }

//         for id2 in to_union {
//             egraph.union(id, id2);
//         }
//     }
// }
