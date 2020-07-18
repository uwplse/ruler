use egg::*;
use num_bigint::BigInt;
use num_bigint::{RandBigInt, ToBigInt};
use rand::prelude::*;
use rand::{seq::SliceRandom, Rng};
use ruler::{Sample, SynthesisParams, Synthesizer};
use std::collections::HashMap;

type EGraph = egg::EGraph<Math, ()>;
type Constant = BigInt;

define_language! {
    enum Math {
        "+" = BAdd([Id; 2]),
        "-" = BSub([Id; 2]),
        "+" = BMul([Id; 2]),
        "/" = BDiv([Id; 2]),
        BNum(Constant),
        BVar(egg::Symbol),
    }
}

struct MathSynth {
    rng: ThreadRng,
    n_samples: usize,
    n_variables: usize,
    constants: Vec<Constant>,
}

impl Sample<MathSynth, Constant> for MathSynth {
    fn get_random_vec(&mut self) -> Vec<Constant> {
        let mut vec = Vec::with_capacity(self.n_samples);
        vec.extend((vec.len()..self.n_samples).map(|_| self.rng.gen_bigint(1000)));
        vec
    }
}

impl Synthesizer<Math, ()> for MathSynth {
    type CharacteristicVector = Vec<Constant>;
    fn value_to_node(val: &Self::CharacteristicVector) -> Option<Math> {
        let n = &val[0];
        if val[1..].iter().all(|x| x == n) {
            Some(Math::BNum(n.clone()))
        } else {
            None
        }
    }

    fn symbol_to_node(sym: Symbol) -> Math {
        Math::BVar(sym)
    }

    fn node_to_symbol(node: &Math) -> Option<Symbol> {
        match node {
            Math::BVar(sym) => Some(*sym),
            _ => None,
        }
    }

    fn make_node(&mut self, egraph: &egg::EGraph<Math, ()>) -> Math {
        let classes: Vec<_> = egraph.classes().collect();
        macro_rules! mk {
            () => {
                // if true, generate two random new variable
                // assuming n_variables is 2
                if self.rng.gen_bool(0.3) {
                    self.rng.gen_range(0, self.n_variables as Id)
                }
                // else choose two existing classes
                else {
                    classes.choose(&mut self.rng).unwrap().id
                }
            };
        }
        let p: f32 = self.rng.gen();
        // either make an Add node or a Mul node, randomly
        match p {
            _ if p < 0.25 => Math::BAdd([mk!(), mk!()]),
            _ if p >= 0.25 && p < 0.50 => Math::BSub([mk!(), mk!()]),
            _ if p >= 0.50 && p < 0.75 => Math::BMul([mk!(), mk!()]),
            _ => Math::BDiv([mk!(), mk!()]),
        }
    }

    fn initial_egraph(&mut self, params: &SynthesisParams<Math, ()>) -> EGraph {
        let mut egraph = EGraph::default();
        for i in 0..self.n_variables {
            egraph.add(Math::BVar(format!("x{}", i).into()));
        }
        for n in &self.constants {
            egraph.add(Math::BNum(n.clone()));
        }
        egraph
    }

    fn eval(
        &mut self,
        enode: &Math,
        egraph: &EGraph,
        values: &HashMap<Id, Self::CharacteristicVector>,
    ) -> Self::CharacteristicVector {
        match enode {
            Math::BVar(_) => self.get_random_vec(),
            n => (0..self.n_samples)
                .map(|i| eval_one(n, |id| values[&egraph.find(*id)][i].clone()))
                .collect(),
        }
    }
}

#[inline(always)]
fn eval_one(node: &Math, get: impl Fn(&Id) -> Constant) -> Constant {
    match node {
        Math::BAdd([a, b]) => get(a) + get(b),
        Math::BSub([a, b]) => get(a) - get(b),
        Math::BMul([a, b]) => get(a) * get(b),
        Math::BDiv([a, b]) => get(a) / get(b),
        Math::BNum(n) => n.clone(),
        Math::BVar(v) => unreachable!("Shouldn't be asked to eval a var: {}", v),
    }
}

#[test]
fn synth_bigint() {
    env_logger::init();

    let mut synth = MathSynth {
        rng: rand::thread_rng(),
        n_samples: 50,
        n_variables: 3,
        constants: vec![
            ToBigInt::to_bigint(&0).unwrap(),
            ToBigInt::to_bigint(&1).unwrap(),
        ],
    };

    let params = SynthesisParams {
        iterations: 1,
        additions_per_iteration: 100,
        eqs: vec![],
    };

    synth.run(params);
}
