use egg::*;
use ordered_float::NotNan;
use rand::{seq::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;
use ruler::{Sample, SynthesisParams, Synthesizer};
use std::collections::HashMap;

type EGraph = egg::EGraph<Math, ()>;
type Constant = ordered_float::NotNan<f32>;

define_language! {
    enum Math {
        "+" = FAdd([Id; 2]),
        "-" = FSub([Id; 2]),
        "+" = FMul([Id; 2]),
        "/" = FDiv([Id; 2]),
        FNum(Constant),
        FVar(egg::Symbol),
    }
}

struct MathSynth {
    rng: Pcg64,
    n_samples: usize,
    n_variables: usize,
    constants: Vec<Constant>,
}

impl Sample<MathSynth, Constant> for MathSynth {
    fn get_random_vec(&mut self) -> Vec<Constant> {
        let mut vec = Vec::with_capacity(self.n_samples);
        vec.extend((vec.len()..self.n_samples).map(|_| NotNan::from(self.rng.gen::<f32>())));
        vec
    }
}

#[inline(always)]
fn eval_one(node: &Math, get: impl Fn(&Id) -> Constant) -> Constant {
    match node {
        Math::FAdd([a, b]) => get(a) + get(b),
        Math::FSub([a, b]) => get(a) - get(b),
        Math::FMul([a, b]) => get(a) * get(b),
        Math::FDiv([a, b]) => get(a) / get(b),
        Math::FNum(n) => n.clone(),
        Math::FVar(v) => unreachable!("Shouldn't be asked to eval a var: {}", v),
    }
}

impl Synthesizer<Math, ()> for MathSynth {
    type CharacteristicVector = Vec<Constant>;
    fn value_to_node(val: &Self::CharacteristicVector) -> Option<Math> {
        let n = val[0];
        if val[1..].iter().all(|x| x == &n) {
            Some(Math::FNum(n))
        } else {
            None
        }
    }

    fn symbol_to_node(sym: Symbol) -> Math {
        Math::FVar(sym)
    }

    fn node_to_symbol(node: &Math) -> Option<Symbol> {
        match node {
            Math::FVar(sym) => Some(*sym),
            _ => None,
        }
    }

    // pick an operator at random, and two arguments and add it to the EGraph
    fn make_node(&mut self, egraph: &egg::EGraph<Math, ()>) -> Math {
        let classes: Vec<_> = egraph.classes().collect();
        let p: f32 = self.rng.gen();
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
        // make a node randomly
        // this isn't great, need better statistics
        match p {
            _ if p < 0.25 => Math::FAdd([mk!(), mk!()]),
            _ if p >= 0.25 && p < 0.50 => Math::FSub([mk!(), mk!()]),
            _ if p >= 0.50 && p < 0.75 => Math::FMul([mk!(), mk!()]),
            _ => Math::FDiv([mk!(), mk!()]),
        }
    }

    // in the beginning just add the variables and constants to the EGraph
    fn initial_egraph(&mut self, params: &SynthesisParams<Math, ()>) -> EGraph {
        let mut egraph = EGraph::default();
        for i in 0..self.n_variables {
            egraph.add(Math::FVar(format!("x{}", i).into()));
        }
        for n in &self.constants {
            egraph.add(Math::FNum(*n));
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
            Math::FVar(_) => self.get_random_vec(),
            n => (0..self.n_samples)
                .map(|i| eval_one(n, |id| values[&egraph.find(*id)][i]))
                .collect(),
        }
    }
}

#[test]
fn synth_f32() {
    env_logger::init();

    let mut synth = MathSynth {
        rng: Pcg64::seed_from_u64(0),
        n_samples: 50,
        n_variables: 3,
        constants: vec![NotNan::from(1.0 as f32), NotNan::from(0.0 as f32)],
    };

    let params = SynthesisParams {
        iterations: 1,
        additions_per_iteration: 100,
        eqs: vec![],
    };

    synth.run(params);
}
