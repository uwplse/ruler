use egg::*;
use rand::{seq::SliceRandom, Rng, SeedableRng};

use rand_pcg::Pcg64;
use ruler::{Sample, SynthesisParams, Synthesizer};
use std::collections::HashMap;

type EGraph = egg::EGraph<Math, ()>;
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

impl Sample<MathSynth, Constant> for MathSynth {
    fn get_random_vec(&mut self) -> Vec<Constant> {
        let mut vec = Vec::with_capacity(self.n_samples);
        vec.extend((vec.len()..self.n_samples).map(|_| self.rng.gen::<Constant>()));
        vec
    }
}

impl Synthesizer<Math, ()> for MathSynth {
    type CharacteristicVector = Vec<Constant>;

    fn value_to_node(val: &Self::CharacteristicVector) -> Option<Math> {
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
            _ => None,
        }
    }

    fn make_node(&mut self, egraph: &egg::EGraph<Math, ()>) -> Math {
        let classes: Vec<_> = egraph.classes().collect();
        macro_rules! mk {
            () => {
                // if true, generate two random new variable
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

    fn eval(
        &mut self,
        enode: &Math,
        egraph: &EGraph,
        values: &HashMap<Id, Self::CharacteristicVector>,
    ) -> Self::CharacteristicVector {
        match enode {
            Math::Var(_) => self.get_random_vec(),
            n => (0..self.n_samples)
                .map(|i| eval_one(n, |id| values[&egraph.find(*id)][i]))
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
fn synth_integer() {
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
