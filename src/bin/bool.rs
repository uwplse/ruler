use egg::*;
use ruler::*;

use std::ops::*;

use rand_pcg::Pcg64;

define_language! {
    pub enum Math {
        "~" = Not(Id),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "^" = Xor([Id; 2]),
        Lit(bool),
        Var(egg::Symbol),
    }
}

impl SynthLanguage for Math {
    type Constant = bool;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Not(a) => map!(v, a => Some(a.not())),

            Math::And([a, b]) => map!(v, a, b => Some(*a & *b)),
            Math::Or([a, b]) => map!(v, a, b => Some(*a | *b)),
            Math::Xor([a, b]) => map!(v, a, b => Some(*a ^ *b)),

            Math::Lit(n) => vec![Some(n.clone()); cvec_len],
            Math::Var(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Math::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::Lit(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let consts: Vec<Option<bool>> = vec![Some(false), Some(true)];

        let consts = self_product(&consts, synth.params.variables);
        println!("cvec len: {}", consts[0].len());

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: consts[0].len(),
        });

        egraph.add(Math::Lit(false));
        egraph.add(Math::Lit(true));

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));
            egraph[id].data.cvec = consts[i].clone();
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>) -> Vec<Self> {
        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                }
                to_add.push(Math::And([i, j]));
                to_add.push(Math::Or([i, j]));
                if !synth.params.no_xor {
                    to_add.push(Math::Xor([i, j]));
                }
            }
            if synth.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Not(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(_rng: &mut Pcg64, _lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> bool {
        true
    }
}

fn main() {
    Math::main()
}
