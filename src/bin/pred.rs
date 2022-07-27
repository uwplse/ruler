use std::{fmt, hash::Hash};

use egg::*;
use rand::Rng;
use rand_pcg::Pcg64;
use ruler::*;

define_language! {
  pub enum Pred {
    Lit(Constant),
    Var(egg::Symbol),
    "<" = Le([Id;2]),
    "<=" = Leq([Id;2]),
    ">" = Ge([Id;2]),
    ">=" = Geq([Id;2]),
    "==" = Eq([Id;2]),
    "!=" = Neq([Id;2]),
    "~" = Not(Id),
    "&" = And([Id;2]),
    "|" = Or([Id;2]),
    "^" = Xor([Id;2]),
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    Bool(bool),
    Int(usize),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Bool(b) => b.fmt(f),
            Constant::Int(i) => i.fmt(f),
        }
    }
}

impl std::str::FromStr for Constant {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(i) = usize::from_str(s) {
            Ok(Self::Int(i))
        } else if let Ok(b) = bool::from_str(s) {
            Ok(Self::Bool(b))
        } else {
            Err(())
        }
    }
}

impl Constant {
    fn to_int(&self) -> Option<usize> {
        match self {
            Constant::Int(n) => Some(*n),
            Constant::Bool(_) => None,
        }
    }

    fn to_bool(&self) -> Option<bool> {
        match self {
            Constant::Bool(b) => Some(*b),
            Constant::Int(_) => None,
        }
    }
}

impl SynthLanguage for Pred {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Pred::Lit(c) => vec![Some(c.clone()); cvec_len],
            Pred::Le([x, y]) => {
                map!(v, x, y => x.to_int().zip(y.to_int()).map(|(x,y)|Constant::Bool(x < y)))
            }
            Pred::Leq([x, y]) => {
                map!(v, x, y => x.to_int().zip(y.to_int()).map(|(x,y)|Constant::Bool(x <= y)))
            }
            Pred::Ge([x, y]) => {
                map!(v, x, y => x.to_int().zip(y.to_int()).map(|(x,y)|Constant::Bool(x > y)))
            }
            Pred::Geq([x, y]) => {
                map!(v, x, y => x.to_int().zip(y.to_int()).map(|(x,y)|Constant::Bool(x >= y)))
            }
            Pred::Eq([x, y]) => {
                map!(v, x, y => x.to_int().zip(y.to_int()).map(|(x,y)|Constant::Bool(x == y)))
            }
            Pred::Neq([x, y]) => {
                map!(v, x, y => x.to_int().zip(y.to_int()).map(|(x,y)|Constant::Bool(x != y)))
            }
            Pred::Not(x) => map!(v, x => x.to_bool().map(|x|Constant::Bool(!x))),
            Pred::And([x, y]) => {
                map!(v, x, y => x.to_bool().zip(y.to_bool()).map(|(x,y)|Constant::Bool(x & y)))
            }
            Pred::Or([x, y]) => {
                map!(v, x, y => x.to_bool().zip(y.to_bool()).map(|(x,y)|Constant::Bool(x | y)))
            }
            Pred::Xor([x, y]) => {
                map!(v, x, y => x.to_bool().zip(y.to_bool()).map(|(x,y)|Constant::Bool(x ^ y)))
            }

            Pred::Var(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Pred::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Pred::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Pred::Lit(_))
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Pred::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut egraph: EGraph<Pred, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 10,
            constant_fold: ConstantFoldMethod::NoFold,
            rule_lifting: false,
        });

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Pred::Var(var));
            let mut vals = vec![];
            let rng = &mut synth.rng;
            for _ in 0..10 {
                vals.push(Some(Constant::Int(rng.gen::<usize>())));
            }
            egraph[id].data.cvec = vals.clone();
        }

        synth.egraph = egraph;
    }

    fn make_layer(_synth: &Synthesizer<Self>, _iter: usize) -> Vec<Self> {
        vec![]
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        let n = synth.params.num_fuzz;
        let mut env = HashMap::default();

        for var in lhs.vars() {
            env.insert(var, vec![]);
        }

        for var in rhs.vars() {
            env.insert(var, vec![]);
        }

        for cvec in env.values_mut() {
            cvec.reserve(n);
            for s in sampler(&mut synth.rng, n) {
                cvec.push(Some(s));
            }
        }

        let lvec = Self::eval_pattern(lhs, &env, n);
        let rvec = Self::eval_pattern(rhs, &env, n);
        ValidationResult::from(lvec == rvec)
    }
}

pub fn sampler(rng: &mut Pcg64, num_samples: usize) -> Vec<Constant> {
    let mut ret = vec![];
    for _ in 0..num_samples {
        let flip = rng.gen::<bool>();
        if flip {
            ret.push(Constant::Int(rng.gen::<usize>() % 10));
        } else {
            ret.push(Constant::Int(rng.gen::<usize>()));
        }
    }
    ret
}

fn main() {
    Pred::main()
}
