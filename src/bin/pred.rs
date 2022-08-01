use std::{
    fmt::{self, Display},
    hash::Hash,
    str::FromStr,
};

use egg::*;
use rand::Rng;
use rand_pcg::Pcg64;
use ruler::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]

pub struct BVar(egg::Symbol);

impl Display for BVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b")
    }
}

impl FromStr for BVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("b") {
            Ok(BVar(Symbol::from(s)))
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]

pub struct IVar(egg::Symbol);

impl Display for IVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "i")
    }
}

impl FromStr for IVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("i") {
            Ok(IVar(Symbol::from(s)))
        } else {
            Err(())
        }
    }
}

define_language! {
  pub enum Pred {
    Lit(Constant),
    BVar(BVar),
    IVar(IVar),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Bool,
    Int,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "b"),
            Type::Int => write!(f, "i"),
        }
    }
}

impl SynthLanguage for Pred {
    type Constant = Constant;
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        match self {
            Pred::Lit(c) => match c {
                Constant::Bool(_) => Type::Bool,
                Constant::Int(_) => Type::Int,
            },
            Pred::IVar(_) => Type::Int,
            _ => Type::Bool,
        }
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Pred::Lit(c) => vec![Some(c.clone()); cvec_len],
            Pred::Le([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() < y.to_int().unwrap())))
            }
            Pred::Leq([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() <= y.to_int().unwrap())))
            }
            Pred::Ge([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() > y.to_int().unwrap())))
            }
            Pred::Geq([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() >= y.to_int().unwrap())))
            }
            Pred::Eq([x, y]) => {
                map!(v, x, y => {
                    match (x,y) {
                        (Constant::Bool(x), Constant::Bool(y)) => Some(Constant::Bool(x == y)),
                        (Constant::Int(x), Constant::Int(y)) => Some(Constant::Bool(x == y)),
                        _ => panic!("Cannot compare Bool and Int: {} {}", x, y)
                    }
                })
            }
            Pred::Neq([x, y]) => {
                map!(v, x, y => {
                    match (x,y) {
                        (Constant::Bool(x), Constant::Bool(y)) => Some(Constant::Bool(x != y)),
                        (Constant::Int(x), Constant::Int(y)) => Some(Constant::Bool(x != y)),
                        _ => panic!("Cannot compare Bool and Int: {} {}", x, y)
                    }
                })
            }
            Pred::Not(x) => map!(v, x => Some(Constant::Bool(!x.to_bool().unwrap()))),
            Pred::And([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_bool().unwrap() & y.to_bool().unwrap())))
            }
            Pred::Or([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_bool().unwrap() | y.to_bool().unwrap())))
            }
            Pred::Xor([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_bool().unwrap() ^ y.to_bool().unwrap())))
            }

            Pred::BVar(_) => vec![],
            Pred::IVar(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Pred::BVar(BVar(sym)) => Some(*sym),
            Pred::IVar(IVar(sym)) => Some(*sym),
            _ => None,
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        if sym.as_str().starts_with("b") {
            Pred::BVar(BVar(sym))
        } else if sym.as_str().starts_with("i") {
            Pred::IVar(IVar(sym))
        } else {
            panic!("invalid variable: {}", sym)
        }
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
            let rng = &mut synth.rng;
            let mut i_vals = vec![];
            let mut b_vals = vec![];
            let i_id = egraph.add(Pred::IVar(IVar(Symbol::from("i".to_owned() + letter(i)))));
            let b_id = egraph.add(Pred::BVar(BVar(Symbol::from("b".to_owned() + letter(i)))));
            for _ in 0..10 {
                i_vals.push(Some(Constant::Int(rng.gen::<usize>())));
                b_vals.push(Some(Constant::Bool(rng.gen::<bool>())));
            }

            egraph[i_id].data.cvec = i_vals.clone();
            egraph[b_id].data.cvec = b_vals.clone();
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
