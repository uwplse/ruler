use std::fmt;

use egg::*;
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
    "!" = Not(Id),
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
        match usize::from_str(s) {
            Ok(i) => Ok(Self::Int(i)),
            Err(_) => Ok(Self::Bool(false)),
        }
    }
}

impl Constant {
    fn to_int(&self) -> Option<usize> {
        if let &Constant::Int(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn to_bool(&self) -> Option<bool> {
        if let &Constant::Bool(b) = self {
            Some(b)
        } else {
            None
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

            Pred::Le([x, y]) => map!(v, x, y => {
                let x = x.to_int().unwrap();
                let y = y.to_int().unwrap();
                Some(Constant::Bool(x < y))
            }),
            Pred::Leq([x, y]) => map!(v, x, y => {
                let x = x.to_int().unwrap();
                let y = y.to_int().unwrap();
                Some(Constant::Bool(x <= y))
            }),
            Pred::Ge([x, y]) => map!(v, x, y => {
                let x = x.to_int().unwrap();
                let y = y.to_int().unwrap();
                Some(Constant::Bool(x > y))
            }),
            Pred::Geq([x, y]) => map!(v, x, y => {
                let x = x.to_int().unwrap();
                let y = y.to_int().unwrap();
                Some(Constant::Bool(x >= y))
            }),
            Pred::Eq([x, y]) => map!(v, x, y => {
                let x = x.to_int().unwrap();
                let y = y.to_int().unwrap();
                Some(Constant::Bool(x == y))
            }),
            Pred::Neq([x, y]) => map!(v, x, y => {
                let x = x.to_int().unwrap();
                let y = y.to_int().unwrap();
                Some(Constant::Bool(x != y))
            }),
            Pred::Not(x) => map!(v,x => {
              let x = x.to_bool().unwrap();
              Some(Constant::Bool(!x))
            }),
            Pred::And([x, y]) => map!(v,x,y => {
              let x = x.to_bool().unwrap();
              let y = y.to_bool().unwrap();
              Some(Constant::Bool(x & y))
            }),
            Pred::Or([x, y]) => map!(v,x,y => {
              let x = x.to_bool().unwrap();
              let y = y.to_bool().unwrap();
              Some(Constant::Bool(x | y))
            }),
            Pred::Xor([x, y]) => map!(v,x,y => {
              let x = x.to_bool().unwrap();
              let y = y.to_bool().unwrap();
              Some(Constant::Bool(x ^ y))
            }),

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
        todo!()
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        todo!()
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        todo!()
    }
}

fn main() {
    Pred::main()
}
