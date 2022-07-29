use std::{fmt, hash::Hash};

use egg::*;
use rand::Rng;
use rand_pcg::Pcg64;
use ruler::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Pred {
    Lit(Constant),
    BVar(egg::Symbol),
    IVar(egg::Symbol),
    Le([Id; 2]),
    Leq([Id; 2]),
    Ge([Id; 2]),
    Geq([Id; 2]),
    Eq([Id; 2]),
    Neq([Id; 2]),
    Not(Id),
    And([Id; 2]),
    Or([Id; 2]),
    Xor([Id; 2]),
}
impl Language for Pred {
    #[inline(always)]
    fn matches(&self, other: &Self) -> bool {
        ::std::mem::discriminant(self) == ::std::mem::discriminant(other)
            && match (self, other) {
                (Pred::Lit(data1), Pred::Lit(data2)) => data1 == data2,
                (Pred::BVar(data1), Pred::BVar(data2)) => data1 == data2,
                (Pred::IVar(data1), Pred::IVar(data2)) => data1 == data2,
                (Pred::Not(l), Pred::Not(r)) => {
                    LanguageChildren::len(l) == LanguageChildren::len(r)
                }
                (Pred::Le(l), Pred::Le(r))
                | (Pred::Leq(l), Pred::Leq(r))
                | (Pred::Ge(l), Pred::Ge(r))
                | (Pred::Geq(l), Pred::Geq(r))
                | (Pred::Eq(l), Pred::Eq(r))
                | (Pred::Neq(l), Pred::Neq(r))
                | (Pred::And(l), Pred::And(r))
                | (Pred::Or(l), Pred::Or(r))
                | (Pred::Xor(l), Pred::Xor(r)) => {
                    LanguageChildren::len(l) == LanguageChildren::len(r)
                }
                _ => false,
            }
    }
    fn children(&self) -> &[Id] {
        match self {
            Pred::Lit(_data) => &[],
            Pred::BVar(_data) => &[],
            Pred::IVar(_data) => &[],
            Pred::Not(ids) => LanguageChildren::as_slice(ids),
            Pred::Le(ids)
            | Pred::Leq(ids)
            | Pred::Ge(ids)
            | Pred::Geq(ids)
            | Pred::Eq(ids)
            | Pred::Neq(ids)
            | Pred::And(ids)
            | Pred::Or(ids)
            | Pred::Xor(ids) => LanguageChildren::as_slice(ids),
        }
    }
    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Pred::Lit(_data) => &mut [],
            Pred::BVar(_data) => &mut [],
            Pred::IVar(_data) => &mut [],
            Pred::Not(ids) => LanguageChildren::as_mut_slice(ids),
            Pred::Le(ids)
            | Pred::Leq(ids)
            | Pred::Ge(ids)
            | Pred::Geq(ids)
            | Pred::Eq(ids)
            | Pred::Neq(ids)
            | Pred::And(ids)
            | Pred::Or(ids)
            | Pred::Xor(ids) => LanguageChildren::as_mut_slice(ids),
        }
    }
}
impl ::std::fmt::Display for Pred {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match (self, f) {
            (Pred::Lit(data), f) => ::std::fmt::Display::fmt(data, f),
            (Pred::BVar(data), f) => ::std::fmt::Display::fmt(data, f),
            (Pred::IVar(data), f) => ::std::fmt::Display::fmt(data, f),
            (Pred::Le(..), f) => f.write_str("<"),
            (Pred::Leq(..), f) => f.write_str("<="),
            (Pred::Ge(..), f) => f.write_str(">"),
            (Pred::Geq(..), f) => f.write_str(">="),
            (Pred::Eq(..), f) => f.write_str("=="),
            (Pred::Neq(..), f) => f.write_str("!="),
            (Pred::Not(..), f) => f.write_str("~"),
            (Pred::And(..), f) => f.write_str("&"),
            (Pred::Or(..), f) => f.write_str("|"),
            (Pred::Xor(..), f) => f.write_str("^"),
        }
    }
}
impl FromOp for Pred {
    type Error = FromOpError;
    fn from_op(
        op: &str,
        children: ::std::vec::Vec<Id>,
    ) -> ::std::result::Result<Self, Self::Error> {
        if children.len() == 0 {
            if op.parse::<Constant>().is_ok() {
                Ok(Pred::Lit(op.parse().unwrap()))
            } else if op.parse::<egg::Symbol>().is_ok() {
                if op.starts_with("b_") {
                    Ok(Pred::BVar(op.parse().unwrap()))
                } else if op.starts_with("i_") {
                    Ok(Pred::IVar(op.parse().unwrap()))
                } else {
                    Err(FromOpError::new(op, children))
                }
            } else {
                Err(FromOpError::new(op, children))
            }
        } else if children.len() == 1 {
            if op == "~" {
                Ok(Pred::Not(<Id as LanguageChildren>::from_vec(children)))
            } else {
                Err(FromOpError::new(op, children))
            }
        } else if children.len() == 2 {
            match op {
                "<" => Ok(Pred::Le(<[Id; 2] as LanguageChildren>::from_vec(children))),
                "<=" => Ok(Pred::Leq(<[Id; 2] as LanguageChildren>::from_vec(children))),
                ">" => Ok(Pred::Ge(<[Id; 2] as LanguageChildren>::from_vec(children))),
                ">=" => Ok(Pred::Geq(<[Id; 2] as LanguageChildren>::from_vec(children))),
                "==" => Ok(Pred::Eq(<[Id; 2] as LanguageChildren>::from_vec(children))),
                "!=" => Ok(Pred::Neq(<[Id; 2] as LanguageChildren>::from_vec(children))),
                "&" => Ok(Pred::And(<[Id; 2] as LanguageChildren>::from_vec(children))),
                "|" => Ok(Pred::Or(<[Id; 2] as LanguageChildren>::from_vec(children))),
                "^" => Ok(Pred::Xor(<[Id; 2] as LanguageChildren>::from_vec(children))),
                _ => Err(FromOpError::new(op, children)),
            }
        } else {
            Err(FromOpError::new(op, children))
        }
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
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() == y.to_int().unwrap())))
            }
            Pred::Neq([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() != y.to_int().unwrap())))
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
            Pred::BVar(sym) => Some(*sym),
            Pred::IVar(sym) => Some(*sym),
            _ => None,
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        if sym.as_str().starts_with("b_") {
            Pred::BVar(sym)
        } else if sym.as_str().starts_with("i_") {
            Pred::IVar(sym)
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
            let var = Symbol::from("i_".to_owned() + letter(i));
            let id = egraph.add(Pred::IVar(var));
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
