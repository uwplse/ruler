use std::{
    fmt::{self, Display},
    hash::Hash,
    ops::Not,
    str::FromStr,
};

use egg::*;
use rand::Rng;
use rand_pcg::Pcg64;
use ruler::*;
use z3::{ast::Ast, SatResult};

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
        if s.starts_with('b') {
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
        if s.starts_with('i') {
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
    "<" = Lt([Id;2]),
    "<=" = Leq([Id;2]),
    ">" = Gt([Id;2]),
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
    Int(i64),
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
        if let Ok(i) = i64::from_str(s) {
            Ok(Self::Int(i))
        } else if let Ok(b) = bool::from_str(s) {
            Ok(Self::Bool(b))
        } else {
            Err(())
        }
    }
}

impl Constant {
    fn to_int(&self) -> Option<i64> {
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
            Pred::Lt([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() < y.to_int().unwrap())))
            }
            Pred::Leq([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_int().unwrap() <= y.to_int().unwrap())))
            }
            Pred::Gt([x, y]) => {
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
            Pred::BVar(BVar(sym)) | Pred::IVar(IVar(sym)) => Some(*sym),
            _ => None,
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        if sym.as_str().starts_with('b') {
            Pred::BVar(BVar(sym))
        } else if sym.as_str().starts_with('i') {
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
                i_vals.push(Some(Constant::Int(rng.gen::<i64>())));
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
        if synth.params.use_smt {
            let mut cfg = z3::Config::new();
            cfg.set_timeout_msec(1000);
            let ctx = z3::Context::new(&cfg);
            let solver = z3::Solver::new(&ctx);
            let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
            let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
            match (lexpr, rexpr) {
                (Z3::Z3Bool(lb), Z3::Z3Bool(rb)) => {
                    solver.assert(&z3::ast::Bool::not(&lb._eq(&rb)))
                }
                (Z3::Z3Int(li), Z3::Z3Int(ri)) => solver.assert(&z3::ast::Bool::not(&li._eq(&ri))),
                _ => return ValidationResult::Invalid,
            };
            match solver.check() {
                SatResult::Unsat => ValidationResult::Valid,
                SatResult::Sat => ValidationResult::Invalid,
                SatResult::Unknown => {
                    synth.smt_unknown += 1;
                    ValidationResult::Unknown
                }
            }
        } else {
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
}

pub fn sampler(rng: &mut Pcg64, num_samples: usize) -> Vec<Constant> {
    let mut ret = vec![];
    for _ in 0..num_samples {
        let flip = rng.gen::<bool>();
        if flip {
            ret.push(Constant::Int(rng.gen::<i64>() % 10));
        } else {
            ret.push(Constant::Int(rng.gen::<i64>()));
        }
    }
    ret
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Z3<'a> {
    Z3Bool(z3::ast::Bool<'a>),
    Z3Int(z3::ast::Int<'a>),
}

impl<'a> Z3<'a> {
    fn get_z3bool(&self) -> Option<z3::ast::Bool<'a>> {
        match self {
            Z3::Z3Bool(b) => Some(b.clone()),
            Z3::Z3Int(_) => None,
        }
    }

    fn get_z3int(&self) -> Option<z3::ast::Int<'a>> {
        match self {
            Z3::Z3Bool(_) => None,
            Z3::Z3Int(i) => Some(i.clone()),
        }
    }
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Pred]) -> Z3<'a> {
    let mut buf: Vec<Z3> = vec![];
    for node in expr.as_ref().iter() {
        match node {
            Pred::Lit(lit) => match lit {
                Constant::Bool(b) => buf.push(Z3::Z3Bool(z3::ast::Bool::from_bool(ctx, *b))),
                Constant::Int(i) => buf.push(Z3::Z3Int(z3::ast::Int::from_i64(ctx, *i))),
            },
            Pred::BVar(bv) => buf.push(Z3::Z3Bool(z3::ast::Bool::new_const(ctx, bv.to_string()))),
            Pred::IVar(iv) => buf.push(Z3::Z3Int(z3::ast::Int::new_const(ctx, iv.to_string()))),
            Pred::Lt([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3int().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3int().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Int::lt(lexpr, rexpr)))
            }
            Pred::Leq([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3int().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3int().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Int::le(lexpr, rexpr)))
            }
            Pred::Gt([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3int().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3int().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Int::gt(lexpr, rexpr)))
            }
            Pred::Geq([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3int().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3int().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Int::ge(lexpr, rexpr)))
            }
            Pred::Eq([a, b]) => {
                let lexpr = &buf[usize::from(*a)].clone();
                let rexpr = &buf[usize::from(*b)].clone();
                match (lexpr, rexpr) {
                    (Z3::Z3Bool(lb), Z3::Z3Bool(rb)) => {
                        buf.push(Z3::Z3Bool(z3::ast::Bool::_eq(lb, rb)))
                    }
                    (Z3::Z3Int(li), Z3::Z3Int(ri)) => {
                        buf.push(Z3::Z3Bool(z3::ast::Int::_eq(li, ri)))
                    }
                    (Z3::Z3Bool(_), Z3::Z3Int(_)) | (Z3::Z3Int(_), Z3::Z3Bool(_)) => panic!(
                        "Rule candidate seems to have different 
                    type of lhs and rhs."
                    ),
                }
            }
            Pred::Neq([a, b]) => {
                let lexpr = &buf[usize::from(*a)].clone();
                let rexpr = &buf[usize::from(*b)].clone();
                match (lexpr, rexpr) {
                    (Z3::Z3Bool(lb), Z3::Z3Bool(rb)) => {
                        buf.push(Z3::Z3Bool(z3::ast::Bool::not(&z3::ast::Bool::_eq(lb, rb))))
                    }
                    (Z3::Z3Int(li), Z3::Z3Int(ri)) => buf.push(Z3::Z3Bool(z3::ast::Bool::not(
                        &z3::ast::Int::_eq(li, ri).not(),
                    ))),
                    (Z3::Z3Bool(_), Z3::Z3Int(_)) | (Z3::Z3Int(_), Z3::Z3Bool(_)) => panic!(
                        "Rule candidate seems to have different 
                    type of lhs and rhs."
                    ),
                }
            }
            Pred::Not(a) => {
                let lexpr = &buf[usize::from(*a)].get_z3bool().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Bool::not(lexpr)))
            }
            Pred::And([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3bool().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3bool().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Bool::and(ctx, &[lexpr, rexpr])))
            }
            Pred::Or([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3bool().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3bool().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Bool::or(ctx, &[lexpr, rexpr])))
            }
            Pred::Xor([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3bool().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3bool().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Bool::xor(lexpr, rexpr)))
            }
        }
    }
    buf.pop().unwrap()
}

fn main() {
    Pred::main()
}
