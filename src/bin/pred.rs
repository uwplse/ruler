use std::{
    fmt::{self, Display},
    hash::Hash,
    str::FromStr,
};

use egg::*;
use num::{
    bigint::{BigInt, RandBigInt, Sign, ToBigInt},
    ToPrimitive,
};
use num::{rational::Ratio, Zero};
use rand::{seq::SliceRandom, Rng};
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

pub struct NVar(egg::Symbol);

impl Display for NVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n")
    }
}

impl FromStr for NVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('n') {
            Ok(NVar(Symbol::from(s)))
        } else {
            Err(())
        }
    }
}

define_language! {
  pub enum Pred {
    Lit(Constant),
    BVar(BVar),
    NVar(NVar),
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

/// Return a non-zero constant.
fn mk_constant(n: &BigInt, d: &BigInt) -> Option<Constant> {
    if d.is_zero() {
        None
    } else {
        Some(Constant::Num(Ratio::new(n.clone(), d.clone())))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    Bool(bool),
    Num(Ratio<BigInt>),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Bool(b) => b.fmt(f),
            Constant::Num(i) => i.fmt(f),
        }
    }
}

impl std::str::FromStr for Constant {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(i) = Ratio::<BigInt>::from_str(s) {
            Ok(Self::Num(i))
        } else if let Ok(b) = bool::from_str(s) {
            Ok(Self::Bool(b))
        } else {
            Err(())
        }
    }
}

impl Constant {
    fn to_num(&self) -> Option<Ratio<BigInt>> {
        match self {
            Constant::Num(n) => Some(n.clone()),
            Constant::Bool(_) => None,
        }
    }

    fn to_bool(&self) -> Option<bool> {
        match self {
            Constant::Bool(b) => Some(*b),
            Constant::Num(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Bool,
    Num,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "b"),
            Type::Num => write!(f, "n"),
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
                Constant::Num(_) => Type::Num,
            },
            Pred::NVar(_) => Type::Num,
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
                map!(v, x, y => Some(Constant::Bool(x.to_num().unwrap() < y.to_num().unwrap())))
            }
            Pred::Leq([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_num().unwrap() <= y.to_num().unwrap())))
            }
            Pred::Gt([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_num().unwrap() > y.to_num().unwrap())))
            }
            Pred::Geq([x, y]) => {
                map!(v, x, y => Some(Constant::Bool(x.to_num().unwrap() >= y.to_num().unwrap())))
            }
            Pred::Eq([x, y]) => {
                map!(v, x, y => {
                    match (x,y) {
                        (Constant::Bool(x), Constant::Bool(y)) => Some(Constant::Bool(x == y)),
                        (Constant::Num(x), Constant::Num(y)) => Some(Constant::Bool(x == y)),
                        _ => panic!("Cannot compare Bool and Int: {} {}", x, y)
                    }
                })
            }
            Pred::Neq([x, y]) => {
                map!(v, x, y => {
                    match (x,y) {
                        (Constant::Bool(x), Constant::Bool(y)) => Some(Constant::Bool(x != y)),
                        (Constant::Num(x), Constant::Num(y)) => Some(Constant::Bool(x != y)),
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
            Pred::NVar(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Pred::BVar(BVar(sym)) | Pred::NVar(NVar(sym)) => Some(*sym),
            _ => None,
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        if sym.as_str().starts_with('b') {
            Pred::BVar(BVar(sym))
        } else if sym.as_str().starts_with('n') {
            Pred::NVar(NVar(sym))
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
        let rng = &mut synth.rng;
        let mut consts: Vec<Option<Constant>> = vec![];

        for i in 0..synth.params.important_cvec_offsets {
            consts.push(mk_constant(
                &i.to_bigint().unwrap(),
                &((1_u32).to_bigint().unwrap()),
            ));
            consts.push(mk_constant(
                &(-i.to_bigint().unwrap()),
                &((1_u32).to_bigint().unwrap()),
            ));
        }

        consts.sort();
        consts.dedup();

        let consts = self_product(&consts, synth.params.variables);

        let cvec_len = consts.len();
        let mut egraph: EGraph<Pred, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len,
            constant_fold: ConstantFoldMethod::CvecMatching,
            rule_lifting: false,
        });
        for (i, item) in consts.iter().enumerate().take(synth.params.variables) {
            let n_id = egraph.add(Pred::NVar(NVar(Symbol::from("n".to_owned() + letter(i)))));
            let b_id = egraph.add(Pred::BVar(BVar(Symbol::from("b".to_owned() + letter(i)))));
            let mut b_vals = vec![];
            for _ in 0..cvec_len {
                b_vals.push(Some(Constant::Bool(rng.gen::<bool>())));
            }
            egraph[n_id].data.cvec = item.clone();
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
                (Z3::Z3Real(ln), Z3::Z3Real(rn)) => {
                    solver.assert(&z3::ast::Bool::not(&ln._eq(&rn)))
                }
                _ => return ValidationResult::Invalid,
            };
            match solver.check() {
                SatResult::Unsat => ValidationResult::Valid,
                SatResult::Sat => {
                    println!("z3 validation: failed for {} => {}", lhs, rhs);
                    ValidationResult::Invalid
                }
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

            for (var, cvec) in env.iter_mut() {
                cvec.reserve(n * 3);
                let mut vals = vec![];
                if var.to_string().starts_with("?b") {
                    for _ in 0..(n * 3) {
                        vals.push(Constant::Bool(synth.rng.gen::<bool>()));
                    }
                }
                if var.to_string().starts_with("?n") {
                    vals.append(&mut sampler(&mut synth.rng, 8, 6, n));
                    vals.append(&mut sampler(&mut synth.rng, 6, 8, n));
                    vals.append(&mut sampler(&mut synth.rng, 3, 1, n));
                }

                vals.shuffle(&mut synth.rng);
                for v in vals {
                    cvec.push(Some(v));
                }
            }
            let lvec = Self::eval_pattern(lhs, &env, n);
            let rvec = Self::eval_pattern(rhs, &env, n);
            ValidationResult::from(lvec == rvec)
        }
    }
}

pub fn gen_pos(rng: &mut Pcg64, bits: u64) -> BigInt {
    let mut res: BigInt;
    loop {
        res = BigInt::from_biguint(Sign::Plus, rng.gen_biguint(bits));
        if res != 0.to_bigint().unwrap() {
            break;
        }
    }
    res
}

pub fn sampler(rng: &mut Pcg64, num: u64, denom: u64, num_samples: usize) -> Vec<Constant> {
    let mut ret = vec![];
    for _ in 0..num_samples {
        let num = gen_pos(rng, num);
        let denom = gen_pos(rng, denom);
        ret.push(Constant::Num(Ratio::new(num, denom)));
    }
    ret
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Z3<'a> {
    Z3Bool(z3::ast::Bool<'a>),
    Z3Real(z3::ast::Real<'a>),
}

impl<'a> Z3<'a> {
    fn get_z3bool(&self) -> Option<z3::ast::Bool<'a>> {
        match self {
            Z3::Z3Bool(b) => Some(b.clone()),
            Z3::Z3Real(_) => None,
        }
    }

    fn get_z3rat(&self) -> Option<z3::ast::Real<'a>> {
        match self {
            Z3::Z3Bool(_) => None,
            Z3::Z3Real(n) => Some(n.clone()),
        }
    }
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Pred]) -> Z3<'a> {
    let mut buf: Vec<Z3> = vec![];
    for node in expr.as_ref().iter() {
        match node {
            Pred::Lit(lit) => match lit {
                Constant::Bool(b) => buf.push(Z3::Z3Bool(z3::ast::Bool::from_bool(ctx, *b))),
                Constant::Num(n) => buf.push(Z3::Z3Real(z3::ast::Real::from_real(
                    ctx,
                    (n.numer()).to_i32().unwrap(),
                    (n.denom()).to_i32().unwrap(),
                ))),
            },
            Pred::BVar(bv) => buf.push(Z3::Z3Bool(z3::ast::Bool::new_const(ctx, bv.to_string()))),
            Pred::NVar(nv) => buf.push(Z3::Z3Real(z3::ast::Real::new_const(ctx, nv.to_string()))),
            Pred::Lt([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3rat().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3rat().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Real::lt(lexpr, rexpr)))
            }
            Pred::Leq([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3rat().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3rat().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Real::le(lexpr, rexpr)))
            }
            Pred::Gt([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3rat().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3rat().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Real::gt(lexpr, rexpr)))
            }
            Pred::Geq([a, b]) => {
                let lexpr = &buf[usize::from(*a)].get_z3rat().unwrap();
                let rexpr = &buf[usize::from(*b)].get_z3rat().unwrap();
                buf.push(Z3::Z3Bool(z3::ast::Real::ge(lexpr, rexpr)))
            }
            Pred::Eq([a, b]) => {
                let lexpr = &buf[usize::from(*a)].clone();
                let rexpr = &buf[usize::from(*b)].clone();
                match (lexpr, rexpr) {
                    (Z3::Z3Bool(lb), Z3::Z3Bool(rb)) => {
                        buf.push(Z3::Z3Bool(z3::ast::Bool::_eq(lb, rb)))
                    }
                    (Z3::Z3Real(ln), Z3::Z3Real(rn)) => {
                        buf.push(Z3::Z3Bool(z3::ast::Real::_eq(ln, rn)))
                    }
                    (Z3::Z3Bool(_), Z3::Z3Real(_)) | (Z3::Z3Real(_), Z3::Z3Bool(_)) => panic!(
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
                    (Z3::Z3Real(ln), Z3::Z3Real(rn)) => {
                        buf.push(Z3::Z3Bool(z3::ast::Bool::not(&z3::ast::Real::_eq(ln, rn))))
                    }
                    _ => panic!(
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
