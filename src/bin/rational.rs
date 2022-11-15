use egg::*;
use num::{rational::Ratio, BigInt, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use ruler::*;
use std::ops::*;
use z3::ast::Ast;

/// define `Constant` for rationals.
pub type Constant = Ratio<BigInt>;

fn mk_rat(n: i64, d: i64) -> Ratio<BigInt> {
    if d.is_zero() {
        panic!("mk_rat: denominator is zero!");
    }
    let n = n
        .to_bigint()
        .expect(&format!("Could not make bigint from {}", n));
    let d = d
        .to_bigint()
        .expect(&format!("Could not make bigint from {}", d));

    Ratio::new(n, d)
}

define_language! {
  pub enum Math {
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    "~" = Neg(Id),
    Lit(Constant),
    Var(egg::Symbol),
  }
}

impl SynthLanguage for Math {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Add([x, y]) => map!(get_cvec, x, y => Some(x + y)),
            Math::Sub([x, y]) => map!(get_cvec, x, y => Some(x - y)),
            Math::Mul([x, y]) => map!(get_cvec, x, y => Some(x * y)),
            Math::Div([x, y]) => map!(get_cvec, x, y => {
              if y.is_zero() {
                None
              } else {
                Some(x / y)
              }
            }),
            Math::Neg(x) => map!(get_cvec, x => Some(-x)),
            Math::Lit(c) => vec![Some(c.clone()); cvec_len],
            Math::Var(_) => vec![],
        }
    }

    fn mk_interval(&self, _egraph: &EGraph<Self, SynthAnalysis>) -> Interval<Self::Constant> {
        Interval::new(None, None)
    }

    fn initialize_vars(synth: &mut Synthesizer<Self>, vars: Vec<String>) {
        println!("initializing vars: {:?}", vars);

        let consts = vec![Some(mk_rat(-1, 1)), Some(mk_rat(0, 1)), Some(mk_rat(1, 1))];
        let cvecs = self_product(&consts, vars.len());

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: cvecs[0].len(),
        });

        for (i, v) in vars.iter().enumerate() {
            let id = egraph.add(Math::Var(Symbol::from(v.clone())));
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }

        synth.egraph = egraph;
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Math::Var(v) => Some(*v),
            _ => None,
        }
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
        let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
        solver.assert(&lexpr._eq(&rexpr).not());
        match solver.check() {
            z3::SatResult::Unsat => ValidationResult::Valid,
            z3::SatResult::Unknown => ValidationResult::Unknown,
            z3::SatResult::Sat => ValidationResult::Invalid,
        }
    }

    fn is_constant(&self) -> bool {
        matches!(self, Math::Lit(_))
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Lit(c)
    }
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Math]) -> z3::ast::Real<'a> {
    let mut buf: Vec<z3::ast::Real> = vec![];
    for node in expr.as_ref().iter() {
        match node {
            Math::Add([x, y]) => buf.push(z3::ast::Real::add(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Math::Sub([x, y]) => buf.push(z3::ast::Real::sub(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Math::Mul([x, y]) => buf.push(z3::ast::Real::mul(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Math::Div([x, y]) => buf.push(z3::ast::Real::div(
                &buf[usize::from(*x)],
                &buf[usize::from(*y)],
            )),
            Math::Neg(x) => buf.push(z3::ast::Real::unary_minus(&buf[usize::from(*x)])),
            Math::Lit(c) => buf.push(z3::ast::Real::from_real(
                ctx,
                (c.numer()).to_i32().unwrap(),
                (c.denom()).to_i32().unwrap(),
            )),
            Math::Var(v) => buf.push(z3::ast::Real::new_const(ctx, v.to_string())),
        }
    }
    buf.pop().unwrap()
}

fn main() {
    Math::run_synth()
}
