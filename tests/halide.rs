use num::{ToPrimitive, Zero};
use ruler::*;
use z3::ast::Ast;

type Constant = i64;

egg::define_language! {
  pub enum Pred {
    Lit(Constant),
    "<" = Lt([Id;2]),
    "<=" = Leq([Id;2]),
    "==" = Eq([Id;2]),
    "!=" = Neq([Id;2]),
    "->" = Implies([Id; 2]),
    "!" = Not(Id),
    "-" = Neg(Id),
    "&&" = And([Id;2]),
    "||" = Or([Id;2]),
    "^" = Xor([Id;2]),
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    "min" = Min([Id; 2]),
    "max" = Max([Id; 2]),
    "select" = Select([Id; 3]),
    Var(Symbol),
  }
}

impl SynthLanguage for Pred {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        let one = 1.to_i64().unwrap();
        let zero = 0.to_i64().unwrap();
        match self {
            Pred::Lit(c) => vec![Some(c.clone()); cvec_len],
            Pred::Lt([x, y]) => {
                map!(get_cvec, x, y => if x < y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Leq([x, y]) => {
                map!(get_cvec, x, y => if x <= y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Eq([x, y]) => {
                map!(get_cvec, x, y => if x == y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Neq([x, y]) => {
                map!(get_cvec, x, y => if x != y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Implies([x, y]) => {
                map!(get_cvec, x, y => {
                  let xbool = x.clone() != zero;
                  let ybool = y.clone() != zero;
                  if !xbool || ybool {Some(one.clone())} else {Some(zero.clone())}
                })
            }
            Pred::Not(x) => {
                map!(get_cvec, x => if x.clone() == zero { Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Neg(x) => map!(get_cvec, x => Some(-x)),
            Pred::And([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool && ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Or([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool || ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Xor([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool ^ ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Add([x, y]) => map!(get_cvec, x, y => x.checked_add(*y)),
            Pred::Sub([x, y]) => map!(get_cvec, x, y => x.checked_sub(*y)),
            Pred::Mul([x, y]) => map!(get_cvec, x, y => x.checked_mul(*y)),
            Pred::Div([x, y]) => map!(get_cvec, x, y => {
              if y.is_zero() {
                Some(zero.clone())
              } else {
                x.checked_div(*y)
              }
            }),
            Pred::Min([x, y]) => map!(get_cvec, x, y => Some(x.min(y).clone())),
            Pred::Max([x, y]) => map!(get_cvec, x, y => Some(x.max(y).clone())),
            Pred::Select([x, y, z]) => map!(get_cvec, x, y, z => {
              let xbool = x.clone() != zero;
              if xbool {Some(y.clone())} else {Some(z.clone())}
            }),
            Pred::Var(_) => vec![],
        }
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let consts = vec![
            Some((-10).to_i64().unwrap()),
            Some((-1).to_i64().unwrap()),
            Some(0.to_i64().unwrap()),
            Some(1.to_i64().unwrap()),
            Some(2.to_i64().unwrap()),
            Some(5.to_i64().unwrap()),
            Some(100.to_i64().unwrap()),
        ];
        let cvecs = self_product(&consts, vars.len());

        egraph.analysis.cvec_len = cvecs[0].len();

        for (i, v) in vars.iter().enumerate() {
            let id = egraph.add(Pred::Var(Symbol::from(v.clone())));
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Pred::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Pred::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Pred::Lit(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Pred::Lit(c)
    }

    fn validate(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> ValidationResult {
        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        z3::with_z3_config(&cfg, || {
            let solver = z3::Solver::new();
            let lexpr = egg_to_z3(Self::instantiate(lhs).as_ref());
            let rexpr = egg_to_z3(Self::instantiate(rhs).as_ref());
            solver.assert(&lexpr.eq(&rexpr).not());
            match solver.check() {
                z3::SatResult::Unsat => ValidationResult::Valid,
                z3::SatResult::Unknown => ValidationResult::Unknown,
                z3::SatResult::Sat => ValidationResult::Invalid,
            }
        })
    }
}

fn egg_to_z3(expr: &[Pred]) -> z3::ast::Int {
    let mut buf: Vec<z3::ast::Int> = vec![];
    let zero = z3::ast::Int::from_i64(0);
    let one = z3::ast::Int::from_i64(1);
    for node in expr.as_ref().iter() {
        match node {
            Pred::Lit(c) => buf.push(z3::ast::Int::from_i64(c.to_i64().unwrap())),
            Pred::Lt([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(l.lt(r).ite(&one, &zero))
            }
            Pred::Leq([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(l.le(r).ite(&one, &zero))
            }
            Pred::Eq([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(l.eq(r).ite(&one, &zero))
            }
            Pred::Neq([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(l.eq(r).ite(&zero, &one))
            }
            Pred::Implies([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = l.eq(&zero).not();
                let r_not_z = r.eq(&zero).not();
                buf.push(l_not_z.implies(&r_not_z).ite(&one, &zero))
            }
            Pred::Not(x) => {
                let l = &buf[usize::from(*x)];
                buf.push(l.eq(&zero).ite(&one, &zero))
            }
            Pred::Neg(x) => buf.push(buf[usize::from(*x)].unary_minus()),
            Pred::And([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = l.eq(&zero).not();
                let r_not_z = r.eq(&zero).not();
                buf.push(z3::ast::Bool::and(&[l_not_z, r_not_z]).ite(&one, &zero))
            }
            Pred::Or([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = l.eq(&zero).not();
                let r_not_z = r.eq(&zero).not();
                buf.push(z3::ast::Bool::or(&[l_not_z, r_not_z]).ite(&one, &zero))
            }
            Pred::Xor([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = l.eq(&zero).not();
                let r_not_z = r.eq(&zero).not();
                buf.push(l_not_z.xor(&r_not_z).ite(&one, &zero))
            }
            Pred::Add([x, y]) => buf.push(z3::ast::Int::add(&[
                buf[usize::from(*x)].clone(),
                buf[usize::from(*y)].clone(),
            ])),
            Pred::Sub([x, y]) => buf.push(z3::ast::Int::sub(&[
                buf[usize::from(*x)].clone(),
                buf[usize::from(*y)].clone(),
            ])),
            Pred::Mul([x, y]) => buf.push(z3::ast::Int::mul(&[
                buf[usize::from(*x)].clone(),
                buf[usize::from(*y)].clone(),
            ])),
            Pred::Div([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(r.eq(&zero).ite(&zero, &l.div(r)))
            }
            Pred::Min([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(l.le(r).ite(l, r))
            }
            Pred::Max([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(l.le(r).ite(r, l))
            }
            Pred::Select([x, y, z]) => {
                let cond = buf[usize::from(*x)].eq(&zero).not();
                buf.push(cond.ite(&buf[usize::from(*y)], &buf[usize::from(*z)]))
            }
            Pred::Var(v) => buf.push(z3::ast::Int::new_const(v.to_string())),
        }
    }
    buf.pop().unwrap()
}

#[cfg(test)]
#[path = "./recipes/halide.rs"]
mod halide;

mod test {
    use crate::halide::halide_rules;
    use crate::Pred;
    use std::time::{Duration, Instant};

    use ruler::{
        enumo::{Filter, Metric, Ruleset, Workload},
        logger,
        recipe_utils::{recursive_rules, run_workload, Lang},
        Limits,
    };

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let all_rules = halide_rules();
        let duration = start.elapsed();

        // oopsla-halide-baseline branch
        // Run on nightly 4/12/2023
        // time cargo run --release --bin halide -- synth --iters 1 --use-smt
        // real	0m2.707s
        // user	0m2.681s
        // sys	0m0.028s
        let baseline: Ruleset<Pred> = Ruleset::from_file("baseline/halide.rules");
        let oopsla_halide: Ruleset<Pred> = Ruleset::from_file("baseline/oopsla-halide.rules");
        let oopsla_duration = Duration::from_secs_f32(3.354);

        logger::write_baseline(&all_rules, "halide", &baseline, "halide", duration);

        logger::write_baseline(
            &oopsla_halide,
            "oopsla halide (1 iter)",
            &baseline,
            "halide",
            oopsla_duration,
        );
    }
}
