use crate::*;

use egg::RecExpr;
use enumo::{Filter, Metric, Ruleset, Sexp, Workload};
use num::ToPrimitive;
use recipe_utils::{base_lang, iter_metric, recursive_rules_cond, run_workload, Lang};
use z3::ast::Ast;

type Constant = i64;

egg::define_language! {
  pub enum Pred {
    Lit(Constant),
    "abs" = Abs(Id),
    "<" = Lt([Id;2]),
    "<=" = Leq([Id;2]),
    ">" = Gt([Id;2]),
    ">=" = Geq([Id;2]),
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
    "%" = Mod([Id; 2]),
    "min" = Min([Id; 2]),
    "max" = Max([Id; 2]),
    "select" = Select([Id; 3]),
    Var(Symbol),
  }
}

impl SynthLanguage for Pred {
    type Constant = Constant;

    fn constant_to_bool(c: &Self::Constant) -> Option<bool> {
        Some(c != &0)
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        let one = 1.to_i64().unwrap();
        let zero = 0.to_i64().unwrap();
        match self {
            Pred::Lit(c) => vec![Some(*c); cvec_len],
            Pred::Abs(x) => map!(get_cvec, x => Some(x.abs())),
            Pred::Lt([x, y]) => {
                map!(get_cvec, x, y => if x < y {Some(one)} else {Some(zero)})
            }
            Pred::Leq([x, y]) => {
                map!(get_cvec, x, y => if x <= y {Some(one)} else {Some(zero)})
            }
            Pred::Gt([x, y]) => {
                map!(get_cvec, x, y => if x > y {Some(one)} else {Some(zero)})
            }
            Pred::Geq([x, y]) => {
                map!(get_cvec, x, y => if x >= y {Some(one)} else {Some(zero)})
            }
            Pred::Eq([x, y]) => {
                map!(get_cvec, x, y => if x == y {Some(one)} else {Some(zero)})
            }
            Pred::Neq([x, y]) => {
                map!(get_cvec, x, y => if x != y {Some(one)} else {Some(zero)})
            }
            Pred::Implies([x, y]) => {
                map!(get_cvec, x, y => {
                  let xbool = *x != zero;
                  let ybool = *y != zero;
                  if !xbool || ybool {Some(one)} else {Some(zero)}
                })
            }
            Pred::Not(x) => {
                map!(get_cvec, x => if *x == zero { Some(one)} else {Some(zero)})
            }
            Pred::Neg(x) => map!(get_cvec, x => Some(-x)),
            Pred::And([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = *x != zero;
                    let ybool = *y != zero;
                    if xbool && ybool { Some(one) } else { Some(zero) }
                })
            }
            Pred::Or([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = *x != zero;
                    let ybool = *y != zero;
                    if xbool || ybool { Some(one) } else { Some(zero) }
                })
            }
            Pred::Xor([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = *x != zero;
                    let ybool = *y != zero;
                    if xbool ^ ybool { Some(one) } else { Some(zero) }
                })
            }
            Pred::Add([x, y]) => map!(get_cvec, x, y => x.checked_add(*y)),
            Pred::Sub([x, y]) => map!(get_cvec, x, y => x.checked_sub(*y)),
            Pred::Mul([x, y]) => map!(get_cvec, x, y => x.checked_mul(*y)),
            Pred::Div([x, y]) => map!(get_cvec, x, y => x.checked_div(*y)),
            Pred::Mod([x, y]) => map!(get_cvec, x, y => x.checked_rem(*y)),
            Pred::Min([x, y]) => map!(get_cvec, x, y => Some(*x.min(y))),
            Pred::Max([x, y]) => map!(get_cvec, x, y => Some(*x.max(y))),
            Pred::Select([x, y, z]) => map!(get_cvec, x, y, z => {
              let xbool = *x != zero;
              if xbool {Some(*y)} else {Some(*z)}
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

    fn condition_implies(
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
        cache: &mut HashMap<(String, String), bool>,
    ) -> bool {
        let lhs_str = lhs.to_string();
        let rhs_str = rhs.to_string();
        if cache.contains_key(&(lhs_str.clone(), rhs_str.clone())) {
            return *cache.get(&(lhs_str, rhs_str)).unwrap();
        }

        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let zero = z3::ast::Int::from_i64(&ctx, 0);

        // given that the lhs is true, can we make the rhs false?

        let lhs = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref())
            ._eq(&zero)
            .not();

        let rhs = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref())
            ._eq(&zero)
            .not();

        let assertion = &lhs;

        solver.assert(assertion);

        if matches!(solver.check(), z3::SatResult::Unsat) {
            // don't want something that is always false
            cache.insert((lhs_str, rhs_str), false);
            return false;
        }

        solver.reset();
        let assertion = &rhs;

        solver.assert(&assertion.not());

        if matches!(solver.check(), z3::SatResult::Unsat) {
            // don't want something that is always true
            cache.insert((lhs_str, rhs_str), false);
            return false;
        }

        solver.reset();

        let assertion = &z3::ast::Bool::implies(&lhs, &rhs).not();

        solver.assert(assertion);

        let res = solver.check();
        let implies = matches!(res, z3::SatResult::Unsat);
        cache.insert((lhs_str, rhs_str), implies);
        implies
    }

    fn validate(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> ValidationResult {
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

    fn validate_with_cond(
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
        cond: &Pattern<Self>,
    ) -> ValidationResult {
        assert!(cond.to_string().len() > 2, "Conditional pattern: {}", cond);
        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let zero = z3::ast::Int::from_i64(&ctx, 0);
        let cexpr =
            z3::ast::Bool::not(&egg_to_z3(&ctx, Self::instantiate(cond).as_ref())._eq(&zero));

        let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
        let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
        solver.assert(&z3::ast::Bool::implies(&cexpr, &lexpr._eq(&rexpr)).not());

        match solver.check() {
            z3::SatResult::Unsat => ValidationResult::Valid,
            z3::SatResult::Unknown => ValidationResult::Unknown,
            z3::SatResult::Sat => ValidationResult::Invalid,
        }
    }
}

pub fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Pred]) -> z3::ast::Int<'a> {
    let mut buf: Vec<z3::ast::Int> = vec![];
    let zero = z3::ast::Int::from_i64(ctx, 0);
    let one = z3::ast::Int::from_i64(ctx, 1);
    for node in expr.as_ref().iter() {
        match node {
            Pred::Lit(c) => buf.push(z3::ast::Int::from_i64(ctx, c.to_i64().unwrap())),
            Pred::Abs(x) => {
                let l = &buf[usize::from(*x)];
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Int::lt(l, &zero),
                    &z3::ast::Int::unary_minus(l),
                    l,
                ))
            }
            Pred::Lt([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::lt(l, r), &one, &zero))
            }
            Pred::Leq([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::le(l, r), &one, &zero))
            }
            Pred::Gt([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::gt(l, r), &one, &zero))
            }
            Pred::Geq([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::ge(l, r), &one, &zero))
            }
            Pred::Eq([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::_eq(l, r), &one, &zero))
            }
            Pred::Neq([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::_eq(l, r), &zero, &one))
            }
            Pred::Implies([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_z = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::implies(&l_not_z, &r_not_z),
                    &one,
                    &zero,
                ))
            }
            Pred::Not(x) => {
                let l = &buf[usize::from(*x)];
                buf.push(z3::ast::Bool::ite(&l._eq(&zero), &one, &zero))
            }
            Pred::Neg(x) => buf.push(z3::ast::Int::unary_minus(&buf[usize::from(*x)])),
            Pred::And([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_z = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::and(ctx, &[&l_not_z, &r_not_z]),
                    &one,
                    &zero,
                ))
            }
            Pred::Or([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_z = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::or(ctx, &[&l_not_z, &r_not_z]),
                    &one,
                    &zero,
                ))
            }
            Pred::Xor([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                let l_not_z = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_z = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::xor(&l_not_z, &r_not_z),
                    &one,
                    &zero,
                ))
            }
            Pred::Add([x, y]) => buf.push(z3::ast::Int::add(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Pred::Sub([x, y]) => buf.push(z3::ast::Int::sub(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Pred::Mul([x, y]) => buf.push(z3::ast::Int::mul(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Pred::Div([x, y]) => buf.push(z3::ast::Int::div(
                &buf[usize::from(*x)],
                &buf[usize::from(*y)].clone(),
            )),
            Pred::Mod([x, y]) => buf.push(z3::ast::Int::modulo(
                &buf[usize::from(*x)],
                &buf[usize::from(*y)].clone(),
            )),
            Pred::Min([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::le(l, r), l, r))
            }
            Pred::Max([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::le(l, r), r, l))
            }
            Pred::Select([x, y, z]) => {
                let cond = z3::ast::Bool::not(&buf[usize::from(*x)]._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &cond,
                    &buf[usize::from(*y)],
                    &buf[usize::from(*z)],
                ))
            }
            Pred::Var(v) => buf.push(z3::ast::Int::new_const(ctx, v.to_string())),
        }
    }
    buf.pop().unwrap()
}

// This function returns if the given expression is valid or not, where validity is defined as:
// Valid   ==> The expression is always true (evaluates to 1), no matter the values of the variables inside.
// Invalid ==> The expression is always false (evaluates to 0), no matter the values of the
// variables inside.
// Unknown ==> Either the solver timed out, or the expression is impossible to condense to one of the two above.
//             One such expression is `x < 0`, which is neither always true nor always false.
//             In Caviar, this corresponds to the "Impossible" stop result.
// This function is different from `Self::validate` in that `validate` only checks to see if a
// given statement is true, while this function checks if the statement is always true or always
// false (forall).
pub fn validate_expression(expr: &Sexp) -> ValidationResult {
    pub fn sexpr_to_z3<'a>(ctx: &'a z3::Context, expr: &Sexp) -> z3::ast::Int<'a> {
        match expr {
            Sexp::Atom(a) => {
                if let Ok(c) = a.parse::<i64>() {
                    z3::ast::Int::from_i64(ctx, c)
                } else {
                    z3::ast::Int::new_const(ctx, a.to_string())
                }
            }
            Sexp::List(l) => {
                let mut iter = l.iter();
                let head = iter.next().unwrap();
                let tail = iter.collect::<Vec<_>>();
                match head.to_string().as_str() {
                    "abs" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::lt(&x, &z3::ast::Int::from_i64(ctx, 0)),
                            &z3::ast::Int::unary_minus(&x),
                            &x,
                        )
                    }
                    "<" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::lt(&x, &y),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "<=" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::le(&x, &y),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    ">" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::gt(&x, &y),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    ">=" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::ge(&x, &y),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "==" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::_eq(&x, &y),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "!=" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::_eq(&x, &y),
                            &z3::ast::Int::from_i64(ctx, 0),
                            &z3::ast::Int::from_i64(ctx, 1),
                        )
                    }
                    "->" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        let x_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &x,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        let y_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &y,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        z3::ast::Bool::ite(
                            &z3::ast::Bool::implies(&x_not_z, &y_not_z),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "!" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        z3::ast::Bool::ite(
                            &z3::ast::Int::_eq(&x, &z3::ast::Int::from_i64(ctx, 0)),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "-" => {
                        if tail.len() == 1 {
                            z3::ast::Int::unary_minus(&sexpr_to_z3(ctx, tail[0]))
                        } else {
                            let x = sexpr_to_z3(ctx, tail[0]);
                            let y = sexpr_to_z3(ctx, tail[1]);
                            z3::ast::Int::sub(ctx, &[&x, &y])
                        }
                    }
                    "&&" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        let x_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &x,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        let y_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &y,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        z3::ast::Bool::ite(
                            &z3::ast::Bool::and(ctx, &[&x_not_z, &y_not_z]),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "||" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        let x_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &x,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        let y_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &y,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        z3::ast::Bool::ite(
                            &z3::ast::Bool::or(ctx, &[&x_not_z, &y_not_z]),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "^" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        let x_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &x,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        let y_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &y,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        z3::ast::Bool::ite(
                            &z3::ast::Bool::xor(&x_not_z, &y_not_z),
                            &z3::ast::Int::from_i64(ctx, 1),
                            &z3::ast::Int::from_i64(ctx, 0),
                        )
                    }
                    "+" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Int::add(ctx, &[&x, &y])
                    }
                    "*" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Int::mul(ctx, &[&x, &y])
                    }
                    "/" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Int::div(&x, &y)
                    }
                    "%" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Int::modulo(&x, &y)
                    }
                    "min" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(&z3::ast::Int::le(&x, &y), &x, &y)
                    }
                    "max" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        z3::ast::Bool::ite(&z3::ast::Int::le(&x, &y), &y, &x)
                    }
                    "select" => {
                        let x = sexpr_to_z3(ctx, tail[0]);
                        let y = sexpr_to_z3(ctx, tail[1]);
                        let z = sexpr_to_z3(ctx, tail[2]);
                        let x_not_z = z3::ast::Bool::not(&z3::ast::Int::_eq(
                            &x,
                            &z3::ast::Int::from_i64(ctx, 0),
                        ));
                        z3::ast::Bool::ite(&x_not_z, &y, &z)
                    }
                    _ => panic!("Unknown operator: {}", head),
                }
            }
        }
    }

    let mut cfg = z3::Config::new();
    cfg.set_timeout_msec(1000);
    let ctx = z3::Context::new(&cfg);
    let solver = z3::Solver::new(&ctx);
    let zero = z3::ast::Int::from_i64(&ctx, 0);
    let expr = sexpr_to_z3(&ctx, expr);

    // Check if expr == 0 is unsat (i.e., expr can never be false)
    solver.assert(&expr._eq(&zero));
    if matches!(solver.check(), z3::SatResult::Unsat) {
        return ValidationResult::Valid; // AlwaysTrue
    }
    solver.reset();

    // Check if expr != 0 is unsat (i.e., expr can never be true)
    solver.assert(&expr._eq(&zero).not());
    match solver.check() {
        z3::SatResult::Unsat => ValidationResult::Invalid,
        z3::SatResult::Unknown => ValidationResult::Unknown,
        z3::SatResult::Sat => ValidationResult::Unknown,
    }
}

pub fn compute_conditional_structures(
    conditional_soup: &Workload,
) -> (HashMap<Vec<bool>, Vec<Pattern<Pred>>>, Ruleset<Pred>) {
    let egraph: EGraph<Pred, SynthAnalysis> = conditional_soup.to_egraph();
    let mut pvec_to_terms: HashMap<Vec<bool>, Vec<Pattern<Pred>>> = HashMap::default();

    let cond_prop_ruleset = Pred::get_condition_propogation_rules(&conditional_soup);

    println!("cond prop rules: {}", cond_prop_ruleset.len());
    for rule in &cond_prop_ruleset {
        println!("{}", rule.0);
    }

    for cond in conditional_soup.force() {
        let cond: RecExpr<Pred> = cond.to_string().parse().unwrap();
        let cond_pat = Pattern::from(&cond);

        let cond_id = egraph
            .lookup_expr(&cond_pat.to_string().parse().unwrap())
            .unwrap();

        let pvec = egraph[cond_id]
            .data
            .cvec
            .clone()
            .iter()
            .map(|b| *b != Some(0))
            .collect();

        pvec_to_terms
            .entry(pvec)
            .or_default()
            .push(cond_pat.clone());
    }

    (pvec_to_terms, cond_prop_ruleset)
}

/// Incrementally construct a ruleset by running rule inference up to a size bound,
/// using previously-learned rules at each step.
/// Importantly, this function is different from `recursive_rules_cond` in that it does not
/// actually generate the terms that it derives equivalences for.
pub fn soup_to_rules(
    soup: &Workload,
    conditions: Option<&Workload>,
    prior_rules: &Ruleset<Pred>,
    n: usize,
) -> Ruleset<Pred> {
    for t in soup.force() {
        println!("{:?}", t.to_string());
    }

    let (pvec_to_terms, cond_prop_ruleset) = if let Some(conditions) = conditions {
        // If we have a workload of conditions, compute the conditional structures
        // to help with rule inference.
        let (fst, snd) = compute_conditional_structures(conditions);
        (Some(fst), Some(snd))
    } else {
        (None, None)
    };

    let mut ruleset = Ruleset::<Pred>::default();
    for i in 1..n {
        let workload = soup.clone().filter(Filter::MetricLt(Metric::Atoms, i + 1));
        let rules = run_workload(
            workload,
            prior_rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            true,
            pvec_to_terms.clone(),
            cond_prop_ruleset.clone(),
        );
        ruleset.extend(rules);
    }
    ruleset
}
