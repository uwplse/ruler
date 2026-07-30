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
            Pred::Lit(c) => vec![Some(*c); cvec_len],
            Pred::Lt([x, y]) => {
                map!(get_cvec, x, y => if x < y {Some(one)} else {Some(zero)})
            }
            Pred::Leq([x, y]) => {
                map!(get_cvec, x, y => if x <= y {Some(one)} else {Some(zero)})
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
            Pred::Div([x, y]) => map!(get_cvec, x, y => {
              if y.is_zero() {
                Some(zero)
              } else {
                x.checked_div(*y)
              }
            }),
            Pred::Min([x, y]) => map!(get_cvec, x, y => Some(*x.min(y))),
            Pred::Max([x, y]) => map!(get_cvec, x, y => Some(*x.max(y))),
            Pred::Select([x, y, z]) => map!(get_cvec, x, y, z => {
              let xbool = *x != zero;
              if xbool {Some(*y)} else {Some(*z)}
            }),
            Pred::Var(_) => vec![],
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        let mut get_const = |x| {
            let ival = get_interval(x);
            if ival.low == ival.high {
                ival.low
            } else {
                None
            }
        };
        let val = match self {
            Pred::Lit(n) => Some(*n),
            Pred::Lt([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if x < y { 1 } else { 0 }),
                _ => None,
            },
            Pred::Leq([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if x <= y { 1 } else { 0 }),
                _ => None,
            },
            Pred::Eq([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if x == y { 1 } else { 0 }),
                _ => None,
            },
            Pred::Neq([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if x != y { 1 } else { 0 }),
                _ => None,
            },
            Pred::Implies([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if x == 0 || y != 0 { 1 } else { 0 }),
                _ => None,
            },
            Pred::Not(x) => get_const(x).map(|c| if c == 0 { 1 } else { 0 }),
            Pred::Neg(x) => get_const(x).map(|c| -c),
            Pred::And([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if x != 0 && y != 0 { 1 } else { 0 }),
                _ => None,
            },
            Pred::Or([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if x != 0 || y != 0 { 1 } else { 0 }),
                _ => None,
            },
            Pred::Xor([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(if (x != 0) ^ (y != 0) { 1 } else { 0 }),
                _ => None,
            },
            Pred::Add([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_add(y),
                _ => None,
            },
            Pred::Sub([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_sub(y),
                _ => None,
            },
            Pred::Mul([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_mul(y),
                _ => None,
            },
            Pred::Div([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => {
                    if y == 0 {
                        None
                    } else {
                        x.checked_div(y)
                    }
                }
                _ => None,
            },
            Pred::Min([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(x.min(y)),
                _ => None,
            },
            Pred::Max([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => Some(x.max(y)),
                _ => None,
            },
            Pred::Select([x, y, z]) => match (get_const(x), get_const(y), get_const(z)) {
                (Some(x), Some(y), Some(z)) => Some(if x == 0 { z } else { y }),
                _ => None,
            },
            Pred::Var(_) => None,
        };
        if val.is_some() {
            Interval::new(val, val)
        } else {
            Interval::new(None, None)
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

        println!("Cvec len: {}", egraph.analysis.cvec_len);
        println!("Vars: {vars:?}");

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
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Pred]) -> z3::ast::Int<'a> {
    let mut buf: Vec<z3::ast::Int> = vec![];
    let zero = z3::ast::Int::from_i64(ctx, 0);
    let one = z3::ast::Int::from_i64(ctx, 1);
    for node in expr.as_ref().iter() {
        match node {
            Pred::Lit(c) => buf.push(z3::ast::Int::from_i64(ctx, c.to_i64().unwrap())),
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
            Pred::Div([x, y]) => {
                let l = &buf[usize::from(*x)];
                let r = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(
                    &r._eq(&zero),
                    &zero,
                    &z3::ast::Int::div(l, r),
                ))
            }
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

#[cfg(test)]
#[path = "./recipes/halide.rs"]
mod halide;

mod test {
    use crate::halide::halide_rules;
    use crate::Pred;
    use dotenv::dotenv;
    use std::io::Write;
    use std::{
        fs::OpenOptions,
        io,
        time::{Duration, Instant},
    };

    use ruler::enumo::{Scheduler, Workload};
    use ruler::recipe_utils::{recursive_rules, Lang};
    use ruler::{enumo::Ruleset, logger, Limits};
    use serde_json::{json, to_string_pretty};

    fn write(f: &str, s: &str) -> io::Result<()> {
        let mut file = OpenOptions::new().append(true).create(true).open(f)?;

        writeln!(file, "{s}")?;
        Ok(())
    }

    fn write_derivability(
        rules: Ruleset<Pred>,
        rules_name: &str,
        against: &Ruleset<Pred>,
        against_name: &str,
        subdir: &str,
    ) {
        let derive_t = Instant::now();
        let (can, cannot) = rules.derive(ruler::DeriveType::LhsAndRhs, against, Limits::deriving());
        let derive_t_elapsed = derive_t.elapsed();
        let v = json!({
            "duration": derive_t_elapsed,
            "num_rules": rules.len(),
            "num_against": against.len(),
            "can": can.to_str_vec(),
            "cannot": cannot.to_str_vec()
        });
        let _ = write(
            &format!("jfp/{subdir}/halide/log.txt"),
            &format!(
                "{rules_name}->{against_name} | {:.1} ({:.1?})",
                (can.len() as f64 / against.len() as f64) * 100.0,
                derive_t_elapsed
            ),
        );
        let filename = format!("jfp/{subdir}/halide/{rules_name}-{against_name}-derive.json");
        let _ = write(&filename, &to_string_pretty(&v).unwrap());
    }

    #[test]
    fn establish_baseline() {
        let halide_baseline = Ruleset::from_file("baseline/halide.rules");
        let a5_t = Instant::now();
        let a5: Ruleset<Pred> = recursive_rules(
            ruler::enumo::Metric::Atoms,
            5,
            Lang::new(
                &["0", "1"],
                &["a", "b", "c"],
                &[
                    &["-", "!"],
                    &[
                        "&&", "||", "^", "+", "-", "*", "min", "max", "<", "<=", "==", "!=",
                    ],
                    &["select"],
                ],
            ),
            Ruleset::default(),
        );
        let duration = a5_t.elapsed();
        let _ = write(
            "jfp/baseline/log.txt",
            &format!("ATOMS5 HALIDE | {} rules | {:?}", a5.len(), duration),
        );
        a5.to_file("jfp/baseline/atoms5_halide.rules");
        write_derivability(a5.clone(), "A5", &halide_baseline, "Halide", "baseline");

        write_derivability(a5.clone(), "A5", &a5, "A5", "baseline");

        let enumo_t = Instant::now();
        let enumo_rules = halide_rules();
        let duration = enumo_t.elapsed();
        let _ = write(
            "jfp/baseline/log.txt",
            &format!(
                "ENUMO HALIDE | {} rules | {:?}",
                enumo_rules.len(),
                duration
            ),
        );
        enumo_rules.to_file("jfp/baseline/enumo_halide.rules");

        write_derivability(a5.clone(), "A5", &enumo_rules, "Enumo", "baseline");
        write_derivability(
            enumo_rules.clone(),
            "Enumo",
            &halide_baseline,
            "Halide",
            "baseline",
        );
        write_derivability(enumo_rules.clone(), "Enumo", &a5, "A5", "baseline");
        write_derivability(
            enumo_rules.clone(),
            "Enumo",
            &enumo_rules,
            "Enumo",
            "baseline",
        );
    }

    #[tokio::test]
    async fn case_study1() {
        let _ = write("jfp/cs1/halide/log.txt", "Starting Case Study 1");
        dotenv().ok();

        let halide_baseline = Ruleset::from_file("baseline/halide.rules");

        let prompt = "
        Your task is to perform rule inference for equality saturation.
        The domain is boolean logic and arithmetic, as follows:
            Values: integers
            Unary Operators: -, !
            Binary Operators: <, <=, ==, !=, &&, ||, ^, +, -, *, min, max
            Ternary Operators: select

        Terms must be written using s-expressions and prefix notation.
        Variables are ?x, ?y, and ?z.

        Your task is to generate sound, useful, and complete rewrite rules for the domain.
        The set of rewrite rules should be sufficient to decide the equality between any
        two terms in the domain.
        You should generate at least 200 rules.
        A rewrite rule has the form `l => r` where `l` and `r` are valid terms from
        the domain that are always equivalent.
        Print only the rules, one rule per line, with no additional text or explanation.
        ";
        let rules_t = Instant::now();
        let candidates: Ruleset<Pred> = Ruleset::from_llm(prompt).await;
        let _ = write(
            "jfp/cs1/halide/log.txt",
            &format!(
                "{} rule candidates from LLM | {:?}",
                candidates.len(),
                rules_t.elapsed()
            ),
        );
        candidates.to_file("jfp/cs1/halide/LLM-1-candidates.rules");

        let priors = [
            ("None", Ruleset::default()),
            ("A5", Ruleset::from_file("jfp/baseline/atoms5_halide.rules")),
            (
                "Enumo",
                Ruleset::from_file("jfp/baseline/enumo_halide.rules"),
            ),
        ];

        for (prior_name, prior_rules) in &priors {
            let mut candidates_copy = candidates.clone();
            let minimize_t = Instant::now();
            let (sound, invalid) = candidates_copy.minimize(
                prior_rules.clone(),
                Scheduler::Compress(Limits::minimize()),
                1,
            );
            let _ = write(
                "jfp/cs1/halide/log.txt",
                &format!(
                    "{} | {} selected rules ({} invalid) | {:?}",
                    prior_name,
                    sound.len(),
                    invalid.len(),
                    minimize_t.elapsed(),
                ),
            );
            let name = format!("LLM-{prior_name}-1");

            sound.to_file(&format!("jfp/cs1/halide/{name}.rules"));

            write_derivability(
                sound.union(prior_rules),
                &name,
                &halide_baseline,
                "Halide",
                "cs1",
            );
            // Don't do Halide->X because Halide rules aren't designed for eqsat

            for (prior_name1, prior_rules1) in &priors {
                if prior_rules1.is_empty() {
                    continue;
                }
                // LLM-1->X
                write_derivability(
                    sound.union(prior_rules),
                    &name,
                    prior_rules1,
                    prior_name1,
                    "cs1",
                );
                // X->LLM-1
                write_derivability(prior_rules1.clone(), prior_name1, &sound, &name, "cs1");
            }

            // Reprompt for missing rules
            let reprompt = &format!("
            The following are rewrite rules for the domain of boolean logic and arithmetic:
            {}
            {}

            These rules will be used for equality saturation.
            Are there any rules missing? Please generate the missing rules.
            A rewrite rule has the form `l ==> r` where `l` and `r` are valid terms from the domain that are always equivalent.
            Do not use = or => to write rules, only use ==>.
            Print only the rules, one rule per line, with no additional text or explanation.
            ", sound.to_str_vec().join("\n"), prior_rules.to_str_vec().join("\n"));
            let reprompted_rules_t = Instant::now();
            let mut reprompted_candidates: Ruleset<Pred> = Ruleset::from_llm(reprompt).await;
            let _ = write(
                "jfp/cs1/halide/log.txt",
                &format!(
                    "{} rule candidates (reprompted) | {:?}",
                    reprompted_candidates.len(),
                    reprompted_rules_t.elapsed()
                ),
            );
            reprompted_candidates.to_file(&format!(
                "jfp/cs1/halide/LLM-{prior_name}-2-candidates.rules"
            ));

            // Minimize reprompted
            let reprompt_minimize_t = Instant::now();
            let (reprompted_sound, invalid) = reprompted_candidates.minimize(
                sound.union(prior_rules),
                Scheduler::Compress(Limits::minimize()),
                1,
            );
            let _ = write(
                "jfp/cs1/halide/log.txt",
                &format!(
                    "{} | Reprompt | {} selected rules ({} invalid) | {:?}",
                    prior_name,
                    reprompted_sound.len(),
                    invalid.len(),
                    reprompt_minimize_t.elapsed(),
                ),
            );
            let reprompted_name = format!("LLM-{prior_name}-2");

            reprompted_sound.to_file(&format!("jfp/cs1/halide/{reprompted_name}.rules"));

            write_derivability(
                reprompted_sound.union(&sound).union(prior_rules),
                &reprompted_name,
                &halide_baseline,
                "Halide",
                "cs1",
            );
            // Don't do Halide->X because Halide rules aren't designed for eqsat

            for (prior_name1, prior_rules1) in &priors {
                if prior_rules1.is_empty() {
                    continue;
                }
                // LLM-2->X
                write_derivability(
                    reprompted_sound.union(&sound).union(prior_rules),
                    &reprompted_name,
                    prior_rules1,
                    prior_name1,
                    "cs1",
                );

                // X->LLM-2
                write_derivability(
                    prior_rules1.clone(),
                    prior_name1,
                    &reprompted_sound.union(&sound),
                    &reprompted_name,
                    "cs1",
                );
            }
        }
    }

    #[tokio::test]
    async fn case_study2() {
        dotenv().ok();
        let enumo_baseline: Ruleset<Pred> = Ruleset::from_file("jfp/baseline/enumo_halide.rules");
        let a5_baseline: Ruleset<Pred> = Ruleset::from_file("jfp/baseline/atoms5_halide.rules");
        let halide_baseline: Ruleset<Pred> = Ruleset::from_file("baseline/halide.rules");
        let llm_2: Ruleset<Pred> = Ruleset::from_file("jfp/cs1/halide/LLM-None-1.rules")
            .union(&Ruleset::from_file("jfp/cs1/halide/LLM-None-2.rules"));

        let prompt = "
        Your task is to perform term enumeration for rule inference.
        The domain is boolean logic and arithmetic, as follows:
            Values: integers
            Unary Operators: -, !
            Binary Operators: <, <=, ==, !=, &&, ||, ^, +, -, *, min, max
            Ternary Operators: select

        Terms must be written using s-expressions and prefix notation.
        For example, (a + b) is not a valid term, but (+ a b) is a valid term.
        Use 0 and 1 for constants and w, x, y, and z for variables.
        Do not use any variables other than `w`, `x`, `y`, and `z`.

        Binary operators must have exactly two operands. For example, (+ 1 2 3) is not a valid term, but (+ 1 (+ 2 3)) is.
        Do not use any operators or syntax not listed here.

        Your task is to generate a list of terms from this domain, from which a set of rewrite rules will be inferred.
        Try to generate pairs of terms that might be equivalent, so that rewrite rules can be inferred.
        The generated terms should adequately cover the set of all possible terms.
        The generated terms should vary in complexity and size so that they lead to interesting rewrite rules.
        You should generate at least 1000 terms.
        Your response should not contain `...` or another indicator that you have stopped before finishing term enumeration.
        Print only the terms, one term per line, with no additional text or explanation.
        ";
        let wkld_t = Instant::now();
        let wkld = Workload::from_llm(prompt)
            .await
            .as_lang_with_vars::<Pred>(vec!["w".into(), "x".into(), "y".into(), "z".into()]);
        let _ = write(
            "jfp/cs2/halide/log.txt",
            &format!(
                "LLM Workload: {} | {:?}",
                wkld.force().len(),
                wkld_t.elapsed()
            ),
        );
        wkld.to_file("jfp/cs2/halide/llm-wkld.terms");

        let priors = [
            ("None", Ruleset::default()),
            ("A5", Ruleset::from_file("jfp/baseline/atoms5_halide.rules")),
            ("Enumo", enumo_baseline.clone()),
            ("LLM-2", llm_2.clone()),
        ];
        for (prior_name, prior_rules) in &priors {
            let _ = write("jfp/cs2/halide/log.txt", &format!("--- {prior_name} ---"));
            let rule_syn_t = Instant::now();
            // Wkld -> Egraph
            let egraph = wkld.to_egraph::<Pred>();
            println!("Initial egraph size: {}", egraph.number_of_classes());

            // Run prior rules
            let compress_t = Instant::now();
            let compressed = Scheduler::Compress(Limits::synthesis()).run(&egraph, prior_rules);
            println!(
                "{} prior rules | {} eclasses | {:?}",
                prior_name,
                compressed.number_of_classes(),
                compress_t.elapsed()
            );

            // cvec match
            let cvec_match_t = Instant::now();
            let mut candidates = Ruleset::cvec_match(&compressed);
            println!(
                "{} candidates | {:?}",
                candidates.len(),
                cvec_match_t.elapsed()
            );

            // minimize
            let minimize_t = Instant::now();
            let (rules, _) = candidates.minimize(
                prior_rules.clone(),
                Scheduler::Compress(Limits::minimize()),
                1,
            );
            println!("{} selected | {:?}", rules.len(), minimize_t);

            let rule_syn_t = rule_syn_t.elapsed();
            let _ = write(
                "jfp/cs2/halide/log.txt",
                &format!(
                    "W-{prior_name} | {} rules | rule synth time: {:?}",
                    rules.len(),
                    rule_syn_t
                ),
            );
            rules.to_file(&format!("jfp/cs2/halide/w-{prior_name}.rules"));

            // Derive
            let all_rules = rules.union(prior_rules);
            write_derivability(
                all_rules.clone(),
                &format!("w-{prior_name}"),
                &halide_baseline,
                "Halide",
                "cs2",
            );
            write_derivability(
                all_rules.clone(),
                &format!("w-{prior_name}"),
                &a5_baseline,
                "A5",
                "cs2",
            );
            write_derivability(
                all_rules.clone(),
                &format!("w-{prior_name}"),
                &enumo_baseline,
                "Enumo",
                "cs2",
            );
        }
    }

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
