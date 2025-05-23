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

    use ruler::enumo::Scheduler;
    use ruler::recipe_utils::{recursive_rules, Lang};
    use ruler::{enumo::Ruleset, logger, Limits};
    use serde_json::{json, to_string_pretty};

    fn write(f: &str, s: &str) -> io::Result<()> {
        let mut file = OpenOptions::new().append(true).create(true).open(f)?;

        writeln!(file, "{}", s)?;
        Ok(())
    }

    fn write_derivability(
        rules: Ruleset<Pred>,
        rules_name: &str,
        against: &Ruleset<Pred>,
        against_name: &str,
    ) {
        let derive_t = Instant::now();
        let (can, cannot) = rules.derive(ruler::DeriveType::Lhs, against, Limits::deriving());
        let v = json!({
            "duration": derive_t.elapsed(),
            "num_rules": rules.len(),
            "num_against": against.len(),
            "can": can.to_str_vec(),
            "cannot": cannot.to_str_vec()
        });
        let _ = write(
            "jfp/halide/log.txt",
            &format!(
                "{rules_name}->{against_name} {} Derivability",
                can.len() as f64 / against.len() as f64
            ),
        );
        let filename = format!("jfp/halide/{rules_name}-{against_name}-derive.json");
        let _ = write(&filename, &to_string_pretty(&v).unwrap());
    }

    fn establish_baseline() {
        let halide_baseline = Ruleset::from_file("baseline/halide.rules");
        let rules_t = Instant::now();
        let rules: Ruleset<Pred> = recursive_rules(
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
        let duration = rules_t.elapsed();
        let _ = write(
            "jfp/halide/log.txt",
            &format!("ATOMS5 | {} rules | {:?}", rules.len(), duration),
        );
        rules.to_file("jfp/halide/atoms5.rules");
        write_derivability(rules, "A5", &halide_baseline, "Halide");

        let rules_t = Instant::now();
        let rules = halide_rules();
        let duration = rules_t.elapsed();
        let _ = write(
            "jfp/halide/log.txt",
            &format!("ENUMO | {} rules | {:?}", rules.len(), duration),
        );
        rules.to_file("jfp/halide/enumo.rules");
        write_derivability(rules, "Enumo", &halide_baseline, "Halide");
    }

    fn priors() -> Vec<(String, Ruleset<Pred>)> {
        vec![
            ("none".into(), Ruleset::default()),
            ("A5".into(), Ruleset::from_file("jfp/halide/atoms5.rules")),
            ("Enumo".into(), Ruleset::from_file("jfp/halide/enumo.rules")),
        ]
    }

    #[tokio::test]
    async fn case_study2() {
        dotenv().ok();

        establish_baseline();

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
        let candidates: Ruleset<Pred> = Ruleset::from_llm(&prompt).await;
        let _ = write(
            "jfp/halide/log.txt",
            &format!(
                "{} rule candidates | {:?}",
                candidates.len(),
                rules_t.elapsed()
            ),
        );
        candidates.to_file("jfp/halide/candidates.rules");

        for (prior_name, prior_rules) in priors() {
            let mut candidates_copy = candidates.clone();
            let minimize_t = Instant::now();
            let (sound, invalid) = candidates_copy
                .minimize(prior_rules.clone(), Scheduler::Compress(Limits::minimize()));
            let _ = write(
                "jfp/halide/log.txt",
                &format!(
                    "{} | {} selected rules ({} invalid) | {:?}",
                    prior_name,
                    sound.len(),
                    invalid.len(),
                    minimize_t.elapsed(),
                ),
            );
            sound.to_file(&format!("jfp/halide/{}-rules.rules", prior_name));

            write_derivability(
                sound.union(&prior_rules),
                &format!("llm_and_{prior_name}"),
                &halide_baseline,
                "Halide",
            );

            // Reprompt for missing rules
            let reprompt = &format!("
            The following are rewrite rules for the domain of boolean logic and arithmetic:
            {}
            {}

            These rules will be used for equality saturation.
            Are there any rules missing? Please generate the missing rules.
            A rewrite rule has the form `l ==> r` where `l` and `r` are valid terms from the domain that are always equivalent.
            Print only the rules, one rule per line, with no additional text or explanation.
            ", sound.to_str_vec().join("\n"), prior_rules.to_str_vec().join("\n"));
            let reprompted_rules_t = Instant::now();
            let mut reprompted_candidates: Ruleset<Pred> = Ruleset::from_llm(&reprompt).await;
            let _ = write(
                "jfp/halide/log.txt",
                &format!(
                    "{} rule candidates (reprompted) | {:?}",
                    reprompted_candidates.len(),
                    reprompted_rules_t.elapsed()
                ),
            );
            reprompted_candidates.to_file(&format!(
                "jfp/halide/reprompted-candidates-{}.rules",
                prior_name
            ));

            // Minimize reprompted
            let reprompt_minimize_t = Instant::now();
            let (reprompted_sound, invalid) = reprompted_candidates.minimize(
                sound.union(&prior_rules),
                Scheduler::Compress(Limits::minimize()),
            );
            let _ = write(
                "jfp/halide/log.txt",
                &format!(
                    "{} | Reprompt | {} selected rules ({} invalid) | {:?}",
                    prior_name,
                    reprompted_sound.len(),
                    invalid.len(),
                    reprompt_minimize_t.elapsed(),
                ),
            );
            reprompted_sound.to_file(&format!("jfp/halide/{}-reprompted-rules.rules", prior_name));

            write_derivability(
                reprompted_sound.union(&sound).union(&prior_rules),
                &format!("llm_and_{prior_name}_reprompt"),
                &halide_baseline,
                "Halide",
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
