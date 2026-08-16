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
                // Conservative: eval defines x/0 = 0, but we simply
                // don't fold division by zero.
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
    use std::time::{Duration, Instant};

    use ruler::{
        enumo::{Metric, Ruleset, Scheduler},
        logger,
        recipe_utils::{recursive_rules, Lang},
        Limits,
    };

    #[tokio::test]
    async fn case_study1() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }
        dotenv::dotenv().ok();
        // Skip (rather than fail) when no API key is configured locally
        if std::env::var("OPENROUTER_API_KEY").is_err() {
            eprintln!("Skipping case_study1: OPENROUTER_API_KEY not set");
            return;
        }
        for f in [
            "jfp/baseline/atoms5_halide.rules",
            "jfp/baseline/enumo_halide.rules",
        ] {
            assert!(
                std::path::Path::new(f).exists(),
                "missing {}: run establish_baseline first",
                f
            );
        }

        let dir = "jfp/cs1/halide";
        let halide_baseline: Ruleset<Pred> = Ruleset::from_file("baseline/halide.rules");

        let prompt = "
        You are generating rewrite rules for an equality saturation system.
        The domain is boolean logic and integer arithmetic, as follows:
            Values: integers. Comparisons and boolean operators return 1 (true) or 0 (false); any nonzero value is treated as true.
            Unary Operators: - (negation), ! (logical not)
            Binary Operators: <, <=, ==, !=, &&, ||, ^ (xor), +, - (subtraction), *, min, max
            Ternary Operators: select ((select c t f) evaluates to t if c is nonzero, and f otherwise)

        Terms must be written using s-expressions and prefix notation.
        Every operator takes exactly the number of operands stated above: (+ 1 2 3) is not a valid term, but (+ 1 (+ 2 3)) is.
        Variables are ?x, ?y, and ?z. The integer constants 0 and 1 may also appear in rules.
        Do not use any operators or syntax not listed here.

        A rewrite rule has the form `l ==> r` where `l` and `r` are terms that are equal for ALL integer values of the variables. For example:
        (+ ?x ?y) ==> (+ ?y ?x)
        (min ?x ?x) ==> ?x
        (select 1 ?x ?y) ==> ?x

        Generate a comprehensive set of sound rewrite rules for this domain, covering at least the following categories:
        - identity and annihilator rules for each operator (e.g. adding 0, multiplying by 0 or 1)
        - commutativity and associativity of +, *, min, max, &&, ||, ^, and commutativity of == and !=
        - distributivity rules (e.g. * over +, && over ||, min and max over +, min over max)
        - negation and logical-not rules (double negation, De Morgan's laws, ! of a comparison as the flipped comparison)
        - relationships among <, <=, ==, and != (e.g. swapping argument order, complements)
        - absorption and idempotence rules for min, max, &&, ||
        - select rules (constant condition, equal branches, pushing operators into select, nested selects)
        - rules connecting comparisons with min and max (e.g. (<= (min ?x ?y) ?x) ==> 1)

        Generate at least 200 rules.
        Every rule must be sound: both sides must be equal for every assignment of integer values to the variables, including 0 and negative values.
        Print only the rules, one rule per line, in the exact `l ==> r` syntax shown above.
        Plain text only - no markdown, no code fences, no numbering, no extra commentary.
        ";
        let start = Instant::now();
        let candidates: Ruleset<Pred> = Ruleset::from_llm(prompt).await;
        logger::log_line(
            &format!("{dir}/log.txt"),
            &format!(
                "LLM-1: {} candidates | {:.1?}",
                candidates.len(),
                start.elapsed()
            ),
        );
        candidates.to_file(&format!("{dir}/LLM-1-candidates.rules"));

        let priors = [
            ("None", Ruleset::default()),
            ("A5", Ruleset::from_file("jfp/baseline/atoms5_halide.rules")),
            (
                "Enumo",
                Ruleset::from_file("jfp/baseline/enumo_halide.rules"),
            ),
        ];

        for (prior_name, prior_rules) in &priors {
            // Minimize the candidates against this prior (minimize
            // validates via z3 as it selects)
            let name = format!("LLM-{prior_name}-1");
            let mut candidates_copy = candidates.clone();
            let start = Instant::now();
            let (sound, invalid) = candidates_copy
                .minimize(prior_rules.clone(), Scheduler::Compress(Limits::minimize()));
            logger::log_line(
                &format!("{dir}/log.txt"),
                &format!(
                    "{name}: {} selected ({} invalid) | {:.1?}",
                    sound.len(),
                    invalid.len(),
                    start.elapsed()
                ),
            );
            sound.to_file(&format!("{dir}/{name}.rules"));

            logger::write_derivability(
                dir,
                &sound.union(prior_rules),
                &name,
                &halide_baseline,
                "Halide",
            );
            for (p_name, p_rules) in &priors {
                if p_rules.is_empty() {
                    continue;
                }
                logger::write_derivability(dir, &sound.union(prior_rules), &name, p_rules, p_name);
                logger::write_derivability(dir, p_rules, p_name, &sound, &name);
            }

            // Reprompt for rules missing from what we kept
            let reprompt = format!("
            You are generating rewrite rules for an equality saturation system.
            The domain is boolean logic and integer arithmetic, as follows:
                Values: integers. Comparisons and boolean operators return 1 (true) or 0 (false); any nonzero value is treated as true.
                Unary Operators: - (negation), ! (logical not)
                Binary Operators: <, <=, ==, !=, &&, ||, ^ (xor), +, - (subtraction), *, min, max
                Ternary Operators: select ((select c t f) evaluates to t if c is nonzero, and f otherwise)

            The following rewrite rules are already in the ruleset:
            {}
            {}

            Identify sound rewrite rules for this domain that are missing from the ruleset above, and print them.
            Do not repeat rules from the list above, and do not print trivial variants of them (e.g. renamed variables or swapped arguments of commutative operators).
            Terms are s-expressions in prefix notation; variables are ?x, ?y, and ?z, and the integer constants 0 and 1 may also appear.
            A rewrite rule has the form `l ==> r` where `l` and `r` are terms that are equal for ALL integer values of the variables. For example: (min ?x ?x) ==> ?x
            If no rules are missing, print nothing.
            Print only the rules, one rule per line.
            Plain text only - no markdown, no code fences, no numbering, no extra commentary.
            ", sound.to_str_vec().join("\n"), prior_rules.to_str_vec().join("\n"));
            let start = Instant::now();
            let mut reprompted: Ruleset<Pred> = Ruleset::from_llm(&reprompt).await;
            let name2 = format!("LLM-{prior_name}-2");
            logger::log_line(
                &format!("{dir}/log.txt"),
                &format!(
                    "{name2}: {} candidates (reprompted) | {:.1?}",
                    reprompted.len(),
                    start.elapsed()
                ),
            );
            reprompted.to_file(&format!("{dir}/{name2}-candidates.rules"));

            // Minimize the reprompted candidates against everything
            // already selected
            let start = Instant::now();
            let (sound2, invalid2) = reprompted.minimize(
                sound.union(prior_rules),
                Scheduler::Compress(Limits::minimize()),
            );
            logger::log_line(
                &format!("{dir}/log.txt"),
                &format!(
                    "{name2}: {} selected ({} invalid) | {:.1?}",
                    sound2.len(),
                    invalid2.len(),
                    start.elapsed()
                ),
            );
            sound2.to_file(&format!("{dir}/{name2}.rules"));

            logger::write_derivability(
                dir,
                &sound2.union(&sound).union(prior_rules),
                &name2,
                &halide_baseline,
                "Halide",
            );
            for (p_name, p_rules) in &priors {
                if p_rules.is_empty() {
                    continue;
                }
                logger::write_derivability(
                    dir,
                    &sound2.union(&sound).union(prior_rules),
                    &name2,
                    p_rules,
                    p_name,
                );
                logger::write_derivability(dir, p_rules, p_name, &sound2.union(&sound), &name2);
            }
        }
    }

    #[test]
    fn establish_baseline() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let halide_baseline: Ruleset<Pred> = Ruleset::from_file("baseline/halide.rules");

        // A5: rules from plain atoms-5 enumeration over the full op set
        let start = Instant::now();
        let a5: Ruleset<Pred> = recursive_rules(
            Metric::Atoms,
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
        logger::log_line(
            "jfp/baseline/log.txt",
            &format!(
                "ATOMS5 HALIDE | {} rules | {:.1?}",
                a5.len(),
                start.elapsed()
            ),
        );
        a5.to_file("jfp/baseline/atoms5_halide.rules");

        // Enumo: rules from the halide recipe
        let start = Instant::now();
        let enumo: Ruleset<Pred> = halide_rules();
        logger::log_line(
            "jfp/baseline/log.txt",
            &format!(
                "ENUMO HALIDE | {} rules | {:.1?}",
                enumo.len(),
                start.elapsed()
            ),
        );
        enumo.to_file("jfp/baseline/enumo_halide.rules");

        // Baseline-vs-baseline derivability (skipping Halide).
        logger::write_derivability("jfp/baseline", &a5, "A5", &halide_baseline, "Halide");
        logger::write_derivability("jfp/baseline", &a5, "A5", &a5, "A5");
        logger::write_derivability("jfp/baseline", &a5, "A5", &enumo, "Enumo");
        logger::write_derivability("jfp/baseline", &enumo, "Enumo", &halide_baseline, "Halide");
        logger::write_derivability("jfp/baseline", &enumo, "Enumo", &a5, "A5");
        logger::write_derivability("jfp/baseline", &enumo, "Enumo", &enumo, "Enumo");
    }

    #[test]
    fn interval_constant_fold() {
        use ruler::{enumo::Workload, EGraph, Interval, SynthAnalysis};

        let egraph: EGraph<Pred, SynthAnalysis> =
            Workload::new(["(+ 1 2)", "(min 0 1)", "(select 0 1 0)", "(+ a 0)"]).to_egraph();
        let interval_of = |s: &str| {
            let expr: egg::RecExpr<Pred> = s.parse().unwrap();
            let id = egraph.lookup_expr(&expr).unwrap();
            egraph[id].data.interval.clone()
        };
        assert_eq!(interval_of("(+ 1 2)"), Interval::new(Some(3), Some(3)));
        assert_eq!(interval_of("(min 0 1)"), Interval::new(Some(0), Some(0)));
        assert_eq!(
            interval_of("(select 0 1 0)"),
            Interval::new(Some(0), Some(0))
        );
        assert_eq!(interval_of("(+ a 0)"), Interval::new(None, None));
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
