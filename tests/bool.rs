use ruler::{enumo::Scheduler, *};
use std::ops::*;
use z3::ast::Ast;
#[path = "./recipes/bool.rs"]
pub mod bool;

egg::define_language! {
  pub enum Bool {
    "~" = Not(Id),
    "&" = And([Id; 2]),
    "|" = Or([Id; 2]),
    "^" = Xor([Id; 2]),
    "->" = Implies([Id; 2]),
    Lit(bool),
    Var(egg::Symbol),
  }
}

impl SynthLanguage for Bool {
    type Constant = bool;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Bool::Not(x) => map!(get_cvec, x => Some(x.not())),
            Bool::And([x, y]) => map!(get_cvec, x, y => Some(*x & *y)),
            Bool::Or([x, y]) => map!(get_cvec, x, y => Some(*x | *y)),
            Bool::Xor([x, y]) => map!(get_cvec, x, y => Some(*x ^ *y)),
            Bool::Implies([x, y]) => map!(get_cvec, x, y => Some(!(*x) | *y)),
            Bool::Lit(c) => vec![Some(*c); cvec_len],
            Bool::Var(_) => vec![],
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        let unwrap_interval = |interval: &Interval<Self::Constant>| {
            (
                interval
                    .low
                    .expect("Bool shouldn't have infinite intervals"),
                interval
                    .high
                    .expect("Bool shouldn't have infinite intervals"),
            )
        };
        match self {
            Bool::Lit(c) => Interval::new(Some(*c), Some(*c)),
            Bool::Var(_) => Interval::new(Some(false), Some(true)),
            Bool::Not(x) => {
                let (low, high) = unwrap_interval(get_interval(x));
                Interval::new(Some(!high), Some(!low))
            }
            Bool::And([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                Interval::new(Some(x_low && y_low), Some(x_high && y_high))
            }
            Bool::Or([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                Interval::new(Some(x_low || y_low), Some(x_high || y_high))
            }
            Bool::Xor([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                if x_low == x_high && y_low == y_high {
                    Interval::new(Some(x_low != y_low), Some(x_low != y_low))
                } else {
                    Interval::new(Some(false), Some(true))
                }
            }
            Bool::Implies([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                Interval::new(Some(!x_high || y_low), Some(!x_low || y_high))
            }
        }
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let consts = vec![Some(true), Some(false)];
        let cvecs = self_product(&consts, vars.len());

        egraph.analysis.cvec_len = cvecs[0].len();

        for (i, v) in vars.iter().enumerate() {
            let id = egraph.add(Bool::Var(Symbol::from(v.clone())));
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Bool::Var(sym)
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Bool::Var(v) => Some(*v),
            _ => None,
        }
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

    fn is_constant(&self) -> bool {
        matches!(self, Bool::Lit(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Bool::Lit(c)
    }
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Bool]) -> z3::ast::Bool<'a> {
    let mut buf = vec![];
    for node in expr.as_ref().iter() {
        match node {
            Bool::Not(x) => buf.push(z3::ast::Bool::not(&buf[usize::from(*x)])),
            Bool::And([x, y]) => buf.push(z3::ast::Bool::and(
                &ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Bool::Or([x, y]) => buf.push(z3::ast::Bool::or(
                &ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Bool::Xor([x, y]) => buf.push(z3::ast::Bool::xor(
                &buf[usize::from(*x)],
                &buf[usize::from(*y)],
            )),
            Bool::Implies([x, y]) => buf.push(z3::ast::Bool::implies(
                &buf[usize::from(*x)],
                &buf[usize::from(*y)],
            )),
            Bool::Lit(c) => buf.push(z3::ast::Bool::from_bool(&ctx, *c)),
            Bool::Var(sym) => buf.push(z3::ast::Bool::new_const(&ctx, sym.to_string())),
        }
    }
    buf.pop().unwrap()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::bool::bool_rules;
    use dotenv::dotenv;
    use ruler::{
        enumo::{Filter, Metric, Ruleset, Workload},
        recipe_utils::{base_lang, iter_metric, recursive_rules, run_workload, Lang},
    };
    use std::time::Instant;

    fn iter_bool(n: usize) -> Workload {
        iter_metric(base_lang(2), "EXPR", Metric::Atoms, n)
            .filter(Filter::Contains("VAR".parse().unwrap()))
            .plug("VAL", &Workload::new(["true", "false"]))
            .plug("VAR", &Workload::new(["a", "b", "c"]))
            .plug("OP1", &Workload::new(["~"]))
            .plug("OP2", &Workload::new(["&", "|", "^", "->"]))
    }

    #[test]
    fn dsl() {
        let mut all_rules: Ruleset<Bool> = Ruleset::default();
        let atoms3 = iter_bool(3);
        assert_eq!(atoms3.force().len(), 93);

        let scheduler = Scheduler::Compress(Limits::synthesis());

        let egraph = scheduler.run(&atoms3.to_egraph(), &all_rules);
        let mut candidates = Ruleset::cvec_match(&egraph);
        let rules3 = candidates.minimize(all_rules.clone(), scheduler).0;
        all_rules.extend(rules3);

        let atoms4 = iter_bool(4);
        assert_eq!(atoms4.force().len(), 348);

        let egraph = scheduler.run(&atoms4.to_egraph(), &all_rules);
        candidates = Ruleset::cvec_match(&egraph);
        let rules4 = candidates.minimize(all_rules.clone(), scheduler).0;
        all_rules.extend(rules4);

        let atoms5 = iter_bool(5);
        assert_eq!(atoms5.force().len(), 4599);

        let egraph = scheduler.run(&atoms5.to_egraph(), &all_rules);
        candidates = Ruleset::cvec_match(&egraph);
        let rules5 = candidates.minimize(all_rules.clone(), scheduler).0;
        all_rules.extend(rules5);

        let expected: Ruleset<Bool> = Ruleset::new(&[
            "(^ ?b ?a) ==> (^ ?a ?b)",
            "(& ?b ?a) ==> (& ?a ?b)",
            "(| ?b ?a) ==> (| ?a ?b)",
            "(& ?a ?a) <=> ?a",
            "?a <=> (~ (~ ?a))",
            "?a <=> (| ?a ?a)",
            "(-> ?a ?a) ==> true",
            "(^ ?a ?a) ==> false",
            "(| ?a false) <=> ?a",
            "?a <=> (-> true ?a)",
            "?a <=> (& ?a true)",
            "?a <=> (^ false ?a)",
            "(~ ?a) <=> (-> ?a false)",
            "(~ ?a) <=> (^ ?a true)",
            "(-> ?b (~ ?a)) <=> (~ (& ?b ?a))",
            "(~ (^ ?b ?a)) ==> (^ ?b (~ ?a))",
            "(-> (~ ?b) ?a) ==> (| ?a ?b)",
            "(-> ?b (~ ?a)) ==> (^ ?a (-> ?a ?b))",
            "(| ?b ?a) ==> (-> (-> ?b ?a) ?a)",
            "(-> ?b ?a) <=> (-> (| ?a ?b) ?a)",
            "(| ?b ?a) <=> (| ?a (^ ?b ?a))",
            "(& ?b ?a) ==> (& ?b (-> ?b ?a))",
            "(| ?b (& ?b ?a)) ==> ?b",
            "(& ?a (| ?b ?a)) ==> ?a",
            "(& ?a (-> ?b ?a)) ==> ?a",
            "(-> (-> ?a ?b) ?a) ==> ?a",
            "(-> ?c (-> ?b ?a)) <=> (-> (& ?c ?b) ?a)",
            "(| ?c (| ?b ?a)) ==> (| ?a (| ?b ?c))",
            "(-> ?c (-> ?b ?a)) ==> (-> ?b (-> ?c ?a))",
            "(^ ?c (^ ?b ?a)) ==> (^ ?a (^ ?c ?b))",
        ]);
        let (can, cannot) = all_rules.derive(DeriveType::LhsAndRhs, &expected, Limits::deriving());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }

    #[test]
    fn simple() {
        let mut all_rules = Ruleset::default();
        let atoms3 = iter_bool(3);
        assert_eq!(atoms3.force().len(), 93);

        let rules3 = run_workload(
            atoms3,
            all_rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            false,
        );
        all_rules.extend(rules3);

        let atoms4 = iter_bool(4);
        assert_eq!(atoms4.force().len(), 348);

        let rules4 = run_workload(
            atoms4,
            all_rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            false,
        );
        all_rules.extend(rules4);

        let atoms5 = iter_bool(5);
        assert_eq!(atoms5.force().len(), 4599);

        let rules5 = run_workload(
            atoms5,
            all_rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            false,
        );
        all_rules.extend(rules5);

        let expected: Ruleset<Bool> = Ruleset::new(&[
            "(^ ?b ?a) ==> (^ ?a ?b)",
            "(& ?b ?a) ==> (& ?a ?b)",
            "(| ?b ?a) ==> (| ?a ?b)",
            "(& ?a ?a) <=> ?a",
            "?a <=> (~ (~ ?a))",
            "?a <=> (| ?a ?a)",
            "(-> ?a ?a) ==> true",
            "(^ ?a ?a) ==> false",
            "(| ?a false) <=> ?a",
            "?a <=> (-> true ?a)",
            "?a <=> (& ?a true)",
            "?a <=> (^ false ?a)",
            "(~ ?a) <=> (-> ?a false)",
            "(~ ?a) <=> (^ ?a true)",
            "(-> ?b (~ ?a)) <=> (~ (& ?b ?a))",
            "(~ (^ ?b ?a)) ==> (^ ?b (~ ?a))",
            "(-> (~ ?b) ?a) ==> (| ?a ?b)",
            "(-> ?b (~ ?a)) ==> (^ ?a (-> ?a ?b))",
            "(| ?b ?a) ==> (-> (-> ?b ?a) ?a)",
            "(-> ?b ?a) <=> (-> (| ?a ?b) ?a)",
            "(| ?b ?a) <=> (| ?a (^ ?b ?a))",
            "(& ?b ?a) ==> (& ?b (-> ?b ?a))",
            "(| ?b (& ?b ?a)) ==> ?b",
            "(& ?a (| ?b ?a)) ==> ?a",
            "(& ?a (-> ?b ?a)) ==> ?a",
            "(-> (-> ?a ?b) ?a) ==> ?a",
            "(-> ?c (-> ?b ?a)) <=> (-> (& ?c ?b) ?a)",
            "(| ?c (| ?b ?a)) ==> (| ?a (| ?b ?c))",
            "(-> ?c (-> ?b ?a)) ==> (-> ?b (-> ?c ?a))",
            "(^ ?c (^ ?b ?a)) ==> (^ ?a (^ ?c ?b))",
        ]);
        let (can, cannot) = all_rules.derive(DeriveType::LhsAndRhs, &expected, Limits::deriving());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bool_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<_>::from_file("baseline/bool.rules");

        logger::write_baseline(&rules, "bool", &baseline, "oopsla", duration);
    }

    #[test]
    fn parse_invalid_wkld() {
        let wkld = Workload::from_file("llm/out/bool.wkld");
        assert!(wkld.force().len() == 278);
        let valid = wkld.as_lang::<Bool>();
        assert!(valid.force().len() == 183);
    }

    #[tokio::test]
    async fn llm_rules() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        dotenv().ok();

        let enumo_rules = bool_rules();

        let prompt = "
        Your task is to perform rule inference for equality saturation.
        The domain is boolean logic, as follows:
        Values: true, false
        Unary Operators: ~
        Binary Operators: &, |, ^, ->

        Terms must be written using s-expressions and prefix notation.
        Variables are ?x, ?y, and ?z.

        Your task is to generate sound, useful, and complete rewrite rules for the domain.
        The set of rewrite rules should be sufficient to decide the equality between any
        two terms in the domain.
        A rewrite rule has the form `l => r` where `l` and `r` are valid terms from
        the domain that are always equivalent.
        Print only the rules, one rule per line, with no additional text or explanation.
        ";

        let models = llm::models();
        for model in models {
            let start = Instant::now();
            let rules: Ruleset<Bool> = Ruleset::from_llm(&prompt, &model).await;
            let (sound, unsound) = rules.partition(|r| r.is_valid());
            let duration = start.elapsed();

            println!(
                "{} synthesized {} sound and {} unsound rules",
                &model,
                sound.len(),
                unsound.len()
            );
            logger::write_baseline(
                &rules,
                &format!("{}-ALL", model),
                &enumo_rules,
                "enumo",
                duration,
            );
        }
    }

    #[test]
    fn test_validate() {
        let rules: Ruleset<Bool> = Ruleset::new(&[
            "(^ ?b ?a) ==> (^ ?a ?b)",
            "(& ?b ?a) ==> (& ?a ?b)",
            "(| ?b ?a) ==> (| ?a ?b)",
            "(^ ?a true) ==> (~ ?a)",
            "(^ true ?a) ==> (~ ?a)",
            "(^ ?a (~ ?a)) ==> false", // unsound
            "(& ?a ?a) ==> ?a",
            "?a ==> (~ (~ ?a))",
        ]);

        let (sound, unsound) = rules.partition(|r| r.is_valid());
        assert!(sound.len() == 7);
        assert!(unsound.len() == 1);
    }

    #[tokio::test]
    async fn llm_term_enumeration() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        dotenv().ok();

        let enumo_rules = bool_rules();

        let prompt = "
        Your task is to perform term enumeration for rule inference.
        The domain is boolean logic, as follows:
        Values: true, false
        Unary Operators: ~
        Binary Operators: &, |, ^, ->

        Terms must be written using s-expressions and prefix notation.
        For example, instead of (?x & ?y), you should write (& ?x ?y).
        Variables are ?x, ?y, and ?z.

        Your task is to generate terms from the grammar, from which a set of rewrite rules can be inferred.
        The generated terms should adequately cover the set of all possible terms.
        The generated terms should vary in complexity and size so that they lead to interesting rewrite rules.
        Generate at least 1000 terms. Do not stop before you have generated 1000 terms.
        Your response should not contain `...` or another indicator that you have stopped before finishing term enumeration.
        Print only the terms, one term per line, with no additional text or explanation.
        ";

        let models = llm::models();
        for model in models {
            let start = Instant::now();
            let wkld = Workload::from_llm(&prompt, &model).await.as_lang::<Bool>();

            let rules: Ruleset<Bool> = if wkld.force().len() == 0 {
                // LLM generated no valid terms
                Ruleset::default()
            } else {
                run_workload(
                    wkld,
                    Ruleset::default(),
                    Limits::synthesis(),
                    Limits::minimize(),
                    true,
                )
            };
            let (sound, unsound) = rules.partition(|r| r.is_valid());
            let duration = start.elapsed();

            println!(
                "{} synthesized {} sound and {} unsound rules",
                &model,
                sound.len(),
                unsound.len()
            );
            logger::write_baseline(
                &rules,
                &format!("{}-TE", model),
                &enumo_rules,
                "enumo",
                duration,
            );
        }
    }

    #[tokio::test]
    async fn test_llm_after_exhaustive() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        dotenv().ok();

        let enumo_rules = bool_rules();

        let a5_start = Instant::now();
        // First, do exhaustive synthesis up to size 5
        let rules5: Ruleset<Bool> = recursive_rules(
            Metric::Atoms,
            5,
            Lang::new(
                &["true", "false"],
                &["x", "y", "z"],
                &[&["~"], &["&", "|", "^", "->"]],
            ),
            Ruleset::default(),
        );
        let a5_duration = a5_start.elapsed();

        let prompt = "
        Your task is to perform term enumeration for rule inference.
        The domain is boolean logic, as follows:
        Values: true, false
        Unary Operators: ~
        Binary Operators: &, |, ^, ->

        Terms must be written using s-expressions and prefix notation.
        Variables are ?x, ?y, and ?z.

        Your task is to generate terms from the grammar, from which a set of rewrite rules can be inferred.
        The generated terms should adequately cover the set of all possible terms.
        The generated terms should vary in complexity and size so that they lead to interesting rewrite rules.
        Generate at least 1000 terms. Do not stop before you have generated 1000 terms.
        Your response should not contain `...` or another indicator that you have stopped before finishing term enumeration.
        Print only the terms, one term per line, with no additional text or explanation.
        ";

        let models = llm::models();
        for model in models {
            let start = Instant::now();
            let wkld = Workload::from_llm(&prompt, &model).await.as_lang::<Bool>();
            let rules: Ruleset<Bool> = run_workload(
                wkld,
                rules5.clone(),
                Limits::synthesis(),
                Limits::minimize(),
                true,
            );
            let (sound, unsound) = rules.partition(|r| r.is_valid());
            let duration = start.elapsed();

            println!(
                "{} synthesized {} sound and {} unsound rules",
                &model,
                sound.len(),
                unsound.len()
            );
            let all = rules.union(&rules5);

            logger::write_baseline(
                &all,
                &format!("{}-A5-TE", model),
                &enumo_rules,
                "enumo",
                duration + a5_duration,
            );
        }
    }

    #[test]
    fn round_trip_to_file() {
        let rules: Ruleset<Bool> = Ruleset::new(&[
            "(^ ?b ?a) ==> (^ ?a ?b)",
            "(& ?b ?a) ==> (& ?a ?b)",
            "(| ?b ?a) ==> (| ?a ?b)",
            "(& ?a ?a) ==> ?a",
            "?a ==> (~ (~ ?a))",
        ]);

        rules.to_file("out.txt");

        let read: Ruleset<Bool> = Ruleset::from_file("out.txt");

        assert_eq!(rules, read)
    }

    #[test]
    fn derive_rules() {
        let limits = Limits {
            iter: 4,
            node: 1000000,
            match_: 200_000,
        };
        let three: Ruleset<Bool> =
            run_workload(iter_bool(3), Ruleset::default(), limits, limits, false);
        three.to_file("three.txt");

        let four = run_workload(iter_bool(4), Ruleset::default(), limits, limits, false);
        four.to_file("four.txt");

        let (can, cannot) = three.derive(
            DeriveType::LhsAndRhs,
            &four,
            Limits {
                iter: 10,
                node: 1000000,
                match_: 1000,
            },
        );
        assert!(can.len() > 0);
        assert!(cannot.len() > 0);
    }
}
