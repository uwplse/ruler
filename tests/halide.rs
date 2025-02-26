#[cfg(test)]
#[path = "./recipes/halide.rs"]
mod halide;

#[allow(unused_imports)]
mod test {
    use crate::halide::{
        halide_rules, halide_rules_for_caviar_conditional, halide_rules_for_caviar_total_only,
    };
    use ruler::halide::{egg_to_z3, Pred};
    use std::{
        sync::Arc,
        time::{Duration, Instant},
    };

    use egg::{AstSize, Extractor, Pattern, RecExpr};
    use ruler::{
        enumo::{Filter, Metric, Rule, Ruleset, Workload},
        logger,
        recipe_utils::{recursive_rules, run_workload, Lang},
        Limits,
    };
    use ruler::{SynthAnalysis, SynthLanguage};
    use z3::ast::Ast;

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        // let all_rules = halide_rules();
        let all_rules = halide_rules_for_caviar_conditional();
        println!("done");
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
