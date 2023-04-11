/*!
4 bit implementation of Bitvectors.
!*/

#[path = "./recipes/bv4.rs"]
pub mod bv4;

ruler::impl_bv!(4);

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::bv4::bv4_rules;
    use ruler::{
        enumo::{Filter, Ruleset, Workload},
        recipe_utils::{iter_metric, recursive_rules, run_workload, Lang},
    };
    use std::time::Instant;

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bv4_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<_>::from_file("baseline/bv4.rules");

        logger::write_output(&rules, &baseline, "bv4", "oopsla", duration);
    }

    #[test]
    fn gen() {
        let start = Instant::now();
        let mut rules: Ruleset<Bv> = Ruleset::default();
        let lang = Lang::new(
            &["0", "1"],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+", "<<", ">>"],
            &[],
        );
        rules.extend(recursive_rules(
            enumo::Metric::Atoms,
            5,
            lang.clone(),
            Ruleset::default(),
        ));

        let base_lang = Workload::new(["VAR", "CONST", "(UOP EXPR)", "(BOP EXPR EXPR)"]);

        let a6_canon = iter_metric(base_lang, "EXPR", enumo::Metric::Atoms, 6)
            .plug("VAR", &Workload::new(lang.vars))
            .plug("CONST", &Workload::empty())
            .plug("UOP", &Workload::new(lang.uops))
            .plug("BOP", &Workload::new(lang.bops))
            .filter(Filter::Canon(vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
            ]));

        rules.extend(run_workload(
            a6_canon,
            rules.clone(),
            Limits::rulefinding(),
            true,
        ));
        rules.to_file("bv4.rules_");

        let duration = start.elapsed();
        println!("duration: {}", duration.as_millis());
    }

    #[test]
    fn read_bv4() {
        let bv4_rules: Ruleset<Bv> = Ruleset::from_file("bv4.rules_");
        bv4_rules.pretty_print();
        let (sound, unsound) = bv4_rules.partition(|rule| rule.is_valid());
        println!("{} {}", sound.len(), unsound.len());
    }
}
