/*!
8 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(8);

#[path = "./recipes/bv4_fancy.rs"]
pub mod bv4_fancy;

#[cfg(test)]
pub mod test {
    use std::time::{Duration, Instant};

    use crate::bv4_fancy::bv4_fancy_rules;

    use ruler::{
        enumo::{self, Filter, Ruleset, Workload},
        logger,
        recipe_utils::{iter_metric, recursive_rules, run_workload, Lang},
        Limits,
    };

    use crate::Bv;

    fn gen() -> (Ruleset<Bv>, Duration) {
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

        let duration = start.elapsed();
        (rules, duration)
    }

    #[test]
    fn compare() {
        let domain = "BV8";
        // Port the bv4 rules into domain
        let actual_bv4_rules: Ruleset<_> = bv4_fancy_rules();
        let ported_bv4_rules: Ruleset<Bv> = Ruleset::new(actual_bv4_rules.to_str_vec());

        // Generate the rules directly
        let (gen, gen_time): (Ruleset<Bv>, Duration) = gen();

        logger::write_bv_derivability(domain, gen, gen_time, ported_bv4_rules)
    }
}
