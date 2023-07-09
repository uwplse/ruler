/*!
128 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(128);

#[path = "./recipes/bv4_fancy.rs"]
pub mod bv4_fancy;

#[cfg(test)]
pub mod test {
    use std::time::{Duration, Instant};

    use crate::bv4_fancy::bv4_fancy_rules;

    use ruler::{
        enumo::{self, Ruleset},
        logger,
        recipe_utils::{recursive_rules, Lang},
    };

    use crate::Bv;

    fn gen() -> (Ruleset<Bv>, Duration) {
        let start = Instant::now();
        let mut rules: Ruleset<Bv> = Ruleset::default();
        let lang = Lang::new(
            &["0", "1"],
            &["a", "b", "c"],
            &[&["~", "-"], &["&", "|", "*", "--", "+", "<<", ">>"]],
        );
        rules.extend(recursive_rules(
            enumo::Metric::Atoms,
            5,
            lang.clone(),
            Ruleset::default(),
        ));

        // too slow for 128
        // let a6_canon = iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 6)
        //     .plug("VAR", &Workload::new(lang.vars))
        //     .plug("VAL", &Workload::empty())
        //     .plug("OP1", &Workload::new(lang.uops))
        //     .plug("OP2", &Workload::new(lang.bops))
        //     .filter(Filter::Canon(vec![
        //         "a".to_string(),
        //         "b".to_string(),
        //         "c".to_string(),
        //     ]));
        // let consts = Workload::new(["0", "1"]);
        // let wkld = Workload::Append(vec![a6_canon, consts]);
        // rules.extend(run_workload(
        //     wkld,
        //     rules.clone(),
        //     Limits::rulefinding(),
        //     true,
        // ));

        let duration = start.elapsed();
        (rules, duration)
    }

    #[test]
    fn compare() {
        let domain = "BV128";
        // Port the bv4 rules into domain
        let actual_bv4_rules: Ruleset<_> = bv4_fancy_rules();
        let ported_bv4_rules: Ruleset<Bv> = Ruleset::new(actual_bv4_rules.to_str_vec());

        // Generate the rules directly
        let (gen, gen_time): (Ruleset<Bv>, Duration) = gen();

        logger::write_bv_derivability(domain, gen, gen_time, ported_bv4_rules)
    }
}
