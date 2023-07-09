/*!
64 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(64);

#[path = "./recipes/bv4_fancy.rs"]
pub mod bv4_fancy;

#[cfg(test)]
pub mod test {
    use std::time::{Duration, Instant};

    use crate::bv4_fancy::bv4_fancy_rules;

    use ruler::{
        enumo::{self, Filter, Ruleset, Workload},
        logger,
        recipe_utils::{base_lang, iter_metric, recursive_rules, run_workload, Lang},
        Limits,
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

        let a6_canon = iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 6)
            .plug("VAR", &Workload::new(lang.vars))
            .plug("VAL", &Workload::empty())
            .plug("OP1", &Workload::new(lang.ops[0].clone()))
            .plug("OP2", &Workload::new(lang.ops[1].clone()))
            .filter(Filter::Canon(vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
            ]));
        let consts = Workload::new(["0", "1"]);
        let wkld = Workload::Append(vec![a6_canon, consts]);
        rules.extend(run_workload(
            wkld,
            rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            true,
        ));

        let duration = start.elapsed();
        (rules, duration)
    }

    #[test]
    fn compare() {
        let domain = "BV64";
        // Port the bv4 rules into domain
        let actual_bv4_rules: Ruleset<_> = bv4_fancy_rules();
        let ported_bv4_rules: Ruleset<Bv> = Ruleset::new(actual_bv4_rules.to_str_vec());

        // Generate the rules directly
        let (gen, gen_time): (Ruleset<Bv>, Duration) = gen();

        logger::write_bv_derivability(domain, gen, gen_time, ported_bv4_rules)
    }
}
