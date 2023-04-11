/*!
128 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(128);

#[cfg(test)]
pub mod test {
    use std::time::{Duration, Instant};

    use ruler::{
        enumo::{self, Filter, Ruleset, Workload},
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

    fn from_bv4() -> (Ruleset<Bv>, Duration) {
        let bv4_rules: Ruleset<Bv> = Ruleset::from_file("bv4.rules_");
        let start = Instant::now();
        let (sound, _) = bv4_rules.partition(|rule| rule.is_valid());
        (sound, start.elapsed())
    }

    #[test]
    fn compare() {
        let domain = "BV128";
        // Generate the rules directly
        let (gen, gen_time): (Ruleset<Bv>, Duration) = gen();
        gen.to_file(&format!("gen-{}.rules_", domain));

        // Validate bv4 rules
        let (sound_bv4, sound_bv4_time) = from_bv4();
        sound_bv4.to_file(&format!("sound-bv4-{}.rules_", domain));

        println!("Summary for {}", domain);
        println!(
            "Generated {} rules for {} in {}ms",
            gen.len(),
            domain,
            gen_time.as_secs_f32()
        );
        println!(
            "Validating bv4 rules as {} rules resulted in {} sound rules in {}ms",
            domain,
            sound_bv4.len(),
            sound_bv4_time.as_secs_f32()
        );
    }
}
