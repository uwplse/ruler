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
    use ruler::enumo::Ruleset;
    use std::time::Instant;

    fn bv4_fancy_rules() -> Ruleset<Bv> {
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
        rules
    }    

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bv4_fancy_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<_>::from_file("baseline/bv4.rules");

        logger::write_output(&rules, &baseline, "bv4", "oopsla", duration, true);

        let start = Instant::now();
        let rules = bv4_rules();
        let duration = start.elapsed(); 

        logger::write_output(&rules, &baseline, "bv4_baseline", "oopsla", duration, true);
    }
}
