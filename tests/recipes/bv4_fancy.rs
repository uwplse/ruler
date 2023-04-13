use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::{iter_metric, recursive_rules, run_workload, Lang},
};

ruler::impl_bv!(4);

pub fn bv4_fancy_rules() -> Ruleset<Bv> {
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
