use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::{base_lang, iter_metric, recursive_rules, run_workload, Lang},
};

ruler::impl_bv!(4);

pub fn bv4_fancy_rules() -> Ruleset<Bv> {
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

    rules
}
