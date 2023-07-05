use super::*;
use ruler::{
    enumo::{Filter, Metric, Ruleset, Workload},
    recipe_utils::{base_lang, iter_metric, recursive_rules, run_workload, Lang},
};

pub fn bool_rules() -> Ruleset<Bool> {
    let mut all = Ruleset::default();
    let r5 = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["true", "false"],
            &["a", "b", "c"],
            &[&["~"], &["&", "|", "^"]],
        ),
        Ruleset::default(),
    );
    all.extend(r5);

    let a7_canon = iter_metric(base_lang(2), "EXPR", Metric::Atoms, 7)
        .plug("VAL", &Workload::empty())
        .plug("VAR", &Workload::new(["a", "b", "c"]))
        .plug("OP1", &Workload::new(["~"]))
        .plug("OP2", &Workload::new(["&", "|", "^"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
    let r7 = run_workload(
        a7_canon,
        all.clone(),
        Limits::synthesis(),
        Limits::minimize(),
        true,
    );
    all.extend(r7);

    all
}
