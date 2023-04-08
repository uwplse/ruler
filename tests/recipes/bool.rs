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
            &["~"],
            &["&", "|", "^"],
            &[],
        ),
        Ruleset::default(),
    );
    all.extend(r5);

    let a7_canon = iter_metric(
        base_lang().filter(Filter::Invert(Box::new(Filter::Contains(
            "TOP".parse().unwrap(),
        )))),
        "EXPR",
        Metric::Atoms,
        7,
    )
    .plug("CONST", &Workload::empty())
    .plug("VAR", &Workload::new(["a", "b", "c"]))
    .plug("UOP", &Workload::new(["~"]))
    .plug("BOP", &Workload::new(["&", "|", "^"]))
    .filter(Filter::Canon(vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
    ]));
    let r7 = run_workload(a7_canon, all.clone(), Limits::rulefinding(), true);
    all.extend(r7);

    all
}
