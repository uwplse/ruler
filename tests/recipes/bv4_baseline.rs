use ruler::{
    enumo::{Filter, Ruleset, Workload, Metric},
    recipe_utils::{iter_metric, recursive_rules, run_workload, Lang},
};

ruler::impl_bv!(4);

pub fn bv4_rules_baseline() -> Ruleset<Bv> {
    recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["0"],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+", "<<", ">>"],
            &[],
        ),
        Ruleset::default(),
        Limits::default(),
    )
}
