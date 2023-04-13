use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::{iter_metric, recursive_rules, run_workload, Lang},
};

ruler::impl_bv!(4);

pub fn bv4_rules() -> Ruleset<Bv> {
    recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["0", "1"],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+", "<<", ">>"],
            &[],
        ),
        Ruleset::default(),
    )
}
