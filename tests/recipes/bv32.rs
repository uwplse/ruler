use super::*;
use ruler::{
    enumo::{Metric, Ruleset},
    recipe_utils::{recursive_rules, Lang},
};

pub fn bv32_rules() -> Ruleset<Bv> {
    recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["0"],
            &["a", "b", "c"],
            &[&["~", "-"], &["&", "|", "*", "--", "+", "<<", ">>"]],
        ),
        Ruleset::default(),
    )
}
