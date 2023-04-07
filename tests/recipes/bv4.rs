use super::*;
use ruler::{
    enumo::{Metric, Ruleset},
    recipe_utils::{recursive_rules, Lang},
};

pub fn bv4_rules() -> Ruleset<Bv> {
    recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &[],
            &["a", "b", "c", "0", "1"],
            &["~", "-"],
            &["&", "|", "*", "--", "+", "<<", ">>"],
            &[],
        ),
        Ruleset::default(),
    )
}
