use super::*;
use ruler::{
    enumo::{Metric, Ruleset},
    recipe_utils::{recursive_rules, Lang},
    Limits,
};

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
        Limits::rulefinding(),
    )
}
