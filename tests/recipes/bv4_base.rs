use ruler::{
    enumo::{Metric, Ruleset},
    recipe_utils::{recursive_rules, Lang},
};

ruler::impl_bv!(4);

pub fn bv4_rules() -> Ruleset<Bv> {
    recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["0", "1"],
            &["a", "b", "c"],
            &[&["~", "-"], &["&", "|", "*", "--", "+", "<<", ">>"]],
        ),
        Ruleset::default(),
    )
}
