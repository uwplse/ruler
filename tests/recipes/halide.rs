use ruler::{
    enumo::{Metric, Ruleset},
    halide::Pred,
    recipe_utils::{recursive_rules, Lang},
};

#[allow(dead_code)]
pub fn halide_rules_for_caviar_total_only() -> Ruleset<Pred> {
    let mut all_rules = Ruleset::default();
    let bool_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(&["0", "1"], &["a", "b", "c"], &[&["!"], &["&&", "||"]]),
        all_rules.clone(),
    );
    all_rules.extend(bool_only);
    let rat_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[&[], &["+", "-", "*", "min", "max"]],
        ),
        all_rules.clone(),
    );
    all_rules.extend(rat_only.clone());
    let pred_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[&[], &["<", "<=", "==", "!="], &[]],
        ),
        all_rules.clone(),
    );
    all_rules.extend(pred_only);

    let full = recursive_rules(
        Metric::Atoms,
        4,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[
                &["!"],
                &[
                    "&&", "||", "+", "-", "*", "/", "%", "min", "max", "<", "<=", "==", "!=",
                ],
                &[],
            ],
        ),
        all_rules.clone(),
    );
    all_rules.extend(full);

    all_rules
}
