use ruler::{
    enumo::{Metric, Ruleset},
    recipe_utils::{recursive_rules, Lang},
};

use crate::Pred;

pub fn halide_rules() -> Ruleset<Pred> {
    // This is porting the halide recipe at incremental/halide.spec
    // on the branch "maybe-useful" in the old recipes repo
    let mut all_rules = Ruleset::default();

    // Bool rules up to size 5:
    let bool_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["0", "1"],
            &["a", "b", "c"],
            &["!"],
            &["&&", "||", "^"],
            &[],
        ),
        all_rules.clone(),
    );
    all_rules.extend(bool_only);

    // Rational rules up to size 5:
    let rat_only = recursive_rules(
        Metric::Atoms,
        5,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&["-"]),
        &Workload::new(&["+", "-", "*", "/", "min", "max"]),
        &Workload::Set(vec![]),
        all_rules.clone(),
    );
    all_rules.extend(rat_only);

    // Pred rules up to size 5
    let pred_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &["-"],
            &["<", "<=", "==", "!="],
            &["select"],
        ),
        all_rules.clone(),
    );
    all_rules.extend(pred_only);

    // All terms up to size 4
    let full = recursive_rules(
        Metric::Atoms,
        4,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&["-", "!"]),
        &Workload::new(&[
            "&&", "||", "^", "+", "-", "*", "/", "min", "max", "<", "<=", "==", "!=",
        ]),
        &Workload::new(&["select"]),
        all_rules.clone(),
    );
    all_rules.extend(full);
    all_rules
}
