use ruler::{
    enumo::{Metric, Ruleset, Filter, Workload},
    recipe_utils::{recursive_rules, Lang, run_workload},
    Limits
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
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &["-"],
            &["+", "-", "*", "/", "min", "max"],
            &[],
        ),
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
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &["-", "!"],
            &[
                "&&", "||", "^", "+", "-", "*", "/", "min", "max", "<", "<=", "==", "!=",
            ],
            &["select"],
        ),
        all_rules.clone(),
    );
    all_rules.extend(full);
    let nested_bops_full = Workload::new(&["(bop e e)", "v", "0", "1"])
        .plug("e", &Workload::new(&["(bop v v)", "(uop v)", "v"]))
        .plug(
            "bop",
            &Workload::new(&["&&", "||", "!=", "<=", "==", "max", "min"]),
        )
        .plug("uop", &Workload::new(&["-", "!"]))
        .plug("v", &Workload::new(&["a", "b", "c"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
    println!("Writing nested-bops-full workload...");
    nested_bops_full.to_file("nested-bops-full.terms");
    let new = run_workload(
        nested_bops_full,
        all_rules.clone(),
        Limits::rulefinding(),
        true,
    );
    all_rules.extend(new.clone());
    println!("nested_bops_full finished.");
    new.to_file("nested_bops_full.rules");
    all_rules.to_file("all-rules.rules");
    all_rules
}
