use ruler::{
    enumo::{Filter, Metric, Ruleset, Workload},
    recipe_utils::{recursive_rules, run_workload, Lang},
    Limits,
};

use crate::Pred;

pub fn halide_rules() -> Ruleset<Pred> {
    let mut all_rules = Ruleset::default();
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
    println!("full finished.");
    all_rules.to_file("thru_full.rules");
    let nested_bops_arith = Workload::new(&["(bop e e)", "v"])
        .plug("e", &Workload::new(&["(bop v v)", "(uop v)", "v"]))
        .plug(
            "bop",
            &Workload::new(&["+", "-", "*", "<", "/", "max", "min"]),
        )
        .plug("uop", &Workload::new(&["-", "!"]))
        .plug("v", &Workload::new(&["a", "b", "c"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
    let new = run_workload(
        nested_bops_arith,
        all_rules.clone(),
        Limits::rulefinding(),
        true,
    );
    all_rules.extend(new);
    println!("nested_bops_arith finished.");

    let nested_bops_full = Workload::new(&["(bop e e)", "v"])
        .plug("e", &Workload::new(&["(bop v v)", "(uop v)", "v"]))
        .plug(
            "bop",
            &Workload::new(&["&&", "||", "!=", "<=", "==", "<", "/", "max", "min"]),
        )
        .plug("uop", &Workload::new(&["-", "!"]))
        .plug("v", &Workload::new(&["a", "b", "c"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
    let new = run_workload(
        nested_bops_full,
        all_rules.clone(),
        Limits::rulefinding(),
        true,
    );
    all_rules.extend(new.clone());
    println!("nested_bops_full finished.");
    all_rules.to_file("thru_nested_bops_full.rules");

    all_rules.to_file("all_rules.rules");
    all_rules
}
