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
            &["+", "-", "*", "min", "max"],
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
                "&&", "||", "^", "+", "-", "*", "min", "max", "<", "<=", "==", "!=",
            ],
            &["select"],
        ),
        all_rules.clone(),
    );
    let nested_bops_arith = Workload::new(&["(bop e e)", "v"])
        .plug("e", &Workload::new(&["(bop v v)", "(uop v)", "v"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "<", "max", "min"]))
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
    let nested_bops_full = Workload::new(&["(bop e e)", "v"])
        .plug("e", &Workload::new(&["(bop v v)", "(uop v)", "v"]))
        .plug(
            "bop",
            &Workload::new(&["&&", "||", "!=", "<=", "==", "<", "max", "min"]),
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
    let select_max = Workload::new(&["(max s s)", "(min s s)", "(select v s s)"])
        .plug("s", &Workload::new(&["(select v v v)", "(bop v v)", "v"]))
        .plug("v", &Workload::new(&["a", "b", "c", "d", "e", "f"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "/", "min", "max"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
            "e".to_string(),
            "f".to_string(),
        ]));
    let new = run_workload(select_max, all_rules.clone(), Limits::rulefinding(), true);
    println!("select_max finished.");
    new.to_file("select-max.rules");
    all_rules.extend(new.clone());
    let select_arith = Workload::new(&["(select v e e)", "(bop v e)", "(bop e v)"])
        .plug("e", &Workload::new(&["(bop v v)", "(select v v v)", "v"]))
        .plug(
            "bop",
            &Workload::new(&["+", "-", "*", "<", "/"]),
        )
        .plug("v", &Workload::new(&["a", "b", "c", "d", "e", "f"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
            "e".to_string(),
            "f".to_string(),
        ]));
    let new = run_workload(select_arith, all_rules.clone(), Limits::rulefinding(), true);
    println!("select_arith finished.");
    new.to_file("select-arith.rules");
    all_rules.extend(new.clone());
    all_rules
}
