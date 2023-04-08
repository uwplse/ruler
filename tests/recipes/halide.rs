use ruler::{
    enumo::{Metric, Ruleset, Filter, Workload},
    recipe_utils::{recursive_rules, run_workload, Lang},
    Limits
};

use crate::Pred;

pub fn halide_rules() -> Ruleset<Pred> {
    // This is porting the halide recipe at incremental/halide.spec
    // on the branch "maybe-useful" in the old recipes repo
    let mut all_rules = Ruleset::default();
    let limits = Limits {
        iter: 3,
        node: 100_000,
    };

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
        limits
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
        limits
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
        limits
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
        limits
    );
    all_rules.extend(full);
    all_rules.to_file("thru-full.rules");
    println!("full complete.");
    let nested_bops = Workload::new(&["(bop e e)", "v", "0", "1"])
        .plug("e", &Workload::new(&["(bop v v)", "v"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "/", "max", "min"]))
        .plug("v", &Workload::new(&["a", "b", "c", "d"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
        ]));
    let new = run_workload(
        nested_bops,
        all_rules.clone(),
        Limits::rulefinding(),
        true
    );
    all_rules.extend(new.clone());
    println!("nested_bops finished.");
    new.to_file("nested-bops.rules");
    let triple_nested_bops = Workload::new(&[
        "(bop e e)",
        "(bop (bop (bop v v) v) v)",
        "(bop v (bop v (bop v v)))",
        "0",
        "1",
    ])
    .plug("e", &Workload::new(&["(bop v v)", "v"]))
    .plug("bop", &Workload::new(&["+", "-", "*", "/", "max", "min"]))
    .plug("v", &Workload::new(&["a", "b", "c", "d"]))
    .filter(Filter::Canon(vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
        "d".to_string(),
    ]));
    let new = run_workload(
        triple_nested_bops,
        all_rules.clone(),
        Limits::rulefinding(),
        true
    );
    println!("triple_nested_bops finished.");
    new.to_file("triple-nested-bops.rules");
    all_rules.extend(new.clone());
    let select_arith = Workload::new(&["(select v e e)", "(bop v e)", "(bop e v)"])
        .plug("e", &Workload::new(&["(bop v v)", "(select v v v)", "v"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "<", "/", "max", "min"]))
        .plug(
            "v",
            &Workload::new(&["a", "b", "c", "d", "e", "f", "0", "1"]),
        )
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
            "e".to_string(),
            "f".to_string(),
            "0".to_string(),
            "1".to_string(),
        ]));
    let new = run_workload(
        select_arith,
        all_rules.clone(),
        Limits::rulefinding(),
        true
    );
    println!("select_arith finished.");
    new.to_file("select-arith.rules");
    all_rules.extend(new.clone());
    let select_max = Workload::new(&["(max s s)", "(min s s)", "(select v s s)"])
        .plug("s", &Workload::new(&["(select v v v)", "(bop v v)", "v"]))
        .plug(
            "v",
            &Workload::new(&["a", "b", "c", "d", "e", "f", "0", "1"]),
        )
        .plug("bop", &Workload::new(&["+", "-", "*", "/", "min", "max"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
            "e".to_string(),
            "f".to_string(),
            "0".to_string(),
            "1".to_string(),
        ]));
    let new = run_workload(
        select_max,
        all_rules.clone(),
        Limits::rulefinding(),
        true
    );
    println!("select_max finished.");
    new.to_file("select-max.rules");
    all_rules.extend(new.clone());
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
    let new = run_workload(
        nested_bops_full,
        all_rules.clone(),
        Limits::rulefinding(),
        true
    );
    all_rules.extend(new.clone());
    println!("nested_bops_full finished.");
    new.to_file("nested_bops_full.rules");
    all_rules.to_file("all-rules.rules");
    all_rules
}
