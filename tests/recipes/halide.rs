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

    // let full: Ruleset<Pred> = Ruleset::from_file("thru_full.rules");
    // all_rules.extend(full);
    // let mut full_ruleset = Ruleset::default();
    // full_ruleset.extend(all_rules.clone());
    
    println!("full finished.");
    all_rules.to_file("thru_full.rules");
/*
    let select_arith = Workload::new(&["(select v e e)", "(bop e e)", "v"])
        .plug("e", &Workload::new(&["(bop v v)", "v"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "<", "max", "min"]))
        .plug("v", &Workload::new(&["a", "b", "c", "d"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
        ]));
    let new = run_workload(
        select_arith,
        all_rules.clone(),
        Limits::rulefinding(),
        true,
    );
    all_rules.extend(new.clone());
*/

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
    println!("nested_bops_arith finished.");

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
    println!("nested_bops_full finished.");
    all_rules.to_file("thru_nested_bops_full.rules");

    all_rules.to_file("all_rules.rules");
    all_rules
}
