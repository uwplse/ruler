use ruler::{
    enumo::{Filter, Metric, Ruleset, Workload},
    Limits,
};

use crate::Pred;

fn recursive_rules(
    metric: Metric,
    n: usize,
    consts: &Workload,
    vars: &Workload,
    uops: &Workload,
    bops: &Workload,
    tops: &Workload,
    prior: Ruleset<Pred>,
) -> Ruleset<Pred> {
    let lang = Workload::new(&[
        "const",
        "var",
        "(uop expr)",
        "(bop expr expr)",
        "(top expr expr expr)",
    ]);
    if n < 1 {
        Ruleset::default()
    } else {
        let mut rec = recursive_rules(metric, n - 1, consts, vars, uops, bops, tops, prior.clone());
        let wkld = lang
            .iter_metric("expr", metric, n)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug("var", vars)
            .plug("const", consts)
            .plug("uop", uops)
            .plug("bop", bops)
            .plug("top", tops);
        rec.extend(prior);
        let new = Pred::run_workload(wkld, rec.clone(), Limits {
            iter: 3,
            node: 1_000_000,
        });
        let mut all = new;
        all.extend(rec);
        all
    }
}

pub fn halide_rules() -> Ruleset<Pred> {
    // This is porting the halide recipe at incremental/halide.spec
    // on the branch "maybe-useful" in the old recipes repo
    // let thru_pred: Ruleset<Pred> = Ruleset::from_file("pred-rules.rules");
    let mut all_rules = Ruleset::default();
   // all_rules.extend(thru_pred);
/*
    // Bool rules up to size 5:
    let bool_only = recursive_rules(
        Metric::Atoms,
        5,
        &Workload::new(&["0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&["!"]),
        &Workload::new(&["&&", "||", "^"]),
        &Workload::Set(vec![]),
        all_rules.clone(),
    );
    all_rules.extend(bool_only);
    all_rules.to_file("halide-bool-rules.rules");
    println!("bool finished.");

    // Rational rules up to size 6:
    let rat_only = recursive_rules(
        Metric::Atoms,
        6,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&["-"]),
        &Workload::new(&["+", "-", "*", "min", "max"]), // No div for now
        &Workload::Set(vec![]),
        all_rules.clone(),
    );
    all_rules.extend(rat_only);
    all_rules.to_file("halide-rat-rules.rules");
    println!("rat finished.");

    // Pred rules up to size 6:
    let pred_only = recursive_rules(
        Metric::Atoms,
        6,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&[""]),
        &Workload::new(&["<", "<=", "==", "!="]),
        &Workload::new(&["select"]),
        all_rules.clone(),
    );
    all_rules.extend(pred_only);
    all_rules.to_file("halide-pred-rules.rules");
    println!("pred finished.");
    // All terms up to size 4
    let full = recursive_rules(
        Metric::Atoms,
        5,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&["-", "!"]),
        &Workload::new(&[
            "&&", "||", "^", "+", "-", "*", "min", "max", "<", "<=", "==", "!=",
        ]),
        &Workload::new(&["select"]),
        all_rules.clone(),
    );
    all_rules.extend(full);
    all_rules.to_file("full-rules.rules");
    println!("full finished.");
    
    let triple_nested_bops_full = Workload::new(&[
        "(bop (bop (bop v v) v) v)",
        "(bop v (bop (bop v v) v))",
        "v",
        "0",
        "1"
    ])
    .plug("bop", &Workload::new(&["&&", "||", "!=", "<=", "=="]))
    .plug("v", &Workload::new(&["a", "b", "c"]))
    .filter(Filter::Canon(vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string()
    ]));
    let new = Pred::run_workload(
        triple_nested_bops_full,
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 1_000_000,
        }
    );
    all_rules.extend(new.clone());
    println!("triple_nested_bops_full finished.");
    new.to_file("triple_nested_bops_full.rules");

    let double_nested_bops_full = Workload::new(&[
        "(bop e e)",
        "v",
        "0",
        "1"
    ])
    .plug("e", &Workload::new(&[
        "(bop v v)",
        "(uop v)",
        "v"
    ]))
    .plug("bop", &Workload::new(&["&&", "||", "!=", "<=", "=="]))
    .plug("uop", &Workload::new(&["-", "!"]))
    .plug("v", &Workload::new(&["a", "b", "c"]))
    .filter(Filter::Canon(vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
    ]));
    let new = Pred::run_workload(
        double_nested_bops_full,
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 1_000_000,
        }
    );
    all_rules.extend(new.clone());
    println!("double_nested_bops_full finished.");
    new.to_file("double_nested_bops_full.rules");

    let nested_bops = Workload::new(&[
        "(bop e e)",
        "v"
    ])
    .plug("e", &Workload::new(&[
        "(bop v v)",
        "v"
    ]))
    .plug("bop", &Workload::new(&["+", "-", "*", "/", "<=", "max", "min"]))
    .plug("v", &Workload::new(&["a", "b", "c"]))
    .filter(Filter::Canon(vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
    ]));
    let new = Pred::run_workload(
        nested_bops,
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 1_000_000,
        }
    );
    all_rules.extend(new.clone());
    println!("nested_bops finished.");
    new.to_file("nested-bops.rules");

    let select_max = Workload::new(&[
        "(max s s)", 
        "(min s s)",
        "(select v m m)"
        ])
        .plug("s", &Workload::new(&["(select v v v)", "(bop v v)", "v"]))
        .plug("m", &Workload::new(&["(max v v)", "(min v v)", "v"]))
        .plug("v", &Workload::new(&["a", "b", "c"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "/", "<=", "min", "max"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));

    let new = Pred::run_workload(
        select_max,
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 1_000_000,
        }
    );
    all_rules.extend(new.clone());
    println!("select_max finished.");
    new.to_file("select-max.rules");

    let select_arith = Workload::new(&[
        "(select v e e)",
        "(bop (select v v v) (select v v v))",
        "(bop v (select v v v))",
        "(bop (select v v v) v)"])
        .plug("e", &Workload::new(&["(bop v v)", "(select v v v)", "v"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "/", "<="]))
        .plug("v", &Workload::new(&["a", "b", "c"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
    let new = Pred::run_workload(
        select_arith,
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 1_000_000,
        }
    );
    println!("select_arith finished.");
    all_rules.extend(new.clone());
    new.to_file("select-arith.rules");
    all_rules.to_file("all-rules.rules");
*/
    all_rules
}
