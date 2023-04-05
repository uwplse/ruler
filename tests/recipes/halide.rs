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
    limits: Limits,
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
        let mut rec = recursive_rules(
            metric,
            n - 1,
            consts,
            vars,
            uops,
            bops,
            tops,
            prior.clone(),
            limits,
        );
        let wkld = lang
            .iter_metric("expr", metric, n)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug("var", vars)
            .plug("const", consts)
            .plug("uop", uops)
            .plug("bop", bops)
            .plug("top", tops);
        rec.extend(prior);
        let new = Pred::run_workload(wkld, rec.clone(), limits);
        let mut all = new;
        all.extend(rec);
        all
    }
}

pub fn halide_rules() -> Ruleset<Pred> {
    let mut all_rules = Ruleset::default();
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
        Limits {
            iter: 3,
            node: 100_000,
        },
    );
    all_rules.extend(bool_only);
    // Rational rules up to size 6:
    let rat_only = recursive_rules(
        Metric::Atoms,
        5,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&["-"]),
        &Workload::new(&["+", "-", "*", "min", "max"]), // No div for now
        &Workload::Set(vec![]),
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 100_000,
        },
    );
    all_rules.extend(rat_only);
    // Pred rules up to size 6:
    let pred_only = recursive_rules(
        Metric::Atoms,
        5,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&[""]),
        &Workload::new(&["<", "<=", "==", "!="]),
        &Workload::new(&["select"]),
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 100_000,
        },
    );
    all_rules.extend(pred_only);
    // All terms up to size 5
    let full = recursive_rules(
        Metric::Atoms,
        4,
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&["-", "!"]),
        &Workload::new(&[
            "&&", "||", "^", "+", "-", "*", "min", "max", "<", "<=", "==", "!=",
        ]),
        &Workload::new(&["select"]),
        all_rules.clone(),
        Limits {
            iter: 3,
            node: 100_000,
        },
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
    let new = Pred::run_workload(
        nested_bops_full,
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 300_000,
        },
    );
    all_rules.extend(new.clone());
    println!("nested_bops_full finished.");
    new.to_file("nested_bops_full.rules");

    let nested_bops = Workload::new(&["(bop e e)", "v", "0", "1"])
        .plug("e", &Workload::new(&["(bop v v)", "v"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "max", "min"]))
        .plug("v", &Workload::new(&["a", "b", "c", "d"]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
        ]));
    let new = Pred::run_workload(
        nested_bops,
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 300_000,
        },
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
    .plug("bop", &Workload::new(&["+", "-", "*", "max", "min"]))
    .plug("v", &Workload::new(&["a", "b", "c", "d"]))
    .filter(Filter::Canon(vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
        "d".to_string(),
    ]));
    let new = Pred::run_workload(
        triple_nested_bops,
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 300_000,
        },
    );
    all_rules.extend(new.clone());
    let select_max = Workload::new(&["(max s s)", "(min s s)", "(select v s s)"])
        .plug("s", &Workload::new(&["(select v v v)", "(bop v v)", "v"]))
        .plug(
            "v",
            &Workload::new(&["a", "b", "c", "d", "e", "f", "0", "1"]),
        )
        .plug("bop", &Workload::new(&["+", "-", "*", "min", "max"]))
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
    let new = Pred::run_workload(
        select_max,
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 300_000,
        },
    );
    all_rules.extend(new.clone());
    let select_arith = Workload::new(&["(select v e e)", "(bop v e)", "(bop e v)"])
        .plug("e", &Workload::new(&["(bop v v)", "(select v v v)", "v"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "<", "max", "min"]))
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
    let new = Pred::run_workload(
        select_arith,
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 300_000,
        },
    );
    all_rules.extend(new.clone());
    all_rules
}
