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
        let new = Pred::run_workload(wkld, rec.clone(), Limits::default());
        let mut all = new;
        all.extend(rec);
        all
    }
}

pub fn halide_rules() -> Ruleset<Pred> {
    // This is porting the halide recipe at incremental/halide.spec
    // on the branch "maybe-useful" in the old recipes repo
    let thru_pred: Ruleset<Pred> = Ruleset::from_file("full-rules.rules");
    let mut all_rules = Ruleset::default();
    all_rules.extend(thru_pred);
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

            // Rational rules up to size 5:
            let rat_only = recursive_rules(
                Metric::Atoms,
                5,
                &Workload::new(&["-1", "0", "1"]),
                &Workload::new(&["a", "b", "c"]),
                &Workload::new(&["-"]),
                &Workload::new(&["+", "-", "*", "min", "max"]), // No div for now
                &Workload::Set(vec![]),
                all_rules.clone(),
            );
            all_rules.extend(rat_only);
            all_rules.to_file("halide-rat-rules.rules");

            // Pred rules up to size 5
            let pred_only = recursive_rules(
                Metric::Atoms,
                5,
                &Workload::new(&["-1", "0", "1"]),
                &Workload::new(&["a", "b", "c"]),
                &Workload::new(&[""]),
                &Workload::new(&["<", "<=", "==", "!="]),
                &Workload::new(&["select"]),
                all_rules.clone(),
            );
            all_rules.extend(pred_only);
            all_rules.to_file("halide-pred-rules.rules");

            // All terms up to size 4
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
            );
            all_rules.extend(full);
            all_rules.to_file("full-rules.rules");
    */
    let select_max = Workload::new(&["(max s s)", "(min s s)"])
        .plug("s", &Workload::new(&["(select v v v)", "(bop v v)", "v"]))
        .plug("v", &Workload::new(&["a", "b", "c"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "/", "<="]))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));

    let new = Pred::run_workload(
        select_max,
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 200000,
        },
    );
    all_rules.extend(new);
    println!("select_max finished.");
    all_rules.to_file("select-max.rules");

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
            iter: 2,
            node: 200000,
        },
    );
    println!("select_arith finished.");
    all_rules.extend(new);
    all_rules.to_file("select-arith.rules");
    all_rules
}
