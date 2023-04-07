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
        let new = Pred::run_workload(wkld, rec.clone(), Limits::rulefinding());
        let mut all = new;
        all.extend(rec);
        all
    }
}

pub fn halide_rules() -> Ruleset<Pred> {
    // This is porting the halide recipe at incremental/halide.spec
    // on the branch "maybe-useful" in the old recipes repo
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
        &Workload::new(&["-1", "0", "1"]),
        &Workload::new(&["a", "b", "c"]),
        &Workload::new(&[""]),
        &Workload::new(&["<", "<=", "==", "!="]),
        &Workload::new(&["select"]),
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
