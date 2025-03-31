use egg::{EGraph, Pattern, RecExpr, Rewrite};
use ruler::halide::Pred;
use ruler::{
    enumo::{Filter, Metric, Rule, Ruleset, Workload},
    halide::{compute_conditional_structures, Pred},
    llm::Recipe,
    recipe_utils::{
        base_lang, iter_metric, recursive_rules, recursive_rules_cond, run_workload, Lang,
    },
    HashMap, Limits, SynthAnalysis, SynthLanguage,
};

pub fn halide_rules_for_caviar_conditional() -> Ruleset<Pred> {
    let cond_lang = Lang::new(&["0"], &["a", "b", "c"], &[&[], &["<", "<=", "!="]]);

    let base_lang = if cond_lang.ops.len() == 3 {
        base_lang(3)
    } else {
        base_lang(2)
    };

    let mut wkld = iter_metric(base_lang, "EXPR", Metric::Atoms, 3)
        .filter(Filter::Contains("VAR".parse().unwrap()))
        .plug("VAR", &Workload::new(base_lang.vars))
        .plug("VAL", &Workload::new(base_lang.vals));
    // let ops = vec![lang.uops, lang.bops, lang.tops];
    for (i, ops) in lang.ops.iter().enumerate() {
        wkld = wkld.plug(format!("OP{}", i + 1), &Workload::new(ops));
    }

    let (pvec_to_terms, cond_prop_ruleset) = compute_conditional_structures(&wkld);
    let mut all_rules = Ruleset::default();

    let equality = recursive_rules_cond(
        Metric::Atoms,
        5,
        Lang::new(
            &[],
            &["a", "b", "c"],
            &[&["!"], &["==", "!=", "<", ">", ">=", "<="]],
        ),
        all_rules.clone(),
        &pvec_to_terms,
        &cond_prop_ruleset,
    );

    all_rules.extend(equality);

    let bool_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(&[], &["a", "b", "c"], &[&["!"], &["&&", "||"]]),
        all_rules.clone(),
    );

    all_rules.extend(bool_only);

    let rat_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(&[], &["a", "b", "c"], &[&[], &["+", "-", "*"]]),
        all_rules.clone(),
    );

    all_rules.extend(rat_only);

    let div_only = recursive_rules_cond(
        Metric::Atoms,
        5,
        Lang::new(&[], &["a", "b", "c"], &[&[], &["/", "%"]]),
        all_rules.clone(),
        &pvec_to_terms,
        &cond_prop_ruleset,
    );

    all_rules.extend(div_only);

    let min_plus = recursive_rules_cond(
        Metric::Atoms,
        7,
        Lang::new(&[], &["a", "b", "c"], &[&[], &["+", "min"]]),
        all_rules.clone(),
        &pvec_to_terms,
        &cond_prop_ruleset,
    );

    all_rules.extend(min_plus);

    let max_plus = recursive_rules_cond(
        Metric::Atoms,
        7,
        Lang::new(&[], &["a", "b", "c"], &[&[], &["+", "max"]]),
        all_rules.clone(),
        &pvec_to_terms,
        &cond_prop_ruleset,
    );

    all_rules.extend(max_plus);

    all_rules
}

#[allow(dead_code)]
pub fn halide_rules_for_caviar_total_only() -> Ruleset<Pred> {
    let mut all_rules = Ruleset::default();
    let bool_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(&["0", "1"], &["a", "b", "c"], &[&["!"], &["&&", "||"]]),
        all_rules.clone(),
    );
    all_rules.extend(bool_only);
    let rat_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[&[], &["+", "-", "*", "min", "max"]],
        ),
        all_rules.clone(),
    );
    all_rules.extend(rat_only.clone());
    let pred_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[&[], &["<", "<=", "==", "!="], &[]],
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
            &[
                &["!"],
                &[
                    "&&", "||", "+", "-", "*", "/", "%", "min", "max", "<", "<=", "==", "!=",
                ],
                &[],
            ],
        ),
        all_rules.clone(),
    );
    all_rules.extend(full);

    all_rules
}

#[allow(dead_code)]
pub fn halide_rules() -> Ruleset<Pred> {
    let cond_lang = Lang::new(&["0"], &["a", "b", "c"], &[&[], &["<", "<=", "!="]]);

    let mut wkld = iter_metric(cond_lang, "EXPR", Metric::Atoms, 3)
        .filter(Filter::Contains("VAR".parse().unwrap()))
        .plug("VAR", &Workload::new(lang.vars))
        .plug("VAL", &Workload::new(lang.vals));
    // let ops = vec![lang.uops, lang.bops, lang.tops];
    for (i, ops) in lang.ops.iter().enumerate() {
        wkld = wkld.plug(format!("OP{}", i + 1), &Workload::new(ops));
    }

    let (pvec_to_terms, cond_prop_ruleset) = compute_conditional_structures(&wkld);

    let mut all_rules = Ruleset::default();
    let bool_only = recursive_rules(
        Metric::Atoms,
        5,
        Lang::new(&["0", "1"], &["a", "b", "c"], &[&["!"], &["&&", "||", "^"]]),
        all_rules.clone(),
    );
    all_rules.extend(bool_only);
    let rat_only = recursive_rules_cond(
        Metric::Atoms,
        5,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[&["-"], &["+", "-", "*", "min", "max"]],
        ),
        all_rules.clone(),
        &pvec_to_terms,
        &cond_prop_ruleset,
    );
    all_rules.extend(rat_only.clone());
    let pred_only = recursive_rules_cond(
        Metric::Atoms,
        5,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[&["-"], &["<", "<=", "==", "!="], &["select"]],
        ),
        all_rules.clone(),
        &pvec_to_terms,
        &cond_prop_ruleset,
    );
    all_rules.extend(pred_only);

    let full = recursive_rules_cond(
        Metric::Atoms,
        4,
        Lang::new(
            &["-1", "0", "1"],
            &["a", "b", "c"],
            &[
                &["-", "!"],
                &[
                    "&&", "||", "^", "+", "-", "*", "min", "max", "<", "<=", "==", "!=",
                ],
                &["select"],
            ],
        ),
        all_rules.clone(),
        &pvec_to_terms,
        &cond_prop_ruleset,
    );
    all_rules.extend(full);

    println!("all_rules: done");

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
        Limits::synthesis(),
        Limits::minimize(),
        true,
        None,
        None,
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
        Limits::synthesis(),
        Limits::minimize(),
        true,
        None,
        None,
    );
    all_rules.extend(new.clone());

    // NOTE: The following workloads do NOT use all_rules as prior rules
    // Using all_rules as prior_rules leads to OOM
    let select_base = Workload::new([
        "(select V V V)",
        "(select V (OP V V) V)",
        "(select V V (OP V V))",
        "(select V (OP V V) (OP V V))",
        "(OP V (select V V V))",
    ])
    .plug("V", &Workload::new(["a", "b", "c", "d"]));

    let arith = select_base
        .clone()
        .plug("OP", &Workload::new(["+", "-", "*"]));
    let new = run_workload(
        arith,
        rat_only.clone(),
        Limits::synthesis(),
        Limits {
            iter: 1,
            node: 100_000,
            match_: 100_000,
        },
        true,
        None,
        None,
    );
    all_rules.extend(new);

    let arith = select_base.plug("OP", &Workload::new(["min", "max"]));
    let new = run_workload(
        arith,
        rat_only.clone(),
        Limits::synthesis(),
        Limits {
            iter: 1,
            node: 100_000,
            match_: 100_000,
        },
        true,
        None,
        None,
    );
    all_rules.extend(new);

    all_rules
}
