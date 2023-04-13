use super::*;
use egg::{Extractor, RecExpr};
use ruler::{
    enumo::{Filter, Ruleset, Workload},
    logger::get_derivability_results,
    recipe_utils::{iter_metric, run_workload},
};

pub fn best_enumo_recipe() -> Ruleset<Math> {
    let mut rules = Ruleset::default();
    let limits = Limits {
        iter: 3,
        node: 1_000_000,
    };

    // Domain
    let vars = &Workload::new(["a", "b", "c"]);
    let consts = &Workload::new(["0", "-1", "1", "2", "(- a a)"]);
    let uops = &Workload::new(["~", "fabs"]);
    let bops = &Workload::new(["+", "-", "*", "/"]);

    let lang = Workload::new(&["var", "const", "(uop expr)", "(bop expr expr)"])
        .plug("var", vars)
        .plug("const", consts)
        .plug("uop", uops)
        .plug("bop", bops);

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = iter_metric(lang.clone(), "expr", enumo::Metric::Depth, 2);
    let layer1_rules = Math::run_workload_conditional(layer1.clone(), rules.clone(), limits, false);
    rules.extend(layer1_rules);

    // Layer 2
    println!("layer2");
    let layer2 = iter_metric(lang.clone(), "expr", enumo::Metric::Atoms, 4);
    let layer2_rules = Math::run_workload_conditional(layer2.clone(), rules.clone(), limits, false);
    rules.extend(layer2_rules);

    // Layer 3
    println!("layer3");
    let layer3 = iter_metric(lang, "expr", enumo::Metric::Depth, 3);

    let layer3_rules = run_workload(layer3, rules.clone(), limits, false);
    rules.extend(layer3_rules);

    let target_rules = Ruleset::new(vec!["2 => (+ 1 1)"]);

    let (_can_f, cannot_f) = rules.derive(
        DeriveType::Lhs,
        &target_rules,
        Limits {
            iter: 4,
            node: 1_000_000,
        },
    );

    println!("cant_target:");
    cannot_f.pretty_print();
    assert!(cannot_f.len() == 0);

    // Contains var filter
    let contains_var_filter = Filter::Or(vec![
        Filter::Contains("a".parse().unwrap()),
        Filter::Contains("b".parse().unwrap()),
        Filter::Contains("c".parse().unwrap()),
    ]);

    // Contains abs filter
    let contains_abs_filter = Filter::Contains("fabs".parse().unwrap());

    // Nested fabs
    println!("nested fabs");
    let op_layer = Workload::new(["(uop expr)", "(bop expr expr)"])
        .plug("uop", &Workload::new(&["~", "fabs"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "/"]));
    let vars_and_consts = vars.clone().append(consts.clone());
    let layer1 = op_layer.clone().plug("expr", &vars_and_consts);
    let layer2 = op_layer
        .plug("expr", &layer1)
        .filter(contains_var_filter.clone())
        .filter(contains_abs_filter);
    let nested_abs = Workload::new(["(fabs e)"]).plug("e", &layer2);
    let nested_abs_rules = Math::run_workload_conditional(nested_abs, rules.clone(), limits, true);
    rules.extend(nested_abs_rules);

    // Factorization
    println!("factorization");
    let variables_multiplied = iter_metric(
        Workload::new(&["var", "(* expr expr)"]),
        "expr",
        enumo::Metric::Atoms,
        3,
    )
    .plug("var", &Workload::new(["a", "b"]));

    let factor_term = iter_metric(
        Workload::new(&["var", "(bop expr expr)"]),
        "expr",
        enumo::Metric::Depth,
        2,
    )
    .plug("var", &variables_multiplied)
    .plug("bop", &Workload::new(["+", "-"]));

    let factor_div = Workload::new(["(/ v v)"]).plug("v", &factor_term);

    let factor_rules = Math::run_workload_conditional(factor_div, rules.clone(), limits, false);
    rules.extend(factor_rules);

    rules
}
