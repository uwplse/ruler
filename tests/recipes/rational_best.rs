use crate::rational_replicate::replicate_ruler1_recipe;

use super::*;
use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::{iter_metric, run_workload},
};

pub fn best_enumo_recipe() -> Ruleset<Math> {
    let mut rules = Ruleset::default();
    let limits = Limits {
        iter: 5,
        node: 600_000,
    };

    // Domain
    let vars = &Workload::new(["a", "b", "c"]);
    let vars_4 = &Workload::new(["a", "b", "c", "d"]);
    let consts = &Workload::new(["0", "-1", "1"]);
    let uops = &Workload::new(["~", "fabs"]);
    let bops = &Workload::new(["+", "-", "*", "/"]);
    let lang = Workload::new(&["var", "const", "(uop expr)", "(bop expr expr)"])
        .plug("var", &vars)
        .plug("const", &consts)
        .plug("uop", &uops)
        .plug("bop", &bops)
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
        ]));
    let lang_with_if = Workload::new(&[
        "var",
        "const",
        "(uop expr)",
        "(bop expr expr)",
        "(if expr expr expr)",
    ])
    .plug("var", &vars_4)
    .plug("const", &consts)
    .plug("uop", &uops)
    .plug("bop", &bops)
    .filter(Filter::Canon(vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
        "d".to_string(),
    ]));

    let empty = &Workload::Set(vec![]);

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = iter_metric(lang_with_if.clone(), "expr", enumo::Metric::Depth, 2);
    let layer1_rules = Math::run_workload_conditional(layer1.clone(), rules.clone(), limits, false);
    rules.extend(layer1_rules);

    // If rules
    let if_vars = Workload::new(&["(if expr expr expr)", "expr"]).plug("expr", &vars_4);
    println!("if rules");
    let if_rules = Math::run_workload_conditional(
        Workload::new(&["(if var expr expr)", "expr"])
            .plug("expr", &if_vars)
            .plug("var", &vars_4),
        rules.clone(),
        limits,
        false,
    );
    rules.extend(if_rules);

    // Layer 2
    println!("layer2");
    let layer2 = iter_metric(lang.clone(), "expr", enumo::Metric::Atoms, 4);
    let layer2_rules = Math::run_workload_conditional(layer2, rules.clone(), limits, false);
    rules.extend(layer2_rules);

    // Layer 3
    println!("layer3");
    let layer3 = iter_metric(lang, "expr", enumo::Metric::Depth, 3);
    let layer3_rules = Math::run_workload_conditional(layer3, rules.clone(), limits, false);
    rules.extend(layer3_rules);

    // Division
    println!("division");
    let division = Workload::new(&["(/ expr e)"])
        .plug("expr", &layer1)
        .append(Workload::new(&["(/ e expr)"]).plug("expr", &layer1));
    let division_rules = Math::run_workload_conditional(division, rules.clone(), limits, false);
    rules.extend(division_rules);

    // Factorization
    println!("factorization");
    let variables_multiplied = iter_metric(
        Workload::new(&["var", "(* expr expr)"]),
        "expr",
        enumo::Metric::Atoms,
        5,
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

    let factor_div = Workload::new(["(/ v v)"])
        .plug("v", &factor_term)
        .filter(Filter::Canon(vec!["a".to_string(), "b".to_string()]));

    let factor_rules = Math::run_workload_conditional(factor_div, rules.clone(), limits, false);
    rules.extend(factor_rules);

    rules
}
