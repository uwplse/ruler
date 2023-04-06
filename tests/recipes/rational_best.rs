use super::*;
use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::{base_lang, iter_metric},
};

pub fn best_enumo_recipe() -> Ruleset<Math> {
    let mut rules = Ruleset::default();
    let limits = Limits::default();

    // Domain
    let vars = &Workload::new(["a", "b", "c"]);
    let consts = &Workload::new(["0", "-1", "1"]);
    let uops = &Workload::new(["~", "fabs"]);
    let bops = &Workload::new(["+", "-", "*", "/"]);

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = iter_metric(base_lang(), "EXPR", enumo::Metric::Depth, 2)
        .filter(Filter::Contains("var".parse().unwrap()))
        .plug("CONST", consts)
        .plug("VAR", vars)
        .plug("UOP", uops)
        .plug("BOP", bops)
        .plug("TOP", &Workload::empty());
    let layer1_rules = Math::run_workload_conditional(layer1.clone(), rules.clone(), limits, false);
    rules.extend(layer1_rules);

    // Layer 2 (two ops)
    println!("layer2");
    let layer2 = iter_metric(base_lang(), "EXPR", enumo::Metric::Depth, 3)
        .filter(Filter::Contains("var".parse().unwrap()))
        .plug("CONST", consts)
        .plug("VAR", vars)
        .plug("UOP", uops)
        .plug("BOP", bops)
        .plug("TOP", &Workload::empty());
    layer2.to_file("replicate_layer2_terms");
    let layer2_rules = Math::run_workload_conditional(layer2.clone(), rules.clone(), limits, true);
    rules.extend(layer2_rules);

    // Contains var filter
    let contains_var_filter = Filter::Or(vec![
        Filter::Contains("a".parse().unwrap()),
        Filter::Contains("b".parse().unwrap()),
        Filter::Contains("c".parse().unwrap()),
    ]);

    // Safe filter
    let safe_filter = Filter::Invert(Box::new(Filter::Contains("(/ ?x 0)".parse().unwrap())));

    // Contains abs filter
    let contains_abs_filter = Filter::Contains("fabs".parse().unwrap());

    let vars = Workload::new(["a", "b", "c"]);
    let consts = Workload::new(["-1", "0", "1", "2"]);

    // Div
    println!("div");
    let div = Workload::new(["(/ v (/ v v))"]).plug("v", &vars);
    let div_rules = Math::run_workload_conditional(div, rules.clone(), limits, true);
    rules.extend(div_rules);

    // Nested fabs
    println!("nested fabs");
    let op_layer = Workload::new(["(uop expr)", "(bop expr expr)"])
        .plug("uop", &Workload::new(&["~", "fabs"]))
        .plug("bop", &Workload::new(&["+", "-", "*", "/"]));
    let layer1 = op_layer.clone().plug("expr", &vars.append(consts));
    let layer2 = op_layer
        .plug("expr", &layer1)
        .filter(safe_filter.clone())
        .filter(contains_var_filter.clone())
        .filter(contains_abs_filter);
    let nested_abs = Workload::new(["(fabs e)"]).plug("e", &layer2);
    let nested_abs_rules = Math::run_workload_conditional(nested_abs, rules.clone(), limits, true);
    rules.extend(nested_abs_rules);

    rules
}
