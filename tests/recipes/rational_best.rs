use crate::rational_replicate::replicate_ruler1_recipe;

use super::*;
use ruler::enumo::{Filter, Ruleset, Workload};

pub fn best_enumo_recipe() -> Ruleset<Math> {
    let limits = Limits::rulefinding();

    // Start from the "basic" rational rules
    let mut rules = replicate_ruler1_recipe();

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
