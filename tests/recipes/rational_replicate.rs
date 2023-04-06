use super::*;
use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::{base_lang, iter_metric, run_workload},
};

pub fn replicate_ruler1_recipe() -> Ruleset<Math> {
    let mut rules = Ruleset::default();
    let limits = Limits::default();

    // Domain
    let vars = &Workload::new(["a", "b", "c"]);
    let consts = &Workload::new(["0", "-1", "1"]);
    let uops = &Workload::new(["~", "fabs"]);
    let bops = &Workload::new(["+", "-", "*", "/"]);
    let lang = base_lang()
        .filter(Filter::Invert(Box::new(Filter::Contains(
            "TOP".parse().unwrap(),
        ))))
        .plug("UOP", uops)
        .plug("BOP", bops);

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = lang
        .clone()
        .plug("CONST", consts)
        .plug("VAR", vars)
        .plug("EXPR", &vars.clone().append(consts.clone()));

    let layer1_rules = run_workload(layer1.clone(), rules.clone(), limits, false);
    rules.extend(layer1_rules);

    // Layer 2 (two ops)
    println!("layer2");
    let filter = Filter::Or(vec![
        Filter::Contains("a".parse().unwrap()),
        Filter::Contains("b".parse().unwrap()),
        Filter::Contains("c".parse().unwrap()),
    ]);
    let layer2 = lang
        .clone()
        .plug("EXPR", &layer1)
        .filter(filter)
        .plug("CONST", &Workload::empty())
        .plug("VAR", &Workload::empty());
    layer2.to_file("l2.terms");
    let layer2_rules = run_workload(layer2.clone(), rules.clone(), limits, true);
    rules.extend(layer2_rules);

    rules
}
