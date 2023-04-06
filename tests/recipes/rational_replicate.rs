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

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = iter_metric(base_lang(), "EXPR", enumo::Metric::Depth, 2)
        .filter(Filter::Contains("VAR".parse().unwrap()))
        .plug("CONST", consts)
        .plug("VAR", vars)
        .plug("UOP", uops)
        .plug("BOP", bops)
        .plug("TOP", &Workload::empty());
    let layer1_rules = run_workload(layer1.clone(), rules.clone(), limits, false);
    rules.extend(layer1_rules);

    // Layer 2 (two ops)
    println!("layer2");
    let layer2 = iter_metric(base_lang(), "EXPR", enumo::Metric::Depth, 3)
        .filter(Filter::Contains("VAR".parse().unwrap()))
        .plug("CONST", consts)
        .plug("VAR", vars)
        .plug("UOP", uops)
        .plug("BOP", bops)
        .plug("TOP", &Workload::empty());
    let layer2_rules = run_workload(layer2.clone(), rules.clone(), limits, true);
    rules.extend(layer2_rules);

    rules
}
