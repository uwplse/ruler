use super::*;
use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::{base_lang, run_workload},
};

pub fn replicate_ruler1_recipe() -> Ruleset<Math> {
    let mut rules = Ruleset::default();

    // Domain
    let vars = &Workload::new(["a", "b", "c"]);
    let consts = &Workload::new(["0", "-1", "1"]);
    let uops = &Workload::new(["~", "fabs"]);
    let bops = &Workload::new(["+", "-", "*", "/"]);
    let lang = base_lang(2).plug("OP1", uops).plug("OP2", bops);

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = lang
        .clone()
        .plug("VAL", consts)
        .plug("VAR", vars)
        .plug("EXPR", &vars.clone().append(consts.clone()));

    let layer1_rules = run_workload(
        layer1.clone(),
        rules.clone(),
        Limits::synthesis(),
        Limits::minimize(),
        false,
    );
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
        .plug("VAL", &Workload::empty())
        .plug("VAR", &Workload::empty());
    let layer2_rules = run_workload(
        layer2.clone(),
        rules.clone(),
        Limits::synthesis(),
        Limits::minimize(),
        true,
    );
    rules.extend(layer2_rules);

    rules
}
