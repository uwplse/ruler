use super::*;
use ruler::enumo::{Filter, Ruleset, Workload};

pub fn replicate_ruler1_recipe() -> Ruleset<Math> {
    let mut rules = Ruleset::default();
    let limits = Limits {
        iter: 2,
        node: 300_000,
    };

    // Domain
    let lang = Workload::new(&["var", "const", "(uop expr)", "(bop expr expr)"]);
    let vars = &Workload::new(["a", "b", "c"]);
    let consts = &Workload::new(["0", "-1", "1"]);
    let uops = &Workload::new(["~", "fabs"]);
    let bops = &Workload::new(["+", "-", "*", "/"]);

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = lang
        .clone()
        .iter_metric("expr", enumo::Metric::Depth, 2)
        .filter(Filter::Contains("var".parse().unwrap()))
        .plug_lang(vars, consts, uops, bops);
    let layer1_rules = Math::run_workload(layer1.clone(), rules.clone(), limits);
    rules.extend(layer1_rules);

    // Layer 2 (two ops)
    println!("layer2");
    let layer2 = lang
        .clone()
        .iter_metric("expr", enumo::Metric::Depth, 3)
        .filter(Filter::Contains("var".parse().unwrap()))
        .plug_lang(vars, consts, uops, bops);
    let layer2_rules = Math::run_workload_fast_match(layer2.clone(), rules.clone(), limits);
    rules.extend(layer2_rules);

    rules
}
