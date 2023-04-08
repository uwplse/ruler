use crate::rational_replicate::replicate_ruler1_recipe;

use super::*;
use ruler::enumo::{Filter, Ruleset, Workload};

pub fn best_enumo_recipe() -> Ruleset<Math> {
    let mut rules = Ruleset::default();
    let limits = Limits {
        iter: 3,
        node: 1_000_000,
    };

    // Domain
    let lang = Workload::new(&["var", "const", "(uop expr)", "(bop expr expr)"]);
    let vars = &Workload::new(["a", "b", "c"]);
    let consts = &Workload::new(["0", "-1", "1"]);
    let uops = &Workload::new(["~", "fabs"]);
    let bops = &Workload::new(["+", "-", "*", "/"]);
    let empty = &Workload::Set(vec![]);

    // Layer 1 (one op)
    println!("layer1");
    let layer1 = lang
        .clone()
        .iter_metric("expr", enumo::Metric::Depth, 2)
        .filter(Filter::Contains("var".parse().unwrap()))
        .plug_lang(vars, consts, uops, bops);
    let layer1_rules = Math::run_workload_conditional(layer1.clone(), rules.clone(), limits, false);
    rules.extend(layer1_rules);

    // Layer 2 (two ops)
    println!("layer2");
    let layer2 = lang
        .clone()
        .iter_metric("expr", enumo::Metric::Depth, 3)
        .filter(Filter::Contains("var".parse().unwrap()))
        .plug_lang(vars, consts, uops, bops);
    let layer2_rules = Math::run_workload_conditional(layer2.clone(), rules.clone(), limits, false);
    rules.extend(layer2_rules);

    // Contains var filter
    /*let contains_var_filter = Filter::Or(vec![
        Filter::Contains("a".parse().unwrap()),
        Filter::Contains("b".parse().unwrap()),
        Filter::Contains("c".parse().unwrap()),
    ]);

    // Safe filter
    let safe_filter = Filter::Invert(Box::new(Filter::Contains("(/ ?x 0)".parse().unwrap())));

    // Contains abs filter
    let contains_abs_filter = Filter::Contains("fabs".parse().unwrap());

    let vars = Workload::new(["a", "b", "c"]);
    let consts = Workload::new(["-1", "0", "1", "2"]);*/

    // Factorization
    println!("factorization");
    let factor_term = Workload::new(&["var", "(bop expr expr)"])
        .iter_metric("expr", enumo::Metric::Depth, 3)
        .plug_lang(
            &Workload::new(["a", "b"]),
            empty,
            empty,
            &Workload::new(["+", "-", "*"]),
        );
    let factor_div = Workload::new(["(/ v v)"])
        .plug("v", &factor_term)
        .filter(Filter::Canon(vec!["a".to_string(), "b".to_string()]));
    let factor_rules = Math::run_workload_conditional(factor_div, rules.clone(), limits, false);
    rules.extend(factor_rules);

    rules
}
