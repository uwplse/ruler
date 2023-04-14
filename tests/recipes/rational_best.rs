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
        iter: 2,
        node: 300_000,
    };

    // Domain
    let vars = &Workload::new(["a", "b", "c"]);
    let consts = &Workload::new(["0", "-1", "1"]);
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
    let layer1_rules = run_workload(layer1.clone(), rules.clone(), limits, false);
    rules.extend(layer1_rules);

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
    let factor_rules_filtered = Ruleset(
        factor_rules
            .0
            .into_iter()
            .filter_map(|rule| {
                if rule.1.lhs.to_string() == "(- ?a ?a)" {
                    None
                } else {
                    Some(rule)
                }
            })
            .collect(),
    );
    rules.extend(factor_rules_filtered);

    println!("full ruleset:");
    rules.pretty_print();

    rules
}
