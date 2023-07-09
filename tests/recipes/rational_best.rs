use crate::rational_replicate::replicate_ruler1_recipe;

use super::*;
use ruler::{
    enumo::{Ruleset, Workload},
    recipe_utils::iter_metric,
};

pub fn best_enumo_recipe() -> Ruleset<Math> {
    let mut rules = replicate_ruler1_recipe();
    let limits = Limits::synthesis();

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
    // don't include rules that already contain division
    let factor_rules_filtered = Ruleset(
        factor_rules
            .0
            .into_iter()
            .filter_map(|rule| {
                if rule.1.lhs.to_string().contains("/") {
                    None
                } else {
                    Some(rule)
                }
            })
            .collect(),
    );
    rules.extend(factor_rules_filtered);

    rules
}
