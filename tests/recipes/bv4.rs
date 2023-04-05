use super::*;
use ruler::enumo::{Filter, Metric, Ruleset, Workload};

pub fn bv4_rules() -> Ruleset<Bv> {
    let mut all_rules = Ruleset::default();
    let initial_vals = Workload::new(["a", "b", "c", "0"]);
    let uops = Workload::new(["~", "-"]);
    let bops = Workload::new(["&", "|", "*", "--", "+", "<<", ">>"]);

    let layer_1 = Workload::make_layer(initial_vals.clone(), uops.clone(), bops.clone());
    let terms_1 = layer_1.clone().append(initial_vals.clone());
    let rules_1 = Bv::run_workload(
        terms_1.clone(),
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 300_000,
        },
    );
    all_rules.extend(rules_1.clone());

    let layer_2 = Workload::make_layer(layer_1.clone(), uops.clone(), bops.clone())
        .filter(Filter::MetricLt(Metric::Lists, 3));
    let terms_2 = layer_2.clone().append(terms_1.clone());
    let rules_2 = Bv::run_workload(
        terms_2.clone(),
        all_rules.clone(),
        Limits {
            iter: 2,
            node: 300_000,
        },
    );

    all_rules.extend(rules_2.clone());
    all_rules
}
