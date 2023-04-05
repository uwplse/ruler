use super::*;
use ruler::enumo::{Filter, Metric, Ruleset, Workload};

pub fn bool_rules() -> Ruleset<Bool> {
    let mut all_rules = Ruleset::default();
    let initial_vals = Workload::new(["a", "b", "c"]);
    let uops = Workload::new(["~"]);
    let bops = Workload::new(["&", "|", "^"]);

    let layer_1 = Workload::make_layer(initial_vals.clone(), uops.clone(), bops.clone())
        .filter(Filter::MetricLt(Metric::Lists, 2));
    let terms_1 = layer_1.clone().append(initial_vals.clone());
    let rules_1 = Bool::run_workload(terms_1.clone(), all_rules.clone(), Limits::default());
    all_rules.extend(rules_1.clone());

    let layer_2 = Workload::make_layer(layer_1.clone(), uops.clone(), bops.clone())
        .filter(Filter::MetricLt(Metric::Lists, 3))
        .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::Lists, 1))));
    let terms_2 = layer_2.clone().append(terms_1.clone());
    let rules_2 = Bool::run_workload(terms_2.clone(), all_rules.clone(), Limits::default());
    all_rules.extend(rules_2.clone());

    let layer_3 = Workload::make_layer_clever(
        initial_vals,
        layer_1,
        layer_2,
        uops.clone(),
        bops.clone(),
        3,
    );
    let terms_3 = layer_3.clone().append(terms_2.clone());
    let rules_3 = Bool::run_workload(terms_3.clone(), all_rules.clone(), Limits::default());
    all_rules.extend(rules_3.clone());
    all_rules
}
