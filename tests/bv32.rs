/*!
32 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(32);

#[cfg(test)]
mod test {
    use super::*;
    use ruler::enumo::{Filter, Metric, Ruleset, Workload};
    use std::time::Instant;

    #[test]
    fn bv32_oopsla_equiv() {
        let mut all_rules = Ruleset::default();
        let start = Instant::now();

        let layer_1 = Workload::make_layer(
            1,
            &[],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+"],
        );
        let rules_1 = Bv::run_workload(layer_1.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_1);

        let layer_2 = Workload::make_layer(
            2,
            &[],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+"],
        );
        let rules_2 = Bv::run_workload(layer_2.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_2);

        let sexp_vec_l1 = layer_1.clone().force();
        let sexp_vec_l2 = layer_2.clone().force();

        let str_vec_l1: Vec<String> = sexp_vec_l1.iter().map(|se| se.to_string()).collect();
        let str_vec_l2: Vec<String> = sexp_vec_l2.iter().map(|se| se.to_string()).collect();
        let consts = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let consts_str: Vec<&str> = consts.iter().map(|s| &**s).collect();

        let l3_uops = Workload::make_layer_uops(str_vec_l2.clone(), &["~", "-"])
            .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 3))));
        let l3_bops_1 = Workload::make_layer_bops(
            consts.clone(),
            str_vec_l2.clone(),
            &["&", "|", "*", "--", "+"],
        )
        .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 3))));
        let l3_bops_2 = Workload::make_layer_bops(
            str_vec_l1.clone(),
            str_vec_l1.clone(),
            &["&", "|", "*", "--", "+"],
        )
        .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 3))));

        let layer_3 = l3_uops
            .clone()
            .append(l3_bops_1.clone())
            .append(l3_bops_2.clone())
            .append(layer_1.clone())
            .append(layer_2.clone())
            .append(Workload::from_vec(consts_str.clone()));

        let rules_3 = Bv::run_workload(layer_3.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_3);
        let duration = start.elapsed();
        all_rules.to_file("equivalent/bv32.rules");

        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");

        all_rules.write_json_rules("bv32.json");
        all_rules.write_json_equiderivability(
            baseline.clone(),
            110,
            "bv32.json",
            Limits::default(),
            duration.clone(),
        );
    }
}
