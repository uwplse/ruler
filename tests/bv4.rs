/*!
4 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(4);

#[cfg(test)]
mod test {
    use super::*;
    use ruler::enumo::{Filter, Metric, Ruleset, Workload};
    use std::fs::OpenOptions;
    use std::io::Write;
    use std::time::Instant;

    #[test]
    fn bv4_oopsla_equiv() {
        let mut all_rules = Ruleset::default();
        let start = Instant::now();

        let initial_vals = Workload::new(["a", "b", "c"]);
        let layer_1 = Workload::make_layer(
            initial_vals.clone(),
            &["~", "-"],
            &["&", "|", "*", "--", "+"],
        )
        .filter(Filter::MetricLt(Metric::List, 2));
        let terms_1 = layer_1.clone().append(initial_vals.clone());
        let rules_1 = Bv::run_workload(terms_1.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_1.clone());

        let layer_2 =
            Workload::make_layer(layer_1.clone(), &["~", "-"], &["&", "|", "*", "--", "+"])
                .filter(Filter::MetricLt(Metric::List, 3))
                .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 1))));
        let terms_2 = layer_2.clone().append(terms_1.clone());
        let rules_2 = Bv::run_workload(terms_2.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_2.clone());

        let uops = Workload::make_layer_uops(layer_2.clone(), &["~", "-"]);
        let bops_1 = Workload::make_layer_bops(
            layer_2.clone(),
            initial_vals.clone(),
            &["&", "|", "*", "--", "+"],
        );
        let bops_2 = Workload::make_layer_bops(
            layer_1.clone(),
            layer_1.clone(),
            &["&", "|", "*", "--", "+"],
        );
        let layer_3 = uops
            .append(bops_1)
            .append(bops_2)
            .filter(Filter::MetricLt(Metric::List, 4))
            .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 2))));
        let terms_3 = layer_3.clone().append(terms_2.clone());
        let rules_3 = Bv::run_workload(layer_3.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_3.clone());

        let terms: Vec<String> = terms_3
            .clone()
            .force()
            .iter()
            .map(|se| se.to_string())
            .collect();
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open("terms_bv4.txt")
            .expect("Unable to open file");
        terms.iter().for_each(|t| {
            file.write_all(t.as_bytes())
                .expect("Unable to write to file");
            file.write_all("\n".as_bytes())
                .expect("Unable to write to file");
        });
        let duration = start.elapsed();

        let baseline = Ruleset::<_>::from_file("baseline/bv4.rules");

        all_rules.write_json_rules("bv4.json");
        all_rules.write_json_equiderivability(
            baseline.clone(),
            110,
            "bv4.json",
            Limits::default(),
            duration.clone(),
        );
    }
}
