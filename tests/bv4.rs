/*!
4 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(4);

#[cfg(test)]
mod test {
    use super::*;
    use ruler::enumo::{Filter, Metric, Ruleset, Workload};
    use serde_json::*;
    use std::fs::OpenOptions;
    use std::io::Write;
    use std::time::Instant;

    #[test]
    fn bv4_oopsla_equiv() {
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

        let mut terms = OpenOptions::new()
            .append(true)
            .open("terms.workload")
            .expect("Unable to open file");

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

        let mut layer_3_copy = layer_3.clone().force();
        terms
            .write_all("LAYER 3, BV4: \n".to_string().as_bytes())
            .expect("write failed");
        for _n in 0..layer_3_copy.clone().len() {
            terms
                .write_all(layer_3_copy.pop().unwrap().to_string().as_bytes())
                .expect("write failed");
            terms
                .write_all("\n".to_string().as_bytes())
                .expect("write failed");
        }

        let rules_3 = Bv::run_workload(layer_3.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_3);
        let duration = start.elapsed();
        all_rules.to_file("equivalent/bv4.rules");

        let baseline = Ruleset::<_>::from_file("baseline/bv4.rules");

        let (can, cannot) = all_rules.derive(
            baseline.clone(),
            Limits {
                iter: 6,
                node: 1000000,
            },
        );

        let (canr, cannotr) = baseline.derive(
            all_rules.clone(),
            Limits {
                iter: 6,
                node: 1000000,
            },
        );

        let derivability = json!({
            "forwards derivable": can.to_str_vec(),
            "forwards underivable": cannot.to_str_vec(),
            "backwards derivable": canr.to_str_vec(),
            "backwards underivable": cannotr.to_str_vec()
        });

        let derivability_str = derivability.to_string();

        let mut file = OpenOptions::new()
            .append(true)
            .open("rep/json/derivable_rules/bv4.json")
            .expect("Unable to open file");
        file.write_all(derivability_str.as_bytes())
            .expect("write failed");

        let num_rules = &all_rules.len();
        let forwards_derivable = &can.len();
        let backwards_derivable = &canr.len();
        let time = &duration.as_secs();

        let stats = json!({
            "spec": "bv4",
            "num_rules": num_rules,
            "num_baseline": 110,
            "enumo_derives_oopsla": forwards_derivable,
            "oopsla_derives_enumo": backwards_derivable,
            "time": time
        });

        let stats_str = stats.to_string();

        let mut file = OpenOptions::new()
            .append(true)
            .open("rep/json/output.json")
            .expect("Unable to open file");
        file.write_all(stats_str.as_bytes()).expect("write failed");
        file.write_all(", ".as_bytes()).expect("write failed");
    }
}
