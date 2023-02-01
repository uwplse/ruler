/*!
32 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(32);

#[cfg(test)]
mod test {
    use ruler::enumo::{Ruleset, Workload, Metric, Filter};
    use std::time::Instant;
    use super::*;
    use serde_json::*;
    use std::fs::OpenOptions;
    use std::io::Write;

    #[test]
    fn bv32_oopsla_equiv() {
        let mut all_rules = Ruleset::default();
        let start = Instant::now();

        let layer_1 = Workload::make_layer(1, 
            &[],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+"],
        );
        let rules_1 = Bv::run_workload(layer_1.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_1);

        let layer_2 = Workload::make_layer(2, 
            &[],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+"],
        );
        let rules_2 = Bv::run_workload(layer_2.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_2);

        let sexp_vec_l1 = layer_1.clone().force();
        let sexp_vec_l2 = layer_2.clone().force();

        let mut terms = OpenOptions::new().append(true).open("terms.workload").expect("Unable to open file");

        let str_vec_l1: Vec<String> = sexp_vec_l1.iter().map(|se| se.to_string()).collect();
        let str_vec_l2: Vec<String> = sexp_vec_l2.iter().map(|se| se.to_string()).collect();
        let consts = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let consts_str : Vec<&str> = consts.iter().map(|s| &**s).collect();

        let l3_uops = Workload::make_layer_uops(str_vec_l2.clone(), &["~", "-"]).filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 3))));
        let l3_bops_1 = Workload::make_layer_bops(consts.clone(), str_vec_l2.clone(), &["&", "|", "*", "--", "+"]).filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 3))));
        let l3_bops_2 = Workload::make_layer_bops(str_vec_l1.clone(), str_vec_l1.clone(), &["&", "|", "*", "--", "+"]).filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::List, 3))));

        let layer_3 = l3_uops.clone()
            .append(l3_bops_1.clone())
            .append(l3_bops_2.clone())
            .append(layer_1.clone())
            .append(layer_2.clone())
            .append(Workload::from_vec(consts_str.clone()));
        
        let mut layer_3_copy = layer_3.clone().force();
/*
        let mut layer_3_uops = l3_uops.clone().force();
        let mut layer_3_bops_1 = l3_bops_1.clone().force();
        let mut layer_3_bops_2 = l3_bops_2.clone().force();
        let mut layer_3_copy = layer_3.clone().force();
        let mut layer_2_copy = layer_2.clone().force();
        let mut layer_1_copy = layer_1.clone().force();
        let mut consts_copy = Workload::from_vec(consts_str.clone()).force();

        terms.write_all("CONSTS, BV32: \n".to_string().as_bytes()).expect("write failed");
        for _n in 0..consts_copy.clone().len() {
            terms.write_all(consts_copy.pop().unwrap().to_string().as_bytes()).expect("write failed");
            terms.write_all("\n".to_string().as_bytes()).expect("write failed");
        }

        terms.write_all("LAYER 1, BV32: \n".to_string().as_bytes()).expect("write failed");
        for _n in 0..layer_1_copy.clone().len() {
            terms.write_all(layer_1_copy.pop().unwrap().to_string().as_bytes()).expect("write failed");
            terms.write_all("\n".to_string().as_bytes()).expect("write failed");
        }

        terms.write_all("LAYER 2, BV32: \n".to_string().as_bytes()).expect("write failed");
        for _n in 0..layer_2_copy.clone().len() {
            terms.write_all(layer_2_copy.pop().unwrap().to_string().as_bytes()).expect("write failed");
            terms.write_all("\n".to_string().as_bytes()).expect("write failed");
        }

        terms.write_all("LAYER 3 UOPS, BV32: \n".to_string().as_bytes()).expect("write failed");
        for _n in 0..layer_3_uops.clone().len() {
            terms.write_all(layer_3_uops.pop().unwrap().to_string().as_bytes()).expect("write failed");
            terms.write_all("\n".to_string().as_bytes()).expect("write failed");
        }

        terms.write_all("LAYER 3 BOPS 1, BV32: \n".to_string().as_bytes()).expect("write failed");
        for _n in 0..layer_3_bops_1.clone().len() {
            terms.write_all(layer_3_bops_1.pop().unwrap().to_string().as_bytes()).expect("write failed");
            terms.write_all("\n".to_string().as_bytes()).expect("write failed");
        }

        terms.write_all("LAYER 3 BOPS 2, BV32: \n".to_string().as_bytes()).expect("write failed");
        for _n in 0..layer_3_bops_2.clone().len() {
            terms.write_all(layer_3_bops_2.pop().unwrap().to_string().as_bytes()).expect("write failed");
            terms.write_all("\n".to_string().as_bytes()).expect("write failed");
        }
*/
        terms.write_all("LAYER 3, BV32: \n".to_string().as_bytes()).expect("write failed");
        for _n in 0..layer_3_copy.clone().len() {
            terms.write_all(layer_3_copy.pop().unwrap().to_string().as_bytes()).expect("write failed");
            terms.write_all("\n".to_string().as_bytes()).expect("write failed");
        }

        let rules_3 = Bv::run_workload(layer_3.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_3);
        let duration = start.elapsed();
        all_rules.to_file("equivalent/bv32_rules_oopsla.rules");

        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");

        let (can, cannot) = all_rules.derive(baseline.clone(),
            Limits {
                iter: 5,
                node: 1000000,
            },);

        let (canr, cannotr) = baseline.derive(all_rules.clone(),
            Limits {
                iter: 5,
                node: 1000000,
            },);
        
        
        let derivability = json!({
            "forwards derivable": can.to_str_vec(),
            "forwards underivable": cannot.to_str_vec(),
            "backwards derivable": canr.to_str_vec(),
            "backwards underivable": cannotr.to_str_vec()
        });

        let derivability_str = derivability.to_string();

        let mut file = OpenOptions::new().append(true).open("rep/json/bv32.json").expect("Unable to open file");     
        file.write_all(derivability_str.as_bytes()).expect("write failed");

        let num_rules = &all_rules.len();
        let forwards_derivable = &can.len();
        let backwards_derivable = &canr.len();
        let time = &duration.as_secs();

        let stats = json!({
            "spec": "bv32",
            "num_rules": num_rules,
            "num_baseline": 60,
            "enumo_derives_oopsla": forwards_derivable,
            "oopsla_derives_enumo": backwards_derivable,
            "time": time
        });

        let stats_str = stats.to_string();

        let mut file = OpenOptions::new().append(true).open("rep/json/output.json").expect("Unable to open file");
        file.write_all(stats_str.as_bytes()).expect("write failed");
        file.write_all(", ".as_bytes()).expect("write failed");
    }
}