/*!
32 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(32);

#[cfg(test)]
mod test {
    use ruler::enumo::{Ruleset, Workload};
    use std::time::Instant;
    use std::fs::File;
    use std::io::Write;
    use super::*;

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
        let rules_1 = Bv::run_workload(layer_1, all_rules.clone(), Limits::default());
        all_rules.extend(rules_1);

        let layer_2 = Workload::make_layer(2, 
            &[],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+"],
        );
        let rules_2 = Bv::run_workload(layer_2, all_rules.clone(), Limits::default());
        all_rules.extend(rules_2);

        let layer_3 = Workload::make_layer(3, 
            &[],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+"],
        );
        let rules_3 = Bv::run_workload(layer_3, all_rules.clone(), Limits::default());
        all_rules.extend(rules_3);

        let duration = start.elapsed();
        all_rules.to_file("equivalent/bv32_rules_oopsla.rules");

        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");
        let (can, _cannot) = all_rules.derive(baseline,
            Limits {
                iter: 3,
                node: 1000000,
            },);
    }
}