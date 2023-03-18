/*!
32 bit implementation of Bitvectors.
!*/

use std::time::Instant;

use ruler::enumo::{Ruleset, Scheduler, Workload};

ruler::impl_bv!(32);

impl Bv {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let mut candidates = Ruleset::cvec_match(&compressed);

        let num_prior = prior.len();
        let chosen = candidates.minimize(prior, limits);
        let time = t.elapsed().as_secs_f64();

        println!(
            "Learned {} bidirectional rewrites ({} total rewrites) in {} using {} prior rewrites",
            chosen.bidir_len(),
            chosen.len(),
            time,
            num_prior
        );

        chosen.pretty_print();

        chosen
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ruler::enumo::{Filter, Metric, Ruleset, Workload};
    use std::time::Instant;

    // #[test]
    fn bv32_oopsla_equiv() {
        let mut all_rules = Ruleset::default();
        let start = Instant::now();

        let initial_vals = Workload::new(["a", "b", "c"]);
        let uops = Workload::new(["~", "-"]);
        let bops = Workload::new(["&", "|", "*", "--", "+"]);

        let layer_1 = Workload::make_layer(initial_vals.clone(), uops.clone(), bops.clone())
            .filter(Filter::MetricLt(Metric::Lists, 2));
        let terms_1 = layer_1.clone().append(initial_vals.clone());
        let rules_1 = Bv::run_workload(terms_1.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_1.clone());

        let layer_2 = Workload::make_layer(layer_1.clone(), uops.clone(), bops.clone())
            .filter(Filter::MetricLt(Metric::Lists, 3))
            .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::Lists, 1))));
        let terms_2 = layer_2.clone().append(terms_1.clone());
        let rules_2 = Bv::run_workload(terms_2.clone(), all_rules.clone(), Limits::default());
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
        let rules_3 = Bv::run_workload(layer_3.clone(), all_rules.clone(), Limits::default());
        all_rules.extend(rules_3.clone());
        let duration = start.elapsed();

        terms_3.write_terms_to_file("terms_bv32.txt");
        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");

        all_rules.write_json_rules("bv32.json");
        all_rules.write_json_equiderivability(
            DeriveType::Lhs,
            baseline.clone(),
            "bv32.json",
            Limits {
                iter: 3,
                node: 300000,
            },
            duration.clone(),
        );
    }
}
