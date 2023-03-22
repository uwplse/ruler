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

    fn bv32_rules() -> Ruleset<Bv> {
        let mut all_rules = Ruleset::default();
        let initial_vals = Workload::new(["a", "b", "c"]);
        let uops = Workload::new(["~", "-"]);
        let bops = Workload::new(["&", "|", "*", "--", "+", "<<", ">>"]);

        let layer_1 = Workload::make_layer(initial_vals.clone(), uops.clone(), bops.clone())
            .filter(Filter::MetricLt(Metric::Lists, 2));
        let terms_1 = layer_1.clone().append(initial_vals.clone());
        let rules_1 = Bv::run_workload(
            terms_1.clone(),
            all_rules.clone(),
            Limits {
                iter: 2,
                node: 300000,
            },
        );
        all_rules.extend(rules_1.clone());

        let layer_2 = Workload::make_layer(layer_1.clone(), uops.clone(), bops.clone())
            .filter(Filter::MetricLt(Metric::Lists, 3))
            .filter(Filter::Invert(Box::new(Filter::MetricLt(Metric::Lists, 1))));
        let terms_2 = layer_2.clone().append(terms_1.clone());
        let rules_2 = Bv::run_workload(
            terms_2.clone(),
            all_rules.clone(),
            Limits {
                iter: 2,
                node: 300000,
            },
        );
        all_rules.extend(rules_2.clone());
        all_rules
    }

    #[test]
    fn run() {
        let start = Instant::now();
        let rules = bv32_rules();
        let duration = start.elapsed();

        rules.write_json_rules("bv32.json");
        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");
        rules.baseline_compare_to(
            baseline,
            "ruler1",
            "bv32",
            duration,
            Limits {
                iter: 3,
                node: 200000,
            },
        );
    }
}
