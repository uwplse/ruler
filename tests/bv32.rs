/*!
32 bit implementation of Bitvectors.
!*/

use std::time::Instant;
use ruler::enumo::{Ruleset, Scheduler, Workload};
#[path = "./recipes/bv32.rs"]
mod bv32;
ruler::impl_bv!(32);

impl Bv {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let mut candidates = Ruleset::cvec_match(&compressed);

        let num_prior = prior.len();
        let chosen = candidates.minimize(prior, Scheduler::Compress(limits));
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
    use crate::bv32::bv32_rules;
    use ruler::enumo::{Ruleset};
    use std::time::Instant;

    #[test]
    fn run() {
        let start = Instant::now();
        let rules = bv32_rules();
        let duration = start.elapsed();

        rules.write_json_rules("bv32.json");
        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");
        rules.write_baseline_row(
            baseline.clone(),
            "bv32",
            "oopsla",
            "recipes/bv32.rs",
            "baseline.json",
            Limits {
                iter: 3,
                node: 200000,
            },
            duration,
        );
        rules.write_baseline_row_big_object(
            baseline,
            "bv32",
            "oopsla_bv32",
            "recipes/bv32.rs",
            Limits {
                iter: 3,
                node: 200000,
            },
            duration,
        );
    }
}
