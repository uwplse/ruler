/*!
32 bit implementation of Bitvectors.
!*/

use ruler::enumo::{Ruleset, Scheduler, Workload};
use std::time::Instant;
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
    use ruler::enumo::Ruleset;
    use std::time::Instant;

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bv32_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");

        println!("{} rules found.", rules.clone().len());

        let (can, cannot) =
            rules.derive(DeriveType::LhsAndRhs, &baseline, Limits {
                iter: 5,
                node: 100_000,
            });
        println!("LHS/RHS: {} / {}", can.len(), can.len() + cannot.len());
        cannot.to_file("underivable.txt");
        let (can, cannot) =
            rules.derive(DeriveType::Lhs, &baseline, Limits {
                iter: 5,
                node: 100_000,
            });
        println!("LHS: {} / {}", can.len(), can.len() + cannot.len());


        logger::write_output(
            &rules,
            &baseline,
            "bv32",
            "oopsla",
            Limits {
                iter: 3,
                node: 200000,
            },
            duration,
        );
    }
}
