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

        logger::write_output(&rules, &baseline, "bv32", "oopsla", duration);
    }

    #[test]
    fn read_bv4() {
        let bv4_rules: Ruleset<Bv> = Ruleset::from_file("bv4.rules");
        // bv4_rules.pretty_print();
        let start = Instant::now();
        let (sound, unsound) = bv4_rules.partition(|rule| rule.is_valid());
        println!("Validating BV4 rules in BV32");
        println!("{} sound, {} unsound", sound.len(), unsound.len());
        println!("Elapsed time: {}", start.elapsed().as_secs());
    }
}
