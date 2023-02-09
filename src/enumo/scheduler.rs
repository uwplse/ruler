use egg::Runner;

use crate::{Limits, SynthAnalysis, SynthLanguage};

use super::*;

pub enum Scheduler {
    Simple(Limits),
}

impl Scheduler {
    pub fn run<L: SynthLanguage>(
        &self,
        runner: Runner<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
    ) -> Runner<L, SynthAnalysis> {
        match self {
            Scheduler::Simple(limits) => {
                let rewrites = ruleset.0.values().map(|eq| &eq.rewrite);
                runner
                    .with_iter_limit(limits.iter)
                    .with_node_limit(limits.node)
                    .with_scheduler(egg::SimpleScheduler)
                    .run(rewrites)
            }
        }
    }
}
