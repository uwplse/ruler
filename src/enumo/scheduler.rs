use std::time::Duration;

use egg::{Rewrite, Runner};

use crate::{EGraph, Limits, SynthAnalysis, SynthLanguage};

use super::*;

pub enum Scheduler {
    Simple(Limits),
    Saturating(Limits),
}

impl Scheduler {
    fn mk_runner<L: SynthLanguage>(
        egraph: EGraph<L, SynthAnalysis>,
        limits: &Limits,
    ) -> Runner<L, SynthAnalysis> {
        Runner::default()
            .with_scheduler(egg::SimpleScheduler)
            .with_node_limit(limits.node)
            .with_iter_limit(limits.iter)
            .with_time_limit(Duration::from_secs(600))
            .with_egraph(egraph)
    }

    pub fn run<L: SynthLanguage>(
        &self,
        egraph: EGraph<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
    ) -> Runner<L, SynthAnalysis> {
        match self {
            Scheduler::Simple(limits) => {
                let rewrites = ruleset.0.values().map(|eq| &eq.rewrite);
                Self::mk_runner(egraph, limits)
                    .with_iter_limit(limits.iter)
                    .with_node_limit(limits.node)
                    .with_scheduler(egg::SimpleScheduler)
                    .run(rewrites)
            }
            Scheduler::Saturating(limits) => {
                let (sat, other) = ruleset.partition(|eq| eq.is_saturating());
                let (sat, other): (Vec<Rewrite<_, _>>, Vec<Rewrite<_, _>>) = (
                    (sat.0.iter().map(|(_, eq)| eq.rewrite.clone()).collect()),
                    (other.0.iter().map(|(_, eq)| eq.rewrite.clone()).collect()),
                );

                let mut runner = Self::mk_runner(egraph, limits);

                for _ in 0..limits.iter {
                    // Sat
                    runner = Self::mk_runner(runner.egraph, &Limits::max()).run(&sat);

                    // Other
                    runner = Self::mk_runner(
                        runner.egraph,
                        &Limits {
                            iter: 1,
                            node: limits.node,
                        },
                    )
                    .run(&other);
                }
                Self::mk_runner(runner.egraph, &Limits::max()).run(&sat)
            }
        }
    }
}
