use std::time::Duration;

use egg::{Rewrite, Runner};

use crate::{EGraph, Id, Limits, SynthAnalysis, SynthLanguage};

use super::*;

#[derive(Debug, Clone, Copy)]
pub enum Scheduler {
    Simple(Limits),
    Saturating(Limits),
    Compress(Limits),
}

impl Scheduler {
    fn mk_runner<L: SynthLanguage>(
        egraph: EGraph<L, SynthAnalysis>,
        limits: &Limits,
    ) -> Runner<L, SynthAnalysis> {
        Runner::default()
            .with_scheduler(egg::BackoffScheduler::default().with_initial_match_limit(200_000))
            .with_node_limit(limits.node)
            .with_iter_limit(limits.iter)
            .with_time_limit(Duration::from_secs(600))
            .with_egraph(egraph)
    }

    pub fn run<L: SynthLanguage>(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
    ) -> EGraph<L, SynthAnalysis> {
        match self {
            Scheduler::Simple(limits) => {
                let rewrites = ruleset.0.values().map(|rule| &rule.rewrite);
                let mut runner = Self::mk_runner(egraph.clone(), limits)
                    .with_iter_limit(limits.iter)
                    .with_node_limit(limits.node)
                    .with_scheduler(
                        egg::BackoffScheduler::default().with_initial_match_limit(200_000),
                    )
                    .run(rewrites);
                runner.egraph.rebuild();
                runner.egraph
            }
            Scheduler::Saturating(limits) => {
                let (sat, other) = ruleset.partition(|rule| rule.is_saturating());
                let (sat, other): (Vec<Rewrite<_, _>>, Vec<Rewrite<_, _>>) = (
                    (sat.0.iter().map(|(_, rule)| rule.rewrite.clone()).collect()),
                    (other
                        .0
                        .iter()
                        .map(|(_, rule)| rule.rewrite.clone())
                        .collect()),
                );

                let mut runner = Self::mk_runner(egraph.clone(), limits);

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
                let mut runner = Self::mk_runner(runner.egraph, &Limits::max()).run(&sat);
                runner.egraph.rebuild();
                runner.egraph
            }
            Scheduler::Compress(limits) => {
                let mut clone = egraph.clone();
                let ids: Vec<Id> = egraph.classes().map(|c| c.id).collect();

                let out = Self::Simple(*limits).run(egraph, ruleset);

                // Build a map from id in out to all of the ids in egraph that are equivalent
                let mut unions = HashMap::default();
                for id in ids {
                    let new_id = out.find(id);
                    unions.entry(new_id).or_insert_with(Vec::new).push(id);
                }

                for ids in unions.values() {
                    if ids.len() > 1 {
                        let first = ids[0];
                        for id in &ids[1..] {
                            clone.union(first, *id);
                        }
                    }
                }
                clone.rebuild();
                clone
            }
        }
    }
}
