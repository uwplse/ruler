use std::time::{Duration, Instant};

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
    pub fn run_internal<L: SynthLanguage>(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
        rule: Option<&Rule<L>>,
        timeout: Duration,
    ) -> EGraph<L, SynthAnalysis> {
        let get_runner = |egraph: EGraph<L, SynthAnalysis>, limits: Limits| {
            let base_runner = Runner::default()
                .with_scheduler(egg::SimpleScheduler)
                .with_node_limit(limits.node)
                .with_iter_limit(limits.iter)
                .with_time_limit(timeout)
                .with_egraph(egraph);
            if let Some(rule) = rule {
                let lexpr = L::instantiate(&rule.lhs);
                let rexpr = L::instantiate(&rule.rhs);

                base_runner
                    .with_scheduler(egg::SimpleScheduler)
                    .with_hook(move |r| {
                        let lhs = r.egraph.lookup_expr(&lexpr);
                        let rhs = r.egraph.lookup_expr(&rexpr);
                        match (lhs, rhs) {
                            (Some(l), Some(r)) => {
                                if l == r {
                                    Err("Done".to_owned())
                                } else {
                                    Ok(())
                                }
                            }
                            _ => Ok(()),
                        }
                    })
            } else {
                base_runner
            }
        };
        match self {
            Scheduler::Simple(limits) => {
                let rewrites = ruleset.0.values().map(|rule| &rule.rewrite);
                let mut runner = get_runner(egraph.clone(), *limits)
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
                let start = Instant::now();
                let (sat, other) = ruleset.partition(|rule| rule.is_saturating());
                let (sat, other): (Vec<Rewrite<_, _>>, Vec<Rewrite<_, _>>) = (
                    (sat.0.iter().map(|(_, rule)| rule.rewrite.clone()).collect()),
                    (other
                        .0
                        .iter()
                        .map(|(_, rule)| rule.rewrite.clone())
                        .collect()),
                );

                let mut runner = get_runner(egraph.clone(), *limits);

                for _ in 0..limits.iter {
                    // Early exit if we've passed the timeout
                    if start.elapsed() > timeout {
                        runner.egraph.rebuild();
                        return runner.egraph;
                    }
                    // Sat
                    runner = get_runner(runner.egraph, Limits::max()).run(&sat);

                    // Other
                    runner = get_runner(
                        runner.egraph,
                        Limits {
                            iter: 1,
                            node: limits.node,
                        },
                    )
                    .run(&other);
                }
                let mut runner = get_runner(runner.egraph, Limits::max()).run(&sat);
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

    pub fn run<L: SynthLanguage>(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
    ) -> EGraph<L, SynthAnalysis> {
        // Set timeout to something quite high (10 minutes) to reduce non-deterministic
        // behavior. If egg terminates due to timeout, it's hard to reason about
        // what did/didn't merge, and things vary a lot on different machines, so
        // it is best to avoid terminating due to timeout.
        self.run_internal(egraph, ruleset, None, Duration::from_secs(600))
    }

    pub fn run_derive<L: SynthLanguage>(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
        rule: &Rule<L>,
    ) -> EGraph<L, SynthAnalysis> {
        // We need to include a timeout for derivability checking because with 5 iters,
        // misbehaving rules can really go off the rails.
        // The ruler1 derivability checker had a 10 second timeout,
        // so we do the same here. Ours is actually more generous because (at least
        // in the case of the saturating scheduler), it's possible to run for 30-40
        // seconds because there are multiple runners strung together. In any case,
        // the timeout gives the rules at least as long as the ruler1 derivability
        // checker would have.
        self.run_internal(egraph, ruleset, Some(rule), Duration::from_secs(10))
    }
}
