use egg::{Id, StopReason};
use std::collections::HashMap;
use std::sync::atomic::Ordering;
use std::time::Duration;

use crate::math::*;
use crate::ruleset::Ruleset;

pub type RecExpr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
pub type EGraph = egg::EGraph<Math, ConstantFold>;
pub type Rewrite = egg::Rewrite<Math, ConstantFold>;
pub type Runner = egg::Runner<Math, ConstantFold, IterData>;

#[derive(Debug, Clone, Copy)]
pub struct Limits {
    pub iter: usize,
    pub node: usize,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            iter: 3,
            node: 300000,
        }
    }
}

impl Limits {
    pub fn new(iter: usize, node: usize) -> Limits {
        Self { iter, node }
    }

    pub fn max() -> Self {
        Self {
            iter: usize::MAX,
            node: usize::MAX,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Scheduler {
    Backoff(Limits),
    Simple(Limits),
    Saturating(Limits),
    Compress(Limits),
}

impl Scheduler {
    fn should_bail(runner: &Runner, limits: &Limits) -> bool {
        if runner.egraph.total_number_of_nodes() > limits.node {
            true
        } else if let Some(StopReason::Other(_)) = runner.stop_reason {
            true
        } else {
            false
        }
    }

    fn update_runner(runner: Runner, limits: &Limits) -> Runner {
        let iter_len = runner.iterations.len();
        let mut runner = runner
            .with_scheduler(egg::SimpleScheduler)
            .with_iter_limit(limits.iter.saturating_add(iter_len))
            .with_node_limit(limits.node)
            .with_time_limit(Duration::from_secs(10))
            .with_hook(|r| {
                if r.egraph.analysis.unsound.load(Ordering::SeqCst) {
                    Err("Unsoundness detected".into())
                } else {
                    Ok(())
                }
            });
        runner.stop_reason = None;
        runner
    }

    pub fn run(&self, runner: Runner, ruleset: &Ruleset) -> Runner {
        match self {
            Scheduler::Backoff(limits) => {
                let rewrites = ruleset.0.values().map(|rule| &rule.rewrite);
                let new_runner = Scheduler::update_runner(runner, limits)
                    .with_scheduler(egg::BackoffScheduler::default())
                    .run(rewrites);
                new_runner
            }
            Scheduler::Simple(limits) => {
                let rewrites = ruleset.0.values().map(|rule| &rule.rewrite);
                let new_runner = Scheduler::update_runner(runner, limits).run(rewrites);
                new_runner
            }
            Scheduler::Saturating(limits) => {
                let (sat, other) = ruleset.partition(|rule| rule.is_saturating());
                let (sat, other): (Vec<Rewrite>, Vec<Rewrite>) = (
                    (sat.0.iter().map(|(_, rule)| rule.rewrite.clone()).collect()),
                    (other
                        .0
                        .iter()
                        .map(|(_, rule)| rule.rewrite.clone())
                        .collect()),
                );

                let mut runner = Self::update_runner(runner, limits);
                let max_limits = Limits {
                    iter: usize::MAX,
                    node: limits.node,
                };
                let once_limits = Limits {
                    iter: 1,
                    node: limits.node,
                };

                for _ in 0..limits.iter {
                    // Sat
                    runner = Self::update_runner(runner, &max_limits).run(&sat);
                    if Self::should_bail(&runner, limits) {
                        return runner;
                    }

                    // Other
                    runner = Self::update_runner(runner, &once_limits).run(&other);
                    if Self::should_bail(&runner, limits) {
                        return runner;
                    }
                }

                // TODO need to check soundness?
                Self::update_runner(runner, &Limits::max()).run(&sat)
            }
            Scheduler::Compress(limits) => {
                let mut runner = runner;
                let compress_iters = 5;
                let once_limits = Limits {
                    iter: 1,
                    node: limits.node,
                };

                for _ in 0..limits.iter {
                    // compress (1 iter)
                    let mut clone = runner.egraph.clone();
                    for _ in 0..compress_iters {
                        let runner_clone = Runner::new(Default::default())
                            .with_time_limit(Duration::from_secs(u64::MAX))
                            .with_hook(|r| {
                                if r.egraph.analysis.unsound.load(Ordering::SeqCst) {
                                    Err("Unsoundness detected".into())
                                } else {
                                    Ok(())
                                }
                            })
                            .with_egraph(clone.clone());

                        let out = Self::Simple(once_limits).run(runner_clone, ruleset);
                        runner.stop_reason = out.stop_reason.clone();
                        if Self::should_bail(&out, limits) {
                            break;
                        } else {
                            clone = out.egraph;
                        }
                    }

                    // Build a map from id in out to all of the ids in egraph that are equivalent
                    let mut unions = HashMap::new();
                    let ids: Vec<Id> = runner.egraph.classes().map(|c| c.id).collect();
                    for id in ids {
                        let new_id = clone.find(id);
                        unions.entry(new_id).or_insert_with(Vec::new).push(id);
                    }

                    for ids in unions.values() {
                        if ids.len() > 1 {
                            let first = ids[0];
                            for id in &ids[1..] {
                                runner.egraph.union(first, *id);
                            }
                        }
                    }

                    runner.egraph.rebuild();

                    // simple 1 iter
                    runner = Self::Simple(once_limits).run(runner, ruleset);
                    if Self::should_bail(&runner, limits) {
                        return runner;
                    }
                }
                
                runner
            }
        }
    }
}
