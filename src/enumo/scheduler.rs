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

struct MatchScheduler {
    match_limit: usize,
}
impl<L: SynthLanguage> egg::RewriteScheduler<L, SynthAnalysis> for MatchScheduler {
    fn search_rewrite<'a>(
        &mut self,
        _iteration: usize,
        egraph: &egg::EGraph<L, SynthAnalysis>,
        rewrite: &'a Rewrite<L, SynthAnalysis>,
    ) -> Vec<egg::SearchMatches<'a, L>> {
        rewrite.search_with_limit(egraph, self.match_limit)
    }
}

impl Scheduler {
    pub fn run_internal<L: SynthLanguage>(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
        rule: Option<&Rule<L>>,
    ) -> EGraph<L, SynthAnalysis> {
        let get_runner = |egraph: EGraph<L, SynthAnalysis>, limits: Limits| {
            let base_runner = Runner::default()
                .with_scheduler(MatchScheduler {
                    match_limit: limits.match_,
                })
                .with_node_limit(limits.node)
                .with_iter_limit(limits.iter)
                .with_time_limit(Duration::from_secs(600))
                .with_egraph(egraph);
            if let Some(rule) = rule {
                let lexpr = L::instantiate(&rule.lhs);
                let rexpr = L::instantiate(&rule.rhs);

                base_runner.with_hook(move |r| {
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

                let mut runner = get_runner(egraph.clone(), *limits);

                let max_limits = Limits {
                    iter: usize::MAX,
                    node: usize::MAX,
                    match_: limits.match_,
                };

                for _ in 0..limits.iter {
                    // Sat
                    runner = get_runner(runner.egraph, max_limits).run(&sat);

                    // Other
                    runner = get_runner(
                        runner.egraph,
                        Limits {
                            iter: 1,
                            node: limits.node,
                            match_: limits.match_,
                        },
                    )
                    .run(&other);
                }
                let mut runner = get_runner(runner.egraph, max_limits).run(&sat);
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
        self.run_internal(egraph, ruleset, None)
    }

    pub fn run_derive<L: SynthLanguage>(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        ruleset: &Ruleset<L>,
        rule: &Rule<L>,
    ) -> EGraph<L, SynthAnalysis> {
        self.run_internal(egraph, ruleset, Some(rule))
    }
}
