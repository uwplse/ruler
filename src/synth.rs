use egg::{AstSize, Extractor, StopReason};
use std::{fmt::Debug, hash::BuildHasherDefault, time::Instant};

use crate::*;

/// Validation result
#[derive(Debug, Clone)]
pub enum ValidationResult {
    Valid,
    Invalid,
    Unknown,
}

/// Faster hashMap implementation used in rustc
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
/// Faster hashSet implementation used in rustc
pub type HashSet<K> = rustc_hash::FxHashSet<K>;
/// IndexMap data implementation used in rustc
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<rustc_hash::FxHasher>>;

pub struct Synthesizer<L: SynthLanguage> {
    pub params: SynthParams<L>,
    pub egraph: EGraph<L, SynthAnalysis>,
    pub new_rws: Ruleset<L>,
}

impl<L: SynthLanguage> Synthesizer<L> {
    fn new(params: SynthParams<L>) -> Self {
        Self {
            params,
            egraph: Default::default(),
            new_rws: Default::default(),
        }
    }

    fn apply_unions(&mut self, unions: HashMap<Id, Vec<Id>>) {
        for ids in unions.values() {
            if ids.len() > 1 {
                let first = ids[0];
                for id in &ids[1..] {
                    self.egraph.union(first, *id);
                }
            }
        }
        self.egraph.rebuild();
    }

    // TODO: Figure out what to do with this- it doesn't match the definition
    // of cvec matching from the paper, but it is faster.
    fn _fast_cvec_match(&self) -> Ruleset<L> {
        let mut by_cvec: IndexMap<&CVec<L>, Vec<Id>> = IndexMap::default();

        for class in self.egraph.classes() {
            if class.data.is_defined() {
                by_cvec.entry(&class.data.cvec).or_default().push(class.id);
            }
        }

        let mut candidates = Ruleset::default();
        let extract = Extractor::new(&self.egraph, AstSize);

        for ids in by_cvec.values() {
            let exprs: Vec<_> = ids.iter().map(|&id| extract.find_best(id).1).collect();

            for (idx, e1) in exprs.iter().enumerate() {
                for e2 in exprs[(idx + 1)..].iter() {
                    if let Some(eq) = Equality::new(e1, e2) {
                        candidates.insert(eq);
                    }
                    if let Some(eq) = Equality::new(e2, e1) {
                        candidates.insert(eq);
                    }
                }
            }
        }
        candidates
    }

    fn run_cvec_synth(&mut self) -> Ruleset<L> {
        let (_, unions, _) = self.params.prior_rules.compress_egraph(self.egraph.clone());

        self.apply_unions(unions);

        Ruleset::cvec_match(&self.egraph)
    }

    fn extract_candidates_from_unions(&mut self, unions: HashMap<Id, Vec<Id>>) -> Ruleset<L> {
        let mut candidates: Ruleset<L> = Ruleset::default();
        let clone = self.egraph.clone();
        let extract = Extractor::new(&clone, ExtractableAstSize); // TODO: cost function for allowed
        for ids in unions.values() {
            for id1 in ids.clone() {
                for id2 in ids.clone() {
                    let (c1, e1) = extract.find_best(id1);
                    let (c2, e2) = extract.find_best(id2);
                    if c1 == usize::MAX || c2 == usize::MAX {
                        continue;
                    }
                    if let Some(eq) = Equality::new(&e1, &e2) {
                        if e1 != e2 {
                            if self.params.prior_rules.0.contains_key(&eq.name) {
                                // We already have this rule
                                continue;
                            }
                            // TODO: we used to validate here- do we need to?
                            candidates.insert(eq);
                        }
                    }
                }
            }
        }
        candidates
    }

    fn run_rule_lifting(&mut self) -> Ruleset<L> {
        // Run allowed rules
        println!("{}", self.egraph.number_of_classes());
        let mut allowed: Ruleset<L> = Ruleset::default();
        for (_, eq) in self.params.prior_rules.0.clone() {
            if L::is_allowed_rewrite(&eq.lhs, &eq.rhs) {
                allowed.insert(eq);
            }
        }
        println!("{} allowed rules", allowed.len());
        let (_, unions, _) = allowed.compress_egraph(self.egraph.clone());
        self.apply_unions(unions);

        // Run lifting rules
        let lifting_rules = L::get_lifting_rewrites();
        let (new_egraph, unions, stop_reason) = lifting_rules.compress_egraph_with_limits(
            self.egraph.clone(),
            usize::MAX,
            usize::MAX,
            1000,
        );
        assert!(
            matches!(stop_reason, StopReason::Saturated),
            "lifting rules must saturate. Instead, ended due to {:?}",
            stop_reason
        );
        let mut candidates = self.extract_candidates_from_unions(unions);

        self.egraph = new_egraph;

        // Run all rules
        let mut all_rules = self.params.prior_rules.clone();
        all_rules.extend(lifting_rules);
        let (_, unions, _) = all_rules.compress_egraph(self.egraph.clone());
        candidates.extend(self.extract_candidates_from_unions(unions));

        assert!(candidates
            .0
            .iter()
            .all(|(_, v)| L::is_allowed_rewrite(&v.lhs, &v.rhs)),);

        candidates
    }

    pub fn run(mut self) -> Ruleset<L> {
        let t = Instant::now();

        let egraph = self.params.workload.to_egraph::<L>();
        println!("enumerated {} eclasses", egraph.number_of_classes(),);

        self.egraph = egraph;

        let mut candidates = if L::is_rule_lifting() {
            self.run_rule_lifting()
        } else {
            self.run_cvec_synth()
        };

        let chosen = candidates.minimize(self.params.prior_rules.clone());
        self.new_rws.extend(chosen);

        let time = t.elapsed().as_secs_f64();

        println!(
            "Learned {} new rewrites in {} using {} prior rewrites",
            self.new_rws.len(),
            time,
            self.params.prior_rules.len()
        );

        for (name, eq) in &self.new_rws.0 {
            println!("{:?}      {}", eq.score(), name);
        }

        self.new_rws
    }
}

pub fn synth<L: SynthLanguage>(params: SynthParams<L>) -> Ruleset<L> {
    let syn = Synthesizer::<L>::new(params);
    syn.run()
}

// Cost function for ast size in the domain
// Penalizes ops not in the domain
pub struct ExtractableAstSize;
impl<L: SynthLanguage> egg::CostFunction<L> for ExtractableAstSize {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &L, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if enode.is_allowed_op() {
            enode.fold(1, |sum, id| sum.saturating_add(costs(id)))
        } else {
            usize::max_value()
        }
    }
}
