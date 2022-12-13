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

    pub fn run(mut self) -> Ruleset<L> {
        let t = Instant::now();

        let mut egraph = self.params.workload.to_egraph::<L>();
        println!("enumerated {} eclasses", egraph.number_of_classes(),);

        self.egraph = egraph.clone();

        let mut candidates = if L::is_rule_lifting() {
            Ruleset::lift_rules(&mut egraph, self.params.prior_rules.clone())
        } else {
            let (_, unions, _) = self.params.prior_rules.compress_egraph(self.egraph.clone());

            self.apply_unions(unions);

            Ruleset::cvec_match(&self.egraph)
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
