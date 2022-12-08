use egg::{AstSize, EClass, ENodeOrVar, Extractor, RecExpr, Rewrite, Runner, StopReason};
use std::{
    fmt::Debug,
    hash::BuildHasherDefault,
    sync::Arc,
    time::{Duration, Instant},
};

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

pub type EqualityMap<L> = IndexMap<Arc<str>, Equality<L>>;

pub struct Synthesizer<L: SynthLanguage> {
    pub params: SynthParams<L>,
    pub egraph: EGraph<L, SynthAnalysis>,
    pub prior_rws: EqualityMap<L>,
    pub new_rws: EqualityMap<L>,
}

impl<L: SynthLanguage> Synthesizer<L> {
    fn new(params: SynthParams<L>) -> Self {
        let mut priors: EqualityMap<L> = Default::default();
        for eq in &params.prior_rules.0 {
            priors.insert(eq.name.clone(), eq.clone());
        }

        Self {
            params,
            egraph: Default::default(),
            prior_rws: priors,
            new_rws: Default::default(),
        }
    }

    fn parse_workload(&self, workload: &Workload) -> (Vec<RecExpr<L>>, Vec<String>) {
        let mut terms = vec![];
        let mut vars: HashSet<String> = HashSet::default();
        let sexps = workload.force();
        for sexp in sexps {
            let s = sexp.to_string();
            let expr: RecExpr<L> = s.parse().unwrap();
            for node in expr.as_ref() {
                if let ENodeOrVar::Var(v) = node.clone().to_enode_or_var() {
                    let mut v = v.to_string();
                    v.remove(0);
                    vars.extend(vec![v]);
                }
            }
            terms.push(expr);
        }

        (terms, vars.into_iter().collect())
    }

    fn add_workload(egraph: &mut EGraph<L, SynthAnalysis>, exprs: &[RecExpr<L>]) {
        for expr in exprs {
            egraph.add_expr(expr);
        }
    }

    fn mk_runner(&self, egraph: EGraph<L, SynthAnalysis>) -> Runner<L, SynthAnalysis, ()> {
        Runner::default()
            .with_scheduler(egg::SimpleScheduler)
            .with_node_limit(self.params.node_limit)
            .with_iter_limit(self.params.iter_limit)
            .with_time_limit(Duration::from_secs(self.params.time_limit))
            .with_egraph(egraph)
    }

    fn run_rewrites(
        &self,
        mut runner: Runner<L, SynthAnalysis>,
        rewrites: Vec<&Rewrite<L, SynthAnalysis>>,
    ) -> (EGraph<L, SynthAnalysis>, HashMap<Id, Vec<Id>>, StopReason) {
        let ids: Vec<Id> = runner.egraph.classes().map(|c| c.id).collect();
        runner = runner.run(rewrites);
        let stop_reason = runner.stop_reason.unwrap();

        let mut found_unions = HashMap::default();
        for id in ids {
            let new_id = runner.egraph.find(id);
            found_unions.entry(new_id).or_insert_with(Vec::new).push(id);
        }

        runner.egraph.rebuild();
        (runner.egraph, found_unions, stop_reason)
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

    fn cvec_match(&self) -> EqualityMap<L> {
        // cvecs [𝑎1, . . . , 𝑎𝑛] and [𝑏1, . . . , 𝑏𝑛] match iff:
        // ∀𝑖. 𝑎𝑖 = 𝑏𝑖 ∨ 𝑎𝑖 = null ∨ 𝑏𝑖 = null and ∃𝑖. 𝑎𝑖 = 𝑏𝑖 ∧ 𝑎𝑖 ≠ null ∧ 𝑏𝑖 ≠ null

        println!(
            "starting cvec match with {} eclasses",
            self.egraph.number_of_classes()
        );

        let not_all_none: Vec<&EClass<L, Signature<L>>> = self
            .egraph
            .classes()
            .filter(|x| x.data.cvec.iter().any(|v| v.is_some()))
            .collect();

        let compare = |cvec1: &CVec<L>, cvec2: &CVec<L>| -> bool {
            for tup in cvec1.iter().zip(cvec2) {
                match tup {
                    (Some(a), Some(b)) if a != b => return false,
                    _ => (),
                }
            }
            true
        };
        let mut candidates = EqualityMap::default();
        let extract = Extractor::new(&self.egraph, AstSize);
        for class1 in &not_all_none {
            for class2 in &not_all_none {
                if class1.id == class2.id {
                    continue;
                }
                if compare(&class1.data.cvec, &class2.data.cvec) {
                    let (_, e1) = extract.find_best(class1.id);
                    let (_, e2) = extract.find_best(class2.id);
                    if let Some(eq) = Equality::new(&e1, &e2) {
                        candidates.insert(eq.name.clone(), eq);
                    }
                    if let Some(eq) = Equality::new(&e2, &e1) {
                        candidates.insert(eq.name.clone(), eq);
                    }
                }
            }
        }
        candidates
    }

    // TODO: Figure out what to do with this- it doesn't match the definition
    // of cvec matching from the paper, but it is faster.
    fn _fast_cvec_match(&self) -> EqualityMap<L> {
        let mut by_cvec: IndexMap<&CVec<L>, Vec<Id>> = IndexMap::default();

        for class in self.egraph.classes() {
            if class.data.is_defined() {
                by_cvec.entry(&class.data.cvec).or_default().push(class.id);
            }
        }

        let mut candidates = EqualityMap::default();
        let extract = Extractor::new(&self.egraph, AstSize);

        for ids in by_cvec.values() {
            let exprs: Vec<_> = ids.iter().map(|&id| extract.find_best(id).1).collect();

            for (idx, e1) in exprs.iter().enumerate() {
                for e2 in exprs[(idx + 1)..].iter() {
                    if let Some(eq) = Equality::new(e1, e2) {
                        candidates.insert(eq.name.clone(), eq);
                    }
                    if let Some(eq) = Equality::new(e2, e1) {
                        candidates.insert(eq.name.clone(), eq);
                    }
                }
            }
        }
        candidates
    }

    fn select(&mut self, step_size: usize, candidates: EqualityMap<L>) -> EqualityMap<L> {
        // 1. sort by score
        let mut sorted_candidates: EqualityMap<L> = candidates
            .sorted_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()))
            .collect();

        // 2. insert step_size best candidates into self.new_rws
        let mut selected: EqualityMap<L> = Default::default();
        while selected.len() < step_size {
            let popped = sorted_candidates.pop();
            if let Some((name, eq)) = popped {
                if let ValidationResult::Valid = L::validate(self, &eq.lhs, &eq.rhs) {
                    selected.insert(name, eq);
                }
            } else {
                break;
            }
        }
        self.new_rws.extend(selected);

        // 3. return remaining candidates
        let mut remaining_candidates: EqualityMap<L> = Default::default();

        for (name, candidate) in sorted_candidates {
            remaining_candidates.insert(name, candidate);
        }

        remaining_candidates
    }

    fn shrink(&mut self, candidates: &EqualityMap<L>) -> EqualityMap<L> {
        // 1. make new egraph
        // let mut egraph: EGraph<L, SynthAnalysis> = EGraph::default();
        let mut runner = self.mk_runner(EGraph::default());

        // 2. insert lhs and rhs of all candidates as roots
        for eq in candidates.values() {
            runner = runner.with_expr(&L::instantiate(&eq.lhs));
            runner = runner.with_expr(&L::instantiate(&eq.rhs));
        }

        // 3. run eqsat with self.prior_rws and self.new_rws
        let rewrites = self
            .prior_rws
            .values()
            .map(|eq| &eq.rewrite)
            .chain(self.new_rws.values().map(|eq| &eq.rewrite));
        runner = runner.run(rewrites);

        // 4. go through candidates (root pairs) and if they haven't yet
        // merged, they are still candidates
        let mut remaining_candidates = EqualityMap::default();
        let extract = Extractor::new(&runner.egraph, AstSize);
        for ids in runner.roots.chunks(2) {
            if runner.egraph.find(ids[0]) == runner.egraph.find(ids[1]) {
                // candidate has merged (derivable from other rewrites)
                continue;
            }
            let (_, left) = extract.find_best(ids[0]);
            let (_, right) = extract.find_best(ids[1]);
            if let Some(eq) = Equality::new(&left, &right) {
                remaining_candidates.insert(eq.name.clone(), eq);
            }
        }

        remaining_candidates
    }

    fn choose_eqs(&mut self, candidates: EqualityMap<L>) {
        let step_size = 1;
        let mut remaining_candidates = candidates;
        while !remaining_candidates.is_empty() {
            remaining_candidates = self.select(step_size, remaining_candidates);

            remaining_candidates = self.shrink(&remaining_candidates);
        }
    }

    fn run_cvec_synth(&mut self) -> EqualityMap<L> {
        let eqs = self.prior_rws.clone();

        let runner = self.mk_runner(self.egraph.clone());
        let (_, unions, _) =
            self.run_rewrites(runner, eqs.values().map(|eq| &eq.rewrite).collect());

        self.apply_unions(unions);

        self.cvec_match()
    }

    fn extract_candidates_from_unions(&mut self, unions: HashMap<Id, Vec<Id>>) -> EqualityMap<L> {
        let mut candidates: EqualityMap<L> = EqualityMap::default();
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
                            if self.prior_rws.contains_key(&eq.name) {
                                // We already have this rule
                                continue;
                            }
                            // TODO: we used to validate here- do we need to?
                            candidates.insert(eq.name.clone(), eq);
                        }
                    }
                }
            }
        }
        candidates
    }

    fn run_rule_lifting(&mut self) -> EqualityMap<L> {
        // Run allowed rules
        println!("{}", self.egraph.number_of_classes());
        let mut allowed: EqualityMap<L> = EqualityMap::default();
        for (name, eq) in self.prior_rws.clone() {
            if L::is_allowed_rewrite(&eq.lhs, &eq.rhs) {
                allowed.insert(name, eq);
            }
        }
        println!("{} allowed rules", allowed.len());
        let runner = self.mk_runner(self.egraph.clone());
        let rewrites = allowed.values().map(|eq| &eq.rewrite).collect();
        let (_, unions, _) = self.run_rewrites(runner, rewrites);
        self.apply_unions(unions);

        // Run lifting rules
        let runner = self
            .mk_runner(self.egraph.clone())
            .with_iter_limit(usize::MAX)
            .with_time_limit(Duration::from_secs(1000))
            .with_node_limit(usize::MAX);

        let lifting_rewrites = L::get_lifting_rewrites();
        let (new_egraph, unions, stop_reason) =
            self.run_rewrites(runner, lifting_rewrites.iter().collect());
        assert!(
            matches!(stop_reason, StopReason::Saturated),
            "lifting rules must saturate. Instead, ended due to {:?}",
            stop_reason
        );
        let mut candidates = self.extract_candidates_from_unions(unions);

        self.egraph = new_egraph;

        // Run all rules
        let runner = self.mk_runner(self.egraph.clone());
        let rewrites: Vec<&Rewrite<L, SynthAnalysis>> = self
            .prior_rws
            .values()
            .map(|eq| &eq.rewrite)
            .chain(lifting_rewrites.iter())
            .collect();
        let (_, unions, _) = self.run_rewrites(runner, rewrites);
        candidates.extend(self.extract_candidates_from_unions(unions));

        assert!(candidates
            .iter()
            .all(|(_, v)| L::is_allowed_rewrite(&v.lhs, &v.rhs)),);

        candidates
    }

    pub fn run(mut self) -> Report<L> {
        let t = Instant::now();

        let (workload, vars) = self.parse_workload(&self.params.workload);
        println!(
            "enumerated {} terms with {} vars",
            workload.len(),
            vars.len()
        );
        L::initialize_vars(&mut self, vars);
        Synthesizer::add_workload(&mut self.egraph, &workload);

        let candidates = if L::is_rule_lifting() {
            self.run_rule_lifting()
        } else {
            self.run_cvec_synth()
        };

        self.choose_eqs(candidates);

        let num_rules = self.prior_rws.len() + self.new_rws.len();

        let time = t.elapsed().as_secs_f64();

        println!(
            "Learned {} new rewrites in {} using {} prior rewrites",
            self.new_rws.len(),
            time,
            self.prior_rws.len()
        );

        for (name, eq) in &self.new_rws {
            println!("{:?}      {}", eq.score(), name);
        }

        Report {
            time,
            num_rules,
            prior_rws: self.prior_rws.into_iter().map(|(_, eq)| eq).collect(),
            new_rws: self.new_rws.into_iter().map(|(_, eq)| eq).collect(),
        }
    }
}

pub fn synth<L: SynthLanguage>(params: SynthParams<L>) -> Ruleset<L> {
    let syn = Synthesizer::<L>::new(params);
    let report = syn.run();
    Ruleset(report.new_rws)
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
