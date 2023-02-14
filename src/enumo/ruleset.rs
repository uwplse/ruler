use std::{io::Write, sync::Arc, time::Duration};

use egg::{AstSize, EClass, Extractor, Rewrite, Runner, StopReason};

use crate::{
    CVec, EGraph, Equality, ExtractableAstSize, HashMap, Id, IndexMap, Limits, Signature,
    SynthAnalysis, SynthLanguage, ValidationResult,
};

use super::Workload;

#[derive(Clone, Debug)]
pub struct Ruleset<L: SynthLanguage>(pub IndexMap<Arc<str>, Equality<L>>);

impl<L: SynthLanguage> PartialEq for Ruleset<L> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for ((name1, _), (name2, _)) in self.0.iter().zip(other.0.iter()) {
            if name1 != name2 {
                return false;
            }
        }
        true
    }
}

impl<L: SynthLanguage> Default for Ruleset<L> {
    fn default() -> Self {
        Self(IndexMap::default())
    }
}

impl<L: SynthLanguage> Ruleset<L> {
    pub fn from_str_vec(ss: &[&str]) -> Self {
        let mut map = IndexMap::default();
        let eqs: Vec<Equality<L>> = ss.iter().map(|s| s.parse().unwrap()).collect();
        for eq in eqs {
            map.insert(eq.name.clone(), eq);
        }
        Ruleset(map)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn add(&mut self, eq: Equality<L>) {
        self.0.insert(eq.name.clone(), eq);
    }

    pub fn remove_all(&mut self, other: Self) {
        for (name, _) in other.0 {
            self.0.remove(&name);
        }
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn partition<F>(&self, f: F) -> (Self, Self)
    where
        F: Fn(&Equality<L>) -> bool,
    {
        let mut ins = Ruleset::default();
        let mut outs = Ruleset::default();
        for (_, eq) in &self.0 {
            if f(eq) {
                ins.add(eq.clone());
            } else {
                outs.add(eq.clone());
            }
        }
        (ins, outs)
    }

    pub fn to_file(&self, filename: &str) {
        let mut file = std::fs::File::create(filename)
            .unwrap_or_else(|_| panic!("Failed to open '{}'", filename));
        for (name, _) in &self.0 {
            writeln!(file, "{}", name).expect("Unable to write");
        }
    }

    pub fn from_file(filename: &str) -> Self {
        let infile = std::fs::File::open(filename).expect("can't open file");
        let reader = std::io::BufReader::new(infile);
        let mut eqs = IndexMap::default();
        for line in std::io::BufRead::lines(reader) {
            let line = line.unwrap();
            let eq = line.parse::<Equality<L>>().unwrap();
            eqs.insert(eq.name.clone(), eq);
        }
        Self(eqs)
    }

    fn from_unions(
        egraph: &EGraph<L, SynthAnalysis>,
        unions: HashMap<Id, Vec<Id>>,
        prior: &Self,
    ) -> Self {
        let mut candidates = Ruleset::default();
        let clone = egraph.clone();
        let extract = Extractor::new(&clone, ExtractableAstSize);
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
                            if prior.0.contains_key(&eq.name) {
                                // We already have this rule
                                continue;
                            }
                            candidates.add(eq)
                        }
                    }
                }
            }
        }

        candidates
    }

    pub fn apply_unions(egraph: &mut EGraph<L, SynthAnalysis>, unions: HashMap<Id, Vec<Id>>) {
        for ids in unions.values() {
            if ids.len() > 1 {
                let first = ids[0];
                for id in &ids[1..] {
                    egraph.union(first, *id);
                }
            }
        }
        egraph.rebuild();
    }

    pub fn compress(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        limits: Limits,
    ) -> EGraph<L, SynthAnalysis> {
        let mut clone = egraph.clone();
        let ids: Vec<Id> = egraph.classes().map(|c| c.id).collect();

        let (out_egraph, _) = self.run_internal(egraph.clone(), limits);

        // Build a map from id in out_graph to all of the ids in egraph that are equivalent
        let mut unions = HashMap::default();
        for id in ids {
            let new_id = out_egraph.find(id);
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

    pub fn crunch(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        limits: Limits,
    ) -> EGraph<L, SynthAnalysis> {
        let (new_egraph, _) = self.run_internal(egraph.clone(), limits);
        new_egraph
    }

    fn run_internal(
        &self,
        egraph: EGraph<L, SynthAnalysis>,
        limits: Limits,
    ) -> (EGraph<L, SynthAnalysis>, StopReason) {
        let mut runner = Ruleset::mk_runner(egraph, limits);
        let rewrites: Vec<&Rewrite<L, SynthAnalysis>> =
            self.0.values().map(|eq| &eq.rewrite).collect();

        runner = runner.run(rewrites);

        runner.egraph.rebuild();
        (runner.egraph, runner.stop_reason.unwrap())
    }

    pub fn extract_candidates(
        eg1: &EGraph<L, SynthAnalysis>,
        eg2: &EGraph<L, SynthAnalysis>,
    ) -> Self {
        let mut candidates = Ruleset::default();
        let ids: Vec<Id> = eg1.classes().map(|c| c.id).collect();
        let mut unions = HashMap::default();
        for id in ids {
            let new_id = eg2.find(id);
            unions.entry(new_id).or_insert_with(Vec::new).push(id);
        }

        let clone = eg1.clone();
        let extract = Extractor::new(&clone, ExtractableAstSize);

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
                            candidates.add(eq)
                        }
                    }
                }
            }
        }

        candidates
    }

    fn mk_runner(egraph: EGraph<L, SynthAnalysis>, limits: Limits) -> Runner<L, SynthAnalysis> {
        Runner::default()
            .with_scheduler(egg::SimpleScheduler)
            .with_node_limit(limits.node)
            .with_iter_limit(limits.iter)
            // Egg default time limit is 5 seconds. Bump up to 10 minutes to reduce variance due to timing
            .with_time_limit(Duration::from_secs(600))
            .with_egraph(egraph)
    }

    pub fn allow_forbid_actual(
        egraph: EGraph<L, SynthAnalysis>,
        prior: Ruleset<L>,
        limits: Limits,
    ) -> Self {
        /*
         * eg_init ┌─────────┐ eg_allowed ┌────────┐ eg_denote ┌────────┐  eg_final
         * ───────►│ allowed ├───────────►│ denote ├──────────►│  all   ├────────►
         *         └─────────┘            └────────┘           └────────┘
         */

        let eg_init = egraph;
        // Allowed rules: run on clone, apply unions, no candidates
        let (allowed, _) = prior.partition(|eq| L::is_allowed_rewrite(&eq.lhs, &eq.rhs));
        let eg_allowed = allowed.compress(&eg_init, limits);

        // Translation rules: grow egraph, extract candidates, assert!(saturated)
        let lifting_rules = L::get_lifting_rules();
        let eg_denote = lifting_rules.crunch(&eg_allowed, limits);
        let mut candidates = Self::extract_candidates(&eg_allowed, &eg_denote);

        // All rules: clone/no clone doesn't matter, extract candidates
        let mut all_rules = prior;
        all_rules.extend(lifting_rules);
        let eg_final = all_rules.compress(&eg_denote, limits);
        candidates.extend(Self::extract_candidates(&eg_denote, &eg_final));

        candidates
    }

    fn compress_egraph(
        &self,
        egraph: EGraph<L, SynthAnalysis>,
        limits: Limits,
    ) -> (EGraph<L, SynthAnalysis>, HashMap<Id, Vec<Id>>, StopReason) {
        let mut runner = Ruleset::mk_runner(egraph, limits);
        let ids: Vec<Id> = runner.egraph.classes().map(|c| c.id).collect();
        let rewrites: Vec<&Rewrite<L, SynthAnalysis>> =
            self.0.values().map(|eq| &eq.rewrite).collect();
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

    pub fn compress_workload(
        &self,
        workload: Workload,
        limits: Limits,
    ) -> EGraph<L, SynthAnalysis> {
        let mut egraph = workload.to_egraph();
        let (_, unions, _) = self.compress_egraph(egraph.clone(), limits);
        Self::apply_unions(&mut egraph, unions);
        egraph
    }

    pub fn lift_rules(
        egraph: &mut EGraph<L, SynthAnalysis>,
        prior: Ruleset<L>,
        limits: Limits,
    ) -> Self {
        // 1. Compress egraph using allowed rules
        let (allowed, _) = prior.partition(|eq| L::is_allowed_rewrite(&eq.lhs, &eq.rhs));

        let (_, unions, _) = allowed.compress_egraph(egraph.clone(), limits);
        Self::apply_unions(egraph, unions);

        // 2a. Run lifting rules to saturation
        let lifting_rules = L::get_lifting_rules();
        println!("Running {} lifting rules", lifting_rules.len());
        let (new_egraph, unions, stop_reason) = lifting_rules.compress_egraph(
            egraph.clone(),
            Limits {
                iter: usize::MAX,
                node: usize::MAX,
            },
        );
        assert!(
            matches!(stop_reason, StopReason::Saturated),
            "lifting rules must saturate. Instead, ended due to {:?}",
            stop_reason
        );
        // 2b. Extract candidates from unions
        let mut candidates = Self::from_unions(egraph, unions, &prior);

        // 3a. Run all rules
        let mut all_rules = prior.clone();
        all_rules.extend(lifting_rules);
        println!("Running all {} rules", all_rules.len());
        let (_, unions, _) = all_rules.compress_egraph(new_egraph.clone(), limits);
        // 3b. Extract candidates from unions
        candidates.extend(Self::from_unions(&new_egraph, unions, &prior));

        assert!(candidates
            .0
            .iter()
            .all(|(_, v)| L::is_allowed_rewrite(&v.lhs, &v.rhs)),);

        println!("{} candidates", candidates.len());
        candidates
    }

    pub fn cvec_match(egraph: &EGraph<L, SynthAnalysis>) -> Self {
        // cvecs [𝑎1, . . . , 𝑎𝑛] and [𝑏1, . . . , 𝑏𝑛] match iff:
        // ∀𝑖. 𝑎𝑖 = 𝑏𝑖 ∨ 𝑎𝑖 = null ∨ 𝑏𝑖 = null and ∃𝑖. 𝑎𝑖 = 𝑏𝑖 ∧ 𝑎𝑖 ≠ null ∧ 𝑏𝑖 ≠ null

        println!(
            "starting cvec match with {} eclasses",
            egraph.number_of_classes()
        );

        let not_all_none: Vec<&EClass<L, Signature<L>>> = egraph
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
        let mut candidates = Ruleset::default();
        let extract = Extractor::new(egraph, AstSize);
        for class1 in &not_all_none {
            for class2 in &not_all_none {
                if class1.id == class2.id {
                    continue;
                }
                if compare(&class1.data.cvec, &class2.data.cvec) {
                    let (_, e1) = extract.find_best(class1.id);
                    let (_, e2) = extract.find_best(class2.id);
                    if let Some(eq) = Equality::new(&e1, &e2) {
                        candidates.add(eq);
                    }
                    if let Some(eq) = Equality::new(&e2, &e1) {
                        candidates.add(eq);
                    }
                }
            }
        }
        candidates
    }

    // TODO: Figure out what to do with this- it doesn't match the definition
    // of cvec matching from the paper, but it is faster.
    pub fn fast_cvec_match(egraph: &EGraph<L, SynthAnalysis>) -> Ruleset<L> {
        let mut by_cvec: IndexMap<&CVec<L>, Vec<Id>> = IndexMap::default();

        for class in egraph.classes() {
            if class.data.is_defined() {
                by_cvec.entry(&class.data.cvec).or_default().push(class.id);
            }
        }

        let mut candidates = Ruleset::default();
        let extract = Extractor::new(egraph, AstSize);

        for ids in by_cvec.values() {
            let exprs: Vec<_> = ids.iter().map(|&id| extract.find_best(id).1).collect();

            for (idx, e1) in exprs.iter().enumerate() {
                for e2 in exprs[(idx + 1)..].iter() {
                    if let Some(eq) = Equality::new(e1, e2) {
                        candidates.add(eq);
                    }
                    if let Some(eq) = Equality::new(e2, e1) {
                        candidates.add(eq);
                    }
                }
            }
        }
        candidates
    }

    fn select(&mut self, step_size: usize) -> Self {
        let mut chosen = Self::default();
        self.0
            .sort_by(|_, eq1, _, eq2| eq1.score().cmp(&eq2.score()));

        // 2. insert step_size best candidates into self.new_rws
        let mut selected: Ruleset<L> = Default::default();
        while selected.len() < step_size {
            let popped = self.0.pop();
            if let Some((_, eq)) = popped {
                if let ValidationResult::Valid = L::validate(&eq.lhs, &eq.rhs) {
                    selected.add(eq);
                }
            } else {
                break;
            }
        }
        chosen.extend(selected);

        // 3. return chosen candidates
        chosen
    }

    fn shrink(&mut self, chosen: &Self, limits: Limits) {
        // 1. make new egraph
        // let mut egraph: EGraph<L, SynthAnalysis> = EGraph::default();
        let mut egraph = EGraph::default();

        let mut initial = vec![];
        // 2. insert lhs and rhs of all candidates as roots
        for eq in self.0.values() {
            let lhs = egraph.add_expr(&L::instantiate(&eq.lhs));
            let rhs = egraph.add_expr(&L::instantiate(&eq.rhs));
            initial.push((lhs, rhs));
        }

        // 3. compress with the rules we've chosen so far
        (egraph, _, _) = chosen.compress_egraph(egraph, limits);
        // let egraph = chosen.compress(&egraph, limits);

        // 4. go through candidates and if they have merged, then
        // they are no longer candidates
        let extract = Extractor::new(&egraph, AstSize);
        self.0 = Default::default();
        for (l_id, r_id) in initial {
            if egraph.find(l_id) == egraph.find(r_id) {
                // candidate has merged (derivable from other rewrites)
                continue;
            }
            let (_, left) = extract.find_best(l_id);
            let (_, right) = extract.find_best(r_id);
            if let Some(eq) = Equality::new(&left, &right) {
                self.add(eq);
            }
        }
    }

    pub fn minimize(&mut self, prior: Ruleset<L>, limits: Limits) -> Self {
        let mut chosen = prior.clone();
        let step_size = 1;
        while !self.is_empty() {
            let selected = self.select(step_size);
            chosen.extend(selected.clone());
            self.shrink(&chosen, limits);
        }
        // Return only the new rules
        chosen.remove_all(prior);

        chosen
    }

    pub fn can_derive(&self, rule: &Equality<L>, limits: Limits) -> bool {
        let mk_runner = |egraph: EGraph<L, SynthAnalysis>, limits: Limits| {
            Runner::default()
                .with_scheduler(egg::SimpleScheduler)
                .with_iter_limit(limits.iter)
                .with_node_limit(limits.node)
                .with_time_limit(Duration::from_secs(600))
                .with_egraph(egraph)
                .with_expr(&L::instantiate(&rule.lhs))
                .with_expr(&L::instantiate(&rule.rhs))
                .with_hook(|r| {
                    if r.egraph.find(r.roots[0]) == r.egraph.find(r.roots[1]) {
                        Err("Done".to_owned())
                    } else {
                        Ok(())
                    }
                })
        };
        let (sat, other) = self.partition(|eq| eq.is_saturating());
        let (sat, other): (Vec<Rewrite<_, _>>, Vec<Rewrite<_, _>>) = (
            (sat.0.iter().map(|(_, eq)| eq.rewrite.clone()).collect()),
            (other.0.iter().map(|(_, eq)| eq.rewrite.clone()).collect()),
        );

        let mut runner: Runner<L, SynthAnalysis> = mk_runner(Default::default(), limits);

        let mut l_id;
        let mut r_id;

        for _ in 0..limits.iter {
            // Sat
            runner = mk_runner(runner.egraph, Limits::max()).run(&sat);

            l_id = runner.egraph.find(runner.roots[0]);
            r_id = runner.egraph.find(runner.roots[1]);

            if l_id == r_id {
                break;
            }

            // Other
            runner = mk_runner(
                runner.egraph,
                Limits {
                    iter: 1,
                    node: limits.node,
                },
            )
            .run(&other);

            l_id = runner.egraph.find(runner.roots[0]);
            r_id = runner.egraph.find(runner.roots[1]);

            if l_id == r_id {
                break;
            }
        }

        // One more sat
        runner = mk_runner(runner.egraph, Limits::max()).run(&sat);
        l_id = runner.egraph.find(runner.roots[0]);
        r_id = runner.egraph.find(runner.roots[1]);

        l_id == r_id
    }

    // Use self rules to derive against rules. That is, partition against
    // into derivable / not-derivable with respect to self
    pub fn derive(&self, against: Self, limits: Limits) -> (Self, Self) {
        against.partition(|eq| self.can_derive(eq, limits))
    }
}
