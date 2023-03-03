use egg::{AstSize, EClass, Extractor};
use serde_json::*;
use std::fs::*;
use std::{io::Read, io::Write, sync::Arc, time::Duration};

use crate::{
    CVec, EGraph, Equality, ExtractableAstSize, HashMap, Id, IndexMap, Limits, Signature,
    SynthAnalysis, SynthLanguage, ValidationResult,
};

use super::Scheduler;

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

    pub fn to_str_vec(&self) -> Vec<String> {
        match self {
            Ruleset(m) => m.iter().map(|(name, _val)| name.to_string()).collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn bidir_len(&self) -> usize {
        let mut bidir = 0;
        let mut unidir = 0;
        for (_, eq) in &self.0 {
            let reverse = Equality::new(eq.rhs.clone(), eq.lhs.clone());
            if reverse.is_some() && self.contains(&reverse.unwrap()) {
                bidir += 1;
            } else {
                unidir += 1;
            }
        }
        unidir + (bidir / 2)
    }

    pub fn contains(&self, eq: &Equality<L>) -> bool {
        self.0.contains_key(&eq.name)
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

    pub fn pretty_print(&self) {
        let mut strs = vec![];
        for (name, eq) in &self.0 {
            let reverse = Equality::new(eq.rhs.clone(), eq.lhs.clone());
            if reverse.is_some() && self.contains(&reverse.unwrap()) {
                let reverse_name = format!("{} <=> {}", eq.rhs, eq.lhs);
                if !strs.contains(&reverse_name) {
                    strs.push(format!("{} <=> {}", eq.lhs, eq.rhs));
                }
            } else {
                strs.push(name.to_string());
            }
        }

        for s in strs {
            println!("{s}");
        }
    }

    // Writes the ruleset to the json/ subdirectory, to be uploaded to the
    // nightly server. The ruleset is written as a json object with a single
    // field, "rules".
    pub fn write_json_rules(&self, filename: &str) {
        let mut filepath = "rep/json/".to_owned();

        std::fs::create_dir_all(filepath.clone())
            .unwrap_or_else(|e| panic!("Error creating dir: {}", e));

        filepath.push_str(filename);
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(filepath)
            .expect("Unable to open file");
        let rules = json!({
            "rules": &self.to_str_vec(),
        })
        .to_string();
        file.write_all(rules.as_bytes())
            .expect("Unable to write to file");
    }

    // Given two rulesets, computes bidirectional derivability between them
    // and writes the results to the json/derivable_rules/ subdirectory,
    // to be uploaded to the nightly server. The results are formatted as a
    // json object with four fields: "forwards derivable", "forwards underivable",
    // "backwards derivable", and "backwards underivable". Also updates the
    // "output.json" file in the json/ subdirectory with the the derivability results
    // and statistics about runtime and the number of rules in each ruleset.
    pub fn write_json_equiderivability(
        &self,
        baseline: Self,
        name: &str,
        limit: Limits,
        duration: Duration,
    ) {
        let mut filepath = "rep/json/derivable_rules/".to_owned();

        std::fs::create_dir_all(filepath.clone())
            .unwrap_or_else(|e| panic!("Error creating dir: {}", e));

        filepath.push_str(name);
        let mut file = std::fs::File::create(filepath.clone())
            .unwrap_or_else(|_| panic!("Failed to open '{}'", filepath.clone()));

        let (can_f, cannot_f) = self.derive(baseline.clone(), limit);

        let (can_b, cannot_b) = baseline.derive(self.clone(), limit);

        let derivability_results = json!({
            "forwards derivable": &can_f.to_str_vec(),
            "forwards underivable": &cannot_f.to_str_vec(),
            "backwards derivable": &can_b.to_str_vec(),
            "backwards underivable": &cannot_b.to_str_vec(),
        })
        .to_string();

        file.write_all(derivability_results.as_bytes())
            .expect("Unable to write to file");

        let num_rules = &self.len();
        let forwards_derivable = &can_f.len();
        let backwards_derivable = &can_b.len();
        let time = &duration.as_secs();

        let mut outfile = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open("rep/json/output.json")
            .expect("Unable to open file");

        let mut outfile_string = String::new();
        outfile
            .read_to_string(&mut outfile_string)
            .expect("Unable to read file");
        let mut json_arr = vec![];

        if !(outfile_string.is_empty()) {
            json_arr = serde_json::from_str::<Vec<serde_json::Value>>(&outfile_string).unwrap();
        }

        let stats = json!({
            "spec": name,
            "num_rules": num_rules,
            "num_baseline": baseline.len(),
            "enumo_derives_oopsla": forwards_derivable,
            "oopsla_derives_enumo": backwards_derivable,
            "time": time
        });
        json_arr.push(stats);

        let mut file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open("rep/json/output.json")
            .expect("Unable to open file");
        file.write_all("[".as_bytes()).expect("write failed");

        let arr_str = json_arr.to_vec();

        for (object, is_last_element) in arr_str
            .iter()
            .enumerate()
            .map(|(i, w)| (w, i == arr_str.len() - 1))
        {
            file.write_all(object.to_string().as_bytes())
                .expect("write failed");
            if !(is_last_element) {
                file.write_all(", ".as_bytes()).expect("write failed");
            }
        }
        file.write_all("]".as_bytes()).expect("write failed");
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

    pub fn run(
        &self,
        egraph: &EGraph<L, SynthAnalysis>,
        scheduler: Scheduler,
    ) -> EGraph<L, SynthAnalysis> {
        scheduler.run(egraph.clone(), self)
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
                    if e1 != e2 {
                        if let Some(eq) = Equality::from_recexprs(&e1, &e2) {
                            candidates.add(eq)
                        }
                        if let Some(eq) = Equality::from_recexprs(&e2, &e1) {
                            candidates.add(eq)
                        }
                    }
                }
            }
        }

        candidates
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
        let eg_allowed = allowed.run(&eg_init, Scheduler::KStep(limits));

        // Translation rules: grow egraph, extract candidates, assert!(saturated)
        let lifting_rules = L::get_lifting_rules();
        let eg_denote = lifting_rules.run(&eg_allowed, Scheduler::Simple(limits));
        let mut candidates = Self::extract_candidates(&eg_allowed, &eg_denote);

        // All rules: clone/no clone doesn't matter, extract candidates
        let mut all_rules = prior;
        all_rules.extend(lifting_rules);
        let eg_final = all_rules.run(&eg_denote, Scheduler::KStep(limits));
        candidates.extend(Self::extract_candidates(&eg_denote, &eg_final));

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
                    if let Some(eq) = Equality::from_recexprs(&e1, &e2) {
                        candidates.add(eq);
                    }
                    if let Some(eq) = Equality::from_recexprs(&e2, &e1) {
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
                    if let Some(eq) = Equality::from_recexprs(e1, e2) {
                        candidates.add(eq);
                    }
                    if let Some(eq) = Equality::from_recexprs(e2, e1) {
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
                    selected.add(eq.clone());
                }

                // If reverse direction is also in candidates, add it at the same time
                let reverse = Equality::new(eq.rhs, eq.lhs);
                if let Some(reverse) = reverse {
                    if self.contains(&reverse) {
                        if let ValidationResult::Valid = L::validate(&reverse.lhs, &reverse.rhs) {
                            selected.add(reverse);
                        }
                    }
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
        let egraph = chosen.run(&egraph, Scheduler::KStep(limits));

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
            if let Some(eq) = Equality::from_recexprs(&left, &right) {
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
        let scheduler = Scheduler::Saturating(limits);
        let mut egraph: EGraph<L, SynthAnalysis> = Default::default();
        let lexpr = &L::instantiate(&rule.lhs);
        let rexpr = &L::instantiate(&rule.rhs);
        egraph.add_expr(lexpr);
        egraph.add_expr(rexpr);
        let egraph = scheduler.run(egraph, self);

        let l_id = egraph
            .lookup_expr(lexpr)
            .unwrap_or_else(|| panic!("Did not find {}", lexpr));
        let r_id = egraph
            .lookup_expr(rexpr)
            .unwrap_or_else(|| panic!("Did not find {}", rexpr));
        l_id == r_id
    }

    // Use self rules to derive against rules. That is, partition against
    // into derivable / not-derivable with respect to self
    pub fn derive(&self, against: Self, limits: Limits) -> (Self, Self) {
        against.partition(|eq| self.can_derive(eq, limits))
    }
}
