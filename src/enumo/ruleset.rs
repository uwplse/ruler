use egg::{AstSize, EClass, Extractor};
use indexmap::map::{IntoIter, Iter, IterMut, Values, ValuesMut};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use serde_json::*;
use std::fs::*;
use std::sync::Mutex;
use std::time::Instant;
use std::{io::Read, io::Write, sync::Arc, time::Duration};

use crate::{
    CVec, DeriveType, EGraph, ExtractableAstSize, HashMap, Id, IndexMap, Limits, Signature,
    SynthAnalysis, SynthLanguage,
};

use super::{Rule, Scheduler};

#[derive(Clone, Debug)]
pub struct Ruleset<L: SynthLanguage>(pub IndexMap<Arc<str>, Rule<L>>);

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

impl<L: SynthLanguage> IntoIterator for Ruleset<L> {
    type Item = (Arc<str>, Rule<L>);
    type IntoIter = IntoIter<Arc<str>, Rule<L>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, L: SynthLanguage> IntoIterator for &'a Ruleset<L> {
    type Item = (&'a Arc<str>, &'a Rule<L>);
    type IntoIter = Iter<'a, Arc<str>, Rule<L>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, L: SynthLanguage> IntoIterator for &'a mut Ruleset<L> {
    type Item = (&'a Arc<str>, &'a mut Rule<L>);
    type IntoIter = IterMut<'a, Arc<str>, Rule<L>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<L: SynthLanguage> Ruleset<L> {
    pub fn new<I>(vals: I) -> Self
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        let mut map = IndexMap::default();
        for v in vals {
            if let Ok((forwards, backwards)) = Rule::from_string(v.as_ref()) {
                map.insert(forwards.name.clone(), forwards);
                if let Some(backwards) = backwards {
                    map.insert(backwards.name.clone(), backwards);
                }
            }
        }
        Ruleset(map)
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut map = self.0.clone();
        map.extend(other.0.clone());
        Ruleset(map)
    }

    pub fn iter(&self) -> Values<'_, Arc<str>, Rule<L>> {
        self.0.values()
    }

    pub fn iter_mut(&mut self) -> ValuesMut<'_, Arc<str>, Rule<L>> {
        self.0.values_mut()
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
        for (_, rule) in &self.0 {
            let reverse = Rule::new(rule.rhs.clone(), rule.lhs.clone());
            if reverse.is_some() && self.contains(&reverse.unwrap()) {
                bidir += 1;
            } else {
                unidir += 1;
            }
        }
        unidir + (bidir / 2)
    }

    pub fn contains(&self, rule: &Rule<L>) -> bool {
        self.0.contains_key(&rule.name)
    }

    pub fn add(&mut self, rule: Rule<L>) {
        self.0.insert(rule.name.clone(), rule);
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
        F: Fn(&Rule<L>) -> bool + std::marker::Sync,
    {
        let results = Mutex::new((Ruleset::default(), Ruleset::default()));
        let rules: Vec<&Rule<L>> = self.0.values().collect();
        rules.into_par_iter().for_each(|rule| {
            let f_rule = f(rule);
            let mut results = results.lock().unwrap();
            if f_rule {
                results.0.add(rule.clone());
            } else {
                results.1.add(rule.clone());
            }
        });
        results.into_inner().unwrap()
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
        let mut all_rules = IndexMap::default();
        for line in std::io::BufRead::lines(reader) {
            let line = line.unwrap();
            if let Ok((forwards, backwards)) = Rule::from_string(&line) {
                all_rules.insert(forwards.name.clone(), forwards);
                if let Some(backwards) = backwards {
                    all_rules.insert(backwards.name.clone(), backwards);
                }
            }
        }
        Self(all_rules)
    }

    pub fn pretty_print(&self) {
        let mut strs = vec![];
        for (name, rule) in &self.0 {
            let reverse = Rule::new(rule.rhs.clone(), rule.lhs.clone());
            if reverse.is_some() && self.contains(&reverse.unwrap()) {
                let reverse_name = format!("{} <=> {}", rule.rhs, rule.lhs);
                if !strs.contains(&reverse_name) {
                    strs.push(format!("{} <=> {}", rule.lhs, rule.rhs));
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
        let mut filepath = "nightly/json/".to_owned();

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

    pub fn write_baseline_row_big_object(
        &self,
        baseline: Self,
        enumo_name: &str,
        baseline_name: &str,
        limits: Limits,
        time_rules: Duration,
    ) {
        let ((forwards_lhs, backwards_lhs), (lhs_f, lhs_b), results_lhs) =
            self.write_derivability_results_big_object(DeriveType::Lhs, baseline.clone(), limits);
        let ((forwards_lhs_rhs, backwards_lhs_rhs), (lhs_rhs_f, lhs_rhs_b), results_lhs_rhs) = self
            .write_derivability_results_big_object(DeriveType::LhsAndRhs, baseline.clone(), limits);
        let ((forwards_all, backwards_all), (all_f, all_b), results_all) = self
            .write_derivability_results_big_object(DeriveType::AllRules, baseline.clone(), limits);

        let mut outfile = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open("nightly/json/output.json")
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
            "baseline_name": baseline_name,
            "enumo_spec_name": enumo_name,
            "num_rules": self.len(),
            "rules": json!({"rules": self.to_str_vec()}),
            "num_baseline": baseline.len(),
            "time": time_rules.as_secs_f64(),
            "enumo_to_baseline_lhs": results_lhs,
            "enumo_to_baseline_lhs_num": forwards_lhs,
            "enumo_to_baseline_lhs_time": lhs_f.as_secs_f64(),
            "enumo_to_baseline_lhs_rhs": results_lhs_rhs,
            "enumo_to_baseline_lhsrhs_num": forwards_lhs_rhs,
            "enumo_to_baseline_lhsrhs_time": lhs_rhs_f.as_secs_f64(),
            "enumo_to_baseline_all": results_all,
            "enumo_to_baseline_all_num": forwards_all,
            "enumo_to_baseline_all_time": all_f.as_secs_f64(),
            "baseline_to_enumo_lhs_num": backwards_lhs,
            "baseline_to_enumo_lhs_time": lhs_b.as_secs_f64(),
            "baseline_to_enumo_lhsrhs_num": backwards_lhs_rhs,
            "baseline_to_enumo_lhsrhs_time": lhs_rhs_b.as_secs_f64(),
            "baseline_to_enumo_all_num": backwards_all,
            "baseline_to_enumo_all_time": all_b.as_secs_f64(),
            "minimization strategy": "compress",
        });

        json_arr.push(stats);

        let mut file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open("nightly/json/output.json")
            .expect("Unable to open file");

        file.write_all("[".as_bytes()).expect("write failed");

        for (object, is_last_element) in json_arr
            .iter()
            .enumerate()
            .map(|(i, w)| (w, i == json_arr.len() - 1))
        {
            file.write_all(object.to_string().as_bytes())
                .expect("write failed");
            if !(is_last_element) {
                file.write_all(", ".as_bytes()).expect("write failed");
            }
        }
        file.write_all("]".as_bytes()).expect("write failed");
    }

    pub fn write_baseline_row(
        &self,
        baseline: Self,
        enumo_name: &str,
        baseline_name: &str,
        outfile: &str,
        limits: Limits,
        time_rules: Duration,
    ) {
        let ((forwards_lhs, backwards_lhs), (lhs_f, lhs_b)) =
            self.write_derivability_results(DeriveType::Lhs, baseline.clone(), enumo_name, limits);
        let ((forwards_lhs_rhs, backwards_lhs_rhs), (lhs_rhs_f, lhs_rhs_b)) = self
            .write_derivability_results(
                DeriveType::LhsAndRhs,
                baseline.clone(),
                enumo_name,
                limits,
            );
        let ((forwards_all, backwards_all), (all_f, all_b)) = self.write_derivability_results(
            DeriveType::AllRules,
            baseline.clone(),
            enumo_name,
            limits,
        );

        let mut filepath = "nightly/json/".to_owned();
        filepath.push_str(outfile);

        let mut outfile = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(filepath.clone())
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
            "baseline_name": baseline_name,
            "enumo_spec_name": enumo_name,
            "num_rules": self.len(),
            "num_baseline": baseline.len(),
            "time": time_rules.as_secs_f64(),
            "enumo_derives_baseline (lhs, lhs & rhs, all)":
                format!("{}, {}, {}", forwards_lhs, forwards_lhs_rhs, forwards_all),
            "enumo_derives_baseline_time":
                format!("{}, {}, {}", lhs_f.as_secs_f64(), lhs_rhs_f.as_secs_f64(), all_f.as_secs_f64()),
            "baseline_derives_enumo (lhs, lhs & rhs, all)":
                format!("{}, {}, {}", backwards_lhs, backwards_lhs_rhs, backwards_all),
            "baseline_derives_enumo_time":
                format!("{}, {}, {}", lhs_b.as_secs_f64(), lhs_rhs_b.as_secs_f64(), all_b.as_secs_f64()),
            "minimization strategy": "compress",
        });

        json_arr.push(stats);

        let mut file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(filepath)
            .expect("Unable to open file");

        file.write_all("[".as_bytes()).expect("write failed");

        for (object, is_last_element) in json_arr
            .iter()
            .enumerate()
            .map(|(i, w)| (w, i == json_arr.len() - 1))
        {
            file.write_all(object.to_string().as_bytes())
                .expect("write failed");
            if !(is_last_element) {
                file.write_all(", ".as_bytes()).expect("write failed");
            }
        }
        file.write_all("]".as_bytes()).expect("write failed");
    }

    pub fn write_derivability_results(
        &self,
        derive_type: DeriveType,
        baseline: Self,
        name: &str,
        limits: Limits,
    ) -> ((String, String), (Duration, Duration)) {
        let mut filepath = "nightly/json/derivable_rules/".to_owned();

        std::fs::create_dir_all(filepath.clone())
            .unwrap_or_else(|e| panic!("Error creating dir: {}", e));

        filepath.push_str(name);

        match derive_type {
            DeriveType::Lhs => filepath.push_str("_lhs.json"),
            DeriveType::LhsAndRhs => filepath.push_str("_lhs_rhs.json"),
            _ => filepath.push_str("_all.json"),
        }

        let mut file = std::fs::File::create(filepath.clone())
            .unwrap_or_else(|_| panic!("Failed to open '{}'", filepath.clone()));

        let start_f = Instant::now();
        let (can_f, cannot_f) = self.derive(derive_type, &baseline, limits);
        let time_f = start_f.elapsed();
        let start_b = Instant::now();
        let (can_b, cannot_b) = baseline.derive(derive_type, self, limits);
        let time_b = start_b.elapsed();

        let derivability_results = json!({
            "enumo_derives_baseline_derivable": &can_f.to_str_vec(),
            "enumo_derives_baseline_underivable": &cannot_f.to_str_vec(),
            "baseline_derives_enumo_derivable": &can_b.to_str_vec(),
            "baseline_derives_enumo_underivable": &cannot_b.to_str_vec(),
        })
        .to_string();

        file.write_all(derivability_results.as_bytes())
            .expect("Unable to write to file");

        let derivable_ratio_enumo = format!("{}/{}", can_f.len(), baseline.len());
        let derivable_ratio_oopsla = format!("{}/{}", can_b.len(), self.clone().len());

        (
            (derivable_ratio_enumo, derivable_ratio_oopsla),
            (time_f, time_b),
        )
    }

    pub fn write_derivability_results_big_object(
        &self,
        derive_type: DeriveType,
        baseline: Self,
        limits: Limits,
    ) -> ((usize, usize), (Duration, Duration), Value) {
        let start_f = Instant::now();
        let (can_f, cannot_f) = self.derive(derive_type, &baseline, limits);
        let time_f = start_f.elapsed();
        let start_b = Instant::now();
        let (can_b, cannot_b) = baseline.derive(derive_type, self, limits);
        let time_b = start_b.elapsed();

        let derivability_results = json!({
            "enumo_derives_baseline_derivable": &can_f.to_str_vec(),
            "enumo_derives_baseline_underivable": &cannot_f.to_str_vec(),
            "baseline_derives_enumo_derivable": &can_b.to_str_vec(),
            "baseline_derives_enumo_underivable": &cannot_b.to_str_vec(),
        });

        (
            (can_f.len(), can_b.len()),
            (time_f, time_b),
            derivability_results,
        )
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
                        if let Some(rule) = Rule::from_recexprs(&e1, &e2) {
                            candidates.add(rule)
                        }
                        if let Some(rule) = Rule::from_recexprs(&e2, &e1) {
                            candidates.add(rule)
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
        let (allowed, _) = prior.partition(|rule| L::is_allowed_rewrite(&rule.lhs, &rule.rhs));
        let eg_allowed = Scheduler::Compress(limits).run(&eg_init, &allowed);

        // Translation rules: grow egraph, extract candidates, assert!(saturated)
        let lifting_rules = L::get_lifting_rules();
        let eg_denote = Scheduler::Simple(limits).run(&eg_allowed, &lifting_rules);
        let mut candidates = Self::extract_candidates(&eg_allowed, &eg_denote);

        // All rules: clone/no clone doesn't matter, extract candidates
        let mut all_rules = prior;
        all_rules.extend(lifting_rules);
        let eg_final = Scheduler::Compress(limits).run(&eg_denote, &all_rules);
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
                    if let Some(rule) = Rule::from_recexprs(&e1, &e2) {
                        candidates.add(rule);
                    }
                    if let Some(rule) = Rule::from_recexprs(&e2, &e1) {
                        candidates.add(rule);
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
                    if let Some(rule) = Rule::from_recexprs(e1, e2) {
                        candidates.add(rule);
                    }
                    if let Some(rule) = Rule::from_recexprs(e2, e1) {
                        candidates.add(rule);
                    }
                }
            }
        }
        candidates
    }

    fn select(&mut self, step_size: usize) -> Self {
        let mut chosen = Self::default();
        self.0
            .sort_by(|_, rule1, _, rule2| rule1.score().cmp(&rule2.score()));

        // 2. insert step_size best candidates into self.new_rws
        let mut selected: Ruleset<L> = Default::default();
        while selected.len() < step_size {
            let popped = self.0.pop();
            if let Some((_, rule)) = popped {
                if rule.is_valid() {
                    selected.add(rule.clone());
                }

                // If reverse direction is also in candidates, add it at the same time
                let reverse = Rule::new(rule.rhs, rule.lhs);
                if let Some(reverse) = reverse {
                    if self.contains(&reverse) && reverse.is_valid() {
                        selected.add(reverse);
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

    fn shrink(&mut self, chosen: &Self, scheduler: Scheduler) {
        // 1. make new egraph
        // let mut egraph: EGraph<L, SynthAnalysis> = EGraph::default();
        let mut egraph = EGraph::default();

        let mut initial = vec![];
        // 2. insert lhs and rhs of all candidates as roots
        for rule in self.0.values() {
            let lhs = egraph.add_expr(&L::instantiate(&rule.lhs));
            let rhs = egraph.add_expr(&L::instantiate(&rule.rhs));
            initial.push((lhs, rhs, rule.clone()));
        }

        // 3. compress with the rules we've chosen so far
        let egraph = scheduler.run(&egraph, chosen);

        // 4. go through candidates and if they have merged, then
        // they are no longer candidates
        self.0 = Default::default();
        for (l_id, r_id, rule) in initial {
            if egraph.find(l_id) == egraph.find(r_id) {
                // candidate has merged (derivable from other rewrites)
                continue;
            } else {
                self.add(rule);
            }
        }
    }

    pub fn minimize(&mut self, prior: Ruleset<L>, scheduler: Scheduler) -> Self {
        let mut chosen = prior.clone();
        let step_size = 1;
        while !self.is_empty() {
            let selected = self.select(step_size);
            chosen.extend(selected.clone());
            self.shrink(&chosen, scheduler);
        }
        // Return only the new rules
        chosen.remove_all(prior);

        chosen
    }

    pub fn can_derive(
        &self,
        derive_type: DeriveType,
        rule: &Rule<L>,
        allrules: &Self,
        limits: Limits,
    ) -> bool {
        let scheduler = Scheduler::Saturating(limits);
        let mut egraph: EGraph<L, SynthAnalysis> = Default::default();
        let lexpr = &L::instantiate(&rule.lhs);
        let rexpr = &L::instantiate(&rule.rhs);

        match derive_type {
            DeriveType::Lhs => {
                egraph.add_expr(lexpr);
            }
            DeriveType::LhsAndRhs => {
                egraph.add_expr(lexpr);
                egraph.add_expr(rexpr);
            }
            DeriveType::AllRules => {
                for rule in allrules.0.values() {
                    let lhs = &L::instantiate(&rule.lhs);
                    let rhs = &L::instantiate(&rule.rhs);
                    egraph.add_expr(lhs);
                    egraph.add_expr(rhs);
                }
            }
        }

        let out_egraph = scheduler.run(&egraph, self);

        let l_id = out_egraph
            .lookup_expr(lexpr)
            .unwrap_or_else(|| panic!("Did not find {}", lexpr));
        let r_id = out_egraph.lookup_expr(rexpr);
        if let Some(r_id) = r_id {
            l_id == r_id
        } else {
            false
        }
    }

    // Use self rules to derive against rules. That is, partition against
    // into derivable / not-derivable with respect to self
    pub fn derive(&self, derive_type: DeriveType, against: &Self, limits: Limits) -> (Self, Self) {
        against.partition(|rule| self.can_derive(derive_type, rule, against, limits))
    }

    pub fn print_derive(derive_type: DeriveType, one: &str, two: &str) {
        let r1: Ruleset<L> = Ruleset::from_file(one);
        let r2: Ruleset<L> = Ruleset::from_file(two);

        let (can, cannot) = r1.derive(derive_type, &r2, Limits::default());
        println!(
            "Using {} ({}) to derive {} ({}).\nCan derive {}, cannot derive {}. Missing:",
            one,
            r1.len(),
            two,
            r2.len(),
            can.len(),
            cannot.len()
        );
        cannot.pretty_print();
    }
}
