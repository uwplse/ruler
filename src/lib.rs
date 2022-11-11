use clap::Parser;
use egg::*;

use serde::{Deserialize, Serialize};
use std::{
    fmt::Debug,
    fs::File,
    hash::BuildHasherDefault,
    io::{BufRead, BufReader},
    sync::Arc,
    time::Instant,
};

pub use equality::*;
pub use language::*;

mod equality;
mod language;

/// Validation result
#[derive(Debug, Clone)]
pub enum ValidationResult {
    Valid,
    Invalid,
    Unknown,
}

/// Faster hashMap implementation used in rustc
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
/// IndexMap data implementation used in rustc
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<rustc_hash::FxHasher>>;

pub type EqualityMap<L> = IndexMap<Arc<str>, Equality<L>>;

pub struct Synthesizer<L: SynthLanguage> {
    pub params: SynthParams,
    pub egraph: EGraph<L, SynthAnalysis>,
    pub prior_rws: EqualityMap<L>,
    pub new_rws: EqualityMap<L>,
}

impl<L: SynthLanguage> Synthesizer<L> {
    fn new(params: SynthParams) -> Self {
        let mut priors: EqualityMap<L> = Default::default();
        // TODO: parse prior rules

        Self {
            params,
            egraph: Default::default(),
            prior_rws: priors,
            new_rws: Default::default(),
        }
    }

    fn enumerate_workload(&self, filename: &str) -> Vec<RecExpr<L>> {
        let infile = File::open(filename).expect("can't open file");
        let reader = BufReader::new(infile);
        let mut terms = vec![];
        for line in BufRead::lines(reader) {
            terms.push(line.unwrap().parse().unwrap());
        }
        terms
    }

    fn add_workload(egraph: &mut EGraph<L, SynthAnalysis>, exprs: &[RecExpr<L>]) {
        for expr in exprs {
            egraph.add_expr(expr);
        }
    }

    fn mk_runner(&self, mut egraph: EGraph<L, SynthAnalysis>) -> Runner<L, SynthAnalysis, ()> {
        Runner::default().with_egraph(egraph)
    }

    fn run_rewrites(
        &mut self,
        rewrites: Vec<&Rewrite<L, SynthAnalysis>>,
    ) -> EGraph<L, SynthAnalysis> {
        println!("run_rewrites");
        let starting_ids = self.egraph.classes().map(|c| c.id);

        let mut runner = self.mk_runner(self.egraph.clone());
        runner = runner.run(rewrites.clone());

        println!(
            "Done running {} rewrites. Stop reason: {:?}",
            rewrites.len(),
            runner.stop_reason.unwrap()
        );

        println!("New egraph size: {}", runner.egraph.number_of_classes());
        let mut found_unions = HashMap::default();
        for id in starting_ids {
            let new_id = runner.egraph.find(id);
            found_unions
                .entry(new_id)
                .or_insert_with(Vec::default)
                .push(id);
        }
        for ids in found_unions.values() {
            if ids.len() > 1 {
                let first = ids[0];
                for id in &ids[1..] {
                    self.egraph.union(first, *id);
                }
            }
        }
        runner.egraph.rebuild();
        runner.egraph
    }

    pub fn run(mut self) -> Report<L> {
        println!("run");
        let t = Instant::now();

        let filename = self.params.workload.clone().expect("workload is required");
        let workload = self.enumerate_workload(&filename);
        println!("enumerated {} terms", workload.len());

        let time = t.elapsed().as_secs_f64();
        let num_rules = self.new_rws.len();
        println!("{} prior rules", num_rules);
        let mut new_rws: Vec<Equality<L>> =
            self.new_rws.clone().into_iter().map(|(_, eq)| eq).collect();

        Synthesizer::add_workload(&mut self.egraph, &workload);

        println!(
            "Added workload to egraph. {} eclasses",
            self.egraph.number_of_classes()
        );

        let eqs = self.prior_rws.clone();

        self.run_rewrites(eqs.values().map(|eq| &eq.rewrite).collect());

        println!("done");

        Report {
            params: self.params,
            time,
            num_rules,
            prior_rws: self
                .prior_rws
                .clone()
                .into_iter()
                .map(|(_, eq)| eq)
                .collect(),
            new_rws,
        }
    }
}

#[derive(Parser)]
#[clap(rename_all = "kebab-case")]
pub enum Command {
    Synth(SynthParams),
}

/// All parameters for rule synthesis.
#[derive(Parser, Deserialize, Serialize)]
#[clap(rename_all = "kebab-case")]
pub struct SynthParams {
    /// Output file name
    #[clap(long, default_value = "out.json")]
    pub outfile: String,

    #[clap(long)]
    pub prior_rules: Option<String>,

    #[clap(long)]
    pub workload: Option<String>,
}

#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
pub struct Report<L: SynthLanguage> {
    pub params: SynthParams,
    pub time: f64,
    pub num_rules: usize,
    pub prior_rws: Vec<Equality<L>>,
    pub new_rws: Vec<Equality<L>>,
}
