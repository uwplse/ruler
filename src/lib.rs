use clap::Parser;
use egg::*;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{
    fmt::{Debug, Display},
    fs::File,
    hash::{BuildHasherDefault, Hash},
    io::{BufRead, BufReader, LineWriter},
    sync::Arc,
    time::Instant,
};

pub use equality::*;

mod equality;

/// Validation result
#[derive(Debug, Clone)]
pub enum ValidationResult {
    Valid,
    Invalid,
    Unknown,
}

pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<rustc_hash::FxHasher>>;
pub type EqualityMap<L> = IndexMap<Arc<str>, Equality<L>>;

#[derive(Clone)]
pub struct SynthAnalysis {
    pub cvec_len: usize,
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self { cvec_len: 10 }
    }
}

#[derive(Debug, Clone)]
pub struct Signature<L: SynthLanguage> {
    pub cvec: CVec<L>,
}

impl<L: SynthLanguage> egg::Analysis<L> for SynthAnalysis {
    type Data = Signature<L>;

    fn make(egraph: &EGraph<L, Self>, enode: &L) -> Self::Data {
        todo!()
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> DidMerge {
        todo!()
    }
}

pub type CVec<L> = Vec<Option<<L as SynthLanguage>::Constant>>;

pub trait SynthLanguage: egg::Language + Send + Sync + Display + FromOp + 'static {
    type Constant: Clone + Hash + Eq + Debug + Display + Ord;

    fn eval<'a, F>(&'a self, cvec_len: usize, f: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>;

    fn mk_var(sym: egg::Symbol) -> Self;

    fn instantiate(pattern: &Pattern<Self>) -> RecExpr<Self> {
        let nodes: Vec<_> = pattern
            .ast
            .as_ref()
            .iter()
            .map(|n| match n {
                ENodeOrVar::ENode(n) => n.clone(),
                ENodeOrVar::Var(v) => {
                    let s = v.to_string();
                    assert!(s.starts_with("?"));
                    Self::mk_var(s[1..].into())
                }
            })
            .collect();

        RecExpr::from(nodes)
    }

    fn score(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> [i32; 5];

    fn init_synth(synth: &mut Synthesizer<Self>);

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult;

    fn run_synth() {
        match Command::parse() {
            Command::Synth(params) => {
                let outfile = params.outfile.clone();
                let syn = Synthesizer::<Self>::new(params);
                let report = syn.run();
                let file = std::fs::File::create(&outfile)
                    .unwrap_or_else(|_| panic!("Failed to open '{}'", outfile));
                serde_json::to_writer_pretty(file, &report).expect("failed to write json");
            }
        }
    }
}

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

    pub fn run(mut self) -> Report<L> {
        let t = Instant::now();

        if let Some(filename) = &self.params.workload {
            let terms = self.enumerate_workload(filename);
            // self.run_cvec_synth(terms);
        } else {
            panic!("No workload");
        }

        let time = t.elapsed().as_secs_f64();
        let num_rules = self.new_rws.len();
        let mut new_rws: Vec<Equality<L>> =
            self.new_rws.clone().into_iter().map(|(_, eq)| eq).collect();

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
