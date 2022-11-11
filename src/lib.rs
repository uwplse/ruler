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

/// Faster hashMap implementation used in rustc
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
/// IndexMap data implementation used in rustc
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
        let get_cvec = |id: &Id| &egraph[*id].data.cvec;
        Signature {
            cvec: enode.eval(egraph.analysis.cvec_len, get_cvec),
        }
    }

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            for i in 0..to.cvec.len() {
                match (to.cvec[i].clone(), from.cvec[i].clone()) {
                    (None, Some(_)) => to.cvec[i] = from.cvec[i].clone(),
                    (Some(x), Some(y)) => assert_eq!(x, y, "cvecs do not match!!"),
                    _ => (),
                }
            }
        }

        DidMerge(true, true)
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

    fn score(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> [i32; 5] {
        [0, 0, 0, 0, 0]
    }

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
