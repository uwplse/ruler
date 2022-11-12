use clap::Parser;
use egg::*;

use rand::SeedableRng;
use rand_pcg::Pcg64;
use serde::{Deserialize, Serialize};
use std::{
    fmt::Debug,
    fs::File,
    hash::BuildHasherDefault,
    io::{BufRead, BufReader},
    sync::Arc,
    time::{Duration, Instant},
};

pub use equality::*;
pub use language::*;
pub use util::*;

mod equality;
mod language;
mod util;

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
    pub params: SynthParams,
    pub rng: Pcg64,
    pub egraph: EGraph<L, SynthAnalysis>,
    pub prior_rws: EqualityMap<L>,
    pub new_rws: EqualityMap<L>,
}

impl<L: SynthLanguage> Synthesizer<L> {
    fn new(params: SynthParams) -> Self {
        let mut priors: EqualityMap<L> = Default::default();
        if let Some(filename) = params.prior_rules.clone() {
            let file =
                File::open(&filename).unwrap_or_else(|_| panic!("Failed to open {}", filename));
            let report: SlimReport<L> = serde_json::from_reader(file).unwrap();
            for eq in report.rewrites {
                println!("{}", eq.name);
                priors.insert(eq.name.clone(), eq);
            }
        }
        println!("Parsed {} prior rules", priors.len());

        Self {
            params,
            rng: Pcg64::seed_from_u64(0), // TODO- parameterize?
            egraph: Default::default(),
            prior_rws: priors,
            new_rws: Default::default(),
        }
    }

    fn enumerate_workload(&self, filename: &str) -> (Vec<RecExpr<L>>, Vec<String>) {
        let infile = File::open(filename).expect("can't open file");
        let reader = BufReader::new(infile);
        let mut terms = vec![];
        let mut vars: HashSet<String> = HashSet::default();
        for line in BufRead::lines(reader) {
            let expr: RecExpr<L> = line.unwrap().parse().unwrap();
            for node in expr.as_ref() {
                if let ENodeOrVar::Var(v) = node.clone().to_enode_or_var() {
                    vars.extend(vec![v.to_string()]);
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

    fn mk_runner(&self, mut egraph: EGraph<L, SynthAnalysis>) -> Runner<L, SynthAnalysis, ()> {
        Runner::default()
            .with_scheduler(SimpleScheduler)
            .with_node_limit(1000)
            .with_iter_limit(2)
            .with_time_limit(Duration::from_secs(2))
            .with_egraph(egraph)
    }

    fn run_rewrites(
        &mut self,
        rewrites: Vec<&Rewrite<L, SynthAnalysis>>,
    ) -> EGraph<L, SynthAnalysis> {
        println!("running {} rewrites", rewrites.len());
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

    fn cvec_match(&self) -> EqualityMap<L> {
        let mut by_cvec: IndexMap<&CVec<L>, Vec<Id>> = IndexMap::default();

        for class in self.egraph.classes() {
            if class.data.is_defined() {
                by_cvec.entry(&class.data.cvec).or_default().push(class.id);
            }
        }

        println!("{} unique cvecs", by_cvec.len());

        let mut candidates = EqualityMap::default();
        let extract = Extractor::new(&self.egraph, AstSize);
        for ids in by_cvec.values() {
            let mut terms_ids: Vec<_> = ids.iter().map(|&id| (extract.find_best(id), id)).collect();
            terms_ids.sort_by_key(|x| x.0 .0); // sort by cost
            let ((c1, e1), id1) = terms_ids.remove(0);
            for ((c2, e2), id2) in terms_ids {
                if let Some(eq) = Equality::new(&e1, &e2) {
                    candidates.insert(eq.name.clone(), eq);
                }
            }
        }
        candidates
    }

    pub fn run(mut self) -> Report<L> {
        println!("run");
        let t = Instant::now();

        let time = t.elapsed().as_secs_f64();

        let filename = self.params.workload.clone().expect("workload is required");
        let (workload, vars) = self.enumerate_workload(&filename);
        println!("enumerated {} terms", workload.len());
        L::initialize_vars(&mut self, vars);
        Synthesizer::add_workload(&mut self.egraph, &workload);

        println!(
            "Added workload to egraph. {} eclasses",
            self.egraph.number_of_classes()
        );

        let eqs = self.prior_rws.clone();

        self.run_rewrites(eqs.values().map(|eq| &eq.rewrite).collect());

        let candidates = self.cvec_match();
        println!("{} candidates", candidates.len());

        for v in candidates.values() {
            println!("{} => {}", v.lhs, v.rhs);
        }

        let new_rws = vec![];
        let num_rules = self.prior_rws.len() + self.new_rws.len();

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

#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
struct SlimReport<L: SynthLanguage> {
    rewrites: Vec<Equality<L>>,
}
