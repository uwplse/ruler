use clap::Parser;
use serde::{Deserialize, Serialize};

pub use bv::*;
pub use derive::*;
pub use equality::*;
pub use interval::*;
pub use language::*;
pub use synth::*;
pub use util::*;

mod bv;
mod derive;
mod enumo;
mod equality;
mod interval;
mod language;
mod synth;
mod util;

pub type Id = egg::Id;
pub type Symbol = egg::Symbol;
pub type Var = egg::Var;
pub type EGraph<L, N> = egg::EGraph<L, N>;
pub type Pattern<L> = egg::Pattern<L>;

#[derive(Parser)]
#[clap(rename_all = "kebab-case")]
pub enum Command {
    Synth(SynthParams),
    Derive(DeriveParams),
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

    ////////////////
    // eqsat args //
    ////////////////
    /// node limit for all the eqsats
    #[clap(long, default_value = "300000")]
    pub node_limit: usize,
    /// iter limit for all the eqsats
    #[clap(long, default_value = "2")]
    pub iter_limit: usize,
    /// time limit (seconds) for all the eqsats
    #[clap(long, default_value = "60")]
    pub time_limit: u64,
}

/// All parameters for rule synthesis.
#[derive(Parser, Deserialize, Serialize)]
#[clap(rename_all = "kebab-case")]
pub struct DeriveParams {
    in1: String,
    in2: String,
    /// Output file name
    #[clap(long, default_value = "out.json")]
    outfile: String,

    #[clap(long, default_value = "10")]
    iter_limit: usize,

    #[clap(long)]
    ci: bool,
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
