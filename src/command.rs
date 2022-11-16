use clap::Parser;
use serde::{Deserialize, Serialize};

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
}

/// All parameters for rule synthesis.
#[derive(Parser, Deserialize, Serialize)]
#[clap(rename_all = "kebab-case")]
pub struct DeriveParams {
    /// Output file name
    #[clap(long, default_value = "out.json")]
    pub outfile: String,

    /// Input file name
    #[clap(long)]
    pub in1: String,

    /// Input file name
    #[clap(long)]
    pub in2: String,
}
