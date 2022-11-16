use std::fs::File;

use crate::*;

pub fn parse<L: SynthLanguage>(filename: &str) -> Vec<Equality<L>> {
    let file = File::open(filename).unwrap_or_else(|_| panic!("Failed to open {}", filename));
    let report: Report<L> = serde_json::from_reader(file).unwrap();
    let mut eqs = vec![];
    eqs.extend(report.prior_rws);
    eqs.extend(report.new_rws);
    eqs
}

pub fn derive<L: SynthLanguage>(params: DeriveParams) {
    let test_rules = parse::<L>(&params.in1);
    let baseline_rules = parse::<L>(&params.in2);
}
