use std::time::Instant;

use crate::{
    enumo::{Filter, Metric, Ruleset, Scheduler, Workload},
    Limits, SynthLanguage,
};

pub fn run_workload<L: SynthLanguage>(
    workload: Workload,
    prior: Ruleset<L>,
    limits: Limits,
    fast_match: bool,
) -> Ruleset<L> {
    let t = Instant::now();

    let egraph = workload.to_egraph::<L>();
    let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

    let mut candidates = if fast_match {
        Ruleset::fast_cvec_match(&compressed)
    } else {
        Ruleset::cvec_match(&compressed)
    };

    let num_prior = prior.len();
    let chosen = candidates.minimize(prior, Scheduler::Compress(limits));
    let time = t.elapsed().as_secs_f64();

    println!(
        "Learned {} bidirectional rewrites ({} total rewrites) in {} using {} prior rewrites",
        chosen.bidir_len(),
        chosen.len(),
        time,
        num_prior
    );

    chosen.pretty_print();

    chosen
}

pub fn run_rule_lifting<L: SynthLanguage>(
    workload: Workload,
    prior: Ruleset<L>,
    limits: Limits,
) -> Ruleset<L> {
    let t = Instant::now();

    let egraph = workload.to_egraph::<L>();
    let num_prior = prior.len();
    let mut candidates = Ruleset::allow_forbid_actual(egraph, prior.clone(), limits);

    let chosen = candidates.minimize(prior, Scheduler::Compress(limits));
    let time = t.elapsed().as_secs_f64();

    println!(
        "Learned {} bidirectional rewrites ({} total rewrites) in {} using {} prior rewrites",
        chosen.bidir_len(),
        chosen.len(),
        time,
        num_prior
    );

    chosen.pretty_print();

    chosen
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Lang {
    pub consts: Vec<String>,
    vars: Vec<String>,
    uops: Vec<String>,
    bops: Vec<String>,
    tops: Vec<String>,
}

impl Lang {
    pub fn new(
        consts: &[&str],
        vars: &[&str],
        uops: &[&str],
        bops: &[&str],
        tops: &[&str],
    ) -> Self {
        Self {
            consts: consts.iter().map(|x| x.to_string()).collect(),
            vars: vars.iter().map(|x| x.to_string()).collect(),
            uops: uops.iter().map(|x| x.to_string()).collect(),
            bops: bops.iter().map(|x| x.to_string()).collect(),
            tops: tops.iter().map(|x| x.to_string()).collect(),
        }
    }
}

pub fn recursive_rules<L: SynthLanguage>(
    metric: Metric,
    n: usize,
    lang: Lang,
    prior: Ruleset<L>,
) -> Ruleset<L> {
    if n < 1 {
        Ruleset::default()
    } else {
        let mut rec = recursive_rules(metric, n - 1, lang.clone(), prior.clone());
        let wkld = base_lang()
            .iter_metric("EXPR", metric, n)
            .filter(Filter::Contains("VAR".parse().unwrap()))
            .plug("VAR", &Workload::new(lang.vars))
            .plug("CONST", &Workload::new(lang.consts))
            .plug("UOP", &Workload::new(lang.uops))
            .plug("BOP", &Workload::new(lang.bops))
            .plug("TOP", &Workload::new(lang.tops));
        rec.extend(prior);
        let new = run_workload(wkld, rec.clone(), Limits::rulefinding(), true);
        let mut all = new;
        all.extend(rec);
        all
    }
}

pub fn base_lang() -> Workload {
    Workload::new([
        "VAR",
        "CONST",
        "(UOP EXPR)",
        "(BOP EXPR EXPR)",
        "(TOP EXPR EXPR EXPR)",
    ])
}
