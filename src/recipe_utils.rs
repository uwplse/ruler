use std::time::Instant;

use crate::{
    enumo::{Filter, Metric, Ruleset, Scheduler, Workload},
    Limits, SynthLanguage,
};

pub fn iter_metric(wkld: Workload, atom: &str, met: Metric, n: usize) -> Workload {
    let mut pegs = wkld.clone();
    for i in 1..(n + 1) {
        pegs = wkld
            .clone()
            .plug(atom, &pegs)
            .filter(Filter::MetricLt(met, i + 1));
    }
    pegs
}

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
        let wkld = iter_metric(base_lang(), "EXPR", metric, n)
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

#[cfg(test)]
mod test {
    use crate::{
        enumo::{Metric, Workload},
        recipe_utils::{base_lang, iter_metric},
    };

    #[test]
    fn iter_metric_test() {
        let atoms1 = iter_metric(base_lang(), "expr", Metric::Atoms, 1).force();
        assert_eq!(atoms1.len(), 2);

        let atoms2 = iter_metric(base_lang(), "expr", Metric::Atoms, 2).force();
        assert_eq!(atoms2.len(), 4);

        let atoms3 = iter_metric(base_lang(), "expr", Metric::Atoms, 3).force();
        assert_eq!(atoms3.len(), 10);

        let atoms4 = iter_metric(base_lang(), "expr", Metric::Atoms, 4).force();
        assert_eq!(atoms4.len(), 24);

        let atoms5 = iter_metric(base_lang(), "expr", Metric::Atoms, 5).force();
        assert_eq!(atoms5.len(), 66);

        let atoms6 = iter_metric(base_lang(), "expr", Metric::Atoms, 6).force();
        assert_eq!(atoms6.len(), 188);

        let atoms6 = iter_metric(base_lang(), "expr", Metric::Atoms, 7).force();
        assert_eq!(atoms6.len(), 570);

        let depth1 = iter_metric(base_lang(), "expr", Metric::Depth, 1).force();
        assert_eq!(depth1.len(), 2);

        let depth2 = iter_metric(base_lang(), "expr", Metric::Depth, 2).force();
        assert_eq!(depth2.len(), 8);

        let depth3 = iter_metric(base_lang(), "expr", Metric::Depth, 3).force();
        assert_eq!(depth3.len(), 74);

        let depth4 = iter_metric(base_lang(), "expr", Metric::Depth, 4).force();
        assert_eq!(depth4.len(), 5552);

        let lists1 = iter_metric(base_lang(), "expr", Metric::Lists, 1).force();
        assert_eq!(lists1.len(), 8);

        let lists2 = iter_metric(base_lang(), "expr", Metric::Lists, 2).force();
        assert_eq!(lists2.len(), 38);

        let lists3 = iter_metric(base_lang(), "expr", Metric::Lists, 3).force();
        assert_eq!(lists3.len(), 224);
    }

    #[test]
    fn iter_metric_fast() {
        // This test will not finish if the pushing monotonic filters through plugs optimization is not working.
        let six = iter_metric(base_lang(), "expr", Metric::Atoms, 6);
        assert_eq!(six.force().len(), 188);

        let extended = Workload::new([
            "cnst",
            "var",
            "(uop expr)",
            "(bop expr expr)",
            "(top expr expr expr)",
        ]);
        let three = iter_metric(extended.clone(), "expr", Metric::Atoms, 3);
        assert_eq!(three.force().len(), 10);

        let four = iter_metric(extended.clone(), "expr", Metric::Atoms, 4);
        assert_eq!(four.force().len(), 32);

        let five = iter_metric(extended.clone(), "expr", Metric::Atoms, 5);
        assert_eq!(five.force().len(), 106);

        let six = iter_metric(extended.clone(), "expr", Metric::Atoms, 6);
        assert_eq!(six.force().len(), 388);
    }
}
