/*!
32 bit implementation of Bitvectors.
!*/

use std::time::Instant;

use ruler::enumo::{Ruleset, Scheduler, Workload};

ruler::impl_bv!(32);

#[path = "./recipes/bv4_fancy.rs"]
pub mod bv4_fancy;

#[path = "./recipes/bv32.rs"]
mod bv32;

impl Bv {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let mut candidates = Ruleset::cvec_match(&compressed);

        let num_prior = prior.len();
        let chosen = candidates.minimize(prior, Scheduler::Compress(limits)).0;
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
}

#[cfg(test)]
pub mod test {
    use std::time::{Duration, Instant};

    use crate::bv32::bv32_rules;
    use crate::bv4_fancy::bv4_fancy_rules;

    use ruler::{
        enumo::{self, Filter, Ruleset, Workload},
        logger,
        recipe_utils::{base_lang, iter_metric, recursive_rules, run_workload, Lang},
        Limits,
    };

    use crate::Bv;

    fn gen() -> (Ruleset<Bv>, Duration) {
        let start = Instant::now();
        let mut rules: Ruleset<Bv> = Ruleset::default();
        let lang = Lang::new(
            &["0", "1"],
            &["a", "b", "c"],
            &[&["~", "-"], &["&", "|", "*", "--", "+", "<<", ">>"]],
        );
        rules.extend(recursive_rules(
            enumo::Metric::Atoms,
            5,
            lang.clone(),
            Ruleset::default(),
        ));

        let a6_canon = iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 6)
            .plug("VAR", &Workload::new(lang.vars))
            .plug("VAL", &Workload::empty())
            .plug("OP1", &Workload::new(lang.ops[0].clone()))
            .plug("OP2", &Workload::new(lang.ops[1].clone()))
            .filter(Filter::Canon(vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
            ]));
        let consts = Workload::new(["0", "1"]);
        let wkld = Workload::Append(vec![a6_canon, consts]);
        rules.extend(run_workload(
            wkld,
            rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            true,
        ));

        let duration = start.elapsed();
        (rules, duration)
    }

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bv32_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<_>::from_file("baseline/bv32.rules");

        logger::write_baseline(&rules, "bv32", &baseline, "oopsla", duration);
    }

    #[test]
    fn compare() {
        let domain = "BV32";
        // Port the bv4 rules into domain
        let actual_bv4_rules: Ruleset<_> = bv4_fancy_rules();
        let ported_bv4_rules: Ruleset<Bv> = Ruleset::new(actual_bv4_rules.to_str_vec());

        // Generate the rules directly
        let (gen, gen_time): (Ruleset<Bv>, Duration) = gen();

        logger::write_bv_derivability(domain, gen, gen_time, ported_bv4_rules)
    }
}
