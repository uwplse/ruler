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
        recipe_utils::{iter_metric, recursive_rules, run_workload, Lang},
        Limits,
    };
    use serde_json::json;

    use crate::Bv;

    fn gen() -> (Ruleset<Bv>, Duration) {
        let start = Instant::now();
        let mut rules: Ruleset<Bv> = Ruleset::default();
        let lang = Lang::new(
            &["0", "1"],
            &["a", "b", "c"],
            &["~", "-"],
            &["&", "|", "*", "--", "+", "<<", ">>"],
            &[],
        );
        rules.extend(recursive_rules(
            enumo::Metric::Atoms,
            5,
            lang.clone(),
            Ruleset::default(),
        ));

        let base_lang = Workload::new(["VAR", "CONST", "(UOP EXPR)", "(BOP EXPR EXPR)"]);

        let a6_canon = iter_metric(base_lang, "EXPR", enumo::Metric::Atoms, 6)
            .plug("VAR", &Workload::new(lang.vars))
            .plug("CONST", &Workload::empty())
            .plug("UOP", &Workload::new(lang.uops))
            .plug("BOP", &Workload::new(lang.bops))
            .filter(Filter::Canon(vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
            ]));
        rules.extend(run_workload(
            a6_canon,
            rules.clone(),
            Limits::rulefinding(),
            true,
        ));

        let duration = start.elapsed();
        (rules, duration)
    }

    fn from_bv4() -> (Ruleset<Bv>, Duration) {
        let actual_bv4_rules: Ruleset<_> = bv4_fancy_rules();
        let ported_bv4_rules: Ruleset<Bv> = Ruleset::new(actual_bv4_rules.to_str_vec());
        let start = Instant::now();
        let (sound, _) = ported_bv4_rules.partition(|rule| rule.is_valid());
        (sound, start.elapsed())
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

        logger::write_output(&rules, &baseline, "bv32", "oopsla", duration, true);
    }

    #[test]
    fn compare() {
        let domain = "BV32";
        // Generate the rules directly
        let (gen, gen_time): (Ruleset<Bv>, Duration) = gen();

        // Validate bv4 rules
        let (sound_bv4, sound_bv4_time) = from_bv4();

        println!("Summary for {}", domain);
        println!(
            "Generated {} rules for {} in {}s",
            gen.len(),
            domain,
            gen_time.as_secs_f32()
        );
        println!(
            "Validating bv4 rules as {} rules resulted in {} sound rules in {}s",
            domain,
            sound_bv4.len(),
            sound_bv4_time.as_secs_f32()
        );

        let start = Instant::now();
        let (can, cannot) =
            sound_bv4.derive(ruler::DeriveType::LhsAndRhs, &gen, Limits::deriving());
        let derive_time = start.elapsed();

        std::fs::create_dir_all("nightly/data")
            .unwrap_or_else(|e| panic!("Error creating dir: {}", e));
        let stat = json!({
            "domain": domain,
            "direct_gen": json!({
                "rules": gen.to_str_vec(),
                "time": gen_time.as_secs_f32()
            }),
            "from_bv4": json!({
                "rules": sound_bv4.to_str_vec(),
                "time": sound_bv4_time.as_secs_f32()
            }),
            "derive": json!({
                "can": can.len(),
                "cannot": cannot.len(),
                "missing_rules": cannot.to_str_vec(),
                "time": derive_time.as_secs_f32()
            })

        });
        logger::add_to_data_file("nightly/data/output.json".to_string(), stat);
    }
}
