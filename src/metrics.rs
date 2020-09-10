use serde::Deserialize;
use serde::Serialize;
use std::time::Duration;

#[derive(Serialize, Deserialize)]
struct TimeStats {
    pub niter: usize,
    pub adding_exprs: Duration,
    pub nloop: usize,
    pub cloning_pristine: Duration,
    pub clean_rules: Duration,
    pub tainted_eqsat: Duration,
    pub update_pristine: Duration,
    pub cvec_grouping: Duration,
    pub learn_a_rule: Duration,
    pub eclasses: usize,
    pub enodes: usize,
}

pub struct RulerProfile {
    ruler_profile: Vec<TimeStats>,
}

impl RulerProfile {
    pub fn new() -> RulerProfile {
        RulerProfile {
            ruler_profile: Vec::new(),
        }
    }
    pub fn record(
        &mut self,
        niter: usize,
        adding_exprs: Duration,
        nloop: usize,
        cloning_pristine: Duration,
        clean_rules: Duration,
        tainted_eqsat: Duration,
        update_pristine: Duration,
        cvec_grouping: Duration,
        learn_a_rule: Duration,
        eclasses: usize,
        enodes: usize,
    ) {
        self.ruler_profile.push(TimeStats {
            niter,
            adding_exprs,
            nloop,
            cloning_pristine,
            clean_rules,
            tainted_eqsat,
            update_pristine,
            cvec_grouping,
            learn_a_rule,
            eclasses,
            enodes,
        })
    }

    pub fn print_to_file(&self) {
        let root_dir = env!("CARGO_MANIFEST_DIR");
        let out_dir: String = root_dir.to_string() + "/out";
        let json_path: String = out_dir.clone() + "/ruler_profile.json";
        std::fs::create_dir_all(out_dir).expect("could not create dir");
        let outfile = std::fs::File::create(json_path).expect("failed to open file");
        serde_json::to_writer_pretty(outfile, &self.ruler_profile).unwrap();
    }
}
