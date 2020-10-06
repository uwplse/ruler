// TODO generalize this
//use crate::SimpleMath;
use serde::Deserialize;
use serde::Serialize;
use std::time::{Duration, Instant};

#[derive(Serialize, Deserialize)]
pub enum EventType {
    AddedNodes,
    FoundRules,
}

#[derive(Serialize, Deserialize)]
pub enum Content {
    Comment(String),
    // TODO generalize this
    Nodes(Vec<String>),
    Rules(Vec<String>),
}

#[derive(Serialize, Deserialize)]
pub struct EgraphStats {
    data_points: Vec<EgraphStatsData>,
    time_points: Vec<TimingData>,
}

#[derive(Serialize, Deserialize)]
struct EgraphStatsData {
    eqsat_iter: i32,
    eclasses: usize,
    enodes: usize,
    event: Option<EventType>,
    note: Option<Content>,
}

#[derive(Serialize, Deserialize)]
struct TimingData {
    timing: Duration,
}

pub struct TimeHolder {
    pub before: Instant,
}

impl EgraphStats {
    pub fn new() -> EgraphStats {
        EgraphStats {
            data_points: Vec::new(),
            time_points: Vec::new(),
        }
    }

    pub fn record(&mut self, eqsat_iter: i32, eclasses: usize, enodes: usize) {
        self.data_points.push(EgraphStatsData {
            eqsat_iter,
            eclasses,
            enodes,
            event: None,
            note: None,
        });
    }

    pub fn log_event_metrics(
        &mut self,
        eqsat_iter: i32,
        eclasses: usize,
        enodes: usize,
        event: EventType,
        note: Content,
    ) {
        self.data_points.push(EgraphStatsData {
            eqsat_iter,
            eclasses,
            enodes,
            event: Some(event),
            note: Some(note),
        });
    }

    pub fn start(&mut self) -> TimeHolder {
        TimeHolder {
            before: Instant::now(),
        }
    }

    pub fn stop(&mut self, timer: TimeHolder) {
        let after = Instant::now();
        let duration = after.duration_since(timer.before);
        self.time_points.push(TimingData { timing: duration })
    }

    // todo abstract opening file
    pub fn print_timing_to_file(&self) {
        std::fs::create_dir_all("out").expect("could not create dir");
        //let outfile = std::fs::OpenOptions::new().append(true).create(true).open("out/eqsat_egraph_size.json").expect("failed to open file");
        let outfile =
            std::fs::File::create("out/eqsat_duration.json").expect("failed to open file");
        serde_json::to_writer_pretty(outfile, &self.time_points).unwrap();
    }

    pub fn print_to_file(&self) {
        std::fs::create_dir_all("out").expect("could not create dir");
        //let outfile = std::fs::OpenOptions::new().append(true).create(true).open("out/eqsat_egraph_size.json").expect("failed to open file");
        let outfile =
            std::fs::File::create("out/eqsat_egraph_size.json").expect("failed to open file");
        serde_json::to_writer_pretty(outfile, &self.data_points).unwrap();
    }
}

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
    pub before_eqsat_eclasses: usize,
    pub before_eqsat_enodes: usize,
    pub before_cvec_eclasses: usize,
    pub before_cvec_enodes: usize,
    pub after_cvec_eclasses: usize,
    pub after_cvec_enodes: usize,
    pub cvec_groups: usize,
    pub learned_rule: String,
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
        before_eqsat_eclasses: usize,
        before_eqsat_enodes: usize,
        before_cvec_eclasses: usize,
        before_cvec_enodes: usize,
        after_cvec_eclasses: usize,
        after_cvec_enodes: usize,
        cvec_groups: usize,
        learned_rule: String,
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
            before_eqsat_eclasses,
            before_eqsat_enodes,
            before_cvec_eclasses,
            before_cvec_enodes,
            after_cvec_eclasses,
            after_cvec_enodes,
            learned_rule,
            cvec_groups,
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
