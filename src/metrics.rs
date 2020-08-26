use serde::Deserialize;
use serde::Serialize;
use std::time::{Duration, Instant};

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
}

#[derive(Serialize, Deserialize)]
struct TimingData {
    timing: Duration,
}

pub struct TimeHolder {
    pub before: Instant
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
        })
    }

    pub fn start(&mut self) -> TimeHolder {
        TimeHolder { before: Instant::now() }
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
