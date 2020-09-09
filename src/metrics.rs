// TODO generalize this
//use crate::SimpleMath;
use serde::Deserialize;
use serde::Serialize;

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
    Rules(Vec<String>)
}

#[derive(Serialize, Deserialize)]
pub struct EgraphStats {
    data_points: Vec<EgraphStatsData>,
}

#[derive(Serialize, Deserialize)]
struct EgraphStatsData {
    eqsat_iter: i32,
    eclasses: usize,
    enodes: usize,
    event: Option<EventType>,
    note: Option<Content>,
}

impl EgraphStats {
    pub fn new() -> EgraphStats {
        EgraphStats {
            data_points: Vec::new(),
        }
    }

    pub fn record(&mut self, eqsat_iter: i32, eclasses: usize, enodes: usize) {
        self.data_points.push(EgraphStatsData {
            eqsat_iter,
            eclasses,
            enodes,
            event: None, note: None
        });
    }

    pub fn log_event_metrics(&mut self, eqsat_iter: i32, eclasses: usize, enodes: usize, event: EventType, note: Content) {
        self.data_points.push(EgraphStatsData {
            eqsat_iter,
            eclasses,
            enodes,
            event: Some(event),
            note: Some(note)
        });
    }

    pub fn print_to_file(&self) {
        std::fs::create_dir_all("out").expect("could not create dir");
        //let outfile = std::fs::OpenOptions::new().append(true).create(true).open("out/eqsat_egraph_size.json").expect("failed to open file");
        let outfile =
            std::fs::File::create("out/eqsat_egraph_size.json").expect("failed to open file");
        serde_json::to_writer_pretty(outfile, &self.data_points).unwrap();
    }
}
