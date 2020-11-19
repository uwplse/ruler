use ruler::*;
use structopt::StructOpt;

fn main() {
    let _ = env_logger::builder().try_init();
    let params = SynthParams::from_args();

    let syn = Synthesizer::new(params.clone());
    let report = syn.run();

    if let Some(outfile) = &params.outfile {
        let file = std::fs::File::create(outfile)
            .unwrap_or_else(|_| panic!("Failed to open outfile '{}'", outfile));
        serde_json::to_writer_pretty(file, &report).expect("failed to write json");
    }
}
