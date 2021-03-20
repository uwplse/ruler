use num::bigint::ToBigInt;
use ruler::*;
use num::{rational::Ratio};

fn main() {
    let _ = env_logger::builder().try_init();
    let syn = Synthesizer::new(SynthParams {
        seed: 5,
        n_samples: 10,
        constants: vec![
            Ratio::new(0.to_bigint().unwrap(), 1.to_bigint().unwrap()),
            Ratio::new(1.to_bigint().unwrap(), 1.to_bigint().unwrap()),
            Ratio::new(-1.to_bigint().unwrap(), 1.to_bigint().unwrap())
        ],
        variables: vec!["x".into(), "y".into(), "z".into()],
        iters: 2,
        rules_to_take: 100,
        chunk_size: usize::MAX,
        minimize: true,
        outfile: "minimize.json".to_string()
    });
    let outfile = &syn.params.outfile.clone();
    let report = syn.run();

    let file = std::fs::File::create(outfile)
        .unwrap_or_else(|_| panic!("Failed to open '{}'", outfile));
    serde_json::to_writer_pretty(file, &report).expect("failed to write json");
}
