use ruler::*;

fn main() {
    let _ = env_logger::builder().try_init();
    let syn = Synthesizer::new(SynthParams {
        seed: 5,
        n_samples: 10,
        constants: vec![0, 1],
        variables: vec!["x".into(), "y".into(), "z".into()],
        iters: 1,
        rules_to_take: 1,
        chunk_size: usize::MAX,
        // variables: vec!["x".into(), "y".into(), "z".into(), "w".into()],
    });
    let eqs = syn.run();

    for eq in eqs.values() {
        println!("{}", eq);
    }
    println!("found {} rules", eqs.len());
}
