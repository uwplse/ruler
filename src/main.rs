use num::bigint::ToBigInt;
use rand::SeedableRng;
use ruler::Constant;
use ruler::*;


fn main() {
    let _ = env_logger::builder().try_init();
    let syn = Synthesizer::new(SynthParams {
        seed: 5,
        n_samples: 2,
        constants: vec![
            // num!(-1.to_bigint().unwrap(), 1.to_bigint().unwrap()),
            num!(0.to_bigint().unwrap(), 1.to_bigint().unwrap()),
            num!(1.to_bigint().unwrap(), 1.to_bigint().unwrap()),
        ],
        // TODO: DON'T ENABLE THIS! We don't support eval with more than 3 vars right now, so this will give you index out of bounds.
        // variables: vec!["x".into(), "y".into(), "z".into(), "w".into()],
        variables: vec!["x".into(), "y".into(), "z".into()],
        iters: 1,
        rules_to_take: 1,
        chunk_size: usize::MAX,
    });
    let eqs = syn.run();

    for eq in eqs.values() {
        println!("{}", eq);
    }
    println!("found {} rules", eqs.len());
}