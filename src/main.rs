use ruler::*;
use ordered_float::OrderedFloat;

fn main() {
    let _ = env_logger::builder().try_init();
    let syn = Synthesizer::new(SynthParams {
        seed: 5,
        n_samples: 100,
        constants: vec![OrderedFloat::from(-1.0), OrderedFloat::from(0.0), OrderedFloat::from(1.0)],
        variables: vec!["x".into(), "y".into(), "z".into()],
        // variables: vec!["x".into(), "y".into(), "z".into(), "w".into()],
    });
    let eqs = syn.run_orat(2);

    for eq in eqs.values() {
        println!("{}", eq);
    }
    println!("found {} rules", eqs.len());
}
