use egg::*;
use ruler::*;
use std::time::{Duration, Instant};
use serde::Serialize;
use serde::Deserialize;
use rand::SeedableRng;

#[derive(Serialize, Deserialize)]
struct CVecStats {
    niter : usize,
    nsamples : usize,
    time: Duration,
}

fn ruler(iter: usize, samples : usize) -> CVecStats {
    let mut param = SynthParam {
        rng: SeedableRng::seed_from_u64(5),
        n_iter: iter,
        n_samples: samples,
        variables: vec!["x".into(), "y".into(), "z".into()],
        consts: vec![Constant::Number(0), Constant::Number(1)],
    };

    let before = Instant::now();
    param.run();
    let after = Instant::now();
    return CVecStats {
        niter : iter,
        nsamples : samples,
        time : after.duration_since(before)
    }
}

fn nsamples_vs_exec_time() {
    let cvec_lens = vec![10, 20, 40, 60, 80, 100, 200, 400];//, 750, 1000];
    let iters = vec![1];
    let mut data = vec![];
    let outfile = std::fs::File::create("../../out/sample_vs_time.json").expect("failed to open file");
    for i in iters {
        for l in &cvec_lens {
            data.push(ruler(i, *l));
        }
    }
    serde_json::to_writer_pretty(outfile, &data).unwrap();
}

fn main() {
    nsamples_vs_exec_time();
}
