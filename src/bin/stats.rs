use rand::SeedableRng;
use ruler::*;
use serde::Deserialize;
use serde::Serialize;
use std::time::{Duration, Instant};

#[derive(Serialize, Deserialize)]
struct CVecStats {
    niter: usize,
    nsamples: usize,
    time: Duration,
}

fn cvec_stats_ruler(iter: usize, samples: usize, num_op: usize) -> CVecStats {
    let mut param = SynthParam {
        rng: SeedableRng::seed_from_u64(5),
        n_iter: iter,
        n_samples: samples,
        variables: vec!["x".into(), "y".into(), "z".into()],
        consts: vec![Constant::Boolean(true), Constant::Boolean(false)],
        diff_thresh: 5,
    };

    let before = Instant::now();
    param.run(num_op, false);
    let after = Instant::now();
    return CVecStats {
        niter: iter,
        nsamples: samples,
        time: after.duration_since(before),
    };
}

fn nsamples_vs_exec_time() {
    let cvec_lens = vec![10, 20, 40, 60, 80, 100, 200, 400]; //, 750, 1000];
    let num_ops: Vec<usize> = vec![1, 2, 3];
    let iters = vec![1];
    let mut data = vec![];
    let outfile =
        std::fs::File::create("../../out/sample_vs_time.json").expect("failed to open file");
    for i in iters {
        for l in &cvec_lens {
            for n in &num_ops {
                data.push(cvec_stats_ruler(i, *l, *n));
            }
        }
    }
    serde_json::to_writer_pretty(outfile, &data).unwrap();
}

#[derive(Serialize, Deserialize)]
struct VarRuleStats {
    iter: usize,
    num_vars: usize,
    num_ops: usize,
    num_rules: usize,
    time: Duration,
}

fn var_rule_ruler(iter: usize, num_vars: usize, num_op: usize) -> VarRuleStats {
    if num_vars == 1 {
        let mut param = SynthParam {
            rng: SeedableRng::seed_from_u64(5),
            n_iter: iter,
            n_samples: usize::pow(2, 1),
            variables: vec!["x".into()],
            consts: vec![Constant::Boolean(true), Constant::Boolean(false)],
            diff_thresh: 5,
        };
        let before = Instant::now();
        let eqs = param.run(num_op, false);
        let after = Instant::now();
        return VarRuleStats {
            iter: iter,
            num_vars: num_vars,
            num_ops: num_op,
            num_rules: eqs.len(),
            time: after.duration_since(before),
        };
    } else if num_vars == 2 {
        let mut param = SynthParam {
            rng: SeedableRng::seed_from_u64(5),
            n_iter: iter,
            n_samples: usize::pow(2, 2),
            variables: vec!["x".into(), "y".into()],
            consts: vec![Constant::Boolean(true), Constant::Boolean(false)],
            diff_thresh: 5,
        };
        let before = Instant::now();
        let eqs = param.run(num_op, false);
        let after = Instant::now();
        return VarRuleStats {
            iter: iter,
            num_vars: num_vars,
            num_ops: num_op,
            num_rules: eqs.len(),
            time: after.duration_since(before),
        };
    } else if num_vars == 3 {
        let mut param = SynthParam {
            rng: SeedableRng::seed_from_u64(5),
            n_iter: iter,
            n_samples: usize::pow(2, 3),
            variables: vec!["x".into(), "y".into(), "z".into()],
            consts: vec![Constant::Boolean(true), Constant::Boolean(false)],
            diff_thresh: 5,
        };
        let before = Instant::now();
        let eqs = param.run(num_op, false);
        let after = Instant::now();
        return VarRuleStats {
            iter: iter,
            num_vars: num_vars,
            num_ops: num_op,
            num_rules: eqs.len(),
            time: after.duration_since(before),
        };
    } else {
        panic!("not implemented for more than three vars");
    }
}

fn var_const_rule_time() {
    let iters = vec![1, 2];
    let num_vars = vec![1, 2, 3];
    let num_ops = vec![1, 2, 3];
    let mut data = vec![];
    let outfile =
        std::fs::File::create("../../out/var_rule_vs_time.json").expect("failed to open file");
    for i in iters {
        for nv in &num_vars {
            for n in &num_ops {
                data.push(var_rule_ruler(i, *nv, *n));
            }
        }
    }
    serde_json::to_writer_pretty(outfile, &data).unwrap();
}

fn main() {
    // nsamples_vs_exec_time();
    var_const_rule_time();
}
