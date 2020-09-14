use egg::*;
use rand::SeedableRng;
use ruler::Constant;
use ruler::*;
use std::env;
use std::io::{self, Write};

fn simplify(mut param: SynthParam) -> std::io::Result<()> {
    let eqs = param.run(13, true);
    let rules = eqs.iter().map(|eq| &eq.rewrite);
    println!("Entering simplification loop...");
    loop {
        print!("Input expression: ");
        io::stdout().flush()?;
        let mut expr_str = String::new();
        match io::stdin().read_line(&mut expr_str) {
            Ok(_) => {
                let runner: Runner<SimpleMath, SynthAnalysis, ()> = Runner::default()
                    .with_expr(&expr_str.parse().unwrap())
                    .run(rules.clone());

                let mut ext = Extractor::new(&runner.egraph, AstSize);
                let (_, simp_expr) = ext.find_best(runner.roots[0]);
                println!("Simplified result: {}", simp_expr);
                println!();
            }
            Err(_) => println!("failed to read expression"),
        }
    }
}

fn main() {
    let _ = env_logger::builder().try_init();
    let args: Vec<String> = env::args().collect();

    let mut param = SynthParam {
        rng: SeedableRng::seed_from_u64(5),
        n_iter: 2,
        n_samples: 5,
        variables: vec!["x".into(), "y".into(), "z".into()],
        consts: vec![Constant::Number(0), Constant::Number(1)], //, Constant::Boolean(false)] //, Constant::Boolean(true)],
        diff_thresh: 5,
    };

    if args.len() < 2 {
        param.run(13, false);
    } else if args.len() >= 2 && args[1] == "simplify" {
        let res = simplify(param);
        match res {
            Ok(_) => println!(),
            Err(_) => println!("Error while simplifying"),
        }
    } else {
        println!("USAGE: \n `cargo run` will run Ruler \n `cargo run simplify` will allow simplifying an expression.");
    }
}
