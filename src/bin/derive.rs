use ruler::*;
use std::env;
use std::fs::File;
use std::io::Write;

type Pair = (RecExpr, RecExpr);

pub fn derive(src: &[Pair], test: &[Pair]) -> (Vec<Pair>, Vec<Pair>) {
    let eqs: Vec<Equality> = src.iter().flat_map(|(l, r)| Equality::new(l, r)).collect();

    let mut derivable = vec![];
    let mut not_derivable = vec![];

    for (l, r) in test {
        let runner = Runner::default()
            .with_expr(&l)
            .with_expr(&r)
            .with_iter_limit(5)
            .with_node_limit(1_000_000)
            .with_scheduler(egg::SimpleScheduler)
            .with_hook(|r| {
                if r.egraph.find(r.roots[0]) == r.egraph.find(r.roots[1]) {
                    Err(format!("Done"))
                } else {
                    Ok(())
                }
            })
            .run(eqs.iter().flat_map(|eq| &eq.rewrites));

        let l_id = runner.egraph.find(runner.roots[0]);
        let r_id = runner.egraph.find(runner.roots[1]);
        if l_id == r_id {
            derivable.push((l.clone(), r.clone()));
        } else {
            not_derivable.push((l.clone(), r.clone()));
        }
        print!(
            "\r{} derivable, {} not derivable",
            derivable.len(),
            not_derivable.len()
        );
        std::io::stdout().flush().unwrap();
    }

    (derivable, not_derivable)
}

fn parse_and_validate(filename: &str) -> Vec<Pair> {
    let pairs = parse_rules_from_file(filename);

    let (good, bad) = validate(pairs, 10_000);
    println!("Ignoring {} bad rules from {}", bad.len(), filename);
    for (l, r) in bad {
        println!("  {} =/= {}", l, r);
    }

    good
}

fn main() -> std::io::Result<()> {
    let _ = env_logger::try_init();
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        panic!("Provide two rule files");
    }

    let pairs1 = parse_and_validate(&args[1]);
    let pairs2 = parse_and_validate(&args[2]);

    println!("Using {} to derive {}", args[1], args[2]);
    let (derivable, not_derivable) = derive(&pairs1, &pairs2);
    println!("\nDone!");

    let mut df = File::create("derivable.txt")?;
    let mut ndf = File::create("notderivable.txt")?;
    for d in derivable {
        let wd = format!("{} => {}", d.0.pretty(100), d.1.pretty(100));
        write!(df, "{}\n", wd)?;
    }
    for d in not_derivable {
        let wd = format!("{} => {}", d.0.pretty(100), d.1.pretty(100));
        write!(ndf, "{}\n", wd)?;
    }

    Ok(())
}
