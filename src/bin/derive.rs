use ruler::*;
use std::env;
use std::fs::File;
use std::io::Write;
use std::ops::DerefMut;
use std::sync::Mutex;

use rayon::prelude::*;

type Pair = (RecExpr, RecExpr);

pub fn derive(src: &[Pair], test: &[Pair]) -> (Vec<Pair>, Vec<Pair>) {
    let eqs: Vec<Equality> = src.iter().flat_map(|(l, r)| Equality::new(l, r)).collect();

    let lists = Mutex::new((vec![], vec![]));
    test.iter().for_each(|(l, r)| {
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

        let mut lists = lists.lock().unwrap();
        let (derivable, not_derivable) = lists.deref_mut();
        // let (derivable, not_derivable) = lists;
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
    });

    lists.into_inner().unwrap()
}

fn parse_and_validate(filename: &str) -> (Vec<Pair>, Vec<Pair>) {
    let file = File::open(filename).unwrap();
    let report: Report = serde_json::from_reader(file).unwrap();

    let pairs: Vec<Pair> = report
        .eqs
        .iter()
        .map(|eq| {
            let l = instantiate(&eq.lhs);
            let r = instantiate(&eq.rhs);
            (l, r)
        })
        .collect();

    let (good, bad) = validate(pairs, 10_000);
    println!(
        "{}: {} ok rules, {} bad rules",
        filename,
        good.len(),
        bad.len()
    );
    for (l, r) in &bad {
        println!("  {} =/= {}", l, r);
    }

    (good, bad)
}

fn pairs_to_eqs(pairs: &[Pair]) -> Vec<Equality> {
    pairs
        .iter()
        .map(|(l, r)| Equality::new(l, r).unwrap())
        .collect()
}

fn main() -> std::io::Result<()> {
    let _ = env_logger::try_init();
    let args: Vec<String> = env::args().collect();
    assert_eq!(args.len(), 4, "Provide two reports and an output file");

    let (pairs1, bad1) = parse_and_validate(&args[1]);
    let (pairs2, bad2) = parse_and_validate(&args[2]);

    println!("Using {} to derive {}", args[1], args[2]);
    let (derivable, not_derivable) = derive(&pairs1, &pairs2);
    println!("\nDone!");

    // check the other way just for fun, but don't record it
    println!("Using {} to derive {}", args[2], args[1]);
    let (rev_derivable, rev_not_derivable) = derive(&pairs2, &pairs1);
    println!("\nDone with other way (not recorded)!");

    let json = serde_json::json!({
        "files": [&args[1], &args[2]],
        "forward": {
            "derivable": pairs_to_eqs(&derivable),
            "not_derivable": pairs_to_eqs(&not_derivable),
            "bad": pairs_to_eqs(&bad1),
        },
        "reverse": {
            "derivable": pairs_to_eqs(&rev_derivable),
            "not_derivable": pairs_to_eqs(&rev_not_derivable),
            "bad": pairs_to_eqs(&bad2),
        },
    });

    let file = File::create(&args[3]).unwrap_or_else(|_| panic!("Failed to create '{}'", &args[3]));
    serde_json::to_writer_pretty(file, &json).unwrap();

    Ok(())
}
