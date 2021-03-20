use ruler::*;
use std::env;
use std::fs::File;

type Pair = (RecExpr, RecExpr);

// check the derivability of rules in test using the rules in src
pub fn derive(src: &[Pair], test: &[Pair]) -> (Vec<Pair>, Vec<Pair>) {
    let eqs: Vec<Equality> = src.iter().flat_map(|(l, r)| Equality::new(l, r)).collect();

    let mut derivable: Vec<Pair> = vec![];
    let mut not_derivable: Vec<Pair> = vec![];
    let mut n = 0.0;
    test.iter().for_each(|(l, r)| {
        n += 1.0;
        let runner = Runner::default()
            .with_expr(&l)
            .with_expr(&r)
            .with_iter_limit(5)
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
    });

    (derivable, not_derivable)
}

fn parse(filename: &str) -> Vec<Pair> {
    let file = File::open(filename).unwrap_or_else(|_| panic!("Failed to open {}", filename));
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

    pairs
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
    assert_eq!(args.len(), 4, "Provide two rule files and an output file");

    let pairs1 = parse(&args[1]);
    let pairs2 = parse(&args[2]);

    println!("Using {} to derive {}", args[1], args[2]);
    let (derivable, not_derivable) = derive(&pairs1, &pairs2);
    println!("\nDone!");

    // check the other way just for fun, but don't record it
    println!("Using {} to derive {}", args[2], args[1]);
    let (rev_derivable, rev_not_derivable) = derive(&pairs2, &pairs1);
    println!("\nDone with other way!");

    let json = serde_json::json!({
        "files": [&args[1], &args[2]],
        "forward": {
            "derivable": pairs_to_eqs(&derivable),
            "not_derivable": pairs_to_eqs(&not_derivable),
        },
        "reverse": {
            "derivable": pairs_to_eqs(&rev_derivable),
            "not_derivable": pairs_to_eqs(&rev_not_derivable),
        },
    });

    let file = File::create(&args[3]).unwrap_or_else(|_| panic!("Failed to create '{}'", &args[3]));
    serde_json::to_writer_pretty(file, &json).unwrap();

    Ok(())
}