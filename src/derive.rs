use crate::*;
use rayon::prelude::*;
use std::fs::File;
use std::sync::Mutex;

type Pair<L> = (RecExpr<L>, RecExpr<L>);

pub fn derive<L: SynthLanguage>(params: DeriveParams) {
    let parse = |filename| {
        let file = File::open(filename).unwrap_or_else(|_| panic!("Failed to open {}", filename));
        let report: SlimReport<L> = serde_json::from_reader(file).unwrap();

        let pairs: Vec<Pair<L>> = report
            .eqs
            .iter()
            .map(|eq| {
                let l = L::instantiate(&eq.lhs);
                let r = L::instantiate(&eq.rhs);
                (l, r)
            })
            .collect();

        pairs
    };

    let pairs1 = parse(&params.in1);
    let pairs2 = parse(&params.in2);

    println!("Using {} to derive {}", params.in1, params.in2);
    let (derivable, not_derivable) = one_way(&params, &pairs1, &pairs2);

    println!("\nUsing {} to derive {}", params.in2, params.in1);
    let (rev_derivable, rev_not_derivable) = one_way(&params, &pairs2, &pairs1);

    let json = serde_json::json!({
        "files": [params.in1, params.in2],
        "forward": {
            "derivable": pairs_to_eqs(&derivable),
            "not_derivable": pairs_to_eqs(&not_derivable),
        },
        "reverse": {
            "derivable": pairs_to_eqs(&rev_derivable),
            "not_derivable": pairs_to_eqs(&rev_not_derivable),
        },
    });

    let file =
        File::create(&params.out).unwrap_or_else(|_| panic!("Failed to create '{}'", &params.out));
    serde_json::to_writer_pretty(file, &json).unwrap();
}

// check the derivability of rules in test using the rules in src
fn one_way<L: SynthLanguage>(
    params: &DeriveParams,
    src: &[Pair<L>],
    test: &[Pair<L>],
) -> (Vec<Pair<L>>, Vec<Pair<L>>) {
    let eqs: Vec<Equality<L>> = src.iter().flat_map(|(l, r)| Equality::new(l, r)).collect();

    let results = Mutex::new((vec![], vec![]));
    let test = test.to_vec();
    test.into_par_iter().for_each(|(l, r)| {
        let runner = Runner::default()
            .with_expr(&l)
            .with_expr(&r)
            .with_iter_limit(params.iter_limit)
            .with_node_limit(100_000)
            .with_time_limit(Duration::from_secs(10))
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

        let mut results = results.lock().unwrap();

        print!(
            "\r{} rules are derivable, {} are not.",
            results.0.len(),
            results.1.len(),
        );

        if l_id == r_id {
            results.0.push((l.clone(), r.clone()));
        } else {
            results.1.push((l.clone(), r.clone()));
        }
    });

    let results = results.into_inner().unwrap();
    println!(
        "\r{} rules are derivable, {} are not.",
        results.0.len(),
        results.1.len(),
    );
    results
}

fn pairs_to_eqs<L: SynthLanguage>(pairs: &[Pair<L>]) -> Vec<Equality<L>> {
    pairs
        .iter()
        .map(|(l, r)| Equality::new(l, r).unwrap())
        .collect()
}
