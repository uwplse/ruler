use crate::*;
use rayon::prelude::*;
use std::fs::File;
use std::sync::Mutex;

type Pair<L> = (RecExpr<L>, RecExpr<L>);

pub enum RuleType {
    All,
    New,
    Old,
}

pub fn parse<L: SynthLanguage>(filename: &str, rtype: &RuleType) -> Vec<Pair<L>> {
    let file = File::open(filename).unwrap_or_else(|_| panic!("Failed to open {}", filename));
    let report: SlimReport<L> = serde_json::from_reader(file).unwrap();
    let eqs = match rtype {
        RuleType::All => &report.all_eqs,
        RuleType::New => &report.new_eqs,
        RuleType::Old => &report.old_eqs,
    };

    eqs.iter()
        .map(|eq| (L::instantiate(&eq.lhs), L::instantiate(&eq.rhs)))
        .collect()
}

/// Performs a one-way derivability test and exits with code -1
/// if the left ruleset cannot derive the right ruleset
pub fn derive_ci<L: SynthLanguage>(params: DeriveParams) {
    let rtype = if params.new_eqs {
        RuleType::New
    } else {
        RuleType::All
    };
    let pairs1 = parse::<L>(&params.in1, &rtype);
    let pairs2 = parse::<L>(&params.in2, &rtype);

    println!("Running derive tool in CI mode");
    println!("Using {} to derive {}", params.in1, params.in2);
    let (_, not_derivable) = one_way(&params, &pairs1, &pairs2);

    let not_derivable_eqs = pairs_to_eqs(&not_derivable);
    for eq in not_derivable_eqs {
        println!("Couldn't derive {}", eq.name);
    }

    if !not_derivable.is_empty() {
        std::process::exit(-1);
    }
}

/// Perform derivability test between two rulesets.
pub fn derive<L: SynthLanguage>(params: DeriveParams) {
    let mut pairs_1 = parse::<L>(&params.in1, &RuleType::All);
    let mut pairs_2 = parse::<L>(&params.in2, &RuleType::All);

    if params.new_eqs {
        pairs_1 = parse::<L>(&params.in1, &RuleType::New);
        pairs_2 = parse::<L>(&params.in2, &RuleType::New);

    }
    
    println!("Using {} to derive {}", params.in1, params.in2);
    let (derivable, not_derivable) = one_way(&params, &pairs_1, &pairs_2);

    println!("\nUsing {} to derive {}", params.in2, params.in1);
    let (rev_derivable, rev_not_derivable) = one_way(&params, &pairs_2, &pairs_1);

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

/// Check the derivability of rules in test using the rules in src
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
            .with_scheduler(egg::SimpleScheduler)
            .with_hook(|r| {
                if r.egraph.find(r.roots[0]) == r.egraph.find(r.roots[1]) {
                    Err("Done".to_owned())
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
            results.0.push((l, r));
        } else {
            results.1.push((l, r));
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
