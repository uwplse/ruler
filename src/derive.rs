use crate::*;
use rayon::prelude::*;
use std::fs::File;
use std::sync::Mutex;

type Pair<L> = (RecExpr<L>, RecExpr<L>);

pub fn parse<L: SynthLanguage>(filename: &str, only_new: bool) -> Vec<Equality<L>> {
    let file = File::open(filename).unwrap_or_else(|_| panic!("Failed to open {}", filename));
    let report: SlimReport<L> = serde_json::from_reader(file).unwrap();
    if only_new {
        report.new_eqs
    } else {
        report.all_eqs
    }
}

/// Performs a one-way derivability test and exits with code -1
/// if the left ruleset cannot derive the right ruleset
pub fn derive_ci<L: SynthLanguage>(params: DeriveParams) {
    let pairs1 = parse::<L>(&params.in1, params.only_new);
    let pairs2 = parse::<L>(&params.in2, params.only_new);

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
    let eqs_1 = parse::<L>(&params.in1, params.only_new);
    let eqs_2 = parse::<L>(&params.in2, params.only_new);

    println!("Using {} to derive {}", params.in1, params.in2);
    let (derivable, not_derivable) = one_way(&params, &eqs_1, &eqs_2);

    println!("\nUsing {} to derive {}", params.in2, params.in1);
    let (rev_derivable, rev_not_derivable) = one_way(&params, &eqs_2, &eqs_1);

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

fn is_saturating<L: SynthLanguage>(lhs: &Pattern<L>, rhs: &Pattern<L>) -> bool {
    // TODO
    // 1. Ignore constants
    // 2. If RHS is strictly smaller than LHS
    let mut egraph: EGraph<L, SynthAnalysis> = Default::default();
    let l_id = egraph.add_expr(&L::instantiate(lhs));

    let initial_size = egraph.number_of_classes();

    let r_id = egraph.add_expr(&L::instantiate(rhs));

    egraph.union(l_id, r_id);
    egraph.rebuild();
    let final_size = egraph.number_of_classes();

    initial_size >= final_size
}

fn mk_runner<L: SynthLanguage>(
    params: &DeriveParams,
    egraph: EGraph<L, SynthAnalysis>,
    lhs: &RecExpr<L>,
    rhs: &RecExpr<L>,
) -> Runner<L, SynthAnalysis> {
    Runner::default()
        .with_egraph(egraph)
        .with_expr(lhs)
        .with_expr(rhs)
        .with_iter_limit(params.iter_limit)
        .with_node_limit(params.node_limit)
        .with_time_limit(Duration::from_secs(params.time_limit))
        .with_scheduler(egg::SimpleScheduler)
        .with_hook(|r| {
            if r.egraph.find(r.roots[0]) == r.egraph.find(r.roots[1]) {
                Err("Done".to_owned())
            } else {
                Ok(())
            }
        })
}

/// Check the derivability of rules in test using the rules in src
fn one_way<L: SynthLanguage>(
    params: &DeriveParams,
    src: &[Equality<L>],
    test: &[Equality<L>],
) -> (Vec<Pair<L>>, Vec<Pair<L>>) {
    let mut sat: Vec<Rewrite<L, SynthAnalysis>> = vec![];
    let mut other: Vec<Rewrite<L, SynthAnalysis>> = vec![];

    for v in src {
        if is_saturating(&v.lhs, &v.rhs) {
            sat.push(v.rewrites[0].clone());
        } else {
            other.push(v.rewrites[0].clone());
        }
        if v.rewrites.len() == 2 {
            if is_saturating(&v.rhs, &v.lhs) {
                sat.push(v.rewrites[1].clone());
            } else {
                other.push(v.rewrites[1].clone());
            }
        }
    }
    println!(
        "Partitioned {} eqs into {} sat and {} other",
        src.len(),
        sat.len(),
        other.len()
    );

    let results = Mutex::new((vec![], vec![]));
    let test = test.to_vec();
    test.into_par_iter().for_each(|eq| {
        let l = L::instantiate(&eq.lhs);
        let r = L::instantiate(&eq.rhs);

        let mut runner = mk_runner(params, Default::default(), &l, &r);
        let mut l_id;
        let mut r_id;

        for _ in 0..params.iter_limit {
            // Sat
            runner = mk_runner(params, runner.egraph, &l, &r)
                .with_node_limit(usize::MAX)
                .with_time_limit(Duration::from_secs(30))
                .with_iter_limit(100)
                .run(&sat);

            l_id = runner.egraph.find(runner.roots[0]);
            r_id = runner.egraph.find(runner.roots[1]);

            if l_id == r_id {
                break;
            }

            // Other
            runner = mk_runner(params, runner.egraph, &l, &r)
                .with_iter_limit(1)
                .run(&other);

            l_id = runner.egraph.find(runner.roots[0]);
            r_id = runner.egraph.find(runner.roots[1]);

            if l_id == r_id {
                break;
            }
        }

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
