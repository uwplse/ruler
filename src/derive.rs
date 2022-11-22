use std::{fs::File, time::Duration};

use egg::{RecExpr, Rewrite, Runner};

use crate::*;

pub fn parse<L: SynthLanguage>(filename: &str) -> Vec<Equality<L>> {
    let file = File::open(filename).unwrap_or_else(|_| panic!("Failed to open {}", filename));
    let report: Report<L> = serde_json::from_reader(file).unwrap();
    let mut eqs = vec![];
    eqs.extend(report.prior_rws);
    eqs.extend(report.new_rws);
    eqs
}

fn is_saturating<L: SynthLanguage>(lhs: &Pattern<L>, rhs: &Pattern<L>) -> bool {
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
    egraph: EGraph<L, SynthAnalysis>,
    lhs: &RecExpr<L>,
    rhs: &RecExpr<L>,
) -> Runner<L, SynthAnalysis> {
    Runner::default()
        .with_egraph(egraph)
        .with_expr(lhs)
        .with_expr(rhs)
        .with_scheduler(egg::SimpleScheduler)
        .with_hook(|r| {
            if r.egraph.find(r.roots[0]) == r.egraph.find(r.roots[1]) {
                Err("Done".to_owned())
            } else {
                Ok(())
            }
        })
}

pub fn derive<L: SynthLanguage>(params: DeriveParams) {
    let test_rules = parse::<L>(&params.in1);
    let num_test = test_rules.len();
    let baseline_rules = parse::<L>(&params.in2);

    println!("Using {} to derive {}", params.in1, params.in2);

    let mut sat: Vec<Rewrite<L, SynthAnalysis>> = vec![];
    let mut other: Vec<Rewrite<L, SynthAnalysis>> = vec![];

    for eq in test_rules {
        if is_saturating(&eq.lhs, &eq.rhs) {
            sat.push(eq.rewrite);
        } else {
            other.push(eq.rewrite);
        }
    }

    println!(
        "Partitioned {} eqs into {} sat and {} other",
        num_test,
        sat.len(),
        other.len()
    );

    let mut derivable = vec![];
    let mut not_derivable = vec![];

    baseline_rules.into_iter().for_each(|eq| {
        let l = L::instantiate(&eq.lhs);
        let r = L::instantiate(&eq.rhs);

        let mut runner = mk_runner(Default::default(), &l, &r);
        let mut l_id;
        let mut r_id;
        for _ in 0..params.iter_limit {
            // Sat
            runner = mk_runner(runner.egraph, &l, &r)
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
            runner = mk_runner(runner.egraph, &l, &r)
                .with_iter_limit(1)
                .run(&other);

            l_id = runner.egraph.find(runner.roots[0]);
            r_id = runner.egraph.find(runner.roots[1]);

            if l_id == r_id {
                break;
            }
        }
        // One more sat
        runner = mk_runner(runner.egraph, &l, &r)
            .with_node_limit(usize::MAX)
            .with_time_limit(Duration::from_secs(30))
            .with_iter_limit(100)
            .run(&sat);
        l_id = runner.egraph.find(runner.roots[0]);
        r_id = runner.egraph.find(runner.roots[1]);
        if l_id == r_id {
            derivable.push(eq);
        } else {
            not_derivable.push(eq);
        }
    });
    println!(
        "{} rules are derivable, {} are not",
        derivable.len(),
        not_derivable.len()
    );

    let json = serde_json::json!({
        "files": [params.in1, params.in2],
        "derivable": derivable,
        "not_derivable": not_derivable
    });

    let file = File::create(&params.outfile)
        .unwrap_or_else(|_| panic!("Failed to create {}", &params.outfile));
    serde_json::to_writer_pretty(file, &json).unwrap();

    if params.ci {
        for eq in &not_derivable {
            println!("Couldn't derive {}", eq.name);
        }
        if !not_derivable.is_empty() {
            std::process::exit(-1);
        }
    }
}
