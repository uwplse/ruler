use std::{io::Write, time::Duration};

use egg::{RecExpr, Rewrite, Runner};

use crate::{EGraph, Equality, SynthAnalysis, SynthLanguage};

#[derive(Clone, Debug)]
pub struct Ruleset<L: SynthLanguage>(pub Vec<Equality<L>>);

impl<L: SynthLanguage> PartialEq for Ruleset<L> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for (x, y) in self.0.iter().zip(other.0.iter()) {
            if x.name != y.name {
                return false;
            }
        }
        true
    }
}

impl<L: SynthLanguage> Default for Ruleset<L> {
    fn default() -> Self {
        Self(vec![])
    }
}

impl<L: SynthLanguage> Ruleset<L> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn to_file(&self, filename: &str) {
        let mut file = std::fs::File::create(filename)
            .unwrap_or_else(|_| panic!("Failed to open '{}'", filename));
        for eq in &self.0 {
            writeln!(file, "{}", eq.name).expect("Unable to write");
        }
    }

    pub fn from_file(filename: &str) -> Self {
        let infile = std::fs::File::open(filename).expect("can't open file");
        let reader = std::io::BufReader::new(infile);
        let mut eqs = vec![];
        for line in std::io::BufRead::lines(reader) {
            let line = line.unwrap();
            let l = line.parse::<Equality<L>>().unwrap();
            eqs.push(l);
        }
        Self(eqs)
    }

    pub fn partition_sat(&self) -> (Self, Self) {
        let mut sat = vec![];
        let mut other = vec![];

        for eq in &self.0 {
            if eq.is_saturating() {
                sat.push(eq.clone());
            } else {
                other.push(eq.clone());
            }
        }

        (Ruleset(sat), Ruleset(other))
    }

    fn mk_runner(
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

    pub fn derive(&self, against: Self, iter_limit: usize) -> (Self, Self) {
        let (sat, other) = self.partition_sat();
        let sat: Vec<Rewrite<L, SynthAnalysis>> =
            sat.0.iter().map(|eq| eq.rewrite.clone()).collect();
        let other: Vec<Rewrite<L, SynthAnalysis>> =
            other.0.iter().map(|eq| eq.rewrite.clone()).collect();

        let mut derivable = vec![];
        let mut not_derivable = vec![];

        against.0.into_iter().for_each(|eq| {
            let l = L::instantiate(&eq.lhs);
            let r = L::instantiate(&eq.rhs);

            let mut runner = Self::mk_runner(Default::default(), &l, &r);
            let mut l_id;
            let mut r_id;
            for _ in 0..iter_limit {
                // Sat
                runner = Self::mk_runner(runner.egraph, &l, &r)
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
                runner = Self::mk_runner(runner.egraph, &l, &r)
                    .with_iter_limit(1)
                    .run(&other);

                l_id = runner.egraph.find(runner.roots[0]);
                r_id = runner.egraph.find(runner.roots[1]);

                if l_id == r_id {
                    break;
                }
            }
            // One more sat
            runner = Self::mk_runner(runner.egraph, &l, &r)
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

        (Self(derivable), Self(not_derivable))
    }
}
