use std::{io::Write, sync::Arc, time::Duration};

use egg::{AstSize, EClass, Extractor, RecExpr, Rewrite, Runner};

use crate::{CVec, EGraph, Equality, IndexMap, Signature, SynthAnalysis, SynthLanguage};

#[derive(Clone, Debug)]
pub struct Ruleset<L: SynthLanguage>(pub IndexMap<Arc<str>, Equality<L>>);

impl<L: SynthLanguage> PartialEq for Ruleset<L> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for ((name1, _), (name2, _)) in self.0.iter().zip(other.0.iter()) {
            if name1 != name2 {
                return false;
            }
        }
        true
    }
}

impl<L: SynthLanguage> Default for Ruleset<L> {
    fn default() -> Self {
        Self(IndexMap::default())
    }
}

impl<L: SynthLanguage> Ruleset<L> {
    pub fn from_str_vec(ss: &[&str]) -> Self {
        let mut map = IndexMap::default();
        let eqs: Vec<Equality<L>> = ss.iter().map(|s| s.parse().unwrap()).collect();
        for eq in eqs {
            map.insert(eq.name.clone(), eq);
        }
        Ruleset(map)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn add(&mut self, eq: Equality<L>) {
        self.0.insert(eq.name.clone(), eq);
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn insert(&mut self, eq: Equality<L>) {
        self.0.insert(eq.name.clone(), eq);
    }

    pub fn to_file(&self, filename: &str) {
        let mut file = std::fs::File::create(filename)
            .unwrap_or_else(|_| panic!("Failed to open '{}'", filename));
        for (name, _) in &self.0 {
            writeln!(file, "{}", name).expect("Unable to write");
        }
    }

    pub fn from_file(filename: &str) -> Self {
        let infile = std::fs::File::open(filename).expect("can't open file");
        let reader = std::io::BufReader::new(infile);
        let mut eqs = IndexMap::default();
        for line in std::io::BufRead::lines(reader) {
            let line = line.unwrap();
            let eq = line.parse::<Equality<L>>().unwrap();
            eqs.insert(eq.name.clone(), eq);
        }
        Self(eqs)
    }

    pub fn partition_sat(&self) -> (Self, Self) {
        let mut sat = IndexMap::default();
        let mut other = IndexMap::default();

        for (name, eq) in &self.0 {
            if eq.is_saturating() {
                sat.insert(name.clone(), eq.clone());
            } else {
                other.insert(name.clone(), eq.clone());
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

    pub fn cvec_match(egraph: &EGraph<L, SynthAnalysis>) -> Self {
        // cvecs [𝑎1, . . . , 𝑎𝑛] and [𝑏1, . . . , 𝑏𝑛] match iff:
        // ∀𝑖. 𝑎𝑖 = 𝑏𝑖 ∨ 𝑎𝑖 = null ∨ 𝑏𝑖 = null and ∃𝑖. 𝑎𝑖 = 𝑏𝑖 ∧ 𝑎𝑖 ≠ null ∧ 𝑏𝑖 ≠ null

        println!(
            "starting cvec match with {} eclasses",
            egraph.number_of_classes()
        );

        let not_all_none: Vec<&EClass<L, Signature<L>>> = egraph
            .classes()
            .filter(|x| x.data.cvec.iter().any(|v| v.is_some()))
            .collect();

        let compare = |cvec1: &CVec<L>, cvec2: &CVec<L>| -> bool {
            for tup in cvec1.iter().zip(cvec2) {
                match tup {
                    (Some(a), Some(b)) if a != b => return false,
                    _ => (),
                }
            }
            true
        };
        let mut candidates = Ruleset::default();
        let extract = Extractor::new(egraph, AstSize);
        for class1 in &not_all_none {
            for class2 in &not_all_none {
                if class1.id == class2.id {
                    continue;
                }
                if compare(&class1.data.cvec, &class2.data.cvec) {
                    let (_, e1) = extract.find_best(class1.id);
                    let (_, e2) = extract.find_best(class2.id);
                    if let Some(eq) = Equality::new(&e1, &e2) {
                        candidates.insert(eq);
                    }
                    if let Some(eq) = Equality::new(&e2, &e1) {
                        candidates.insert(eq);
                    }
                }
            }
        }
        candidates
    }

    pub fn derive(&self, against: Self, iter_limit: usize) -> (Self, Self) {
        let (sat, other) = self.partition_sat();
        let sat: Vec<Rewrite<L, SynthAnalysis>> =
            sat.0.iter().map(|(_, eq)| eq.rewrite.clone()).collect();
        let other: Vec<Rewrite<L, SynthAnalysis>> =
            other.0.iter().map(|(_, eq)| eq.rewrite.clone()).collect();

        let mut derivable = IndexMap::default();
        let mut not_derivable = IndexMap::default();

        against.0.into_iter().for_each(|(_, eq)| {
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
                derivable.insert(eq.name.clone(), eq);
            } else {
                not_derivable.insert(eq.name.clone(), eq);
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
