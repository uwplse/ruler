use egg::{EGraph, ENodeOrVar, RecExpr};

use super::*;
use crate::{SynthAnalysis, SynthLanguage};
use std::io::Write;
use std::time::Instant;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Workload {
    Set(Vec<Sexp>),
    Plug(Box<Self>, String, Box<Self>),
    Filter(Filter, Box<Self>),
    Append(Vec<Self>),
}

impl Default for Workload {
    fn default() -> Self {
        Workload::Set(vec![])
    }
}

impl Workload {
    pub fn new<I>(vals: I) -> Self
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        Self::Set(
            vals.into_iter()
                .map(|x| x.as_ref().parse().unwrap())
                .collect(),
        )
    }

    pub fn empty() -> Self {
        Self::Set(vec![])
    }

    pub fn to_file(&self, filename: &str) {
        let mut file = std::fs::File::create(filename)
            .unwrap_or_else(|_| panic!("Failed to open '{}'", filename));
        for name in &self.force() {
            writeln!(file, "{}", name).expect("Unable to write");
        }
    }

    pub fn from_file(filename: &str) -> Self {
        let infile = std::fs::File::open(filename).expect("can't open file");
        let reader = std::io::BufReader::new(infile);
        let mut sexps = vec![];
        for line in std::io::BufRead::lines(reader) {
            sexps.push(line.unwrap().parse().unwrap());
        }
        Self::Set(sexps)
    }

    pub fn to_egraph<L: SynthLanguage>(&self) -> EGraph<L, SynthAnalysis> {
        let mut egraph = EGraph::default();

        let start = Instant::now();
        let sexps = self.force();
        let duration = start.elapsed();
        println!("Forced workload in {} seconds.", duration.as_secs());

        // Have to find all the variables first so that we can initialize
        // their cvecs, which might require doing a multi-way cross product
        // based on how many variables there are.
        // We have to do this before adding any other expressions to the
        // egraph so that the variable cvecs are properly initialized and
        // able to be used by other expressions that contain variables
        // For some reason, it appears the order we initialize these variables
        // can matter, so make sure we preserve the order in the workload.
        // TODO: why does this order matter?
        println!("Finding variables...");
        let start = Instant::now();
        let mut vars: Vec<String> = vec![];
        for sexp in sexps.iter() {
            let expr: RecExpr<L> = sexp.to_string().parse().unwrap();
            for node in expr.as_ref() {
                if let ENodeOrVar::Var(v) = node.clone().to_enode_or_var() {
                    let mut v = v.to_string();
                    v.remove(0);
                    if !vars.contains(&v) {
                        vars.push(v);
                    }
                }
            }
        }
        L::initialize_vars(&mut egraph, &vars);
        let duration = start.elapsed();
        println!("Found and initialized variables in {} seconds.", duration.as_secs());

        println!("Adding exprs to egraph...");
        let start = Instant::now();

        let mut parsed = vec![];
        let start = Instant::now();
        for sexp in sexps.iter() {
            parsed.push(sexp.to_string().parse::<RecExpr<L>>().unwrap());
        }
        let duration = start.elapsed();
        println!("Parsing completed in {} seconds.", duration.as_secs());
        let start = Instant::now();
        for exp in parsed.iter() {
            // Adding a single expr takes between 5 and 14 milliseconds
            // and remains consistent as the egraph grows.
            egraph.add_expr(&exp);
        }
        let duration = start.elapsed();
        println!("Added exprs to egraph in {} seconds.", duration.as_secs());
        egraph
    }

    pub fn force(&self) -> Vec<Sexp> {
        match self {
            Workload::Set(set) => set.clone(),
            Workload::Plug(wkld, name, pegs) => {
                let mut res = vec![];
                let pegs = pegs.force();
                for sexp in wkld.force() {
                    res.extend(sexp.plug(name, &pegs));
                }
                res
            }
            Workload::Filter(f, workload) => {
                let mut set = workload.force();
                set.retain(|sexp| f.test(sexp));
                set
            }
            Workload::Append(workloads) => {
                let mut set = vec![];
                for w in workloads {
                    set.extend(w.force());
                }
                set
            }
        }
    }

    pub fn pretty_print(&self) {
        for t in self.force() {
            println!("{}", t);
        }
    }

    pub fn plug(self, name: impl Into<String>, workload: &Workload) -> Self {
        Workload::Plug(Box::new(self), name.into(), Box::new(workload.clone()))
    }

    pub fn append(self, workload: impl Into<Workload>) -> Self {
        let into: Workload = workload.into();
        match (self, into) {
            (Workload::Set(xs), Workload::Set(ys)) => {
                let mut all = vec![];
                all.extend(xs);
                all.extend(ys);
                Workload::Set(all)
            }
            (Workload::Append(xs), Workload::Append(ys)) => {
                let mut all = vec![];
                all.extend(xs);
                all.extend(ys);
                Workload::Append(all)
            }
            (Workload::Append(xs), y) => {
                let mut all = vec![];
                all.extend(xs);
                all.push(y);
                Workload::Append(all)
            }
            (x, Workload::Append(ys)) => {
                let mut all = vec![];
                all.extend(ys);
                all.push(x);
                Workload::Append(all)
            }
            (x, y) => Workload::Append(vec![x, y]),
        }
    }

    pub fn filter(self, filter: Filter) -> Self {
        if filter.is_monotonic() {
            if let Workload::Plug(wkld, name, pegs) = self {
                Workload::Filter(
                    filter.clone(),
                    Box::new(Workload::Plug(wkld, name, Box::new(pegs.filter(filter)))),
                )
            } else {
                Workload::Filter(filter, Box::new(self))
            }
        } else {
            Workload::Filter(filter, Box::new(self))
        }
    }
}

impl From<&[&str]> for Workload {
    fn from(value: &[&str]) -> Self {
        Workload::new(value.iter().copied())
    }
}

#[cfg(test)]
mod test {
    use crate::recipe_utils::{base_lang, iter_metric};

    use super::*;

    #[test]
    fn filter_optimization() {
        let wkld = Workload::new(["x"]);
        let pegs = Workload::new(["a", "b", "c"]);
        let plugged = wkld
            .plug("x", &pegs)
            .filter(Filter::MetricLt(Metric::Atoms, 2));
        assert_eq!(plugged.force().len(), 3);
    }

    #[test]
    fn contains() {
        let actual3 = iter_metric(base_lang(), "EXPR", Metric::Atoms, 3)
            .filter(Filter::Contains("VAR".parse().unwrap()))
            .force();

        let expected3 = Workload::new([
            "VAR",
            "(UOP VAR)",
            "(UOP (UOP VAR))",
            "(BOP VAR VAR)",
            "(BOP VAR CONST)",
            "(BOP CONST VAR)",
        ])
        .force();

        assert_eq!(actual3, expected3);

        let actual4 = iter_metric(base_lang(), "EXPR", Metric::Atoms, 4)
            .filter(Filter::Contains("VAR".parse().unwrap()))
            .force();

        let expected4 = Workload::new([
            "VAR",
            "(UOP VAR)",
            "(UOP (UOP VAR))",
            "(UOP (UOP (UOP VAR)))",
            "(UOP (BOP VAR VAR))",
            "(UOP (BOP VAR CONST))",
            "(UOP (BOP CONST VAR))",
            "(BOP VAR VAR)",
            "(BOP VAR CONST)",
            "(BOP VAR (UOP VAR))",
            "(BOP VAR (UOP CONST))",
            "(BOP CONST VAR)",
            "(BOP CONST (UOP VAR))",
            "(BOP (UOP VAR) VAR)",
            "(BOP (UOP VAR) CONST)",
            "(BOP (UOP CONST) VAR)",
            "(TOP VAR VAR VAR)",
            "(TOP VAR VAR CONST)",
            "(TOP VAR CONST VAR)",
            "(TOP VAR CONST CONST)",
            "(TOP CONST VAR VAR)",
            "(TOP CONST VAR CONST)",
            "(TOP CONST CONST VAR)",
        ])
        .force();

        assert_eq!(actual4, expected4);
    }

    #[test]
    fn plug() {
        let w1 = Workload::new(["x", "(x x)", "(x x x)"]);
        let w2 = Workload::new(["1", "2"]);

        let expected = Workload::new([
            "1", "2", "(1 1)", "(1 2)", "(2 1)", "(2 2)", "(1 1 1)", "(1 1 2)", "(1 2 1)",
            "(1 2 2)", "(2 1 1)", "(2 1 2)", "(2 2 1)", "(2 2 2)",
        ]);
        let actual = w1.plug("x", &w2).force();
        for t in expected.force() {
            assert!(actual.contains(&t));
        }
    }

    #[test]
    fn append() {
        let empty = Workload::Set(vec![]);
        let w1 = Workload::new(["a", "b"]);
        let w2 = Workload::new(["c", "d"]);
        let w3 = Workload::new(["e", "f"]);
        let w4 = Workload::new(["a"]).plug("a", &empty);

        let wkld = w1.clone().append(w2.clone());
        let wkld = wkld.append(w3.clone());
        assert_eq!(wkld.force().len(), 6);
        assert!(matches!(wkld, Workload::Set(_)));

        let wkld = w3.clone().append(w4.clone());
        let wkld2 = wkld.clone().append(w1.clone());
        assert!(matches!(wkld, Workload::Append(_)));
        assert!(matches!(wkld2, Workload::Append(_)));
        if let Workload::Append(lst) = wkld2 {
            assert_eq!(lst.len(), 3);
        }

        let wkld = w3.clone().append(w4.clone());
        let wkld2 = w1.clone().append(wkld.clone());
        assert!(matches!(wkld, Workload::Append(_)));
        assert!(matches!(wkld2, Workload::Append(_)));
        if let Workload::Append(lst) = wkld2 {
            assert_eq!(lst.len(), 3);
        }

        let wkld = w3.clone().append(w4.clone());
        let wkld2 = w3.clone().append(w4.clone());
        let wkld3 = wkld.clone().append(wkld2.clone());
        assert!(matches!(wkld, Workload::Append(_)));
        assert!(matches!(wkld2, Workload::Append(_)));
        assert!(matches!(wkld3, Workload::Append(_)));
        if let Workload::Append(lst) = wkld3 {
            assert_eq!(lst.len(), 4);
        }
    }
}
