use egg::{EGraph, ENodeOrVar, RecExpr};

use super::*;
use crate::{llm, IndexSet, SynthAnalysis, SynthLanguage};
use std::{io::Write, time::Instant};

/// Workloads are sets of terms from a domain
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
            writeln!(file, "{name}").expect("Unable to write");
        }
    }

    pub fn from_file(filename: &str) -> Self {
        let infile = std::fs::File::open(filename).expect("can't open file");
        let reader = std::io::BufReader::new(infile);
        let mut sexps = vec![];
        for line in std::io::BufRead::lines(reader) {
            let line = line.unwrap();
            if line.trim().is_empty() {
                continue;
            }
            // Tolerate malformed terms, but report
            match line.parse() {
                Ok(sexp) => sexps.push(sexp),
                Err(e) => eprintln!("Skipping invalid term in {filename}: {e}"),
            }
        }
        Self::Set(sexps)
    }

    /// Build a workload by prompting every LLM in `llm::models()` with
    /// `prompt`. Lines that do not parse as s-expressions are reported and
    /// skipped; terms are deduplicated across models, preserving
    /// first-seen order. Note that the terms are *not* checked against any
    /// particular language here — chain with `as_lang`/`as_lang_with_vars`
    /// for that.
    pub async fn from_llm(prompt: &str) -> Self {
        let mut terms: IndexSet<Sexp> = IndexSet::default();
        for model in llm::models() {
            let start = Instant::now();
            let before = terms.len();
            let mut invalid = 0;
            for line in llm::query(prompt, &model).await {
                match line.parse() {
                    Ok(sexp) => {
                        terms.insert(sexp);
                    }
                    Err(e) => {
                        invalid += 1;
                        eprintln!("Skipping invalid term from {model}: {e}");
                    }
                }
            }
            println!(
                "{} | {} new terms ({} invalid lines) | {:?}",
                model,
                terms.len() - before,
                invalid,
                start.elapsed()
            );
        }
        println!("Combined LLM workload: {} terms", terms.len());
        Workload::Set(terms.into_iter().collect())
    }

    /// Filter the workload down to terms that parse in the language `L`
    /// (with no restriction on which variables appear). Workload terms are
    /// untyped s-expressions, and `to_egraph` panics on terms outside `L`;
    /// use this to safely ingest terms from untrusted sources
    pub fn as_lang<L: SynthLanguage>(&self) -> Self {
        self.as_lang_with_vars::<L>(vec![])
    }

    /// Filter the workload down to terms that parse in the language `L`
    /// and (if `expected_vars` is non-empty) mention only those variables.
    /// Filtered-out terms are reported to stderr.
    pub fn as_lang_with_vars<L: SynthLanguage>(&self, expected_vars: Vec<String>) -> Self {
        Workload::Set(
            self.force()
                .iter()
                .filter(|sexp| match sexp.to_string().parse::<RecExpr<L>>() {
                    Ok(expr) => expr.as_ref().iter().all(|node| {
                        if let ENodeOrVar::Var(v) = node.clone().to_enode_or_var() {
                            let mut v = v.to_string();
                            v.remove(0); // strip the leading '?'
                            let ok = expected_vars.is_empty() || expected_vars.contains(&v);
                            if !ok {
                                eprintln!("Skipping term with unexpected var '{v}': {sexp}");
                            }
                            ok
                        } else {
                            true
                        }
                    }),
                    Err(_) => {
                        eprintln!("Skipping term that does not parse in the language: {sexp}");
                        false
                    }
                })
                .cloned()
                .collect(),
        )
    }

    /// Materialize the workload into an e-graph
    /// Will crash if there are terms in the e-graph that are not parseable as terms in domain L
    pub fn to_egraph<L: SynthLanguage>(&self) -> EGraph<L, SynthAnalysis> {
        let mut egraph = EGraph::default();
        let sexps = self.force();

        // Have to find all the variables first so that we can initialize
        // their cvecs, which might require doing a multi-way cross product
        // based on how many variables there are.
        // We have to do this before adding any other expressions to the
        // egraph so that the variable cvecs are properly initialized and
        // able to be used by other expressions that contain variables
        // For some reason, it appears the order we initialize these variables
        // can matter, so make sure we preserve the order in the workload.
        // TODO: why does this order matter?
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

        for sexp in sexps.iter() {
            egraph.add_expr(&sexp.to_string().parse::<RecExpr<L>>().unwrap());
        }
        egraph
    }

    /// Materialize workload into a vector of s-expressions
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
                // Deduplicate across the appended workloads. IndexSet
                // preserves insertion order, which matters downstream
                // (e.g. variable initialization order in to_egraph).
                let mut set: IndexSet<Sexp> = IndexSet::default();
                for w in workloads {
                    set.extend(w.force());
                }
                set.into_iter().collect()
            }
        }
    }

    pub fn pretty_print(&self) {
        for t in self.force() {
            println!("{t}");
        }
    }

    pub fn plug(self, name: impl Into<String>, workload: &Workload) -> Self {
        match workload {
            // Empty plug is the same as filter excludes
            Workload::Set(xs) if xs.is_empty() => {
                self.filter(Filter::Excludes(name.into().parse().unwrap()))
            }
            _ => Workload::Plug(Box::new(self), name.into(), Box::new(workload.clone())),
        }
    }

    pub fn append(self, workload: impl Into<Workload>) -> Self {
        let into: Workload = workload.into();
        match (self, into) {
            (Workload::Set(xs), Workload::Set(ys)) => {
                // Deduplicate, preserving first-occurrence order
                let mut all: IndexSet<Sexp> = IndexSet::default();
                all.extend(xs);
                all.extend(ys);
                Workload::Set(all.into_iter().collect())
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
        match self {
            Workload::Plug(wkld, name, pegs) if filter.is_monotonic() => Workload::Filter(
                filter.clone(),
                Box::new(Workload::Plug(wkld, name, Box::new(pegs.filter(filter)))),
            ),
            Workload::Filter(f, w) => w.filter(f.and(filter)),
            _ => Workload::Filter(filter, Box::new(self)),
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
        let actual3 = iter_metric(base_lang(3), "EXPR", Metric::Atoms, 3)
            .filter(Filter::Contains("VAR".parse().unwrap()))
            .force();

        let expected3 = Workload::new([
            "VAR",
            "(OP1 VAR)",
            "(OP1 (OP1 VAR))",
            "(OP2 VAR VAR)",
            "(OP2 VAR VAL)",
            "(OP2 VAL VAR)",
        ])
        .force();

        assert_eq!(actual3, expected3);

        let actual4 = iter_metric(base_lang(3), "EXPR", Metric::Atoms, 4)
            .filter(Filter::Contains("VAR".parse().unwrap()))
            .force();

        let expected4 = Workload::new([
            "VAR",
            "(OP1 VAR)",
            "(OP1 (OP1 VAR))",
            "(OP1 (OP1 (OP1 VAR)))",
            "(OP1 (OP2 VAR VAR))",
            "(OP1 (OP2 VAR VAL))",
            "(OP1 (OP2 VAL VAR))",
            "(OP2 VAR VAR)",
            "(OP2 VAR VAL)",
            "(OP2 VAR (OP1 VAR))",
            "(OP2 VAR (OP1 VAL))",
            "(OP2 VAL VAR)",
            "(OP2 VAL (OP1 VAR))",
            "(OP2 (OP1 VAR) VAR)",
            "(OP2 (OP1 VAR) VAL)",
            "(OP2 (OP1 VAL) VAR)",
            "(OP3 VAR VAR VAR)",
            "(OP3 VAR VAR VAL)",
            "(OP3 VAR VAL VAR)",
            "(OP3 VAR VAL VAL)",
            "(OP3 VAL VAR VAR)",
            "(OP3 VAL VAR VAL)",
            "(OP3 VAL VAL VAR)",
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
    fn append_dups() {
        // Appending two Sets dedups, preserving first-occurrence order
        let w1 = Workload::new(["a", "b", "x"]);
        let w2 = Workload::new(["c", "x", "d", "d"]);
        let appended = w1.append(w2).force();
        assert_eq!(appended, Workload::new(["a", "b", "x", "c", "d"]).force());

        // Forcing an Append also dedups
        let apps = Workload::Append(vec![Workload::new(["a", "b"]), Workload::new(["b", "c"])]);
        assert_eq!(apps.force(), Workload::new(["a", "b", "c"]).force());
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
