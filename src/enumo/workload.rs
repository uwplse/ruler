use egg::{EGraph, ENodeOrVar, RecExpr};

use super::*;
use crate::{HashSet, SynthAnalysis, SynthLanguage};
use std::{fs::OpenOptions, io::Write};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Workload {
    Set(Vec<Sexp>),
    Plug(Box<Self>, String, Box<Self>),
    Filter(Filter, Box<Self>),
    Append(Vec<Self>),
}

impl Workload {
    pub fn new<'a>(vals: impl IntoIterator<Item = &'a str>) -> Self {
        Self::Set(vals.into_iter().map(|x| x.parse().unwrap()).collect())
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
        let sexps = self.force();

        // Have to find all the variables first so that we can initialize
        // their cvecs, which might require doing a multi-way cross product
        // based on how many variables there are.
        // We have to do this before adding any other expressions to the
        // egraph so that the variable cvecs are properly initialized and
        // able to be used by other expressions that contain variables
        let mut vars: HashSet<String> = HashSet::default();
        for sexp in sexps.iter() {
            let expr: RecExpr<L> = sexp.to_string().parse().unwrap();
            for node in expr.as_ref() {
                if let ENodeOrVar::Var(v) = node.clone().to_enode_or_var() {
                    let mut v = v.to_string();
                    v.remove(0);
                    vars.insert(v);
                }
            }
        }
        let vars: Vec<String> = vars.into_iter().collect();
        L::initialize_vars(&mut egraph, &vars);

        for sexp in sexps.iter() {
            egraph.add_expr(&sexp.to_string().parse::<RecExpr<L>>().unwrap());
        }
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

    fn iter(self, atom: &str, n: usize) -> Self {
        if n == 0 {
            Self::Set(vec![])
        } else {
            let rec = self.clone().iter(atom, n - 1);
            self.plug(atom, &rec)
        }
    }

    pub fn iter_metric(self, atom: &str, met: Metric, n: usize) -> Self {
        self.iter(atom, n).filter(Filter::MetricLt(met, n + 1))
    }

    pub fn iter_lang(
        n: usize,
        consts: &[&str],
        vars: &[&str],
        uops: &[&str],
        bops: &[&str],
    ) -> Self {
        let lang = Workload::new(["cnst", "var", "(uop expr)", "(bop expr expr)"]);

        lang.iter_metric("expr", Metric::Atoms, n)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug("cnst", &consts.into())
            .plug("var", &vars.into())
            .plug("uop", &uops.into())
            .plug("bop", &bops.into())
    }

    pub fn plug(self, name: impl Into<String>, workload: &Workload) -> Self {
        Workload::Plug(Box::new(self), name.into(), Box::new(workload.clone()))
    }

    pub fn append(self, workload: impl Into<Workload>) -> Self {
        Workload::Append(vec![self, workload.into()])
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

    pub fn make_layer(exprs: Workload, uops: Workload, bops: Workload) -> Self {
        let lang = Workload::new(["expr", "(uop expr)", "(bop expr expr)"]);

        lang.plug("expr", &exprs)
            .plug("uop", &uops)
            .plug("bop", &bops)
    }

    pub fn make_layer_clever(
        l0: Workload,
        l1: Workload,
        l2: Workload,
        uops: Workload,
        bops: Workload,
        list_len: usize,
    ) -> Self {
        let lang = Workload::new([
            "l0",
            "l1",
            "l2",
            "(uop l2)",
            "(bop l1 l1)",
            "(bop l0 l2)",
            "(bop l2 l0)",
        ]);
        lang.plug("uop", &uops)
            .plug("bop", &bops)
            .plug("l0", &l0)
            .plug("l1", &l1)
            .plug("l2", &l2)
            .filter(Filter::MetricLt(Metric::List, list_len + 1))
            .filter(Filter::Invert(Box::new(Filter::MetricLt(
                Metric::List,
                std::cmp::max(0, list_len - 1),
            ))))
    }

    pub fn write_terms_to_file(&self, filename: &str) {
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(filename)
            .expect("Unable to open file");
        let terms: Vec<String> = self
            .clone()
            .force()
            .iter()
            .map(|se| se.to_string())
            .collect();
        terms.iter().for_each(|t| {
            file.write_all(t.as_bytes())
                .expect("Unable to write to file");
            file.write_all("\n".as_bytes())
                .expect("Unable to write to file");
        });
    }
}

impl From<&[&str]> for Workload {
    fn from(value: &[&str]) -> Self {
        Workload::new(value.iter().copied())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iter() {
        let lang = Workload::new(["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let actual2 = lang.clone().iter("expr", 2).force();
        assert_eq!(actual2.len(), 8);

        let actual3 = lang.iter("expr", 3).force();
        assert_eq!(actual3.len(), 74);
    }

    #[test]
    fn iter_metric() {
        let lang = Workload::new(["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let actual2 = lang.clone().iter_metric("expr", Metric::Atoms, 2).force();
        assert_eq!(actual2.len(), 4);

        let actual3 = lang.iter_metric("expr", Metric::Atoms, 3).force();
        assert_eq!(actual3.len(), 10);
    }

    #[test]
    fn iter_metric_fast() {
        // This test will not finish if the pushing monotonic filters through plugs optimization is not working.
        let lang = Workload::new(["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let six = lang.iter_metric("expr", Metric::Atoms, 6);
        assert_eq!(six.force().len(), 188);
    }

    #[test]
    fn contains() {
        let lang = Workload::new(["cnst", "var", "(uop expr)", "(bop expr expr)"]);

        let actual3 = lang
            .clone()
            .iter_metric("expr", Metric::Atoms, 3)
            .filter(Filter::Contains("var".parse().unwrap()))
            .force();

        let expected3 = Workload::new([
            "var",
            "(uop var)",
            "(uop (uop var))",
            "(bop cnst var)",
            "(bop var cnst)",
            "(bop var var)",
        ])
        .force();

        assert_eq!(actual3, expected3);

        let actual4 = lang
            .iter_metric("expr", Metric::Atoms, 4)
            .filter(Filter::Contains("var".parse().unwrap()))
            .force();

        let expected4 = Workload::new([
            "var",
            "(uop var)",
            "(uop (uop var))",
            "(uop (uop (uop var)))",
            "(uop (bop cnst var))",
            "(uop (bop var cnst))",
            "(uop (bop var var))",
            "(bop cnst var)",
            "(bop cnst (uop var))",
            "(bop var cnst)",
            "(bop var var)",
            "(bop var (uop cnst))",
            "(bop var (uop var))",
            "(bop (uop cnst) var)",
            "(bop (uop var) cnst)",
            "(bop (uop var) var)",
        ])
        .force();

        assert_eq!(actual4, expected4);
    }
}
