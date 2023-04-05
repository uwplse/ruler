use egg::{EGraph, ENodeOrVar, RecExpr};

use super::*;
use crate::{SynthAnalysis, SynthLanguage};
use std::{fs::OpenOptions, io::Write};

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

    pub fn iter_metric(self, atom: &str, met: Metric, n: usize) -> Self {
        let mut pegs = self.clone();
        for i in 1..(n + 1) {
            pegs = self
                .clone()
                .plug(atom, &pegs)
                .filter(Filter::MetricLt(met, i + 1));
        }
        pegs
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

    pub fn plug_lang(
        self,
        vars: &Workload,
        consts: &Workload,
        uops: &Workload,
        bops: &Workload,
    ) -> Self {
        self.plug("var", vars)
            .plug("const", consts)
            .plug("uop", uops)
            .plug("bop", bops)
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
            .filter(Filter::MetricLt(Metric::Lists, list_len + 1))
            .filter(Filter::Invert(Box::new(Filter::MetricLt(
                Metric::Lists,
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
    fn iter_metric() {
        let lang = Workload::new(["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let atoms1 = lang.clone().iter_metric("expr", Metric::Atoms, 1).force();
        assert_eq!(atoms1.len(), 2);

        let atoms2 = lang.clone().iter_metric("expr", Metric::Atoms, 2).force();
        assert_eq!(atoms2.len(), 4);

        let atoms3 = lang.clone().iter_metric("expr", Metric::Atoms, 3).force();
        assert_eq!(atoms3.len(), 10);

        let atoms4 = lang.clone().iter_metric("expr", Metric::Atoms, 4).force();
        assert_eq!(atoms4.len(), 24);

        let atoms5 = lang.clone().iter_metric("expr", Metric::Atoms, 5).force();
        assert_eq!(atoms5.len(), 66);

        let atoms6 = lang.clone().iter_metric("expr", Metric::Atoms, 6).force();
        assert_eq!(atoms6.len(), 188);

        let depth1 = lang.clone().iter_metric("expr", Metric::Depth, 1).force();
        assert_eq!(depth1.len(), 2);

        let depth2 = lang.clone().iter_metric("expr", Metric::Depth, 2).force();
        assert_eq!(depth2.len(), 8);

        let depth3 = lang.clone().iter_metric("expr", Metric::Depth, 3).force();
        assert_eq!(depth3.len(), 74);

        let depth4 = lang.clone().iter_metric("expr", Metric::Depth, 4).force();
        assert_eq!(depth4.len(), 5552);

        let lists1 = lang.clone().iter_metric("expr", Metric::Lists, 1).force();
        assert_eq!(lists1.len(), 8);

        let lists2 = lang.clone().iter_metric("expr", Metric::Lists, 2).force();
        assert_eq!(lists2.len(), 38);

        let lists3 = lang.clone().iter_metric("expr", Metric::Lists, 3).force();
        assert_eq!(lists3.len(), 224);
    }

    #[test]
    fn iter_metric_fast() {
        // This test will not finish if the pushing monotonic filters through plugs optimization is not working.
        let lang = Workload::new(["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let six = lang.iter_metric("expr", Metric::Atoms, 6);
        assert_eq!(six.force().len(), 188);

        let extended = Workload::new([
            "cnst",
            "var",
            "(uop expr)",
            "(bop expr expr)",
            "(top expr expr expr)",
        ]);
        let three = extended.clone().iter_metric("expr", Metric::Atoms, 3);
        assert_eq!(three.force().len(), 10);

        let four = extended.clone().iter_metric("expr", Metric::Atoms, 4);
        assert_eq!(four.force().len(), 32);

        let five = extended.clone().iter_metric("expr", Metric::Atoms, 5);
        assert_eq!(five.force().len(), 106);

        let six = extended.clone().iter_metric("expr", Metric::Atoms, 6);
        assert_eq!(six.force().len(), 388);
    }

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
