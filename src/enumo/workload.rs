use egg::{EGraph, ENodeOrVar, RecExpr};

use crate::{HashSet, SynthAnalysis, SynthLanguage};

use super::*;

/// A `Workload` compactly and lazily describes the set of terms that the initial
/// egraph will be seeded with. Terms are described in a top-down manner. For
/// example, you can start with the following pattern:
///
/// ```lisp
/// (binop expr expr)
/// ```
///
/// By itself, this only describes a single term. However, you can "plug" in other
/// workloads to any atom in the pattern. So if you have a workload that describes
/// the terms: `[+, -]`. You can plug that in for `binop` to get the terms:
///
/// ```lisp
/// (+ expr expr)
/// (- expr expr)
/// ```
///
/// You can now expand `expr` to get more expressions. You can plug in the workload
/// `[0, 1, a, b]` for `expr` to get the terms:
///
/// ```lisp
/// (+ 0 0) (+ 1 0) (+ a 0) (+ b 0)
/// (+ 0 1) (+ 1 1) (+ a 1) (+ b 1)
/// (+ 0 a) (+ 1 a) (+ a a) (+ b a)
/// (+ 0 b) (+ 1 b) (+ a b) (+ b b)
///
/// (- 0 0) (- 1 0) (- a 0) (- b 0)
/// (- 0 1) (- 1 1) (- a 1) (- b 1)
/// (- 0 a) (- 1 a) (- a a) (- b a)
/// (- 0 b) (- 1 b) (- a b) (- b b)
/// ```
///
/// This simple mechanism allows you to express a large number of terms through
/// the composition of smaller workloads.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Workload {
    Set(Vec<Sexp>),
    Plug(Box<Self>, String, Box<Self>),
    Filter(Filter, Box<Self>),
    Append(Vec<Self>),
}

impl Workload {
    /// Construct a new workload from anything that can iterator over `&str`.
    pub fn new<'a>(vals: impl IntoIterator<Item = &'a str>) -> Self {
        Self::Set(vals.into_iter().map(|x| x.parse().unwrap()).collect())
    }

    /// Construct an `EGraph` from the terms represented by this workload.
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

    /// Force the construction of all the terms represented in this workload.
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

    /// Recursively expands `self` to a depth of `n` by plugging in `self` for `atom`.
    ///
    /// For the workload:
    /// ```
    /// let wkld = Workload::new(["x", "(bop expr expr)"]);
    /// ```
    ///
    /// `wkld.iter("expr", 2)` generates
    ///
    /// ```
    /// x
    /// (uop x x)
    /// ```
    ///
    /// and `wkld.iter("expr", 3)` generates
    ///
    /// ```
    /// x
    /// (uop x x)
    /// (uop x (uop x x))
    /// (uop (uop x x) x)
    /// (uop (uop x x) (uop x x))
    /// ```
    ///
    ///
    fn iter(self, atom: &str, n: usize) -> Self {
        if n == 0 {
            Self::Set(vec![])
        } else {
            let rec = self.clone().iter(atom, n - 1);
            self.plug(atom, rec)
        }
    }

    /// Expands the workload at `atom` up to a depth of `n` and then filters by `met`.
    pub fn iter_metric(self, atom: &str, met: Metric, n: usize) -> Self {
        self.iter(atom, n).filter(Filter::MetricLt(met, n + 1))
    }

    /// A convenience function to quickly create a workload for a standard language.
    ///  - `n`: The depth of terms to generate in the language
    ///  - `consts`: The constant expressions in the language
    ///  - `vars`: The variables in the language
    ///  - `uops`: The unary operators in the language
    ///  - `bops`: The binary operators in the language
    ///
    /// The following workload:
    ///
    /// ```rust
    /// Workload::iter_lang(
    ///   3,
    ///   &["0", "1"],
    ///   &["x", "y"],
    ///   &["~"],
    ///   &["+", "-"]
    /// )
    /// ```
    ///
    /// will generate terms in a simple arithmetic language.
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
            .plug("cnst", consts)
            .plug("var", vars)
            .plug("uop", uops)
            .plug("bop", bops)
    }

    /// Compose two workloads together by replacing every instance of `name` with
    /// `workload`.
    pub fn plug(self, name: impl Into<String>, workload: impl Into<Workload>) -> Self {
        Workload::Plug(Box::new(self), name.into(), Box::new(workload.into()))
    }

    /// Append to workloads together.
    pub fn append(self, workload: impl Into<Workload>) -> Self {
        Workload::Append(vec![self, workload.into()])
    }

    /// Modify a workload by excluding all the terms matched by `filter`.
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
