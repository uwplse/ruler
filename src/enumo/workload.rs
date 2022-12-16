use egg::{EGraph, ENodeOrVar, RecExpr};

use crate::{HashSet, SynthAnalysis, SynthLanguage};

use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Workload {
    Set(Vec<Sexp>),
    Plug(Box<Self>, String, Box<Self>),
    Filter(Filter, Box<Self>),
    Append(Vec<Self>),
}

impl Workload {
    pub fn from_vec(strs: Vec<&str>) -> Self {
        Self::Set(strs.iter().map(|x| x.parse().unwrap()).collect())
    }

    pub fn get_vars<L: SynthLanguage>(&self) -> Vec<String> {
        let mut vars: HashSet<String> = HashSet::default();
        for sexp in self.force() {
            let expr: RecExpr<L> = sexp.to_string().parse().unwrap();
            for node in expr.as_ref() {
                if let ENodeOrVar::Var(v) = node.clone().to_enode_or_var() {
                    let mut v = v.to_string();
                    v.remove(0);
                    vars.insert(v);
                }
            }
        }
        vars.into_iter().collect()
    }

    pub fn to_egraph<L: SynthLanguage>(&self) -> EGraph<L, SynthAnalysis> {
        let mut egraph = EGraph::default();
        let vars = self.get_vars::<L>();
        L::initialize_vars(&mut egraph, &vars);
        for sexp in self.force() {
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
        let lang = Workload::from_vec(vec!["cnst", "var", "(uop expr)", "(bop expr expr)"]);

        lang.iter_metric("expr", Metric::Atoms, n)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug("cnst", &Workload::from_vec(consts.to_vec()))
            .plug("var", &Workload::from_vec(vars.to_vec()))
            .plug("uop", &Workload::from_vec(uops.to_vec()))
            .plug("bop", &Workload::from_vec(bops.to_vec()))
    }

    pub fn plug(self, name: impl Into<String>, workload: &Workload) -> Self {
        Workload::Plug(Box::new(self), name.into(), Box::new(workload.clone()))
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iter() {
        let lang = Workload::from_vec(vec!["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let actual2 = lang.clone().iter("expr", 2).force();
        assert_eq!(actual2.len(), 8);

        let actual3 = lang.iter("expr", 3).force();
        assert_eq!(actual3.len(), 74);
    }

    #[test]
    fn iter_metric() {
        let lang = Workload::from_vec(vec!["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let actual2 = lang.clone().iter_metric("expr", Metric::Atoms, 2).force();
        assert_eq!(actual2.len(), 4);

        let actual3 = lang.iter_metric("expr", Metric::Atoms, 3).force();
        assert_eq!(actual3.len(), 10);
    }

    #[test]
    fn iter_metric_fast() {
        // This test will not finish if the pushing monotonic filters through plugs optimization is not working.
        let lang = Workload::from_vec(vec!["cnst", "var", "(uop expr)", "(bop expr expr)"]);
        let six = lang.iter_metric("expr", Metric::Atoms, 6);
        assert_eq!(six.force().len(), 188);
    }

    #[test]
    fn contains() {
        let lang = Workload::from_vec(vec!["cnst", "var", "(uop expr)", "(bop expr expr)"]);

        let actual3 = lang
            .clone()
            .iter_metric("expr", Metric::Atoms, 3)
            .filter(Filter::Contains("var".parse().unwrap()))
            .force();

        let expected3 = Workload::from_vec(vec![
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

        let expected4 = Workload::from_vec(vec![
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
