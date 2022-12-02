use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Workload {
    Set(Vec<Sexp>),
    Plug(Box<Self>, String, Box<Self>),
    Filter(Filter, Box<Self>),
    Append(Vec<Self>),
}

impl Workload {
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
    use crate::*;

    #[test]
    fn iter() {
        let lang = Workload::Set(vec![s!(cnst), s!(var), s!((uop expr)), s!((bop expr expr))]);
        let actual2 = lang.clone().iter("expr", 2).force();
        assert_eq!(actual2.len(), 8);

        let actual3 = lang.iter("expr", 3).force();
        assert_eq!(actual3.len(), 74);
    }

    #[test]
    fn iter_metric() {
        let lang = Workload::Set(vec![s!(cnst), s!(var), s!((uop expr)), s!((bop expr expr))]);
        let actual2 = lang.clone().iter_metric("expr", Metric::Atoms, 2).force();
        assert_eq!(actual2.len(), 4);

        let actual3 = lang.iter_metric("expr", Metric::Atoms, 3).force();
        assert_eq!(actual3.len(), 10);
    }

    #[test]
    fn iter_metric_fast() {
        // This test will not finish if the pushing monotonic filters through plugs optimization is not working.
        let lang = Workload::Set(vec![s!(cnst), s!(var), s!((uop expr)), s!((bop expr expr))]);
        let six = lang.iter_metric("expr", Metric::Atoms, 6);
        assert_eq!(six.force().len(), 188);
    }

    #[test]
    fn contains() {
        let lang = Workload::Set(vec![s!(cnst), s!(var), s!((uop expr)), s!((bop expr expr))]);

        let actual3 = lang
            .clone()
            .iter_metric("expr", Metric::Atoms, 3)
            .filter(Filter::Contains(enumo::Pattern::Lit("var".into())))
            .force();

        let expected3 = vec![
            s!(var),
            s!(uop var),
            s!((uop (uop var))),
            s!(bop cnst var),
            s!(bop var cnst),
            s!(bop var var),
        ];

        assert_eq!(actual3, expected3);

        let actual4 = lang
            .iter_metric("expr", Metric::Atoms, 4)
            .filter(Filter::Contains(enumo::Pattern::Lit("var".into())))
            .force();

        let expected4 = vec![
            s!(var),
            s!((uop var)),
            s!((uop (uop var))),
            s!((uop (uop (uop var)))),
            s!((uop (bop cnst var))),
            s!((uop (bop var cnst))),
            s!((uop (bop var var))),
            s!((bop cnst var)),
            s!((bop cnst (uop var))),
            s!((bop var cnst)),
            s!((bop var var)),
            s!((bop var (uop cnst))),
            s!((bop var (uop var))),
            s!((bop (uop cnst) var)),
            s!((bop (uop var) cnst)),
            s!((bop (uop var) var)),
        ];

        assert_eq!(actual4, expected4);
    }
}
