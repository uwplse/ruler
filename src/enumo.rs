use crate::HashMap;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Sexp {
    Atom(String),
    List(Vec<Self>),
}

impl std::fmt::Display for Sexp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexp::Atom(x) => write!(f, "{}", x),
            Sexp::List(l) => {
                write!(f, "(").expect("not written");
                for x in l {
                    write!(f, "{} ", x).expect("not written");
                }
                write!(f, ")").expect("not written");
                Ok(())
            }
        }
    }
}

impl Sexp {
    fn plug(&self, name: &str, pegs: &[Self]) -> Vec<Sexp> {
        use itertools::Itertools;
        match self {
            Sexp::Atom(s) if s == name => pegs.to_vec(),
            Sexp::Atom(_) => vec![self.clone()],
            Sexp::List(sexps) => sexps
                .iter()
                .map(|x| x.plug(name, pegs))
                .multi_cartesian_product()
                .map(Sexp::List)
                .collect(),
        }
    }

    fn measure(&self, metric: Metric) -> usize {
        match self {
            Sexp::Atom(_) => match metric {
                Metric::List => 0,
                Metric::Atoms | Metric::Depth => 1,
            },
            Sexp::List(s) => match metric {
                Metric::Atoms => s.len(),
                Metric::List => s.iter().map(|x| x.measure(metric)).sum::<usize>() + 1,
                Metric::Depth => s.iter().map(|x| x.measure(metric)).max().unwrap() + 1,
            },
        }
    }
}

#[derive(Copy, PartialEq, Eq, Clone, Debug)]
enum Metric {
    Atoms,
    List,
    Depth,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum EnumoPattern {
    Wild,
    Var(String),
    Lit(String),
    List(Vec<EnumoPattern>),
}

impl EnumoPattern {
    fn matches(
        &self,
        sexp: &Sexp,
        mut subst: HashMap<String, Sexp>,
    ) -> Option<HashMap<String, Sexp>> {
        match self {
            EnumoPattern::Wild => {
                // Wild matches anything
                Some(subst)
            }
            EnumoPattern::Var(pvar) => {
                // Variables can match anything but must match consistently
                if let Some(x) = subst.get(pvar) {
                    // pvar already bound, check consistency
                    if sexp.eq(x) {
                        Some(subst)
                    } else {
                        None
                    }
                } else {
                    // pvar not yet bound, just add to subst
                    subst.insert(pvar.clone(), sexp.clone());
                    Some(subst)
                }
            }
            EnumoPattern::Lit(plit) => match sexp {
                Sexp::Atom(lit) => {
                    if plit == lit {
                        Some(subst)
                    } else {
                        None
                    }
                }
                Sexp::List(_) => None,
            },
            EnumoPattern::List(pats) => match sexp {
                Sexp::Atom(_) => None,
                Sexp::List(args) => {
                    if pats.len() == args.len() {
                        pats.iter()
                            .zip(args.iter())
                            .fold(Some(subst), |acc, (pat, sexp)| {
                                if let Some(subst) = acc {
                                    pat.matches(sexp, subst)
                                } else {
                                    None
                                }
                            })
                    } else {
                        None
                    }
                }
            },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Filter {
    MetricLt(Metric, usize),
    Contains(EnumoPattern),
    Canon(Vec<String>),
    And(Box<Self>, Box<Self>),
}

impl Filter {
    fn test(&self, sexp: &Sexp) -> bool {
        match self {
            Filter::MetricLt(metric, n) => sexp.measure(*metric) < *n,
            Filter::Contains(pat) => {
                pat.matches(sexp, Default::default()).is_some()
                    || match sexp {
                        Sexp::Atom(_) => false,
                        Sexp::List(args) => args
                            .iter()
                            .any(|s| pat.matches(s, Default::default()).is_some()),
                    }
            }
            Filter::Canon(_) => todo!(),
            Filter::And(f1, f2) => f1.test(sexp) && f2.test(sexp),
        }
    }

    fn is_monotonic(&self) -> bool {
        match self {
            Filter::MetricLt(_, _) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Workload {
    Set(Vec<Sexp>),
    Plug(Box<Self>, String, Box<Self>),
    Filter(Filter, Box<Self>),
    Append(Vec<Self>),
}

impl Workload {
    fn force(&self) -> Vec<Sexp> {
        match self {
            Workload::Set(set) => set.clone(),
            Workload::Plug(tgt, name, workload) => {
                let mut res = vec![];
                let workload = workload.force();
                for sexp in tgt.force() {
                    res.extend(sexp.plug(&name, &workload));
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
            self.plug(atom, rec)
        }
    }

    fn plug(self, name: impl Into<String>, workload: Workload) -> Self {
        Workload::Plug(Box::new(self), name.into(), Box::new(workload))
    }

    fn filter(self, filter: Filter) -> Self {
        if filter.is_monotonic() {
            if let Workload::Plug(wkld, name, pegs) = self {
                Workload::Plug(Box::new(wkld.filter(filter)), name, pegs)
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

    macro_rules! s {
        (( $($x:tt)* )) => { Sexp::List(vec![ $(s!($x)),* ]) };
        ($x:tt) => { Sexp::Atom(format!(stringify!($x))) };
        ($($x:tt)*) => { s!(( $($x)* )) };
    }

    #[test]
    fn test_contains() {
        let wkld = Workload::Set(vec![
            s!((+ a a)),
            s!((+ a b)),
            s!((+ a (+ a b))),
            s!((+ a (+ b b))),
            s!((+ (+ a b) (+ a b))),
            s!((+ (+ a b) (+ b a)) ),
        ]);
        let pat = EnumoPattern::List(vec![
            EnumoPattern::Lit("+".into()),
            EnumoPattern::Var("?x".into()),
            EnumoPattern::Var("?x".into()),
        ]);
        let actual = wkld.filter(Filter::Contains(pat)).force();
        let expected = vec![s!((+ a a)), s!((+ a (+ b b))), s!((+ (+ a b) (+ a b)))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn filter_and() {
        let wkld = Workload::Set(vec![
            s!(x),
            s!(y),
            s!(x y),
            s!(y x),
            s!(x x x),
            s!(y y z),
            s!(x y z),
        ]);
        let actual = wkld
            .filter(Filter::And(
                Box::new(Filter::Contains(EnumoPattern::Lit("x".into()))),
                Box::new(Filter::Contains(EnumoPattern::Lit("y".into()))),
            ))
            .force();
        let expected = vec![s!(x y), s!(y x), s!(x y z)];
        assert_eq!(actual, expected);
    }

    #[test]
    fn simple_plug() {
        let x = s!(x);
        let expected = vec![x.clone()];
        let actual = x.plug("a", &[s!(1), s!(2)]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn simple_plug2() {
        let x = s!(x);
        let pegs = vec![s!(1), s!(2)];
        let expected = pegs.clone();
        let actual = x.plug("x", &pegs);
        assert_eq!(actual, expected);
    }

    #[test]
    fn plug_cross_product() {
        let term = s!(x x);
        let pegs = vec![s!(1), s!(2), s!(3)];
        let expected = vec![
            s!(1 1),
            s!(1 2),
            s!(1 3),
            s!(2 1),
            s!(2 2),
            s!(2 3),
            s!(3 1),
            s!(3 2),
            s!(3 3),
        ];
        let actual = term.plug("x", &pegs);
        assert_eq!(actual, expected);
    }

    #[test]
    fn multi_plug() {
        let wkld = Workload::Set(vec![s!(a b), s!(a), s!(b)]);
        let a_s = Workload::Set(vec![s!(1), s!(2), s!(3)]);
        let b_s = Workload::Set(vec![s!(x), s!(y)]);
        let actual = wkld.plug("a", a_s).plug("b", b_s).force();
        let expected = vec![
            s!(1 x),
            s!(1 y),
            s!(2 x),
            s!(2 y),
            s!(3 x),
            s!(3 y),
            s!(1),
            s!(2),
            s!(3),
            s!(x),
            s!(y),
        ];
        assert_eq!(actual, expected)
    }

    #[test]
    fn push_filter_through_plug() {
        let wkld = Workload::Set(vec![s!(x x x), s!(x x), s!(x)]);
        let pegs = Workload::Set(vec![s!(1), s!(2), s!(3)]);
        let actual = wkld
            .plug("x", pegs)
            .filter(Filter::MetricLt(Metric::Atoms, 3))
            .force();
        let expected = vec![
            s!(1 1),
            s!(1 2),
            s!(1 3),
            s!(2 1),
            s!(2 2),
            s!(2 3),
            s!(3 1),
            s!(3 2),
            s!(3 3),
            s!(1),
            s!(2),
            s!(3),
        ];
        assert_eq!(actual, expected);
    }
}
