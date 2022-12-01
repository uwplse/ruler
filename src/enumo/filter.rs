use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Filter {
    MetricLt(Metric, usize),
    Contains(Pattern),
    Canon(Vec<String>),
    And(Box<Self>, Box<Self>),
}

impl Filter {
    pub(crate) fn test(&self, sexp: &Sexp) -> bool {
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
            Filter::Canon(symbols) => sexp.eq(&sexp.canon(symbols)),
            Filter::And(f1, f2) => f1.test(sexp) && f2.test(sexp),
        }
    }

    pub(crate) fn is_monotonic(&self) -> bool {
        matches!(self, Filter::MetricLt(_, _))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::s;

    #[test]
    fn contains() {
        let wkld = Workload::Set(vec![
            s!((+ a a)),
            s!((+ a b)),
            s!((+ a (+ a b))),
            s!((+ a (+ b b))),
            s!((+ (+ a b) (+ a b))),
            s!((+ (+ a b) (+ b a)) ),
        ]);
        let pat = Pattern::List(vec![
            Pattern::Lit("+".into()),
            Pattern::Var("?x".into()),
            Pattern::Var("?x".into()),
        ]);
        let actual = wkld.filter(Filter::Contains(pat)).force();
        let expected = vec![s!((+ a a)), s!((+ a (+ b b))), s!((+ (+ a b) (+ a b)))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn and() {
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
                Box::new(Filter::Contains(Pattern::Lit("x".into()))),
                Box::new(Filter::Contains(Pattern::Lit("y".into()))),
            ))
            .force();
        let expected = vec![s!(x y), s!(y x), s!(x y z)];
        assert_eq!(actual, expected);
    }
}
