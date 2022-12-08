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
                pat.matches(sexp)
                    || match sexp {
                        Sexp::Atom(_) => false,
                        Sexp::List(args) => args.iter().any(|s| self.test(s)),
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

    #[test]
    fn metric_lt() {
        let wkld = Workload::from_vec(vec![
            "(+ a a)",
            "(+ a b)",
            "(+ a (+ a b))",
            "(+ a (+ b b))",
            "(+ (+ a b) (+ a b))",
            "(+ (+ a b) (+ b a)) ",
            "(~ (+ a b))",
        ]);
        let actual = wkld.filter(Filter::MetricLt(Metric::Atoms, 5)).force();
        let expected = Workload::from_vec(vec!["(+ a a)", "(+ a b)", "(~ (+ a b))"]).force();
        assert_eq!(actual, expected)
    }

    #[test]
    fn contains() {
        let wkld = Workload::from_vec(vec![
            "(+ a a)",
            "(+ a b)",
            "(+ a (+ a b))",
            "(+ a (+ b b))",
            "(+ (+ a b) (+ a b))",
            "(+ (+ a b) (+ b a)) ",
        ]);
        let actual = wkld
            .filter(Filter::Contains("(+ ?x ?x)".parse().unwrap()))
            .force();
        let expected =
            Workload::from_vec(vec!["(+ a a)", "(+ a (+ b b))", "(+ (+ a b) (+ a b))"]).force();
        assert_eq!(actual, expected);
    }

    #[test]
    fn and() {
        let wkld = Workload::from_vec(vec![
            "x", "y", "(x y)", "(y x)", "(x x x)", "(y y z)", "(x y z)",
        ]);
        let actual = wkld
            .filter(Filter::And(
                Box::new(Filter::Contains("x".parse().unwrap())),
                Box::new(Filter::Contains("y".parse().unwrap())),
            ))
            .force();
        let expected = Workload::from_vec(vec!["(x y)", "(y x)", "(x y z)"]).force();
        assert_eq!(actual, expected);
    }
}
