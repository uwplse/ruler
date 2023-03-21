use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Filter {
    MetricLt(Metric, usize),
    Contains(Pattern),
    Canon(Vec<String>),
    And(Vec<Self>),
    Or(Vec<Self>),
    Invert(Box<Self>),
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
            Filter::And(fs) => fs.iter().all(|f| f.test(sexp)),
            Filter::Or(fs) => fs.iter().any(|f| f.test(sexp)),
            Filter::Invert(f) => !f.test(sexp),
        }
    }

    pub(crate) fn is_monotonic(&self) -> bool {
        matches!(self, Filter::MetricLt(_, _))
    }

    pub(crate) fn reduce_monotonic(&self) -> Self {
        match self {
            Filter::MetricLt(m, n) => Filter::MetricLt(*m, n - 1),
            _ => self.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn metric_lt() {
        let wkld = Workload::new([
            "(+ a a)",
            "(+ a b)",
            "(+ a (+ a b))",
            "(+ a (+ b b))",
            "(+ (+ a b) (+ a b))",
            "(+ (+ a b) (+ b a)) ",
            "(~ (+ a b))",
        ]);
        let actual = wkld.filter(Filter::MetricLt(Metric::Atoms, 5)).force();
        let expected = Workload::new(["(+ a a)", "(+ a b)", "(~ (+ a b))"]).force();
        assert_eq!(actual, expected)
    }

    #[test]
    fn contains() {
        let wkld = Workload::new([
            "(+ a a)",
            "(+ a b)",
            "(+ a (+ a b))",
            "(+ a (+ b b))",
            "(+ (+ a b) (+ a b))",
            "(+ (+ a b) (+ b a))",
        ]);
        let actual = wkld
            .filter(Filter::Contains("(+ ?x ?x)".parse().unwrap()))
            .force();
        let expected = Workload::new(["(+ a a)", "(+ a (+ b b))", "(+ (+ a b) (+ a b))"]).force();
        assert_eq!(actual, expected);
    }

    #[test]
    fn and() {
        let wkld = Workload::new(["x", "y", "(x y)", "(y x)", "(x x x)", "(y y z)", "(x y z)"]);
        let actual = wkld
            .filter(Filter::And(vec![
                Filter::Contains("x".parse().unwrap()),
                Filter::Contains("y".parse().unwrap()),
            ]))
            .force();
        let expected = Workload::new(["(x y)", "(y x)", "(x y z)"]).force();
        assert_eq!(actual, expected);
    }

    #[test]
    fn invert() {
        let wkld = Workload::new([
            "(+ a a)",
            "(+ a b)",
            "(+ a (+ a b))",
            "(+ a (+ b b))",
            "(+ (+ a b) (+ a b))",
            "(+ (+ a b) (+ b a))",
        ]);
        let actual = wkld
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(+ ?x ?x)".parse().unwrap(),
            ))))
            .force();
        let expected = Workload::new(["(+ a b)", "(+ a (+ a b))", "(+ (+ a b) (+ b a))"]).force();
        assert_eq!(actual, expected);
    }
}
