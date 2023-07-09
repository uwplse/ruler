use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Filter {
    MetricLt(Metric, usize),
    MetricEq(Metric, usize),
    Contains(Pattern),
    Excludes(Pattern),
    Canon(Vec<String>),
    And(Vec<Self>),
    Or(Vec<Self>),
    Invert(Box<Self>),
}

impl Filter {
    pub(crate) fn test(&self, sexp: &Sexp) -> bool {
        match self {
            Filter::MetricLt(metric, n) => sexp.measure(*metric) < *n,
            Filter::MetricEq(metric, n) => sexp.measure(*metric) == *n,
            Filter::Contains(pat) => {
                pat.matches(sexp)
                    || match sexp {
                        Sexp::Atom(_) => false,
                        Sexp::List(args) => args.iter().any(|s| self.test(s)),
                    }
            }
            Filter::Excludes(pat) => !&Filter::Contains(pat.clone()).test(sexp),
            Filter::Canon(symbols) => sexp.eq(&sexp.canon(symbols)),
            Filter::And(fs) => fs.iter().all(|f| f.test(sexp)),
            Filter::Or(fs) => fs.iter().any(|f| f.test(sexp)),
            Filter::Invert(f) => !f.test(sexp),
        }
    }

    fn subsumed_by(&self, fs: &Vec<Self>) -> bool {
        if let Filter::MetricLt(met, n) = self {
            for f in fs {
                if let Filter::MetricLt(met1, n1) = f {
                    if met == met1 && n >= n1 {
                        return true;
                    }
                }
            }
            false
        } else {
            false
        }
    }

    pub(crate) fn and(self, other: Self) -> Self {
        let all = match (self, other) {
            (Filter::And(f1s), Filter::And(f2s)) => {
                let mut all = f1s;
                all.extend(f2s);
                all
            }
            (Filter::And(fs), f) => {
                let mut all = fs;
                all.push(f);
                all
            }
            (f, Filter::And(fs)) => {
                let mut all = fs;
                all.push(f);
                all
            }
            (f1, f2) => vec![f1, f2],
        };
        let mut minimized = vec![];
        for f in all {
            if !f.subsumed_by(&minimized) {
                minimized.push(f);
            }
        }

        Filter::And(minimized)
    }

    pub(crate) fn is_monotonic(&self) -> bool {
        match self {
            Filter::MetricLt(_, _) => true,
            Filter::Excludes(_) => true,
            // The conjunction of monotonic filters is monotonic
            Filter::And(fs) => fs.iter().all(|f| f.is_monotonic()),
            _ => false,
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

    #[test]
    fn excludes() {
        let pegs = Workload::new(["a", "b", "c", "d"]);
        let wkld = Workload::new(["(OP X X)"])
            .plug("X", &pegs)
            .filter(Filter::Excludes("c".parse().unwrap()));

        assert_eq!(wkld.force().len(), 9)
    }
}
