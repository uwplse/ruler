use std::str::FromStr;

use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Sexp {
    Atom(String),
    List(Vec<Self>),
}

impl FromStr for Sexp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use symbolic_expressions::parser::parse_str;
        let sexp = parse_str(s).unwrap();
        Ok(Self::from_symbolic_expr(sexp))
    }
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
    fn from_symbolic_expr(sexp: symbolic_expressions::Sexp) -> Self {
        match sexp {
            symbolic_expressions::Sexp::String(s) => Self::Atom(s),
            symbolic_expressions::Sexp::List(ss) => Self::List(
                ss.iter()
                    .map(|s| Sexp::from_symbolic_expr(s.clone()))
                    .collect(),
            ),
            symbolic_expressions::Sexp::Empty => Self::List(vec![]),
        }
    }

    fn mk_canon(
        &self,
        symbols: &[String],
        mut idx: usize,
        mut subst: HashMap<String, String>,
    ) -> (HashMap<String, String>, usize) {
        match self {
            Sexp::Atom(x) => {
                if symbols.contains(x) && !subst.contains_key(x) {
                    subst.insert(x.into(), symbols[idx].clone());
                    idx += 1;
                }
                (subst, idx)
            }
            Sexp::List(exps) => exps.iter().fold((subst, idx), |(acc, idx), item| {
                item.mk_canon(symbols, idx, acc)
            }),
        }
    }

    fn apply_subst(&self, subst: &HashMap<String, String>) -> Self {
        match self {
            Sexp::Atom(s) => {
                if let Some(v) = subst.get(s) {
                    Sexp::Atom(v.into())
                } else {
                    Sexp::Atom(s.into())
                }
            }
            Sexp::List(exps) => Sexp::List(exps.iter().map(|s| s.apply_subst(subst)).collect()),
        }
    }

    pub(crate) fn canon(&self, symbols: &[String]) -> Self {
        let (subst, _) = self.mk_canon(symbols, 0, Default::default());
        self.apply_subst(&subst)
    }

    pub(crate) fn plug(&self, name: &str, pegs: &[Self]) -> Vec<Sexp> {
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

    pub(crate) fn measure(&self, metric: Metric) -> usize {
        match self {
            Sexp::Atom(_) => match metric {
                Metric::List => 0,
                Metric::Atoms | Metric::Depth => 1,
            },
            Sexp::List(s) => match metric {
                Metric::Atoms => s.iter().map(|x| x.measure(metric)).sum::<usize>(),
                Metric::List => s.iter().map(|x| x.measure(metric)).sum::<usize>() + 1,
                Metric::Depth => s.iter().map(|x| x.measure(metric)).max().unwrap() + 1,
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn from_str() {
        assert_eq!("a".parse::<Sexp>().unwrap(), Sexp::Atom("a".into()));
        assert_eq!(
            "(+ (- 1 2) 0)".parse::<Sexp>().unwrap(),
            Sexp::List(vec![
                Sexp::Atom("+".into()),
                Sexp::List(vec![
                    Sexp::Atom("-".into()),
                    Sexp::Atom("1".into()),
                    Sexp::Atom("2".into()),
                ]),
                Sexp::Atom("0".into()),
            ])
        )
    }

    #[test]
    fn measure_atoms() {
        let exprs = vec![
            ("a", 1),
            ("(a b)", 2),
            ("(a b c)", 3),
            ("(a (b c))", 3),
            ("(a b (c d))", 4),
            ("(a (b (c d)))", 4),
            ("(a (b c) (d e))", 5),
        ];
        for (expr, size) in exprs {
            assert_eq!(expr.parse::<Sexp>().unwrap().measure(Metric::Atoms), size);
        }
    }

    #[test]
    fn measure_lists() {
        let exprs = vec![
            ("a", 0),
            ("(a b)", 1),
            ("(a b c)", 1),
            ("(a (b c))", 2),
            ("(a b (c d))", 2),
            ("(a (b (c d)))", 3),
            ("(a (b c) (d e))", 3),
        ];
        for (expr, size) in exprs {
            assert_eq!(expr.parse::<Sexp>().unwrap().measure(Metric::List), size);
        }
    }

    #[test]
    fn measure_depth() {
        let exprs = vec![
            ("a", 1),
            ("(a b)", 2),
            ("(a b c)", 2),
            ("(a (b c))", 3),
            ("(a b (c d))", 3),
            ("(a (b (c d)))", 4),
            ("(a (b c) (d e))", 3),
        ];
        for (expr, size) in exprs {
            assert_eq!(expr.parse::<Sexp>().unwrap().measure(Metric::Depth), size);
        }
    }

    #[test]
    fn plug() {
        let x = "x".parse::<Sexp>().unwrap();
        let pegs = Workload::from_vec(vec!["1", "2", "3"]).force();
        let expected = vec![x.clone()];
        let actual = x.plug("a", &pegs);
        assert_eq!(actual, expected);

        let expected = pegs.clone();
        let actual = x.plug("x", &pegs);
        assert_eq!(actual, expected);
    }

    #[test]
    fn plug_cross_product() {
        let term = "(x x)";
        let pegs = Workload::from_vec(vec!["1", "2", "3"]).force();
        let expected = Workload::from_vec(vec![
            "(1 1)", "(1 2)", "(1 3)", "(2 1)", "(2 2)", "(2 3)", "(3 1)", "(3 2)", "(3 3)",
        ])
        .force();
        let actual = term.parse::<Sexp>().unwrap().plug("x", &pegs);
        assert_eq!(actual, expected);
    }

    #[test]
    fn multi_plug() {
        let wkld = Workload::from_vec(vec!["(a b)", "(a)", "(b)"]);
        let a_s = Workload::from_vec(vec!["1", "2", "3"]);
        let b_s = Workload::from_vec(vec!["x", "y"]);
        let actual = wkld.plug("a", &a_s).plug("b", &b_s).force();
        let expected = Workload::from_vec(vec![
            "(1 x)", "(1 y)", "(2 x)", "(2 y)", "(3 x)", "(3 y)", "(1)", "(2)", "(3)", "(x)", "(y)",
        ])
        .force();
        assert_eq!(actual, expected)
    }

    #[test]
    fn canon() {
        let inputs = Workload::from_vec(vec![
            "(+ (/ c b) a)",
            "(+ (- c c) (/ a a))",
            "a",
            "b",
            "x",
            "(+ a a)",
            "(+ b b)",
            "(+ a b)",
            "(+ b a)",
            "(+ a x)",
            "(+ x a)",
            "(+ b x)",
            "(+ x b)",
            "(+ a (+ b c))",
            "(+ a (+ c b))",
        ])
        .force();
        let expecteds = Workload::from_vec(vec![
            "(+ (/ a b) c)",
            "(+ (- a a) (/ b b))",
            "a",
            "a",
            "x",
            "(+ a a)",
            "(+ a a)",
            "(+ a b)",
            "(+ a b)",
            "(+ a x)",
            "(+ x a)",
            "(+ a x)",
            "(+ x a)",
            "(+ a (+ b c))",
            "(+ a (+ b c))",
        ])
        .force();
        for (test, expected) in inputs.iter().zip(expecteds.iter()) {
            assert_eq!(
                &test.canon(vec!["a".into(), "b".into(), "c".into()].as_ref()),
                expected
            );
        }
    }
}
