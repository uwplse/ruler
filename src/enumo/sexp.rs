use super::*;

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
    use crate::*;

    #[test]
    fn measure_atoms() {
        let exprs = vec![
            (Sexp::Atom("a".into()), 1),
            (s!((a b)), 2),
            (s!((a b c)), 3),
            (s!((a (b c))), 3),
            (s!((a b (c d))), 4),
            (s!((a (b (c d)))), 4),
            (s!((a (b c) (d e))), 5),
        ];
        for (expr, size) in exprs {
            assert_eq!(expr.measure(Metric::Atoms), size);
        }
    }

    #[test]
    fn measure_lists() {
        let exprs = vec![
            (Sexp::Atom("a".into()), 0),
            (s!((a b)), 1),
            (s!((a b c)), 1),
            (s!((a (b c))), 2),
            (s!((a b (c d))), 2),
            (s!((a (b (c d)))), 3),
            (s!((a (b c) (d e))), 3),
        ];
        for (expr, size) in exprs {
            assert_eq!(expr.measure(Metric::List), size);
        }
    }

    #[test]
    fn measure_depth() {
        let exprs = vec![
            (Sexp::Atom("a".into()), 1),
            (s!((a b)), 2),
            (s!((a b c)), 2),
            (s!((a (b c))), 3),
            (s!((a b (c d))), 3),
            (s!((a (b (c d)))), 4),
            (s!((a (b c) (d e))), 3),
        ];
        for (expr, size) in exprs {
            assert_eq!(expr.measure(Metric::Depth), size);
        }
    }

    #[test]
    fn plug() {
        let x = s!(x);
        let expected = vec![x.clone()];
        let actual = x.plug("a", &[s!(1), s!(2)]);
        assert_eq!(actual, expected);

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
    fn canon() {
        let inputs = vec![
            s!((+ (/ c b) a)),
            s!((+ (- c c) (/ a a))),
            s!(a),
            s!(b),
            s!(x),
            s!((+ a a)),
            s!((+ b b)),
            s!((+ a b)),
            s!((+ b a)),
            s!((+ a x)),
            s!((+ x a)),
            s!((+ b x)),
            s!((+ x b)),
            s!((+ a (+ b c))),
            s!((+ a (+ c b))),
        ];
        let expecteds = vec![
            s!((+ (/ a b) c)),
            s!((+ (- a a) (/ b b))),
            s!(a),
            s!(a),
            s!(x),
            s!((+ a a)),
            s!((+ a a)),
            s!((+ a b)),
            s!((+ a b)),
            s!((+ a x)),
            s!((+ x a)),
            s!((+ a x)),
            s!((+ x a)),
            s!((+ a (+ b c))),
            s!((+ a (+ b c))),
        ];
        for (test, expected) in inputs.iter().zip(expecteds.iter()) {
            assert_eq!(
                &test.canon(vec!["a".into(), "b".into(), "c".into()].as_ref()),
                expected
            );
        }
    }
}
