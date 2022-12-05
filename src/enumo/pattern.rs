use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Pattern {
    Wild,
    Var(String),
    Lit(String),
    List(Vec<Pattern>),
}

impl Pattern {
    pub(crate) fn matches(&self, sexp: &Sexp) -> bool {
        self.matches_with(sexp, Default::default()).is_some()
    }

    fn matches_with(
        &self,
        sexp: &Sexp,
        mut subst: HashMap<String, Sexp>,
    ) -> Option<HashMap<String, Sexp>> {
        match self {
            Pattern::Wild => {
                // Wild matches anything
                Some(subst)
            }
            Pattern::Var(pvar) => {
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
            Pattern::Lit(plit) => match sexp {
                Sexp::Atom(lit) => {
                    if plit == lit {
                        Some(subst)
                    } else {
                        None
                    }
                }
                Sexp::List(_) => None,
            },
            Pattern::List(pats) => match sexp {
                Sexp::Atom(_) => None,
                Sexp::List(args) => {
                    if pats.len() == args.len() {
                        pats.iter()
                            .zip(args.iter())
                            .fold(Some(subst), |acc, (pat, sexp)| {
                                if let Some(subst) = acc {
                                    pat.matches_with(sexp, subst)
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

#[cfg(test)]
mod test {
    use crate::enumo::Workload;

    use super::Pattern;

    #[test]
    fn matches() {
        let patterns = vec![
            Pattern::Wild,
            Pattern::Lit("x".into()),
            Pattern::List(vec![
                Pattern::Lit("+".into()),
                Pattern::Var("?x".into()),
                Pattern::Var("?x".into()),
            ]),
        ];

        let exprs =
            Workload::from_vec(vec!["a", "x", "(+ x y)", "(+ y y)", "(+ (* a b) (* a b))"]).force();

        let expected = vec![
            vec![true, true, true, true, true],
            vec![false, true, false, false, false],
            vec![false, false, false, true, true],
        ];

        for (i, pat) in patterns.iter().enumerate() {
            for (j, expr) in exprs.iter().enumerate() {
                assert_eq!(pat.matches(expr), expected[i][j]);
            }
        }
    }
}
