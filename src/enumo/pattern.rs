use std::str::FromStr;

use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Pattern {
    Wild,
    Var(String),
    Lit(String),
    List(Vec<Pattern>),
}

impl FromStr for Pattern {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use symbolic_expressions::parser::parse_str;
        let sexp = parse_str(s).unwrap();
        Ok(Self::from_symbolic_expr(sexp))
    }
}

impl Pattern {
    fn from_symbolic_expr(sexp: symbolic_expressions::Sexp) -> Self {
        match sexp {
            symbolic_expressions::Sexp::String(s) if s == "*" => Self::Wild,
            symbolic_expressions::Sexp::String(s) if s.starts_with('?') => Self::Var(s),
            symbolic_expressions::Sexp::String(s) => Self::Lit(s),
            symbolic_expressions::Sexp::List(ss) => Self::List(
                ss.iter()
                    .map(|s| Self::from_symbolic_expr(s.clone()))
                    .collect(),
            ),
            symbolic_expressions::Sexp::Empty => Self::List(vec![]),
        }
    }

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
    fn from_str() {
        assert_eq!("a".parse::<Pattern>().unwrap(), Pattern::Lit("a".into()));
        assert_eq!("*".parse::<Pattern>().unwrap(), Pattern::Wild);
        assert_eq!("?a".parse::<Pattern>().unwrap(), Pattern::Var("?a".into()));
        assert_eq!(
            "(+ ?x ?x)".parse::<Pattern>().unwrap(),
            Pattern::List(vec![
                Pattern::Lit("+".into()),
                Pattern::Var("?x".into()),
                Pattern::Var("?x".into()),
            ])
        );
        assert_eq!(
            "(- * (+ ?x ?x))".parse::<Pattern>().unwrap(),
            Pattern::List(vec![
                Pattern::Lit("-".into()),
                Pattern::Wild,
                Pattern::List(vec![
                    Pattern::Lit("+".into()),
                    Pattern::Var("?x".into()),
                    Pattern::Var("?x".into()),
                ])
            ])
        );
    }

    #[test]
    fn matches() {
        let patterns: Vec<Pattern> = vec!["*", "x", "(+ ?x ?x)"]
            .iter()
            .map(|x| x.parse::<Pattern>().unwrap())
            .collect();

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
