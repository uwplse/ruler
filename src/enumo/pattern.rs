use super::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Pattern {
    Wild,
    Var(String),
    Lit(String),
    List(Vec<Pattern>),
}

impl Pattern {
    pub(crate) fn matches(
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
