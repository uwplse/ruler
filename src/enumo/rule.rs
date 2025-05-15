use egg::{Analysis, Applier, ENodeOrVar, Language, PatternAst, Rewrite, Subst};
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

use crate::*;

/// A Rewrite rule
#[derive(Clone, Debug)]
pub struct Rule<L: SynthLanguage> {
    /// Readable name of the rewrite rule, formatted as lhs ==> rhs
    pub name: Arc<str>,
    /// The pattern to match on
    pub lhs: Pattern<L>,
    /// The pattern to merge
    pub rhs: Pattern<L>,
    /// egg::Rewrite
    pub rewrite: Rewrite<L, SynthAnalysis>,
}

impl<L: SynthLanguage> Display for Rule<L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ==> {}", self.lhs, self.rhs)
    }
}

impl<L: SynthLanguage> Rule<L> {
    pub fn from_string(s: &str) -> Result<(Self, Option<Self>), String> {
        if let Some((l, r)) = s.split_once("=>") {
            let l_pat = l.parse();
            let r_pat = r.parse();
            if l_pat.is_err() || r_pat.is_err() {
                return Err(format!("Failed to parse {}", s));
            }
            let l_pat: Pattern<L> = l_pat.unwrap();
            let r_pat: Pattern<L> = r_pat.unwrap();

            let rewrite = Rewrite::new(
                format!("{} ==> {}", l_pat, r_pat),
                l_pat.clone(),
                Rhs { rhs: r_pat.clone() },
            );

            let forwards = if let Ok(rewrite) = rewrite {
                Some(Self {
                    name: format!("{} ==> {}", l_pat, r_pat).into(),
                    lhs: l_pat.clone(),
                    rhs: r_pat.clone(),
                    rewrite,
                })
            } else {
                None
            };
            let backwards = if s.contains("<=>") {
                let rewrite = Rewrite::new(
                    format!("{} ==> {}", r_pat, l_pat),
                    r_pat.clone(),
                    Rhs { rhs: l_pat.clone() },
                );
                if let Ok(rewrite) = rewrite {
                    Some(Self {
                        name: format!("{} ==> {}", r_pat, l_pat).into(),
                        lhs: r_pat.clone(),
                        rhs: l_pat.clone(),
                        rewrite,
                    })
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(forwards) = forwards {
                Ok((forwards, backwards))
            } else {
                println!("Ignoring invalid rule {}", s);
                Err(format!("Failed to parse {}", s))
            }
        } else {
            Err(format!("Failed to parse {}", s))
        }
    }
}

/// Default Applier for rewrite rules
struct Rhs<L: SynthLanguage> {
    rhs: Pattern<L>,
}

impl<L: SynthLanguage> Applier<L, SynthAnalysis> for Rhs<L> {
    fn vars(&self) -> Vec<Var> {
        self.rhs.vars()
    }

    fn apply_one(
        &self,
        egraph: &mut EGraph<L, SynthAnalysis>,
        matched_id: Id,
        subst: &Subst,
        _ast: Option<&PatternAst<L>>,
        _sym: Symbol,
    ) -> Vec<Id> {
        if !egraph[matched_id].data.is_defined() {
            return vec![];
        }

        let id = apply_pat(self.rhs.ast.as_ref(), egraph, subst);
        if id == matched_id {
            return vec![];
        }

        if !egraph[id].data.is_defined() {
            return vec![];
        }

        egraph.union(id, matched_id);
        vec![id]
    }
}

impl<L: SynthLanguage> Rule<L> {
    pub fn new(l_pat: &Pattern<L>, r_pat: &Pattern<L>) -> Option<Self> {
        let name = format!("{} ==> {}", l_pat, r_pat);
        let rhs = Rhs { rhs: r_pat.clone() };
        let rewrite = Rewrite::new(name.clone(), l_pat.clone(), rhs).ok();

        rewrite.map(|rw| Rule {
            name: name.into(),
            lhs: l_pat.clone(),
            rhs: r_pat.clone(),
            rewrite: rw,
        })
    }

    /// A rule is saturating if applying it is guaranteed not to add any
    /// e-classes to the e-graph.
    pub fn is_saturating(&self) -> bool {
        let mut egraph: EGraph<L, SynthAnalysis> = Default::default();
        let l_id = egraph.add_expr(&L::instantiate(&self.lhs));
        let initial_size = egraph.number_of_classes();

        let r_id = egraph.add_expr(&L::instantiate(&self.rhs));

        egraph.union(l_id, r_id);
        egraph.rebuild();
        let final_size = egraph.number_of_classes();

        initial_size >= final_size
    }

    pub fn score(&self) -> impl Ord + Debug {
        L::score(&self.lhs, &self.rhs)
    }

    /// Whether the rule is sound
    pub fn is_valid(&self) -> bool {
        matches!(L::validate(&self.lhs, &self.rhs), ValidationResult::Valid)
    }
}

fn apply_pat<L: Language, A: Analysis<L>>(
    pat: &[ENodeOrVar<L>],
    egraph: &mut EGraph<L, A>,
    subst: &Subst,
) -> Id {
    let mut ids = vec![0.into(); pat.len()];

    for (i, pat_node) in pat.iter().enumerate() {
        let id = match pat_node {
            ENodeOrVar::Var(w) => subst[*w],
            ENodeOrVar::ENode(e) => {
                let n = e.clone().map_children(|child| ids[usize::from(child)]);
                egraph.add(n)
            }
        };
        ids[i] = id;
    }

    *ids.last().unwrap()
}

#[cfg(test)]
mod test {
    use crate::enumo::Rule;

    #[test]
    fn parse() {
        // Unidirectional rule with => delimeter
        let (forwards, backwards) = Rule::<egg::SymbolLang>::from_string("(* a b) => (* c d)")
            .ok()
            .unwrap();
        assert!(backwards.is_none());
        assert_eq!(forwards.name.to_string(), "(* a b) ==> (* c d)");

        // Unidirectional rule with ==> delimeter
        let (forwards, backwards) = Rule::<egg::SymbolLang>::from_string("(* a b) ==> (* c d)")
            .ok()
            .unwrap();
        assert!(backwards.is_none());
        assert_eq!(forwards.name.to_string(), "(* a b) ==> (* c d)");

        // Bidirectional rule <=>
        let (forwards, backwards) = Rule::<egg::SymbolLang>::from_string("(* a b) <=> (* c d)")
            .ok()
            .unwrap();
        assert!(backwards.is_some());
        assert_eq!(backwards.unwrap().name.to_string(), "(* c d) ==> (* a b)");
        assert_eq!(forwards.name.to_string(), "(* a b) ==> (* c d)");
    }
}
