use egg::{Analysis, Applier, ENodeOrVar, Language, PatternAst, RecExpr, Rewrite, Subst};
use std::fmt::Debug;
use std::sync::Arc;

use crate::*;

#[derive(Clone, Debug)]
pub struct Rule<L: SynthLanguage> {
    pub name: Arc<str>,
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    pub rewrite: Rewrite<L, SynthAnalysis>,
}

impl<L: SynthLanguage> Rule<L> {
    pub fn from_string(s: &str) -> Result<(Self, Option<Self>), String> {
        if let Some((l, r)) = s.split_once("=>") {
            let l_pat: Pattern<L> = l.parse().unwrap();
            let r_pat: Pattern<L> = r.parse().unwrap();

            let forwards = Self {
                name: format!("{} ==> {}", l_pat, r_pat).into(),
                lhs: l_pat.clone(),
                rhs: r_pat.clone(),
                rewrite: Rewrite::new(
                    format!("{} ==> {}", l_pat, r_pat),
                    l_pat.clone(),
                    Rhs { rhs: r_pat.clone() },
                )
                .unwrap(),
            };

            if s.contains("<=>") {
                let backwards = Self {
                    name: format!("{} ==> {}", r_pat, l_pat).into(),
                    lhs: r_pat.clone(),
                    rhs: l_pat.clone(),
                    rewrite: Rewrite::new(
                        format!("{} ==> {}", r_pat, l_pat),
                        r_pat,
                        Rhs { rhs: l_pat },
                    )
                    .unwrap(),
                };
                Ok((forwards, Some(backwards)))
            } else {
                Ok((forwards, None))
            }
        } else {
            Err(format!("Failed to parse {}", s))
        }
    }
}

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
    pub fn new(l_pat: Pattern<L>, r_pat: Pattern<L>) -> Option<Self> {
        let name = format!("{} ==> {}", l_pat, r_pat);
        let rhs = Rhs { rhs: r_pat.clone() };
        let rewrite = Rewrite::new(name.clone(), l_pat.clone(), rhs).ok();

        rewrite.map(|rw| Rule {
            name: name.into(),
            lhs: l_pat,
            rhs: r_pat,
            rewrite: rw,
        })
    }

    pub fn from_recexprs(e1: &RecExpr<L>, e2: &RecExpr<L>) -> Option<Self> {
        let map = &mut HashMap::default();
        let l_pat = L::generalize(e1, map);
        let r_pat = L::generalize(e2, map);
        Self::new(l_pat, r_pat)
    }

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
