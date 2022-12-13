use egg::{Analysis, Applier, ENodeOrVar, Language, PatternAst, RecExpr, Rewrite, Subst};
use std::fmt::Debug;
use std::{str::FromStr, sync::Arc};

use crate::*;

#[derive(Clone, Debug)]
pub struct Equality<L: SynthLanguage> {
    pub name: Arc<str>,
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    pub rewrite: Rewrite<L, SynthAnalysis>,
}

impl<L: SynthLanguage> FromStr for Equality<L> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((l, r)) = s.split_once("=>") {
            let l_pat: Pattern<L> = l.parse().unwrap();
            let r_pat: Pattern<L> = r.parse().unwrap();
            let name = format!("{} ==> {}", l_pat, r_pat);
            let rhs = Rhs { rhs: r_pat.clone() };

            Ok(Self {
                name: name.clone().into(),
                lhs: l_pat.clone(),
                rhs: r_pat,
                rewrite: Rewrite::new(name, l_pat, rhs).unwrap(),
            })
        } else {
            Err(format!("Failed to split {}", s))
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

impl<L: SynthLanguage> Equality<L> {
    pub fn new(e1: &RecExpr<L>, e2: &RecExpr<L>) -> Option<Self> {
        let map = &mut HashMap::default();
        let l_pat = L::generalize(e1, map);
        let r_pat = L::generalize(e2, map);
        let name = format!("{} ==> {}", l_pat, r_pat);
        let rhs = Rhs { rhs: r_pat.clone() };
        let rewrite = Rewrite::new(name.clone(), l_pat.clone(), rhs).ok();

        rewrite.map(|rw| Equality {
            name: name.into(),
            lhs: l_pat,
            rhs: r_pat,
            rewrite: rw,
        })
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
