use egg::{Applier, ENodeOrVar, Id, PatternAst, Subst, Symbol, Var};
use std::fmt::Debug;
use std::sync::Arc;

use crate::math::*;

pub type RecExpr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
pub type EGraph = egg::EGraph<Math, ConstantFold>;
pub type Rewrite = egg::Rewrite<Math, ConstantFold>;

#[derive(Clone, Debug)]
pub struct Rule {
    pub name: Arc<str>,
    pub lhs: Pattern,
    pub rhs: Pattern,
    pub rewrite: Rewrite,
}

struct Rhs {
    rhs: Pattern,
}

fn instantiate(pattern: &Pattern) -> RecExpr {
    let nodes: Vec<_> = pattern
        .ast
        .as_ref()
        .iter()
        .map(|n| match n {
            ENodeOrVar::ENode(n) => n.clone(),
            ENodeOrVar::Var(v) => {
                let s = v.to_string();
                assert!(s.starts_with('?'));
                Math::Symbol(s[1..].into())
            }
        })
        .collect();

    RecExpr::from(nodes)
}

impl Applier<Math, ConstantFold> for Rhs {
    fn vars(&self) -> Vec<Var> {
        self.rhs.vars()
    }

    fn apply_one(
        &self,
        egraph: &mut EGraph,
        matched_id: Id,
        subst: &Subst,
        ast: Option<&PatternAst<Math>>,
        sym: Symbol,
    ) -> Vec<Id> {
        self.rhs.apply_one(egraph, matched_id, subst, ast, sym)
    }
}

impl Rule {
    pub fn new(name: String, l_pat: Pattern, r_pat: Pattern) -> Option<Self> {
        let rhs = Rhs { rhs: r_pat.clone() };
        let rewrite = Rewrite::new(name.clone(), l_pat.clone(), rhs).ok();

        rewrite.map(|rw| Rule {
            name: name.into(),
            lhs: l_pat,
            rhs: r_pat,
            rewrite: rw,
        })
    }

    pub fn is_saturating(&self) -> bool {
        let mut egraph: EGraph = Default::default();
        let l_id = egraph.add_expr(&instantiate(&self.lhs));
        let initial_size = egraph.number_of_classes();

        let r_id = egraph.add_expr(&instantiate(&self.rhs));

        egraph.union(l_id, r_id);
        egraph.rebuild();
        let final_size = egraph.number_of_classes();

        initial_size >= final_size
    }
}
