use crate::*;
use serde::{Deserialize, Serialize};
use std::{str::FromStr, sync::Arc};

/// Definition of an equality.
#[derive(Clone, Serialize, Deserialize)]
#[serde(from = "SerializedEq")]
#[serde(into = "SerializedEq")]
#[serde(bound = "L: SynthLanguage")]
pub struct Equality<L: SynthLanguage> {
    pub name: Arc<str>,
    pub lhs: Pattern<L>,
    pub ids: Option<(Id, Id)>,
    pub rhs: Pattern<L>,
    pub rewrites: Vec<Rewrite<L, SynthAnalysis>>,
}

#[derive(Clone, Serialize, Deserialize)]
struct SerializedEq {
    lhs: String,
    rhs: String,
    bidirectional: bool,
}

impl FromStr for SerializedEq {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((l, r)) = s.split_once("<=>") {
            Ok(Self {
                lhs: l.into(),
                rhs: r.into(),
                bidirectional: true,
            })
        } else if let Some((l, r)) = s.split_once("=>") {
            Ok(Self {
                lhs: l.into(),
                rhs: r.into(),
                bidirectional: false,
            })
        } else if let Some((r, l)) = s.split_once("<=") {
            Ok(Self {
                lhs: l.into(),
                rhs: r.into(),
                bidirectional: false,
            })
        } else {
            Err(format!("Failed to split {}", s))
        }
    }
}

impl<L: SynthLanguage> FromStr for Equality<L> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ser_eq: SerializedEq = s.parse()?;
        Ok(Self::from(ser_eq))
    }
}

impl<L: SynthLanguage + 'static> From<SerializedEq> for Equality<L> {
    fn from(ser: SerializedEq) -> Self {
        Self::from_serialized_eq(ser)
    }
}

impl<L: SynthLanguage> From<Equality<L>> for SerializedEq {
    fn from(eq: Equality<L>) -> Self {
        Self {
            lhs: eq.lhs.to_string(),
            rhs: eq.rhs.to_string(),
            bidirectional: eq.rewrites.len() > 1,
        }
    }
}

impl<L: SynthLanguage> Hash for Equality<L> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<L: SynthLanguage> Display for Equality<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<L: SynthLanguage> PartialEq for Equality<L> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<L: SynthLanguage> Eq for Equality<L> {}

struct NotUndefined<L: SynthLanguage> {
    name: String,
    rhs: Pattern<L>,
}

impl<L: SynthLanguage> Applier<L, SynthAnalysis> for NotUndefined<L> {
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

        if !L::is_valid_rewrite(egraph, &self.rhs, subst) {
            return vec![];
        }

        if let Some(ENodeOrVar::ENode(x)) = self.rhs.ast.as_ref().last() {
            if x.get_type() != egraph[matched_id].data.class_type {
                return vec![];
            }
        }

        let id = apply_pat(self.rhs.ast.as_ref(), egraph, subst);
        if id == matched_id {
            return vec![];
        }

        if !egraph[id].data.is_defined() {
            return vec![];
        }

        // now it's finally safe to union those things
        egraph.union(id, matched_id);

        for (i, (a, b)) in egraph[matched_id]
            .data
            .cvec
            .iter()
            .zip(&egraph[id].data.cvec)
            .enumerate()
        {
            match (a, b) {
                (Some(a), Some(b)) if a != b => {
                    for class in egraph.classes() {
                        if let Some(var) = class.nodes.iter().find_map(|n| n.to_var()) {
                            if let Some(Some(val)) = class.data.cvec.get(i).as_ref() {
                                eprintln!("  {} = {}", var, val);
                            } else {
                                eprintln!("  {} = none", var);
                            }
                        }
                    }
                    assert_eq!(a, b, "bad rule {}", self.name)
                }
                _ => (),
            }
        }

        vec![id]
    }
}

impl<L: SynthLanguage> Equality<L> {
    fn from_serialized_eq(ser: SerializedEq) -> Self {
        let l_pat: Pattern<L> = ser.lhs.parse().unwrap();
        let r_pat: Pattern<L> = ser.rhs.parse().unwrap();
        let l_recexpr = L::instantiate(&l_pat);
        let r_recexpr = L::instantiate(&r_pat);

        if !ser.bidirectional {
            let name = format!("{} => {}", l_pat, r_pat);
            let defined_rhs = NotUndefined {
                name: name.clone(),
                rhs: r_pat.clone(),
            };
            let rw = Rewrite::new(name.clone(), l_pat.clone(), defined_rhs).unwrap();
            Self {
                name: name.into(),
                lhs: l_pat,
                ids: None,
                rhs: r_pat,
                rewrites: vec![rw],
            }
        } else {
            Self::new(&l_recexpr, &r_recexpr).unwrap()
        }
    }

    /// Create a new [Equality] from two [RecExprs](https://docs.rs/egg/latest/egg/struct.RecExpr.html).
    pub fn new<'a>(e1: &'a RecExpr<L>, e2: &'a RecExpr<L>) -> Option<Self> {
        let mut forward: (String, Pattern<L>, Pattern<L>, Option<Rewrite<L, _>>) = {
            let map = &mut HashMap::default();
            let lhs = L::generalize(e1, map);
            let rhs = L::generalize(e2, map);
            let name = format!("{} => {}", lhs, rhs);
            let defined_rhs = NotUndefined {
                name: name.clone(),
                rhs: rhs.clone(),
            };
            (
                name.clone(),
                lhs.clone(),
                rhs,
                Rewrite::new(name, lhs, defined_rhs).ok(),
            )
        };

        let mut back: (String, Pattern<L>, Pattern<L>, Option<Rewrite<L, _>>) = {
            let map = &mut HashMap::default();
            let lhs = L::generalize(e2, map);
            let rhs = L::generalize(e1, map);
            let name = format!("{} => {}", lhs, rhs);
            let defined_rhs = NotUndefined {
                name: name.clone(),
                rhs: rhs.clone(),
            };
            (
                name.clone(),
                lhs.clone(),
                rhs,
                Rewrite::new(name, lhs, defined_rhs).ok(),
            )
        };

        // make sure we always do things in the same order
        if back.0 > forward.0 {
            std::mem::swap(&mut forward, &mut back);
        }

        match (forward, back) {
            ((_, _, _, None), (_, _, _, None)) => None,
            ((name, lhs, rhs, Some(rw)), (_, _, _, None))
            | ((_, _, _, None), (name, lhs, rhs, Some(rw))) => Some(Self {
                name: name.into(),
                lhs,
                rhs,
                ids: None,
                rewrites: vec![rw],
            }),
            ((_, lhs, rhs, Some(rw1)), (_, _, _, Some(rw2))) => Some(Self {
                name: format!("{} <=> {}", lhs, rhs).into(),
                lhs,
                rhs,
                ids: None,
                rewrites: if rw1.name == rw2.name {
                    vec![rw1]
                } else {
                    vec![rw1, rw2]
                },
            }),
        }
    }

    /// Assign a score to this Equality using heursitics mentioned in the Ruler paper
    /// (page 11, footnote 5).
    pub fn score(&self) -> impl Ord + Debug {
        L::score(&self.lhs, &self.rhs)
    }
}

// copied from egg
// we need a way to "apply" a pattern without causing any unions
// as of egg 0.7, apply_one does the union, so we can't use that.
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
