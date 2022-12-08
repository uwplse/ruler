use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use egg::{
    Analysis, AstSize, CostFunction, DidMerge, ENodeOrVar, FromOp, Language, PatternAst, RecExpr,
    Rewrite,
};

use crate::{enumo::Workload, *};

#[derive(Clone)]
pub struct SynthAnalysis {
    pub cvec_len: usize,
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self { cvec_len: 10 }
    }
}

#[derive(Debug, Clone)]
pub struct Signature<L: SynthLanguage> {
    pub cvec: CVec<L>,
    pub interval: Interval<L::Constant>,
}

impl<L: SynthLanguage> Signature<L> {
    pub fn is_defined(&self) -> bool {
        self.cvec.is_empty() || self.cvec.iter().any(|v| v.is_some())
    }
}

impl<L: SynthLanguage> Analysis<L> for SynthAnalysis {
    type Data = Signature<L>;

    fn make(egraph: &EGraph<L, Self>, enode: &L) -> Self::Data {
        let get_cvec = |id: &Id| &egraph[*id].data.cvec;
        let get_interval = |id: &Id| &egraph[*id].data.interval;
        Signature {
            cvec: enode.eval(egraph.analysis.cvec_len, get_cvec),
            interval: enode.mk_interval(get_interval),
        }
    }

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        let mut merge_a = false;
        let mut merge_b = false;

        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            for i in 0..to.cvec.len() {
                match (to.cvec[i].clone(), from.cvec[i].clone()) {
                    (None, Some(_)) => {
                        to.cvec[i] = from.cvec[i].clone();
                        merge_a = true;
                    }
                    (Some(_), None) => {
                        merge_b = true;
                    }
                    (Some(x), Some(y)) => assert_eq!(x, y, "cvecs do not match!!"),
                    _ => (),
                }
            }
        }

        // New interval is max of mins, min of maxes
        let new_min = match (to.interval.low.as_ref(), from.interval.low.as_ref()) {
            (None, None) => None,
            (None, Some(y)) => Some(y.clone()),
            (Some(x), None) => Some(x.clone()),
            (Some(x), Some(y)) => Some(x.max(y).clone()),
        };
        let new_max = match (to.interval.high.as_ref(), from.interval.high.as_ref()) {
            (None, None) => None,
            (None, Some(y)) => Some(y.clone()),
            (Some(x), None) => Some(x.clone()),
            (Some(x), Some(y)) => Some(x.min(y).clone()),
        };
        let new_interval = Interval::new(new_min, new_max);
        if to.interval != new_interval {
            to.interval = new_interval;
            merge_a = true;
        }

        if to.interval != from.interval {
            merge_b = true;
        }

        DidMerge(merge_a, merge_b)
    }

    fn modify(egraph: &mut EGraph<L, Self>, id: Id) {
        L::custom_modify(egraph, id);
        let interval = &egraph[id].data.interval;
        if let Interval {
            low: Some(low),
            high: Some(high),
        } = interval
        {
            if low == high {
                let enode = L::mk_constant(low.clone(), egraph);
                let added = egraph.add(enode);
                egraph.union(id, added);
            }
        }
    }
}

pub type CVec<L> = Vec<Option<<L as SynthLanguage>::Constant>>;

pub trait SynthLanguage: Language + Send + Sync + Display + FromOp + 'static {
    type Constant: Clone + Hash + Eq + Debug + Display + Ord;

    // Overrideable hook into the egraph analysis modify method
    // for language-specific purposes (such as custom constant folding)
    fn custom_modify(_egraph: &mut EGraph<Self, SynthAnalysis>, _id: Id) {}

    fn eval<'a, F>(&'a self, cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>;

    fn mk_interval<'a, F>(&'a self, _get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        Interval::default()
    }

    fn initialize_vars(synth: &mut Synthesizer<Self>, vars: Vec<String>);

    fn to_var(&self) -> Option<Symbol>;
    fn mk_var(sym: Symbol) -> Self;

    fn is_constant(&self) -> bool;
    /**
     * Most domains don't need a reference to the egraph to make a constant node.
     * However, Nat represents numbers recursively, so adding a new constant
     * requires adding multiple nodes to the egraph.
     */
    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self;

    fn to_enode_or_var(self) -> ENodeOrVar<Self> {
        match self.to_var() {
            Some(var) => ENodeOrVar::Var(format!("?{}", var).parse().unwrap()),
            None => ENodeOrVar::ENode(self),
        }
    }

    // Configures whether to run rule lifting or cvec algorithm for
    // finding candidates.
    // If rule lifting is enabled, L::get_lifting_rewrites() and L::is_allowed_op()
    // must be implemented
    fn is_rule_lifting() -> bool {
        false
    }

    // Used for rule lifting
    fn get_lifting_rewrites() -> Vec<Rewrite<Self, SynthAnalysis>> {
        panic!("No lifting rewrites")
    }

    // Used for rule lifting
    fn is_allowed_op(&self) -> bool {
        true
    }

    fn is_allowed_rewrite(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool {
        let pattern_is_extractable = |pat: &Pattern<Self>| {
            pat.ast.as_ref().iter().all(|n| match n {
                ENodeOrVar::ENode(n) => n.is_allowed_op(),
                ENodeOrVar::Var(_) => true,
            })
        };
        pattern_is_extractable(lhs) && pattern_is_extractable(rhs)
    }

    fn generalize(expr: &RecExpr<Self>, map: &mut HashMap<Symbol, Var>) -> Pattern<Self> {
        let mut rename_node = |node: &Self| match node.to_var() {
            Some(sym) => {
                let len = map.len();
                let var = map
                    .entry(sym)
                    .or_insert_with(|| format!("?{}", letter(len)).parse().unwrap());
                let s = var.to_string();
                Self::mk_var(s[1..].into())
            }
            None => node.clone(),
        };
        let root = rename_node(expr.as_ref().last().unwrap());
        let expr = root.build_recexpr(|id| rename_node(&expr[id]));
        let nodes: Vec<ENodeOrVar<Self>> = expr
            .as_ref()
            .iter()
            .map(|node| node.clone().to_enode_or_var())
            .collect();
        PatternAst::from(nodes).into()
    }

    fn instantiate(pattern: &Pattern<Self>) -> RecExpr<Self> {
        let nodes: Vec<_> = pattern
            .ast
            .as_ref()
            .iter()
            .map(|n| match n {
                ENodeOrVar::ENode(n) => n.clone(),
                ENodeOrVar::Var(v) => {
                    let s = v.to_string();
                    assert!(s.starts_with('?'));
                    Self::mk_var(s[1..].into())
                }
            })
            .collect();

        RecExpr::from(nodes)
    }

    fn score(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> [i32; 5] {
        let l_size = AstSize.cost_rec(&lhs.ast) as i32;
        let r_size = AstSize.cost_rec(&rhs.ast) as i32;
        let mut vars: HashSet<Var> = Default::default();
        vars.extend(lhs.vars());
        vars.extend(rhs.vars());

        let mut ops: HashSet<String> = Default::default();
        for node in lhs.ast.as_ref().iter().chain(rhs.ast.as_ref()) {
            if !node.is_leaf() {
                ops.insert(node.to_string());
            }
        }

        let num_consts = lhs
            .ast
            .as_ref()
            .iter()
            .chain(rhs.ast.as_ref())
            .filter(|n| match n {
                ENodeOrVar::ENode(n) => n.is_constant(),
                ENodeOrVar::Var(_) => false,
            })
            .count() as i32;

        [
            vars.len() as i32,
            -num_consts,
            -i32::max(l_size, r_size),
            -(l_size + r_size),
            -(ops.len() as i32),
        ]
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult;

    fn run_workload(workload: Workload, prior_rules: Ruleset<Self>) -> Ruleset<Self> {
        synth::synth(SynthParams {
            workload,
            prior_rules,
            iter_limit: 2,
            time_limit: 60,
            node_limit: 300000,
        })
    }

    fn run_workload_with_limits(
        workload: Workload,
        prior_rules: Ruleset<Self>,
        iter_limit: usize,
        time_limit: u64,
        node_limit: usize,
    ) -> Ruleset<Self> {
        synth::synth(SynthParams {
            workload,
            prior_rules,
            iter_limit,
            time_limit,
            node_limit,
        })
    }
}
