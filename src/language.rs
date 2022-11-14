use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::*;

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
}

impl<L: SynthLanguage> Signature<L> {
    pub fn is_defined(&self) -> bool {
        self.cvec.is_empty() || self.cvec.iter().any(|v| v.is_some())
    }
}

impl<L: SynthLanguage> egg::Analysis<L> for SynthAnalysis {
    type Data = Signature<L>;

    fn make(egraph: &EGraph<L, Self>, enode: &L) -> Self::Data {
        let get_cvec = |id: &Id| &egraph[*id].data.cvec;
        Signature {
            cvec: enode.eval(egraph.analysis.cvec_len, get_cvec),
        }
    }

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        let mut merge_a = false;
        let mut merge_b = false;

        // let cost_fn = |x: &RecExpr<L>| AstSize.cost_rec(x);

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

        DidMerge(merge_a, merge_b)
    }
}

pub type CVec<L> = Vec<Option<<L as SynthLanguage>::Constant>>;

pub trait SynthLanguage: egg::Language + Send + Sync + Display + FromOp + 'static {
    type Constant: Clone + Hash + Eq + Debug + Display + Ord;

    fn eval<'a, F>(&'a self, cvec_len: usize, f: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>;

    fn initialize_vars(synth: &mut Synthesizer<Self>, vars: Vec<String>);

    fn to_var(&self) -> Option<Symbol>;
    fn mk_var(sym: egg::Symbol) -> Self;

    fn to_enode_or_var(self) -> ENodeOrVar<Self> {
        match self.to_var() {
            Some(var) => ENodeOrVar::Var(format!("?{}", var).parse().unwrap()),
            None => ENodeOrVar::ENode(self),
        }
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
                    assert!(s.starts_with("?"));
                    Self::mk_var(s[1..].into())
                }
            })
            .collect();

        RecExpr::from(nodes)
    }

    fn score(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> [i32; 5] {
        [0, 0, 0, 0, 0]
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult;

    fn run_synth() {
        match Command::parse() {
            Command::Synth(params) => {
                let outfile = params.outfile.clone();
                let syn = Synthesizer::<Self>::new(params);
                let report = syn.run();
                let file = std::fs::File::create(&outfile)
                    .unwrap_or_else(|_| panic!("Failed to open '{}'", outfile));
                serde_json::to_writer_pretty(file, &report).expect("failed to write json");
            }
        }
    }
}
