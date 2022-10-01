/**
 * Pos is a datatype representing the strictly positive integers in a binary way.
 * XH represents 1
 * XO and XI represent adding a new least significant digit
 * That is, (XO n) represents 2n and (XI n) represents 2n + 1
 * Example: 6 is represented as (XO (XI XH))
 * See https://coq.inria.fr/library/Coq.Numbers.BinNums.html
 */
use egg::*;
use ruler::*;

define_language! {
    pub enum Pos {
        // Nat
        "Z" = Z,
        "S" = S(Id),

        // Pos
        "XH" = XH,
        "XO" = XO(Id),
        "XI" = XI(Id),

        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),

        Var(egg::Symbol),
    }
}

impl Pos {
    fn mk_constant_id(c: usize, egraph: &mut EGraph<Self, SynthAnalysis>) -> Id {
        match c {
            0 => egraph.add(Pos::Z),
            1 => egraph.add(Pos::XH),
            c if c % 2 == 0 => {
                let pred = Self::mk_constant_id(c / 2, egraph);
                egraph.add(Pos::XO(pred))
            }
            _ => {
                let pred = Self::mk_constant_id((c - 1) / 2, egraph);
                egraph.add(Pos::XI(pred))
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Pos,
}

impl SynthLanguage for Pos {
    type Constant = usize;

    type Type = Type;

    fn get_type(&self) -> Self::Type {
        Type::Pos
    }

    fn eval<'a, F>(&'a self, _cvec_len: usize, _f: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        // No eval needed for rule lifting
        vec![]
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Pos::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Pos::Var(sym)
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        match c {
            0 => Pos::Z,
            1 => Pos::XH,
            c if c % 2 == 0 => Pos::XO(Self::mk_constant_id(c / 2, egraph)),
            _ => Pos::XI(Self::mk_constant_id((c - 1) / 2, egraph)),
        }
    }

    fn is_constant(&self) -> bool {
        matches!(self, Pos::Z | Pos::XH)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut egraph: EGraph<Pos, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 0,
            constant_fold: ConstantFoldMethod::IntervalAnalysis,
            rule_lifting: true,
        });

        egraph.add(Pos::Z);
        egraph.add(Pos::XH);

        synth.lifting_rewrites = vec![
            rewrite!("def-xh"; "XH" <=> "(S Z)"),
            rewrite!("def-xo"; "(XO ?a)" <=> "(+ ?a ?a)"),
            rewrite!("def-xi"; "(XI ?a)" <=> "(+ ?a (+ ?a (S Z)))"),
        ]
        .concat();

        synth.egraph = egraph;
    }

    fn make_layer(_synth: &Synthesizer<Self>, _iter: usize) -> Vec<Self> {
        vec![]
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult<Self> {
        ValidationResult::Valid
    }

    fn is_allowed_rewrite(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool {
        let contains_nat_node = |pat: &Pattern<Self>| {
            pat.ast
                .as_ref()
                .iter()
                .any(|n| matches!(n, ENodeOrVar::ENode(Pos::Z) | ENodeOrVar::ENode(Pos::S(_))))
        };
        let is_extractable = |pat: &Pattern<Self>| {
            pat.ast.as_ref().iter().all(|n| match n {
                ENodeOrVar::Var(_) => true,
                ENodeOrVar::ENode(n) => n.is_extractable(),
            })
        };

        is_extractable(lhs)
            && is_extractable(rhs)
            && !(contains_nat_node(lhs) || contains_nat_node(rhs))
    }
}

fn main() {
    Pos::main()
}
