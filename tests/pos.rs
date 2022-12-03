/**
 * Pos is a datatype representing the strictly positive integers in a binary way.
 * XH represents 1
 * XO and XI represent adding a new least significant digit
 * That is, (XO n) represents 2n and (XI n) represents 2n + 1
 * Example: 6 is represented as (XO (XI XH))
 * See https://coq.inria.fr/library/Coq.Numbers.BinNums.html
 */
use egg::rewrite;
use ruler::*;

egg::define_language! {
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

impl SynthLanguage for Pos {
    type Constant = usize;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rewrites() -> Vec<egg::Rewrite<Self, SynthAnalysis>> {
        vec![
            rewrite!("def-xh"; "XH" <=> "(S Z)"),
            rewrite!("def-xo"; "(XO ?a)" <=> "(+ ?a ?a)"),
            rewrite!("def-xi"; "(XI ?a)" <=> "(+ ?a (+ ?a (S Z)))"),
        ]
        .concat()
    }

    fn is_allowed_op(&self) -> bool {
        matches!(
            self,
            Pos::XH | Pos::XO(_) | Pos::XI(_) | Pos::Add(_) | Pos::Mul(_) | Pos::Var(_)
        )
    }

    // No eval needed for rule lifting
    fn eval<'a, F>(&'a self, _cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    // Nov variable initialization needed
    fn initialize_vars(_synth: &mut Synthesizer<Self>, _vars: Vec<String>) {}

    fn to_var(&self) -> Option<Symbol> {
        if let Pos::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Pos::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Pos::Z | Pos::XH)
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        match c {
            0 => Pos::Z,
            1 => Pos::XH,
            c if c % 2 == 0 => Pos::XO(Self::mk_constant_id(c / 2, egraph)),
            _ => Pos::XI(Self::mk_constant_id((c - 1) / 2, egraph)),
        }
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult {
        ValidationResult::Valid
    }
}
