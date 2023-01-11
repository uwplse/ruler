/*!
    Exponential functions from arithmetic
!*/

use num::rational::Ratio;
use num::BigInt;
use ruler::*;

pub type Rational = Ratio<BigInt>;

egg::define_language! {
    pub enum Exponential {
        // trig operators
        "exp" = Exp(Id),
        "log" = Log(Id),
        "pow" = Pow([Id; 2]),
        "sqrt" = Sqrt(Id),
        "cbrt" = Cbrt(Id),

        // arithmetic operators
        "~" = Neg(Id),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),

        // constants
        Num(Rational),
        Var(Symbol),
    }
}

impl SynthLanguage for Exponential {
    type Constant = Rational;

    // cvec-less domain
    fn eval<'a, F>(&'a self, _cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self> {
        vec![]
    }

    // no variable initialization required
    fn initialize_vars(_egraph: &mut EGraph<Self, SynthAnalysis>, _vars: &[String]) {}

    fn to_var(&self) -> Option<Symbol> {
        if let Exponential::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Exponential::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Exponential::Num(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Exponential::Num(c)
    }

    // no validation possible
    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rules() -> enumo::Ruleset<Self> {
        enumo::Ruleset::from_str_vec(&[
            // definitions (denote)
            "(pow ?a ?b) ==> (exp (* ?b (log ?a)))",
            "(sqrt ?a) ==> (pow ?a 1/2)",
            "(cbrt ?a) ==> (pow ?a 1/3)",
            // definitions (simplify)
            "(exp (* ?b (log ?a))) ==> (pow ?a ?b)",
            "(pow ?a 1/2) ==> (sqrt ?a)",
            "(pow ?a 1/3) ==> (cbrt ?a)",
            // exponential properties (expand)
            "(exp (+ ?a ?b)) ==> (* (exp ?a) (exp ?b))",
            "(exp (~ ?a)) ==> (/ 1 (exp ?a))",
            // exponential properties (simplify)
            "(* (exp ?a) (exp ?b)) ==> (exp (+ ?a ?b))",
            "(/ 1 (exp ?a)) ==> (exp (~ ?a))",
            "(exp 0) ==> 1",
            // inverse properties
            "(log (exp ?a)) ==> ?a",
            "(exp (log ?a)) ==> ?a",
            // inverse (non-trivial applier)
            // "?x" => "(log (exp ?x))" if is_var_str("?x"),
            // "?x" => "(exp (log ?x))" if is_var_str("?x"),
        ])
    }

    fn is_allowed_op(&self) -> bool {
        true
    }
}
