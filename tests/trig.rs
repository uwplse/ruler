use num::rational::Ratio;
use num::BigInt;
use ruler::enumo::Ruleset;
use ruler::*;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

pub type Rational = Ratio<BigInt>;

// custom implementation of real value
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Real(Symbol);

impl Real {
    pub fn as_str(self) -> &'static str {
        self.0.as_str()
    }
}

impl<S: AsRef<str>> From<S> for Real {
    fn from(s: S) -> Self {
        Real(Symbol::from(s.as_ref()))
    }
}

impl From<Real> for &'static str {
    fn from(s: Real) -> Self {
        s.as_str()
    }
}

impl FromStr for Real {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.is_empty() && s.chars().all(|c| c.is_numeric() || c == '-' || c == '/') {
            Ok(s.into())
        } else {
            Err("not real")
        }
    }
}

impl Display for Real {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for Real {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

// custom implementation of a complex value
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable(Symbol);

impl Variable {
    fn as_str(self) -> &'static str {
        self.0.as_str()
    }
}

impl<S: AsRef<str>> From<S> for Variable {
    fn from(s: S) -> Self {
        Variable(Symbol::from(s.as_ref()))
    }
}

impl From<Variable> for &'static str {
    fn from(s: Variable) -> Self {
        s.as_str()
    }
}

impl FromStr for Variable {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 1 && s.chars().next().unwrap().is_alphabetic() {
            Ok(s.into())
        } else if s.len() == 2 && s.starts_with('?') && s.chars().nth(1).unwrap().is_alphabetic() {
            Ok((&s[1..2]).into())
        } else {
            Err("not variable")
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

egg::define_language! {
  pub enum Trig {
    // trig operators
    "sin" = Sin(Id),
    "cos" = Cos(Id),
    "tan" = Tan(Id),
    "csc" = Csc(Id),
    "sec" = Sec(Id),
    "cot" = Cot(Id),

    // complex exponetial
    "cis" = Cis(Id),

    // arithmetic operators
    "~" = Neg(Id),
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    "sqr" = Sqr(Id),

    // constants
    "I" = Imag,
    "PI" = Pi,
    RealConst(Real),
    Var(Variable),
  }
}

impl SynthLanguage for Trig {
    type Constant = Real;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rewrites() -> Ruleset<Self> {
        Ruleset::from_str_vec(&[
            // definition of sine, cosine, tangent
            "(sin ?a) ==> (/ (- (cis ?a) (cis (~ ?a))) (* 2 I))",
            "(/ (- (cis ?a) (cis (~ ?a))) (* 2 I)) ==> (sin ?a)",
            "(cos ?a) ==> (/ (+ (cis ?a) (cis (~ ?a))) 2)",
            "(/ (+ (cis ?a) (cis (~ ?a))) 2) ==> (cos ?a)",
            "(tan ?a) ==> (* I (/ (- (cis (~ ?a)) (cis ?a)) (+ (cis (~ ?a)) (cis ?a))))",
            "(* I (/ (- (cis (~ ?a)) (cis ?a)) (+ (cis (~ ?a)) (cis ?a)))) ==> (tan ?a)",
            // definition of cosecant, secant, cotangent
            "(csc ?a) ==> (/ 1 (sin ?a))",
            "(/ 1 (sin ?a)) ==> (csc ?a)",
            "(sec ?a) ==> (/ 1 (cos ?a))",
            "(/ 1 (cos ?a)) ==> (sec ?a)",
            "(cot ?a) ==> (/ 1 (tan ?a))",
            "(/ 1 (tan ?a)) ==> (cot ?a)",
            // relating tangent to sine and cosine
            "(tan ?a) ==> (/ (sin ?a) (cos ?a))",
            "(/ (sin ?a) (cos ?a)) ==> (tan ?a)",
            // definition of cos^2(a) and sin^2(a)
            "(* (cos ?a) (cos ?a)) ==> (/ (+ (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4)",
            "(/ (+ (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4) ==> (* (cos ?a) (cos ?a))",
            "(* (sin ?a) (sin ?a)) ==> (~ (/ (- (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4))",
            "(~ (/ (- (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4)) ==> (* (sin ?a) (sin ?a))",
            // definition of square
            "(sqr ?a) ==> (* ?a ?a)",
            "(* ?a ?a) ==> (sqr ?a)",
            // constant folding for PI
            "(+ PI PI) ==> (* 2 PI)",
            // constant folding for cis
            "(cis 0) ==> 1",
            "(cis (/ PI 2)) ==> I",
            // cis identities
            "(cis (~ ?a)) ==> (/ 1 (cis ?a))",
            "(cis (+ ?a ?b)) ==> (* (cis ?a) (cis ?b))",
            "(cis (- ?a ?b)) ==> (* (cis ?a) (cis (~ ?b)))",
            "(* (cis ?a) (cis (~ ?a))) ==> 1",
            // definition of cis
            "(* I I) ==> -1",
        ])
    }

    fn is_allowed_op(&self) -> bool {
        !matches!(self, Trig::Imag | Trig::Cis(_))
    }

    // No eval needed for rule lifting
    fn eval<'a, F>(&'a self, _cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    // No variable initialization needed
    fn initialize_vars(_egraph: &mut EGraph<Self, SynthAnalysis>, _vars: &[String]) {}

    fn to_var(&self) -> Option<Symbol> {
        if let Trig::Var(Variable(sym)) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Trig::Var(Variable::from(sym.as_str()))
    }

    fn is_constant(&self) -> bool {
        matches!(self, Trig::RealConst(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Trig::RealConst(c)
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}
