use egg::rewrite;
use num::rational::Ratio;
use num::BigInt;
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

    fn get_lifting_rewrites() -> Vec<egg::Rewrite<Self, SynthAnalysis>> {
        vec![
            // definition of sine, cosine, tangent
            rewrite!("def-sin"; "(sin ?a)" <=> "(/ (- (cis ?a) (cis (~ ?a))) (* 2 I))"),
            rewrite!("def-cos"; "(cos ?a)" <=> "(/ (+ (cis ?a) (cis (~ ?a))) 2)"),
            rewrite!("def-tan"; "(tan ?a)" <=> "(* I (/ (- (cis (~ ?a)) (cis ?a)) (+ (cis (~ ?a)) (cis ?a))))"),

            // definition of cosecant, secant, cotangent
            rewrite!("def-csc"; "(csc ?a)" <=> "(/ 1 (sin ?a))"),
            rewrite!("def-sec"; "(sec ?a)" <=> "(/ 1 (cos ?a))"),
            rewrite!("def-cot"; "(cot ?a)" <=> "(/ 1 (tan ?a))"),

            // relating tangent to sine and cosine
            rewrite!("def-tan-ratio"; "(tan ?a)" <=> "(/ (sin ?a) (cos ?a))"),
            // definition of cos^2(a) and sin^2(a)
            rewrite!("def-cos-sq"; "(* (cos ?a) (cos ?a))" <=>
                     "(/ (+ (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4)"),
            rewrite!("def-sin-sq"; "(* (sin ?a) (sin ?a))" <=>
                     "(~ (/ (- (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4))"),

            // definition of square
            rewrite!("def-sqr"; "(sqr ?a)" <=> "(* ?a ?a)"),
            vec![
                // constant folding for PI
                rewrite!("add-pi"; "(+ PI PI)" => "(* 2 PI)"),

                // constant folding for cis
                rewrite!("cis-0"; "(cis 0)" => "1"),
                rewrite!("cis-pi"; "(cis (/ PI 2))" => "I"),

                // cis identities
                // rewrite!("inv-cis"; "(cis (~ ?a))" => "(/ 1 (cis ?a))"),
                rewrite!("add-cis"; "(cis (+ ?a ?b))" => "(* (cis ?a) (cis ?b))"),
                rewrite!("sub-cis"; "(cis (- ?a ?b))" => "(* (cis ?a) (cis (~ ?b)))"),
                rewrite!("cancel-cis"; "(* (cis ?a) (cis (~ ?a)))" => "1"),

                // definition of cis
                rewrite!("square-i"; "(* I I)" => "-1"),
            ],
        ].concat()
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
    fn initialize_vars(_synth: &mut Synthesizer<Self>, _vars: Vec<String>) {}

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

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult {
        ValidationResult::Valid
    }
}
