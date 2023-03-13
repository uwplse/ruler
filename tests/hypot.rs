use num::rational::Ratio;
use num::BigInt;
use num::{Signed, Zero};
use ruler::enumo::{Ruleset, Workload};
use ruler::*;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;
use std::time::Instant;

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

fn extract_constant(nodes: &[Hypot]) -> Option<Rational> {
    for n in nodes {
        if let Hypot::RealConst(v) = n {
            if let Ok(r) = v.as_str().parse() {
                return Some(r);
            }
        }
    }

    None
}

egg::define_language! {
  pub enum Hypot {
    // Hypot operators
    "sinh" = Sinh(Id),
    "cosh" = Cosh(Id),
    "tanh" = Tanh(Id),

    // complex exponetial
    "exp" = Exp(Id),

    // arithmetic operators
    "~" = Neg(Id),
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),

    // constants
    RealConst(Real),
    Var(Variable),
  }
}

impl SynthLanguage for Hypot {
    type Constant = Real;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::new(&[
            // definition of sinh, cosh, tanh
            "(cosh ?x) ==> (/ (+ (exp ?x) (exp (~ ?x))) 2)",
            "(/ (+ (exp ?x) (exp (~ ?x))) 2) ==> (cosh ?x)",
            "(sinh ?x) ==> (/ (- (exp ?x) (exp (~ ?x))) 2)",
            "(/ (- (exp ?x) (exp (~ ?x))) 2) ==> (sinh ?x)",
            "(tanh ?x) ==> (/ (- (exp ?x) (exp (~ ?x))) (+ (exp ?x) (exp (~ ?x))))",
            "(/ (- (exp ?x) (exp (~ ?x))) (+ (exp ?x) (exp (~ ?x)))) ==> (tanh ?x)",
            "(tanh ?x) ==> (/ (sinh ?x) (cosh ?x))",
            "(/ (sinh ?x) (cosh ?x)) ==> (tanh ?x)",
            // identities of exp
            "(exp (+ ?a ?b)) ==> (* (exp ?a) (exp ?b))",
            "(exp (- ?a ?b)) ==> (* (exp ?a) (exp (~ ?b)))",
            "(* (exp ?a) (exp (~ ?a))) ==> 1",
            "(exp 0) ==> 1",
        ])
    }

    fn is_allowed_op(&self) -> bool {
        // !matches!(self, Hypot::Exp(_))
        true
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
        if let Hypot::Var(Variable(sym)) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Hypot::Var(Variable::from(sym.as_str()))
    }

    fn is_constant(&self) -> bool {
        matches!(self, Hypot::RealConst(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Hypot::RealConst(c)
    }

    fn custom_modify(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if egraph[id]
            .nodes
            .iter()
            .any(|x| matches!(x, Hypot::RealConst(_)))
        {
            return;
        }

        let mut to_add: Option<Hypot> = None;
        for n in &egraph[id].nodes {
            match n {
                Hypot::Neg(i) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        let r = Real::from((-x).to_string());
                        to_add = Some(Self::mk_constant(r, egraph));
                        break;
                    }
                }
                Hypot::Add([i, j]) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        if let Some(y) = extract_constant(&egraph[*j].nodes) {
                            let r = Real::from((x + y).to_string());
                            to_add = Some(Self::mk_constant(r, egraph));
                            break;
                        }
                    }
                }
                Hypot::Sub([i, j]) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        if let Some(y) = extract_constant(&egraph[*j].nodes) {
                            let r = Real::from((x - y).to_string());
                            to_add = Some(Self::mk_constant(r, egraph));
                            break;
                        }
                    }
                }
                Hypot::Mul([i, j]) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        if let Some(y) = extract_constant(&egraph[*j].nodes) {
                            let r = Real::from((x * y).to_string());
                            to_add = Some(Self::mk_constant(r, egraph));
                            break;
                        }
                    }
                }
                Hypot::Div([i, j]) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        if let Some(y) = extract_constant(&egraph[*j].nodes) {
                            if !y.is_zero() {
                                let r = Real::from((x / y).to_string());
                                to_add = Some(Self::mk_constant(r, egraph));
                                break;
                            }
                        }
                    }
                }
                _ => (),
            }
        }

        if let Some(v) = to_add {
            // add (~ v) if v is negative or zero
            if let Hypot::RealConst(n) = v {
                if let Ok(x) = n.as_str().parse::<Rational>() {
                    if x.is_negative() || x.is_zero() {
                        let pos_id = egraph.add(Self::mk_constant(
                            Real::from((-x).to_string()),
                            &mut egraph.clone(),
                        ));
                        let neg_id = egraph.add(Hypot::Neg(pos_id));
                        egraph.union(neg_id, id);
                    }
                }
            }

            let cnst_id = egraph.add(v);
            egraph.union(cnst_id, id);
        }
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}

impl Hypot {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let num_prior = prior.len();
        let mut candidates = Ruleset::allow_forbid_actual(egraph, prior.clone(), limits);

        let chosen = candidates.minimize(prior, limits);
        let time = t.elapsed().as_secs_f64();

        println!(
            "Learned {} bidirectional rewrites ({} total rewrites) in {} using {} prior rewrites",
            chosen.bidir_len(),
            chosen.len(),
            time,
            num_prior
        );

        chosen.pretty_print();

        chosen
    }
}

mod test {
    use super::*;
    use ruler::{
        enumo::{Ruleset, Workload},
        Limits,
    };

    #[test]
    fn main_recipe() {
        let complex: Ruleset<Hypot> = Ruleset::from_file("scripts/trig/complex.rules");
        let limits = Limits {
            iter: 3,
            node: 2000000,
        };

        let h_ops = Workload::new(["sinh", "cosh", "tanh"]);
        let consts = Workload::new(["0", "1", "-1"]);
        let app = Workload::new(["(op v)"]);
        let trig_constants = app.clone().plug("op", &h_ops).plug("v", &consts);

        let simple_terms = app.clone().plug("op", &h_ops).plug(
            "v",
            &Workload::new(["a", "(~ a)", "(+ a a)"]),
        );

        let neg_terms = Workload::new(["(~ x)"]).plug("x", &simple_terms);

        let squares = Workload::new(["(* x x)"])
            .plug("x", &app)
            .plug("op", &h_ops)
            .plug("v", &Workload::new(["a", "b"]));

        let add = Workload::new(["(+ e e)", "(- e e)"]);

        let sum_of_squares = add.plug("e", &squares);

        let mut all = complex;
        let mut new = Ruleset::<Hypot>::default();

        let wkld1 = trig_constants;
        println!("Starting 1");
        let rules1 = Hypot::run_workload(wkld1.clone(), all.clone(), limits);
        all.extend(rules1.clone());
        new.extend(rules1.clone());
        // assert_eq!(rules1.len(), 22);

        let wkld2 = Workload::Append(vec![wkld1, simple_terms, neg_terms]);
        println!("Starting 2");
        let rules2 = Hypot::run_workload(wkld2.clone(), all.clone(), limits);
        all.extend(rules2.clone());
        new.extend(rules2.clone());
        // assert_eq!(rules2.len(), 12);

        let wkld3 = Workload::Append(vec![wkld2.clone(), sum_of_squares.clone()]);
        println!("Starting 3");
        let rules3 = Hypot::run_workload(wkld3, all.clone(), limits);
        all.extend(rules3.clone());
        new.extend(rules3.clone());
        // assert_eq!(rules3.len(), 3);

        // Only new rules should be uploaded!
        new.write_json_rules("hypot.json");
    }
}
