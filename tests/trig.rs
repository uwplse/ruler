use num::rational::Ratio;
use num::BigInt;
use num::{Signed, Zero};
use ruler::enumo::Ruleset;
use ruler::*;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;
use std::time::Instant;
#[path = "./recipes/trig.rs"]
pub mod trig;

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

fn extract_constant(nodes: &[Trig]) -> Option<Rational> {
    for n in nodes {
        if let Trig::RealConst(v) = n {
            if let Ok(r) = v.as_str().parse() {
                return Some(r);
            }
        }
    }

    None
}

egg::define_language! {
  pub enum Trig {
    // trig operators
    "sin" = Sin(Id),
    "cos" = Cos(Id),
    "tan" = Tan(Id),
    // "csc" = Csc(Id),
    // "sec" = Sec(Id),
    // "cot" = Cot(Id),

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

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::new(&[
            // definition of sine, cosine, tangent
            // (sine)
            "(sin ?a) ==> (/ (- (cis ?a) (cis (~ ?a))) (* 2 I))",
            "(/ (- (cis ?a) (cis (~ ?a))) (* 2 I)) ==> (sin ?a)",
            // (cosine)
            "(cos ?a) ==> (/ (+ (cis ?a) (cis (~ ?a))) 2)",
            "(/ (+ (cis ?a) (cis (~ ?a))) 2) ==> (cos ?a)",
            // (tangent)
            "(tan ?a) ==> (* I (/ (- (cis (~ ?a)) (cis ?a)) (+ (cis (~ ?a)) (cis ?a))))",
            "(* I (/ (- (cis (~ ?a)) (cis ?a)) (+ (cis (~ ?a)) (cis ?a)))) ==> (tan ?a)",
            // (sine, alternatively)
            "(sin ?a) ==> (/ (- (* I (cis (~ ?a))) (* I (cis ?a))) 2)",
            "(/ (- (* I (cis (~ ?a))) (* I (cis ?a))) 2) => (sin ?a)",
            // (cosine, alternatively)
            "(cos ?a) ==> (/ (+ (* I (cis ?a)) (* I (cis (~ ?a)))) (* 2 I))",
            "(/ (+ (* I (cis ?a)) (* I (cis (~ ?a)))) (* 2 I)) ==> (cos ?a)",
            // relating tangent to sine and cosine
            "(tan ?a) ==> (/ (sin ?a) (cos ?a))",
            "(/ (sin ?a) (cos ?a)) ==> (tan ?a)",
            // definition of cos(a)*cos(b) and sin(a)*sin(b)
            "(* (cos ?a) (cos ?b)) ==> (/ (+ (+ (cis (- ?a ?b)) (cis (~ (- ?a ?b)))) (+ (cis (+ ?a ?b)) (cis (~ (+ ?a ?b))))) 4)",
            "(* (sin ?a) (sin ?b)) ==> (/ (- (+ (cis (- ?a ?b)) (cis (~ (- ?a ?b)))) (+ (cis (+ ?a ?b)) (cis (~ (+ ?a ?b))))) 4)",
            // definition of cos(a)*sin(b) and sin(a)*cos(b)
            "(* (cos ?a) (sin ?b)) ==> (/ (+ (- (cis (+ ?a ?b)) (cis (~ (+ ?a ?b)))) (- (cis (- ?b ?a)) (cis (~ (- ?b ?a))))) (* 4 I))",
            "(* (sin ?a) (cos ?b)) ==> (/ (+ (- (cis (+ ?a ?b)) (cis (~ (+ ?a ?b)))) (- (cis (- ?a ?b)) (cis (~ (- ?a ?b))))) (* 4 I))",
            // definition of square
            "(sqr ?a) ==> (* ?a ?a)",
            "(* ?a ?a) ==> (sqr ?a)",
            // [Redundant, but left here so we don't have to compute them again]
            // definition of cos^2(a) and sin^2(a)
            // "(* (cos ?a) (cos ?a)) ==> (/ (+ (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4)",
            // "(/ (+ (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4) ==> (* (cos ?a) (cos ?a))",
            // "(* (sin ?a) (sin ?a)) ==> (~ (/ (- (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4))",
            // "(~ (/ (- (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4)) ==> (* (sin ?a) (sin ?a))",
        ])
    }

    fn is_allowed_op(&self) -> bool {
        !matches!(self, Trig::Imag | Trig::Cis(_) | Trig::Sqr(_))
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

    fn custom_modify(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if egraph[id]
            .nodes
            .iter()
            .any(|x| matches!(x, Trig::RealConst(_)))
        {
            return;
        }

        let mut to_add: Option<Trig> = None;
        for n in &egraph[id].nodes {
            match n {
                Trig::Neg(i) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        let r = Real::from((-x).to_string());
                        to_add = Some(Self::mk_constant(r, egraph));
                        break;
                    }
                }
                Trig::Add([i, j]) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        if let Some(y) = extract_constant(&egraph[*j].nodes) {
                            let r = Real::from((x + y).to_string());
                            to_add = Some(Self::mk_constant(r, egraph));
                            break;
                        }
                    }
                }
                Trig::Sub([i, j]) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        if let Some(y) = extract_constant(&egraph[*j].nodes) {
                            let r = Real::from((x - y).to_string());
                            to_add = Some(Self::mk_constant(r, egraph));
                            break;
                        }
                    }
                }
                Trig::Mul([i, j]) => {
                    if let Some(x) = extract_constant(&egraph[*i].nodes) {
                        if let Some(y) = extract_constant(&egraph[*j].nodes) {
                            let r = Real::from((x * y).to_string());
                            to_add = Some(Self::mk_constant(r, egraph));
                            break;
                        }
                    }
                }
                Trig::Div([i, j]) => {
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
            // add (~ v) if v is negative or v is zero
            if let Trig::RealConst(n) = v {
                if let Ok(x) = n.as_str().parse::<Rational>() {
                    if x.is_negative() || x.is_zero() {
                        let pos_id = egraph.add(Self::mk_constant(
                            Real::from((-x).to_string()),
                            &mut egraph.clone(),
                        ));
                        let neg_id = egraph.add(Trig::Neg(pos_id));
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

#[cfg(test)]
mod test {
    use std::time::Duration;

    use super::*;
    use crate::trig::trig_rules;
    use ruler::{
        enumo::{Filter, Ruleset, Scheduler, Workload},
        recipe_utils::run_rule_lifting,
        Limits,
    };

    // Extra rules about `cis` and `I` to "fast-forward" rule synthesis
    pub fn prior_rules() -> Ruleset<Trig> {
        Ruleset::new([
            // constant folding for PI
            "(+ PI PI) ==> (* 2 PI)",
            "(* 2 PI) ==> (+ PI PI)",
            // constant folding for cis
            "(cis 0) ==> 1",
            "(cis (/ PI 2)) ==> I",
            "(cis (~ (/ PI 2))) ==> (~ I)",
            "(cis PI) ==> -1",
            // cis identities
            "(cis (+ ?a ?b)) ==> (* (cis ?a) (cis ?b))",
            "(* (cis ?a) (cis ?b)) ==> (cis (+ ?a ?b))",
            "(cis (- ?a ?b)) ==> (* (cis ?a) (cis (~ ?b)))",
            "(* (cis ?a) (cis (~ ?b))) ==> (cis (- ?a ?b))",
            "(cis (~ ?a)) ==> (/ 1 (cis ?a))",
            "(/ 1 (cis ?a)) ==> (cis (~ ?a))",
            "(* (cis ?a) (cis (~ ?a))) ==> 1",
            // constant folding I
            "(/ 1 I) ==> (~ I)",
            "(* I I) ==> -1",
        ])
    }

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let herbie: Ruleset<Trig> = Ruleset::from_file("baseline/herbie-trig.rules");

        let start = Instant::now();
        let rules = trig_rules();
        let duration = start.elapsed();

        logger::write_baseline(&rules, "trig", &herbie, "herbie", duration);
    }

    #[test]
    fn simple() {
        let complex: Ruleset<Trig> = Ruleset::from_file("scripts/oopsla21/trig/complex.rules");
        assert_eq!(complex.len(), 57);

        let limits = Limits {
            iter: 3,
            node: 2000000,
            match_: 200_000,
        };

        let terms = Workload::new([
            "(sin 0)",
            "(sin (/ PI 6))",
            "(sin (/ PI 4))",
            "(sin (/ PI 3))",
            "(sin (/ PI 2))",
            "(sin PI)",
            "(sin (* PI 2))",
        ]);
        assert_eq!(terms.force().len(), 7);

        let mut all = complex;
        all.extend(prior_rules());

        let rules = run_rule_lifting(&terms, all, limits);

        let expected: Ruleset<Trig> =
            Ruleset::new(&["(sin (* PI 2)) <=> 0", "0 <=> (sin 0)", "0 <=> (sin PI)"]);
        let (can, cannot) = rules.derive(DeriveType::Lhs, &expected, Limits::deriving());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }

    fn lifting_variation(
        w: &Workload,
        phase1: Phase<Trig>,
        phase2: Phase<Trig>,
        phase3: Phase<Trig>,
        minimize: Phase<Trig>,
    ) -> (Ruleset<Trig>, Duration) {
        let start = Instant::now();
        let g = w.to_egraph();
        let g1 = phase1.scheduler.run(&g, &phase1.rules);
        println!(
            "Done with phase 1 after {} secs, {} eclasses",
            start.elapsed().as_secs(),
            g1.number_of_classes()
        );

        let g2 = phase2.scheduler.run(&g1, &phase2.rules);
        println!(
            "Done with phase 2 after {} secs, {} eclasses",
            start.elapsed().as_secs(),
            g2.number_of_classes()
        );
        let mut candidates = Ruleset::extract_candidates(&g1, &g2);

        let g3 = phase3.scheduler.run(&g2, &phase3.rules);
        println!(
            "Done with phase 3 after {} secs, {} eclasses",
            start.elapsed().as_secs(),
            g3.number_of_classes()
        );
        candidates.extend(Ruleset::extract_candidates(&g2, &g3));

        // let (sound, _) = candidates.partition(|r| r.is_valid());
        let (sound, _) = candidates.minimize(minimize.rules, minimize.scheduler);

        logger::write_lifting_phase(phase1, phase2, phase3, start.elapsed(), &sound);

        (sound, start.elapsed())
    }

    #[test]
    fn lifting_phases() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let limits = Limits {
            iter: 3,
            node: 300000,
            match_: 200_000,
        };
        let mut all = Ruleset::from_file("scripts/oopsla21/trig/complex.rules");
        all.extend(prior_rules());
        all.extend(Trig::get_lifting_rules());
        let mut all_but_exploratory = all.clone();
        all_but_exploratory.remove_all(Trig::get_lifting_rules());
        let (allowed, _) = all.partition(|r| Trig::is_allowed_rewrite(&r.lhs, &r.rhs));
        let exploratory = Trig::get_lifting_rules();

        let lits = Workload::new([
            "a", "b", "c", "0", "(/ PI 6)", "(/ PI 4)", "(/ PI 3)", "(/ PI 2)", "PI", "(* PI 2)",
        ]);
        let ops = Workload::new(["sin", "cos", "tan"]);
        let terms = Workload::new(["(OP V)"])
            .plug("OP", &ops)
            .plug("V", &lits)
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(tan (/ PI 2))".parse().unwrap(),
            ))));

        lifting_variation(
            &terms,
            Phase {
                rules: all.clone(),
                rules_name: "R".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: all.clone(),
                rules_name: "R".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: all.clone(),
                rules_name: "R".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Compress(limits),
            },
        );

        lifting_variation(
            &terms,
            Phase {
                rules: all.clone(),
                rules_name: "R".into(),
                scheduler: Scheduler::Simple(limits),
            },
            Phase {
                rules: all.clone(),
                rules_name: "R".into(),
                scheduler: Scheduler::Simple(limits),
            },
            Phase {
                rules: all.clone(),
                rules_name: "R".into(),
                scheduler: Scheduler::Simple(limits),
            },
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Compress(limits),
            },
        );

        lifting_variation(
            &terms,
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: exploratory.clone(),
                rules_name: "E".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: all_but_exploratory.clone(),
                rules_name: "R-E".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Compress(limits),
            },
        );

        lifting_variation(
            &terms,
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Simple(limits),
            },
            Phase {
                rules: exploratory.clone(),
                rules_name: "E".into(),
                scheduler: Scheduler::Simple(limits),
            },
            Phase {
                rules: all_but_exploratory.clone(),
                rules_name: "R-E".into(),
                scheduler: Scheduler::Simple(limits),
            },
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Compress(limits),
            },
        );

        lifting_variation(
            &terms,
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: exploratory.clone(),
                rules_name: "E".into(),
                scheduler: Scheduler::Simple(limits),
            },
            Phase {
                rules: all_but_exploratory.clone(),
                rules_name: "R-E".into(),
                scheduler: Scheduler::Compress(limits),
            },
            Phase {
                rules: allowed.clone(),
                rules_name: "A".into(),
                scheduler: Scheduler::Compress(limits),
            },
        );
    }
}
