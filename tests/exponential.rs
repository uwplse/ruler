/*!
    Exponential functions from arithmetic
!*/

use std::time::Instant;

use num::rational::Ratio;
use num::BigInt;
use ruler::enumo::{Ruleset, Scheduler, Workload};
use ruler::*;
#[path = "./recipes/exponential.rs"]
pub mod exponential;
// mod rational;

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
        // (for compatibility with rationals)
        "fabs" = Abs(Id),

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
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        for v in vars {
            let id = egraph.add(Exponential::Var(Symbol::from(v.clone())));

            let l_id = egraph.add(Exponential::Log(id));
            let el_id = egraph.add(Exponential::Exp(l_id));

            let e_id = egraph.add(Exponential::Exp(id));
            let le_id = egraph.add(Exponential::Log(e_id));

            egraph.union(id, el_id);
            egraph.union(id, le_id);
            egraph.rebuild();
        }
    }

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
        enumo::Ruleset::new(&[
            // definitions (denote)
            "(pow ?a ?b) ==> (exp (* ?b (log ?a)))",
            "(sqrt ?a) ==> (pow ?a 1/2)",
            "(cbrt ?a) ==> (pow ?a 1/3)",
            // definitions (simplify)
            "(exp (* ?b (log ?a))) ==> (pow ?a ?b)",
            "(pow ?a 1/2) ==> (sqrt ?a)",
            "(pow ?a 1/3) ==> (cbrt ?a)",
        ])
    }

    fn is_allowed_op(&self) -> bool {
        true
    }
}

impl Exponential {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let num_prior = prior.len();
        let mut candidates = Ruleset::allow_forbid_actual(egraph, prior.clone(), limits);

        let chosen = candidates.minimize(prior, Scheduler::Compress(limits));
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::exponential::make_rules;
    use ruler::enumo;

    type Workload = enumo::Workload;
    type Ruleset = enumo::Ruleset<Exponential>;

    pub fn starting_exponential_rules() -> Ruleset {
        Ruleset::new(&[
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
        ])
    }

    pub fn rational_rules() -> Ruleset {
        Ruleset::new(&[
            "(+ ?b ?a) ==> (+ ?a ?b)",
            "(* ?b ?a) ==> (* ?a ?b)",
            "(- ?a ?a) ==> 0",
            "(+ ?a 0) ==> ?a",
            "?a ==> (+ ?a 0)",
            "(* ?a 1) ==> ?a",
            "?a ==> (* ?a 1)",
            "(- ?a 0) ==> ?a",
            "?a ==> (- ?a 0)",
            "(/ ?a 1) ==> ?a",
            "?a ==> (/ ?a 1)",
            "(/ ?a -1) ==> (~ ?a)",
            "(~ ?a) ==> (/ ?a -1)",
            "(- 0 ?a) ==> (~ ?a)",
            "(~ ?a) ==> (- 0 ?a)",
            "(* ?a -1) ==> (~ ?a)",
            "(~ ?a) ==> (* ?a -1)",
            "(- ?a ?a) ==> (* ?a 0)",
            "(* ?a 0) ==> (- ?a ?a)",
            "(+ ?a 1) ==> (- ?a -1)",
            "(- ?a -1) ==> (+ ?a 1)",
            "(+ ?a -1) ==> (- ?a 1)",
            "(- ?a 1) ==> (+ ?a -1)",
            "(* (+ ?a 1) (/ -1 ?a)) ==> (/ (- -1 ?a) ?a)",
            "(/ (- -1 ?a) ?a) ==> (* (+ ?a 1) (/ -1 ?a))",
            "(* (/ -1 ?a) (- 1 ?a)) ==> (/ (- ?a 1) ?a)",
            "(/ (- ?a 1) ?a) ==> (* (/ -1 ?a) (- 1 ?a))",
            "(- (/ ?a ?a) (/ 0 ?a)) ==> (* (~ ?a) (/ -1 ?a))",
            "(* (~ ?a) (/ -1 ?a)) ==> (- (/ ?a ?a) (/ 0 ?a))",
            "(* (- 1 ?a) (/ 1 ?a)) ==> (/ (- 1 ?a) ?a)",
            "(/ (- 1 ?a) ?a) ==> (* (- 1 ?a) (/ 1 ?a))",
            "(* ?a (/ 1 ?a)) ==> (- (/ ?a ?a) (/ 0 ?a))",
            "(- (/ ?a ?a) (/ 0 ?a)) ==> (* ?a (/ 1 ?a))",
            "(* (+ ?a 1) (/ 1 ?a)) ==> (/ (+ ?a 1) ?a)",
            "(/ (+ ?a 1) ?a) ==> (* (+ ?a 1) (/ 1 ?a))",
            "(* ?a (/ -1 ?a)) ==> (- (/ 0 ?a) (/ ?a ?a))",
            "(- (/ 0 ?a) (/ ?a ?a)) ==> (* ?a (/ -1 ?a))",
            "(/ (/ 0 ?a) ?a) ==> (/ (/ 0 ?a) (+ ?a ?a))",
            "(/ (/ 0 ?a) (+ ?a ?a)) ==> (/ (/ 0 ?a) ?a)",
            "(/ (/ 0 ?a) ?a) ==> (/ (/ 0 ?a) (* ?a ?a))",
            "(/ (/ 0 ?a) (* ?a ?a)) ==> (/ (/ 0 ?a) ?a)",
            "(/ (/ 0 ?a) ?a) ==> (/ (/ 0 ?a) (fabs ?a))",
            "(/ (/ 0 ?a) (fabs ?a)) ==> (/ (/ 0 ?a) ?a)",
            "(/ 0 (* ?a ?a)) ==> (/ 0 ?a)",
            "(/ 0 ?a) ==> (/ 0 (* ?a ?a))",
            "(/ 0 (+ ?a ?a)) ==> (/ 0 ?a)",
            "(+ ?c (+ ?b ?a)) ==> (+ ?a (+ ?b ?c))",
            "(* ?c (* ?b ?a)) ==> (* ?b (* ?c ?a))",
            "(- ?c (- ?b ?a)) ==> (- ?a (- ?b ?c))",
            "(- ?c (- ?b ?a)) ==> (+ ?a (- ?c ?b))",
            "(+ (* ?b ?c) (* ?b ?a)) ==> (* ?b (+ ?a ?c))",
            "(- (* ?a ?c) (* ?b ?a)) ==> (* ?a (- ?c ?b))",
            "(* (/ ?c ?a) (* ?b ?a)) ==> (* (* ?c ?a) (/ ?b ?a))",
            "(* (* ?c ?a) (/ ?b ?a)) ==> (* (/ ?c ?a) (* ?b ?a))",
            "(- (+ ?c ?c) (+ ?b ?a)) ==> (- (- ?c ?b) (- ?a ?c))",
            "(- (+ ?b ?c) (+ ?b ?a)) ==> (- (+ ?c ?c) (+ ?a ?c))",
            "(fabs (- ?b ?a)) ==> (fabs (- ?a ?b))",
            "(* (fabs ?b) (fabs ?a)) ==> (fabs (* ?a ?b))",
            "(/ (- ?a ?b) (- ?b ?a)) ==> (/ (- ?b ?a) (- ?a ?b))",
            "(/ (- ?b ?a) (- ?b ?a)) ==> (/ (- ?a ?b) (- ?a ?b))",
            "(/ (* ?a ?b) (/ ?a ?a)) ==> (* (/ ?b ?a) (* ?a ?a))",
            "(* (/ ?b ?a) (* ?a ?a)) ==> (/ (* ?a ?b) (/ ?a ?a))",
            "(- (* ?b ?b) (* ?a ?a)) ==> (* (- ?b ?a) (+ ?a ?b))",
            "(- ?a (+ ?b ?a)) ==> (/ (+ ?b ?b) -2)",
            "(- (+ ?a ?b) ?a) ==> (/ (+ ?b ?b) 2)",
            "(* (- ?b ?a) -2) ==> (- (+ ?a ?a) (+ ?b ?b))",
            "(- (+ ?b ?b) (+ ?a ?a)) ==> (* (- ?b ?a) 2)",
            "(* (- ?b ?a) 2) ==> (- (+ ?b ?b) (+ ?a ?a))",
            "(/ (- ?b ?a) -2) ==> (/ (- ?a ?b) 2)",
            "(/ 0 (- ?b ?a)) ==> (/ 0 (- ?a ?b))",
            "(* (+ ?a ?b) (/ 0 ?a)) ==> (* ?b (/ 0 ?a))",
            "(* ?b (/ 0 ?a)) ==> (* (+ ?a ?b) (/ 0 ?a))",
            "(* (* ?a ?b) (/ 0 ?a)) ==> (* (fabs ?a) (/ 0 ?a))",
            "(* (/ 0 ?b) (/ 0 ?a)) ==> (* (/ ?b ?a) (/ 0 ?b))",
            "(fabs (fabs ?a)) ==> (fabs ?a)",
            "(fabs ?a) ==> (fabs (fabs ?a))",
            "(fabs (* ?a ?a)) ==> (* ?a ?a)",
            "(* ?a ?a) ==> (fabs (* ?a ?a))",
            "(/ ?a (fabs ?a)) ==> (/ (fabs ?a) ?a)",
            "(/ (fabs ?a) ?a) ==> (/ ?a (fabs ?a))",
            "(/ ?a ?a) ==> (/ (fabs ?a) (fabs ?a))",
            "(/ (fabs ?a) (fabs ?a)) ==> (/ ?a ?a)",
            "(fabs (+ ?a ?a)) ==> (+ (fabs ?a) (fabs ?a))",
            "(+ (fabs ?a) (fabs ?a)) ==> (fabs (+ ?a ?a))",
            "(/ (* ?a ?a) (* ?a ?a)) ==> (/ ?a ?a)",
            "(/ ?a ?a) ==> (/ (* ?a ?a) (* ?a ?a))",
            "(/ ?a ?a) ==> (/ (+ ?a ?a) (+ ?a ?a))",
            "(/ (+ ?a ?a) (+ ?a ?a)) ==> (/ ?a ?a)",
            "(+ ?a (/ 0 ?a)) ==> (/ (* ?a ?a) ?a)",
            "(/ (* ?a ?a) ?a) ==> (+ ?a (/ 0 ?a))",
            "(/ (* ?a ?a) (fabs ?a)) ==> (+ (fabs ?a) (/ 0 ?a))",
            "(+ (fabs ?a) (/ 0 ?a)) ==> (/ (* ?a ?a) (fabs ?a))",
            "(- (/ ?a ?a) ?a) ==> (* (/ ?a ?a) (- 1 ?a))",
            "(* (/ ?a ?a) (- 1 ?a)) ==> (- (/ ?a ?a) ?a)",
            "(* (/ ?a ?a) (- ?a 1)) ==> (- ?a (/ ?a ?a))",
            "(- ?a (/ ?a ?a)) ==> (* (/ ?a ?a) (- ?a 1))",
            "(/ 0 ?a) ==> (/ 0 (fabs ?a))",
            "(/ 0 (fabs ?a)) ==> (/ 0 ?a)",
            "(/ 0 ?a) ==> (/ 0 (+ ?a ?a))",
            "?a ==> (/ ?a (/ ?a ?a))",
            "(/ ?a (/ ?a ?a)) ==> ?a",
            "(- (/ ?a 2) 1) ==> (/ (- ?a 2) 2)",
            "(/ (- ?a 2) 2) ==> (- (/ ?a 2) 1)",
            "(- 1 (/ ?a 2)) ==> (/ (- 2 ?a) 2)",
            "(/ (- 2 ?a) 2) ==> (- 1 (/ ?a 2))",
            "(/ (+ 2 ?a) 2) ==> (+ 1 (/ ?a 2))",
            "(+ 1 (/ ?a 2)) ==> (/ (+ 2 ?a) 2)",
            "(fabs (- (fabs ?a) (/ ?a 2))) ==> (- (fabs ?a) (/ ?a 2))",
            "(- (fabs ?a) (/ ?a 2)) ==> (fabs (- (fabs ?a) (/ ?a 2)))",
            "(+ (fabs ?a) (- 1 ?a)) ==> (fabs (+ (fabs ?a) (- 1 ?a)))",
            "(fabs (+ (fabs ?a) (- 1 ?a))) ==> (+ (fabs ?a) (- 1 ?a))",
            "(fabs (+ (fabs ?a) (/ ?a 2))) ==> (+ (fabs ?a) (/ ?a 2))",
            "(+ (fabs ?a) (/ ?a 2)) ==> (fabs (+ (fabs ?a) (/ ?a 2)))",
            "(* (fabs ?a) (/ 0 ?a)) ==> (/ 0 (/ ?a ?a))",
            "(/ 0 (/ ?a ?a)) ==> (* (fabs ?a) (/ 0 ?a))",
            "(/ 0 (/ ?a ?a)) ==> (fabs (/ 0 (/ ?a ?a)))",
            "(fabs (/ 0 (/ ?a ?a))) ==> (/ 0 (/ ?a ?a))",
            "(/ (fabs ?a) 4) ==> (fabs (/ (/ ?a 2) 2))",
            "(fabs (/ (/ ?a 2) 2)) ==> (/ (fabs ?a) 4)",
            "(/ (fabs ?a) 4) ==> (fabs (/ (fabs ?a) 4))",
            "(fabs (/ (fabs ?a) 4)) ==> (/ (fabs ?a) 4)",
            "(/ (fabs ?a) 3) ==> (fabs (/ (fabs ?a) 3))",
            "(fabs (/ (fabs ?a) 3)) ==> (/ (fabs ?a) 3)",
            "(fabs (- (fabs ?a) (/ 0 ?a))) ==> (fabs (/ (* ?a ?a) (fabs ?a)))",
            "(fabs (/ (* ?a ?a) (fabs ?a))) ==> (fabs (- (fabs ?a) (/ 0 ?a)))",
            "(/ (+ ?a ?a) (fabs ?a)) ==> (/ (fabs ?a) (/ ?a 2))",
            "(/ (fabs ?a) (/ ?a 2)) ==> (/ (+ ?a ?a) (fabs ?a))",
            "(/ (fabs ?a) (+ ?a ?a)) ==> (/ (/ ?a 2) (fabs ?a))",
            "(/ (/ ?a 2) (fabs ?a)) ==> (/ (fabs ?a) (+ ?a ?a))",
            "(- (fabs ?a) ?a) ==> (fabs (- (fabs ?a) ?a))",
            "(fabs (- (fabs ?a) ?a)) ==> (- (fabs ?a) ?a)",
            "(fabs (+ ?a (fabs ?a))) ==> (+ ?a (fabs ?a))",
            "(+ ?a (fabs ?a)) ==> (fabs (+ ?a (fabs ?a)))",
            "(fabs (* (fabs ?b) (/ ?a 2))) ==> (fabs (/ (* ?a ?b) 2))",
            "(+ (fabs ?b) (fabs ?a)) ==> (fabs (+ (fabs ?b) (fabs ?a)))",
            "(fabs (+ (fabs ?b) (fabs ?a))) ==> (+ (fabs ?b) (fabs ?a))",
            "(fabs (/ ?b (fabs ?a))) ==> (fabs (/ ?b ?a))",
            "(fabs (/ ?b ?a)) ==> (fabs (/ ?b (fabs ?a)))",
            "(fabs (/ (fabs ?c) (- ?b ?a))) ==> (fabs (/ (fabs ?c) (- ?a ?b)))",
            "(fabs (* (fabs ?c) (* ?b ?a))) ==> (fabs (* (fabs ?a) (* ?b ?c)))",
            "(- (fabs ?c) (- ?b ?a)) ==> (+ (fabs ?c) (- ?a ?b))",
        ])
    }

    pub fn run_workload(terms: Workload, prev_rules: &Ruleset) -> Ruleset {
        let rules = Exponential::run_workload(
            terms,
            prev_rules.clone(),
            Limits {
                iter: 3,
                node: 2_000_000,
            },
        );
        rules
    }

    #[test]
    fn run() {
        // let herbie: Ruleset = Ruleset::from_file("baseline/herbie-rational.rules");

        // let start = Instant::now();
        let rules = make_rules();
        // let duration = start.elapsed();

        rules.write_json_rules("exponential.json");
        /*
        rules.write_output(
            herbie,
            "exponential",
            "herbie",
            "herbie.json",
            Limits {
                iter: 2,
                node: 150000,
            },
            duration,
        )
        */
    }
}
