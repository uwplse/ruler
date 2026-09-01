/*!
    Exponential functions from arithmetic
!*/

use std::time::Instant;

use num::rational::Ratio;
use num::BigInt;
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
        // alias for pow, so that externally generated terms parse
        "^" = Pow2([Id; 2]),
        "sqrt" = Sqrt(Id),
        "cbrt" = Cbrt(Id),

        // arithmetic operators
        "-" = Neg(Id),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "if" = If([Id; 3]),
        // (for compatibility with rationals)
        "fabs" = Abs(Id),
        // alias for fabs, so that externally generated terms parse
        "abs" = Abs2(Id),

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

    fn is_fast_forwarding() -> bool {
        true
    }

    fn get_exploratory_rules() -> enumo::Ruleset<Self> {
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::exponential::make_rules;
    use ruler::enumo;

    type Ruleset = enumo::Ruleset<Exponential>;

    #[test]
    fn establish_baseline() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }
        let log = ruler::logger::RunLog::start("jfp/baseline/exp", "establish_baseline");

        let start = Instant::now();
        let rules = make_rules();
        log.line(&format!(
            "ENUMO EXP | {} rules | {:.1?}",
            rules.len(),
            start.elapsed()
        ));
        rules.to_file("jfp/baseline/exp/enumo_exp.rules");
        log.finish();
    }

    #[test]
    fn herbie_baseline_parses() {
        let herbie: Ruleset = Ruleset::from_file("baseline/herbie-exp.rules");
        assert_eq!(herbie.len(), 82);
    }

    #[test]
    fn syntax_aliases() {
        // Bridge rules connecting the alias operators to the canonical
        // ones. Ruleset::new panics on malformed rules, so this also
        // guards the rules' arities (both aliases are used unapplied
        // elsewhere, e.g. in case-study start rules).
        let bridge: Ruleset =
            enumo::Ruleset::new(&["(pow ?x ?y) <=> (^ ?x ?y)", "(abs ?x) <=> (fabs ?x)"]);
        assert_eq!(bridge.len(), 4);

        // Alias terms parse in the language
        let wkld = enumo::Workload::new(["(^ a b)", "(abs a)", "(pow a b)", "(fabs a)"]);
        assert_eq!(wkld.as_lang::<Exponential>().force().len(), 4);
    }

    /// Trusted rules for validating LLM candidates by derivation. The
    /// syntax rules connect the `^`/`abs` aliases to `pow`/`fabs`.
    fn start_rules() -> Ruleset {
        let syntax_rules = Ruleset::new(["(pow ?x ?y) <=> (^ ?x ?y)", "(abs ?x) <=> (fabs ?x)"]);
        let mut rules = Exponential::get_exploratory_rules();
        rules.extend(rational_rules());
        rules.extend(starting_exponential_rules());
        rules.extend(syntax_rules);
        rules
    }

    #[tokio::test]
    async fn case_study1() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }
        dotenv::dotenv().ok();
        // Skip (rather than fail) when no API key is configured locally
        if std::env::var("OPENROUTER_API_KEY").is_err() {
            eprintln!("Skipping case_study1: OPENROUTER_API_KEY not set");
            return;
        }
        assert!(
            std::path::Path::new("jfp/baseline/exp/enumo_exp.rules").exists(),
            "missing jfp/baseline/exp/enumo_exp.rules: run establish_baseline first"
        );

        let dir = "jfp/cs1/exp";
        let log = ruler::logger::RunLog::start(dir, "case_study1");
        let rational = rational_rules();
        let herbie: Ruleset = Ruleset::from_file("baseline/herbie-exp.rules");
        let enumo_baseline: Ruleset = Ruleset::from_file("jfp/baseline/exp/enumo_exp.rules");
        let start = start_rules();

        let prompt = "
        You are generating rewrite rules for an equality saturation system.
        The domain is exponential and logarithmic functions, as follows:
        Values: real numbers
        Unary operators: - (negation), exp, log, sqrt, cbrt
        Binary operators: +, - (subtraction), *, /, pow

        Terms must be written using s-expressions and prefix notation.
        For example, (a + b) is not a valid term, but (+ a b) is a valid term.
        Every operator takes exactly the number of operands stated above: (+ 1 2 3) is not a valid term, but (+ 1 (+ 2 3)) is.
        Variables are ?x, ?y, and ?z.
        Do not use any operators or syntax not listed here.
        Do not use imaginary numbers.

        A rewrite rule has the form `l ==> r` where `l` and `r` are terms that are equal for ALL real values of the variables wherever both sides are defined (log and sqrt are undefined for negative arguments). For example:
        (log (exp ?x)) ==> ?x
        (exp (+ ?x ?y)) ==> (* (exp ?x) (exp ?y))

        Good rewrite rules already exist for pure arithmetic (unary negation, +, -, *, and /), so every rule you generate must involve exp, log, sqrt, cbrt, or pow. Cover at least the following categories:
        - special values (e.g. exp of 0, log of 1)
        - exp and log as inverses of each other
        - the addition and subtraction laws for exp
        - the product, quotient, and power laws for log
        - laws for pow (exponents 0 and 1, sums, products, and negations of exponents)
        - pow expressed using exp and log
        - relationships among sqrt, cbrt, pow, and squaring
        - distributing sqrt and cbrt over products and quotients

        Be careful about soundness over the real numbers: only generate a rule if both sides agree wherever they are defined (beware of rules that hold only for positive arguments).
        Print only the rules, one rule per line, in the exact `l ==> r` syntax shown above.
        Plain text only - no markdown, no code fences, no numbering, no extra commentary.
        ";
        let t = Instant::now();
        let candidates = Ruleset::from_llm(prompt, &log, "LLM-1").await;
        log.line(&format!(
            "LLM-1: {} candidates | {:.1?}",
            candidates.len(),
            t.elapsed()
        ));
        candidates.to_file(&format!("{dir}/LLM-1-candidates.rules"));

        // Validate the candidates by derivation from the start rules
        let t = Instant::now();
        let (mut sound, unverified) = start.derive_all(
            &candidates,
            enumo::Scheduler::Saturating(Limits::deriving()),
        );
        log.line(&format!(
            "LLM-1: {} sound / {} unverified of {} candidates | {:.1?}",
            sound.len(),
            unverified.len(),
            candidates.len(),
            t.elapsed()
        ));
        sound.to_file(&format!("{dir}/LLM-1-sound.rules"));

        // Minimize the sound rules against the rational rules
        let t = Instant::now();
        let (llm1, _) = sound.minimize(
            rational.clone(),
            enumo::Scheduler::Compress(Limits::minimize()),
        );
        log.line(&format!(
            "LLM-1: {} minimized | {:.1?}",
            llm1.len(),
            t.elapsed()
        ));
        llm1.to_file(&format!("{dir}/LLM-1.rules"));

        log.derivability(
            &llm1.union(&rational),
            "LLM-1-RAT",
            &enumo_baseline,
            "Enumo",
        );
        log.derivability(&llm1.union(&rational), "LLM-1-RAT", &herbie, "Herbie");
        log.derivability(&enumo_baseline.union(&rational), "Enumo", &llm1, "LLM-1");

        let reprompt = format!("
        You are generating rewrite rules for an equality saturation system.
        The domain is exponential and logarithmic functions, as follows:
        Values: real numbers
        Unary operators: - (negation), exp, log, sqrt, cbrt
        Binary operators: +, - (subtraction), *, /, pow

        The following rewrite rules are already in the ruleset:
        {}

        Identify sound rewrite rules for this domain that are missing from the ruleset above, and print them.
        Do not repeat rules from the list above, and do not print trivial variants of them (e.g. renamed variables or swapped arguments of commutative operators).
        Every rule must involve exp, log, sqrt, cbrt, or pow; rules for pure arithmetic already exist.
        Terms are s-expressions in prefix notation; variables are ?x, ?y, and ?z.
        A rewrite rule has the form `l ==> r` where `l` and `r` are terms that are equal for ALL real values of the variables wherever both sides are defined. For example: (log (exp ?x)) ==> ?x
        If no rules are missing, print nothing.
        Print only the rules, one rule per line.
        Plain text only - no markdown, no code fences, no numbering, no extra commentary.
        ",
            llm1.to_str_vec().join("\n")
        );
        let t = Instant::now();
        let reprompted = Ruleset::from_llm(&reprompt, &log, "LLM-2").await;
        log.line(&format!(
            "LLM-2: {} candidates (reprompted) | {:.1?}",
            reprompted.len(),
            t.elapsed()
        ));
        reprompted.to_file(&format!("{dir}/LLM-2-candidates.rules"));

        // Validate the reprompted candidates by derivation from the
        // start rules
        let t = Instant::now();
        let (mut sound2, unverified2) = start.derive_all(
            &reprompted,
            enumo::Scheduler::Saturating(Limits::deriving()),
        );
        log.line(&format!(
            "LLM-2: {} sound / {} unverified of {} candidates | {:.1?}",
            sound2.len(),
            unverified2.len(),
            reprompted.len(),
            t.elapsed()
        ));
        sound2.to_file(&format!("{dir}/LLM-2-sound.rules"));

        // Minimize against everything already selected
        let t = Instant::now();
        let (min2, _) = sound2.minimize(
            rational.union(&llm1),
            enumo::Scheduler::Compress(Limits::minimize()),
        );
        log.line(&format!(
            "LLM-2: {} minimized | {:.1?}",
            min2.len(),
            t.elapsed()
        ));
        let llm2 = llm1.union(&min2);
        llm2.to_file(&format!("{dir}/LLM-2.rules"));

        log.derivability(
            &llm2.union(&rational),
            "LLM-2-RAT",
            &enumo_baseline,
            "Enumo",
        );
        log.derivability(&llm2.union(&rational), "LLM-2-RAT", &herbie, "Herbie");
        log.derivability(&enumo_baseline.union(&rational), "Enumo", &llm2, "LLM-2");

        log.finish();
    }

    pub fn starting_exponential_rules() -> Ruleset {
        Ruleset::new(&[
            // exponential properties (expand)
            "(exp (+ ?a ?b)) ==> (* (exp ?a) (exp ?b))",
            "(exp (- ?a)) ==> (/ 1 (exp ?a))",
            // exponential properties (simplify)
            "(* (exp ?a) (exp ?b)) ==> (exp (+ ?a ?b))",
            "(/ 1 (exp ?a)) ==> (exp (- ?a))",
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
            "(/ ?a -1) ==> (- ?a)",
            "(- ?a) ==> (/ ?a -1)",
            "(- 0 ?a) ==> (- ?a)",
            "(- ?a) ==> (- 0 ?a)",
            "(* ?a -1) ==> (- ?a)",
            "(- ?a) ==> (* ?a -1)",
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
            "(- (/ ?a ?a) (/ 0 ?a)) ==> (* (- ?a) (/ -1 ?a))",
            "(* (- ?a) (/ -1 ?a)) ==> (- (/ ?a ?a) (/ 0 ?a))",
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

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let herbie: Ruleset = Ruleset::from_file("baseline/herbie-exp.rules");

        let start = Instant::now();
        let rules = make_rules();
        let duration = start.elapsed();

        logger::write_baseline(&rules, "exponential", &herbie, "herbie", duration);
    }
}
