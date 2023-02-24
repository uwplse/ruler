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
        ])
    }

    fn is_allowed_op(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ruler::enumo;

    type Workload = enumo::Workload;
    type Ruleset = enumo::Ruleset<Exponential>;
    type Filter = enumo::Filter;

    macro_rules! str_vec {
        ($($x:expr),*) => (vec![$($x.to_string()),*]);
    }

    // TODO: actually derive these rules
    fn rational_rules() -> Ruleset {
        Ruleset::from_str_vec(&[
            "(* ?c (* ?b ?a)) ==> (* ?b (* ?a ?c))",
            "(* ?b (* ?a ?c)) ==> (* ?c (* ?b ?a))",
            "(+ ?c (+ ?b ?a)) ==> (+ ?a (+ ?b ?c))",
            "(/ (/ ?c ?b) ?a) ==> (/ (/ ?c ?a) ?b)",
            "(- (- ?c ?b) ?a) ==> (- (- ?c ?a) ?b)",
            "(/ ?c (/ ?b ?a)) ==> (/ ?a (/ ?b ?c))",
            "(/ ?c (* ?b ?a)) ==> (/ (/ ?c ?a) ?b)",
            "(/ (/ ?c ?a) ?b) ==> (/ ?c (* ?b ?a))",
            "(- ?c (+ ?b ?a)) ==> (- (- ?c ?a) ?b)",
            "(- (- ?c ?a) ?b) ==> (- ?c (+ ?b ?a))",
            "(/ (* ?c ?b) ?a) ==> (* ?c (/ ?b ?a))",
            "(* ?c (/ ?b ?a)) ==> (/ (* ?c ?b) ?a)",
            "(/ ?c (/ ?b ?a)) ==> (* ?c (/ ?a ?b))",
            "(* ?c (/ ?a ?b)) ==> (/ ?c (/ ?b ?a))",
            "(/ ?c (- ?b (- ?b ?a))) ==> (/ ?c ?a)",
            "(/ (- ?c (- ?c ?b)) ?a) ==> (/ ?b ?a)",
            "(+ (/ ?c ?a) (/ ?b ?a)) ==> (/ (+ ?c ?b) ?a)",
            "(/ (+ ?c ?b) ?a) ==> (+ (/ ?c ?a) (/ ?b ?a))",
            "(* ?a (- ?c ?b)) ==> (- (* ?a ?c) (* ?b ?a))",
            "(- (* ?a ?c) (* ?b ?a)) ==> (* ?a (- ?c ?b))",
            "(- (/ ?c ?a) (/ ?b ?a)) ==> (/ (- ?c ?b) ?a)",
            "(/ (- ?c ?b) ?a) ==> (- (/ ?c ?a) (/ ?b ?a))",
            "(* ?b (+ ?c ?a)) ==> (+ (* ?c ?b) (* ?b ?a))",
            "(+ (* ?c ?b) (* ?b ?a)) ==> (* ?b (+ ?c ?a))",
            "(/ (+ ?c (- ?a ?b)) ?a) ==> (/ (+ ?a (- ?c ?b)) ?a)",
            "(/ (+ ?a (- ?c ?b)) ?a) ==> (/ (+ ?c (- ?a ?b)) ?a)",
            "(/ (+ ?c (- 2 ?b)) ?a) ==> (/ (+ 2 (- ?c ?b)) ?a)",
            "(/ (+ 2 (- ?c ?b)) ?a) ==> (/ (+ ?c (- 2 ?b)) ?a)",
            "(+ ?b ?a) ==> (+ ?a ?b)",
            "(* ?b ?a) ==> (* ?a ?b)",
            "(- ?a ?b) ==> (~ (- ?b ?a))",
            "(~ (- ?b ?a)) ==> (- ?a ?b)",
            "(+ ?b (~ ?a)) ==> (- ?b ?a)",
            "(- ?b ?a) ==> (+ ?b (~ ?a))",
            "(* ?b (~ ?a)) ==> (~ (* ?b ?a))",
            "(~ (* ?b ?a)) ==> (* ?b (~ ?a))",
            "(/ (~ ?b) ?a) ==> (/ ?b (~ ?a))",
            "(/ ?b (~ ?a)) ==> (/ (~ ?b) ?a)",
            "(/ ?b (~ ?a)) ==> (~ (/ ?b ?a))",
            "(~ (/ ?b ?a)) ==> (/ ?b (~ ?a))",
            "(* ?a (/ ?b ?a)) ==> ?b",
            "(* (- ?b ?a) (+ ?a ?b)) ==> (- (* ?b ?b) (* ?a ?a))",
            "(- (* ?b ?b) (* ?a ?a)) ==> (* (- ?b ?a) (+ ?a ?b))",
            "(/ (- ?a ?b) ?a) ==> (- 1 (/ ?b ?a))",
            "(- 1 (/ ?b ?a)) ==> (/ (- ?a ?b) ?a)",
            "(/ (+ ?a ?b) ?a) ==> (+ 1 (/ ?b ?a))",
            "(+ 1 (/ ?b ?a)) ==> (/ (+ ?a ?b) ?a)",
            "(- ?b (* ?b ?a)) ==> (* ?b (- 1 ?a))",
            "(* ?b (- 1 ?a)) ==> (- ?b (* ?b ?a))",
            "(+ ?a (* ?b ?a)) ==> (* ?a (+ ?b 1))",
            "(* ?a (+ ?b 1)) ==> (+ ?a (* ?b ?a))",
            "(/ ?b (+ ?b (/ ?b ?a))) ==> (/ ?a (+ ?a 1))",
            "(/ ?a (- (/ ?a ?b) ?a)) ==> (/ ?b (- 1 ?b))",
            "(/ ?a (- (* ?a ?b) ?a)) ==> (/ 1 (+ ?b -1))",
            "?a ==> (~ (~ ?a))",
            "(~ (~ ?a)) ==> ?a",
            "?a ==> (- ?a 0)",
            "(- ?a 0) ==> ?a",
            "?a ==> (* ?a 1)",
            "(* ?a 1) ==> ?a",
            "?a ==> (/ ?a 1)",
            "(/ ?a 1) ==> ?a",
            "?a ==> (+ ?a 0)",
            "(+ ?a 0) ==> ?a",
            "(- ?a ?a) ==> 0",
            "(/ ?a ?a) ==> 1",
            "(~ ?a) ==> (- 0 ?a)",
            "(- 0 ?a) ==> (~ ?a)",
            "(~ ?a) ==> (/ ?a -1)",
            "(/ ?a -1) ==> (~ ?a)",
            "(~ ?a) ==> (* ?a -1)",
            "(* ?a -1) ==> (~ ?a)",
            "(+ ?a ?a) ==> (* ?a 2)",
            "(* ?a 2) ==> (+ ?a ?a)",
            "(/ 0 ?a) ==> 0",
            "(* ?a 0) ==> 0",
            "(- ?a 1) ==> (+ ?a -1)",
            "(+ ?a -1) ==> (- ?a 1)",
            "(- ?a -1) ==> (+ ?a 1)",
            "(+ ?a 1) ==> (- ?a -1)",
        ])
    }

    fn run_workload(terms: Workload, prev_rules: &Ruleset) -> Ruleset {
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

    fn constant_rules(prev_rules: &Ruleset) -> Ruleset {
        let terms = Workload::from_vec(vec![
            "(exp 0)",
            "(exp 1)",
            "(log 1)",
            "(sqrt 1)",
            "(cbrt 1)",
            "(pow a 1)",
            "(pow 1 a)",
        ]);

        run_workload(terms, prev_rules)
    }

    fn exp_rules(prev_rules: &Ruleset) -> Ruleset {
        let lower_layer = Workload::from_vec(vec!["v", "(uop v)", "(bop v v)"])
            .plug("v", &Workload::from_vec(vec!["a", "b", "c"]))
            .plug("uop", &Workload::from_vec(vec!["exp"]))
            .plug("bup", &Workload::from_vec(vec!["+", "*"]));

        let upper_layer = Workload::from_vec(vec!["(uop v)", "(bop v v)"])
            .plug("v", &lower_layer)
            .plug("uop", &Workload::from_vec(vec!["exp"]))
            .plug("bop", &Workload::from_vec(vec!["+", "*"]))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(exp (exp ?a))".parse().unwrap(),
            ))))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(log (log ?a))".parse().unwrap(),
            ))));

        run_workload(upper_layer, prev_rules)
    }

    fn log_rules(prev_rules: &Ruleset) -> Ruleset {
        let lower_layer = Workload::from_vec(vec!["v", "(uop v)", "(bop v v)"])
            .plug("v", &Workload::from_vec(vec!["a", "b", "c"]))
            .plug("uop", &Workload::from_vec(vec!["log"]))
            .plug("bup", &Workload::from_vec(vec!["*"]));

        let upper_layer = Workload::from_vec(vec!["(uop v)", "(bop v v)"])
            .plug("v", &lower_layer)
            .plug("uop", &Workload::from_vec(vec!["log"]))
            .plug("bop", &Workload::from_vec(vec!["+"]))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(exp (exp ?a))".parse().unwrap(),
            ))))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(log (log ?a))".parse().unwrap(),
            ))));

        run_workload(upper_layer, prev_rules)
    }

    fn no_pow_rules(prev_rules: &Ruleset) -> Ruleset {
        let vars = Workload::from_vec(vec!["a", "b", "c"]);
        let uops = Workload::from_vec(vec!["exp", "log", "sqrt", "cbrt"]);
        let bops = Workload::from_vec(vec!["+", "*"]);
        let lang = Workload::from_vec(vec!["v", "(uop v)", "(bop v v)"]);

        let lower_layer = lang
            .clone()
            .plug("v", &vars)
            .plug("uop", &uops)
            .plug("bup", &bops);

        let upper_layer = lang
            .plug("v", &Workload::Append(vec![lower_layer, vars]))
            .plug("uop", &uops)
            .plug("bop", &bops)
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(exp (exp ?a))".parse().unwrap(),
            ))))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(log (log ?a))".parse().unwrap(),
            ))));

        run_workload(upper_layer, prev_rules)
    }

    fn simple_rules(prev_rules: &Ruleset) -> Ruleset {
        let vars = Workload::from_vec(vec!["a", "b", "c"]);
        let uops = Workload::from_vec(vec!["exp", "log", "sqrt", "cbrt"]);
        let bops = Workload::from_vec(vec!["+", "*", "pow"]);
        let lang = Workload::from_vec(vec!["v", "(uop v)", "(bop v v)"]);

        let lower_layer = lang
            .clone()
            .plug("v", &vars)
            .plug("uop", &uops)
            .plug("bup", &bops);

        let upper_layer = lang
            .plug("v", &Workload::Append(vec![lower_layer, vars]))
            .plug("uop", &uops)
            .plug("bop", &bops)
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(exp (exp ?a))".parse().unwrap(),
            ))))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(log (log ?a))".parse().unwrap(),
            ))));

        run_workload(upper_layer, prev_rules)
    }

    fn div_rules(prev_rules: &Ruleset) -> Ruleset {
        let vars = Workload::from_vec(vec!["a", "b"]);
        let uops = Workload::from_vec(vec!["sqrt", "cbrt"]);
        let bops = Workload::from_vec(vec!["/"]);
        let lang = Workload::from_vec(vec!["v", "(uop v)", "(bop v v)"]);

        let lower_layer = lang
            .clone()
            .plug("v", &vars)
            .plug("uop", &uops)
            .plug("bup", &bops);

        let upper_layer = lang
            .plug("v", &Workload::Append(vec![lower_layer, vars]))
            .plug("uop", &uops)
            .plug("bop", &bops)
            .filter(Filter::Canon(str_vec!["a", "b"]));

        run_workload(upper_layer, prev_rules)
    }

    #[test]
    fn make_rules() {
        let mut all_rules = rational_rules();

        // Constant layer
        let const_rules = constant_rules(&all_rules);
        all_rules.extend(const_rules);

        // Exponential layer
        let exp_rules = exp_rules(&all_rules);
        all_rules.extend(exp_rules);

        // Logarithm layer
        let log_rules = log_rules(&all_rules);
        all_rules.extend(log_rules);

        // No `pow` layer
        let no_pow_rules = no_pow_rules(&all_rules);
        all_rules.extend(no_pow_rules);

        // Simple layer
        let simple_rules = simple_rules(&all_rules);
        all_rules.extend(simple_rules);

        // div layer
        let div_rules = div_rules(&all_rules);
        all_rules.extend(div_rules);
    }

    #[test]
    fn check_derivability() {
        // a sandbox for derivability tests

        let r1 = Ruleset::from_str_vec(&[
            "1 ==> (sqrt 1)",
            "(exp 0) ==> 1",
            "(log 1) ==> 0",
            "1 ==> (cbrt 1)",
            "(pow 1 ?a) ==> 1",
            "(pow ?a 1) ==> ?a",
            "(exp (+ ?b ?a)) ==> (* (exp ?b) (exp ?a))",
            "(+ ?c (+ (log ?b) (log ?a))) ==> (+ ?c (+ (log ?a) (log ?b)))",
            "(sqrt (cbrt ?a)) ==> (cbrt (sqrt ?a))",
            "(* 1/2 (log (* ?b ?a))) ==> (log (* (sqrt ?b) (sqrt ?a)))",
            "(* 1/3 (log (* ?b ?a))) ==> (log (* (cbrt ?b) (cbrt ?a)))",
            "(+ (log ?b) (log ?a)) ==> (log (* ?b ?a))",
            "(cbrt (* ?b ?a)) ==> (* (cbrt ?b) (cbrt ?a))",
            "(sqrt (* ?b ?a)) ==> (* (sqrt ?b) (sqrt ?a))",
            "(+ (log (* ?c ?b)) (log ?a)) ==> (+ (log ?c) (log (* ?b ?a)))",
            "(+ (log (* ?c ?b)) ?a) ==> (+ ?a (log (* ?c ?b)))",
            "(* (log (* ?b ?b)) (log ?a)) ==> (* (log ?b) (log (* ?a ?a)))",
            "(pow ?b (log (* ?a ?a))) ==> (pow ?a (log (* ?b ?b)))",
            "(* ?b (log (cbrt ?a))) ==> (log (cbrt (pow ?a ?b)))",
            "(pow (exp ?b) (log ?a)) ==> (pow ?a ?b)",
            "(pow (exp ?b) ?a) ==> (exp (* ?b ?a))",
            "(pow ?b (log ?a)) ==> (pow ?a (log ?b))",
            "(pow (cbrt ?b) ?a) ==> (cbrt (pow ?b ?a))",
            "(pow (sqrt ?b) ?a) ==> (sqrt (pow ?b ?a))",
            "(pow ?c (+ ?b ?a)) ==> (* (pow ?c ?a) (pow ?c ?b))",
            "(* (pow ?c ?a) (pow ?b ?a)) ==> (pow (* ?c ?b) ?a)",
            "(pow (exp ?c) (* ?b ?a)) ==> (pow (exp ?a) (* ?b ?c))",
            "(* (log ?c) (* ?b ?a)) ==> (* ?b (log (pow ?c ?a)))",
            "(pow (pow ?c ?b) (log ?a)) ==> (pow (pow ?a ?b) (log ?c))",
            "(pow (pow ?c ?b) ?a) ==> (pow ?c (* ?b ?a))",
            "(pow (pow ?c ?b) ?a) ==> (pow (pow ?c ?a) ?b)",
            "(* 1/2 (log (cbrt ?a))) ==> (* 1/3 (log (sqrt ?a)))",
        ]);

        let r2 = Ruleset::from_str_vec(&[
            "1 ==> (sqrt 1)",
            "1 ==> (cbrt 1)",
            "(pow 1 ?a) ==> 1",
            "(pow ?a 1) ==> ?a",
            "(log (sqrt ?a)) ==> (* 1/2 (log ?a))",
            "(log (cbrt ?a)) ==> (* 1/3 (log ?a))",
            "(sqrt (cbrt ?a)) ==> (cbrt (sqrt ?a))",
            "(cbrt (* ?b ?a)) ==> (* (cbrt ?b) (cbrt ?a))",
            "(* (sqrt ?b) (sqrt ?a)) ==> (sqrt (* ?a ?b))",
            "(* (log (* ?b ?b)) (log ?a)) ==> (* (log ?b) (log (* ?a ?a)))",
            "(pow (sqrt ?b) (log ?a)) ==> (pow (sqrt ?a) (log ?b))",
            "(pow (cbrt ?b) (log ?a)) ==> (pow (cbrt ?a) (log ?b))",
            "(pow (exp ?b) ?a) ==> (exp (* ?b ?a))",
            "(cbrt (pow ?b ?a)) ==> (pow (cbrt ?b) ?a)",
            "(pow (sqrt ?b) ?a) ==> (sqrt (pow ?b ?a))",
            "(* 2 (* (* ?c ?b) (log ?a))) ==> (* (log (* ?a ?a)) (* ?c ?b))",
            "(* (pow ?b ?c) (pow ?b ?a)) ==> (pow ?b (+ ?c ?a))",
            "(pow (* ?c ?b) ?a) ==> (* (pow ?c ?a) (pow ?b ?a))",
            "(pow (exp ?c) (* ?b ?a)) ==> (pow (exp ?a) (* ?b ?c))",
            "(* (log (pow ?c ?b)) ?a) ==> (* ?b (log (pow ?c ?a)))",
            "(pow (pow ?c ?b) (log ?a)) ==> (pow ?a (* ?b (log ?c)))",
            "(pow (pow ?c ?b) ?a) ==> (pow ?c (* ?a ?b))",
            "(pow (pow ?c ?b) ?a) ==> (pow (pow ?c ?a) ?b)",
            "(/ (sqrt ?b) (sqrt ?a)) ==> (sqrt (/ ?b ?a))",
            "(cbrt (/ ?b ?a)) ==> (/ (cbrt ?b) (cbrt ?a))",
        ]);

        println!("Using <current lifting> to derive <definitional lifting>");
        r1.derive(
            r2.clone(),
            Limits {
                iter: 3,
                node: 2_000_000,
            },
        );

        println!("Using <definitional lifting> to derive <current lifting>");
        r2.derive(
            r1.clone(),
            Limits {
                iter: 3,
                node: 2_000_000,
            },
        );
    }
}