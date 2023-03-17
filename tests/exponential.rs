/*!
    Exponential functions from arithmetic
!*/

use std::time::Instant;

use num::rational::Ratio;
use num::BigInt;
use ruler::enumo::{Ruleset, Workload};
use ruler::*;

mod rational;

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

    fn starting_exponential_rules() -> Ruleset {
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

    fn rational_rules() -> Ruleset {
        let rules = rational::test::rational_rules();
        let rule_strs = rules.to_str_vec();
        let rule_strs: Vec<&str> = rule_strs.iter().map(|x| &**x).collect();
        Ruleset::new(&rule_strs)
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
        let terms = Workload::new(vec![
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
        let lower_layer = Workload::new(vec!["v", "(uop v)", "(bop v v)"])
            .plug("v", &Workload::new(vec!["a", "b", "c"]))
            .plug("uop", &Workload::new(vec!["exp"]))
            .plug("bup", &Workload::new(vec!["+", "*"]));

        let upper_layer = Workload::new(vec!["(uop v)", "(bop v v)"])
            .plug("v", &lower_layer)
            .plug("uop", &Workload::new(vec!["exp"]))
            .plug("bop", &Workload::new(vec!["+", "*"]))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(exp (exp ?a))".parse().unwrap(),
            ))))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(log (log ?a))".parse().unwrap(),
            ))));

        run_workload(upper_layer, prev_rules)
    }

    fn log_rules(prev_rules: &Ruleset) -> Ruleset {
        let lower_layer = Workload::new(vec!["v", "(uop v)", "(bop v v)"])
            .plug("v", &Workload::new(vec!["a", "b", "c"]))
            .plug("uop", &Workload::new(vec!["log"]))
            .plug("bup", &Workload::new(vec!["*"]));

        let upper_layer = Workload::new(vec!["(uop v)", "(bop v v)"])
            .plug("v", &lower_layer)
            .plug("uop", &Workload::new(vec!["log"]))
            .plug("bop", &Workload::new(vec!["+"]))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(exp (exp ?a))".parse().unwrap(),
            ))))
            .filter(Filter::Invert(Box::new(Filter::Contains(
                "(log (log ?a))".parse().unwrap(),
            ))));

        run_workload(upper_layer, prev_rules)
    }

    fn no_pow_rules(prev_rules: &Ruleset) -> Ruleset {
        let vars = Workload::new(vec!["a", "b", "c"]);
        let uops = Workload::new(vec!["exp", "log", "sqrt", "cbrt"]);
        let bops = Workload::new(vec!["+", "*"]);
        let lang = Workload::new(vec!["v", "(uop v)", "(bop v v)"]);

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
        let vars = Workload::new(vec!["a", "b", "c"]);
        let uops = Workload::new(vec!["exp", "log", "sqrt", "cbrt"]);
        let bops = Workload::new(vec!["+", "*", "pow"]);
        let lang = Workload::new(vec!["v", "(uop v)", "(bop v v)"]);

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
        let vars = Workload::new(vec!["a", "b"]);
        let uops = Workload::new(vec!["sqrt", "cbrt"]);
        let bops = Workload::new(vec!["/"]);
        let lang = Workload::new(vec!["v", "(uop v)", "(bop v v)"]);

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

    // #[test]
    fn make_rules() {
        let mut all_rules = rational_rules();
        let mut new_rules = Ruleset::default();

        all_rules.extend(starting_exponential_rules());
        new_rules.extend(starting_exponential_rules());

        // Constant layer
        let const_rules = constant_rules(&all_rules);
        all_rules.extend(const_rules.clone());
        new_rules.extend(const_rules);

        // Exponential layer
        let exp_rules = exp_rules(&all_rules);
        all_rules.extend(exp_rules.clone());
        new_rules.extend(exp_rules);

        // Logarithm layer
        let log_rules = log_rules(&all_rules);
        all_rules.extend(log_rules.clone());
        new_rules.extend(log_rules);

        // No `pow` layer
        let no_pow_rules = no_pow_rules(&all_rules);
        all_rules.extend(no_pow_rules.clone());
        new_rules.extend(no_pow_rules);

        // Simple layer
        let simple_rules = simple_rules(&all_rules);
        all_rules.extend(simple_rules.clone());
        new_rules.extend(simple_rules);

        // div layer
        let div_rules = div_rules(&all_rules);
        all_rules.extend(div_rules.clone());
        new_rules.extend(div_rules);

        // only upload new rules
        new_rules.write_json_rules("exponential.json");
    }
}
