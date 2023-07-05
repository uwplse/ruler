use super::*;
use crate::test::{rational_rules, starting_exponential_rules};
use ruler::{enumo, recipe_utils::run_rule_lifting};

type Workload = enumo::Workload;
type Ruleset = enumo::Ruleset<Exponential>;
type Filter = enumo::Filter;

macro_rules! str_vec {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
}

fn limits() -> Limits {
    Limits {
        iter: 3,
        node: 2_000_000,
        match_: 200_000,
    }
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

    run_rule_lifting(terms, prev_rules.clone(), limits(), limits())
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

    run_rule_lifting(upper_layer, prev_rules.clone(), limits(), limits())
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

    run_rule_lifting(upper_layer, prev_rules.clone(), limits(), limits())
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

    run_rule_lifting(upper_layer, prev_rules.clone(), limits(), limits())
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

    run_rule_lifting(upper_layer, prev_rules.clone(), limits(), limits())
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

    run_rule_lifting(upper_layer, prev_rules.clone(), limits(), limits())
}

pub fn make_rules() -> Ruleset {
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
    new_rules
}
