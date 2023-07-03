use super::*;
use crate::test::prior_rules;
use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::run_rule_lifting,
    Limits,
};

pub fn trig_rules() -> Ruleset<Trig> {
    let limits = Limits {
        iter: 3,
        node: 2000000,
        match_: 200_000,
    };
    let mut prior: Ruleset<Trig> = Ruleset::from_file("scripts/oopsla21/trig/complex.rules");
    prior.extend(prior_rules());

    let no_trig_2x = Filter::Invert(Box::new(Filter::Or(vec![
        Filter::Contains("(sin (+ ?a ?a))".parse().unwrap()),
        Filter::Contains("(cos (+ ?a ?a))".parse().unwrap()),
        Filter::Contains("(tan (+ ?a ?a))".parse().unwrap()),
    ])));
    let valid_trig = Filter::Invert(Box::new(Filter::Contains(
        "(tan (/ PI 2))".parse().unwrap(),
    )));

    let t_ops = Workload::new(["sin", "cos", "tan"]);
    let consts = Workload::new([
        "0", "(/ PI 6)", "(/ PI 4)", "(/ PI 3)", "(/ PI 2)", "PI", "(* PI 2)",
    ]);
    let app = Workload::new(["(op v)"]);
    let trig_constants = app
        .clone()
        .plug("op", &t_ops)
        .plug("v", &consts)
        .filter(valid_trig);

    let simple_terms = app.clone().plug("op", &t_ops).plug(
        "v",
        &Workload::new(["a", "(~ a)", "(+ PI a)", "(- PI a)", "(+ a a)"]),
    );

    let neg_terms = Workload::new(["(~ x)"]).plug("x", &simple_terms);

    let squares = Workload::new(["(sqr x)"])
        .plug("x", &app)
        .plug("op", &t_ops)
        .plug("v", &Workload::new(["a", "b"]));

    let add = Workload::new(["(+ e e)", "(- e e)"]);

    let sum_of_squares = add.plug("e", &squares);

    let mut all = prior.clone();
    let mut new = Ruleset::<Trig>::default();

    let wkld1 = trig_constants;
    println!("Starting 1");
    let rules1 = run_rule_lifting(wkld1.clone(), all.clone(), limits, limits);
    all.extend(rules1.clone());
    new.extend(rules1.clone());

    let wkld2 = Workload::Append(vec![wkld1, simple_terms, neg_terms]);
    println!("Starting 2");
    let rules2 = run_rule_lifting(wkld2.clone(), all.clone(), limits, limits);
    all.extend(rules2.clone());
    new.extend(rules2.clone());

    let trimmed_wkld2 = wkld2.clone().filter(no_trig_2x);
    let wkld3 = Workload::Append(vec![trimmed_wkld2.clone(), sum_of_squares.clone()]);
    println!("Starting 3");
    let rules3 = run_rule_lifting(wkld3, all.clone(), limits, limits);
    all.extend(rules3.clone());
    new.extend(rules3.clone());

    let non_square_filter = Filter::Invert(Box::new(Filter::Or(vec![
        Filter::Contains("(* (cos ?x) (cos ?x))".parse().unwrap()),
        Filter::Contains("(* (sin ?x) (sin ?x))".parse().unwrap()),
    ])));

    let two_x_filter = Filter::Invert(Box::new(Filter::Contains("(+ ?x ?x)".parse().unwrap())));

    let trivial_trig_filter = Filter::Invert(Box::new(Filter::Or(vec![
        Filter::Contains("(cos (?op ?a ?b))".parse().unwrap()),
        Filter::Contains("(sin (?op ?a ?b))".parse().unwrap()),
    ])));

    let trig_no_sub_filter = Filter::Invert(Box::new(Filter::Or(vec![
        Filter::Contains("(cos (- ?a ?b))".parse().unwrap()),
        Filter::Contains("(sin (- ?a ?b))".parse().unwrap()),
    ])));

    let t_ops = Workload::new(["sin", "cos"]);
    let app = Workload::new(["(op v)"]);
    let shift = Workload::new(["x", "(- 1 x)", "(+ 1 x)"]);
    let scale_down = Workload::new(["x", "(/ x 2)"]);
    let consts = Workload::new(["-2", "-1", "0", "1", "2"]);

    let simple = app.clone().plug("op", &t_ops).plug(
        "v",
        &Workload::new(["a", "(- (/ PI 2) a)", "(+ (/ PI 2) a)", "(* 2 a)"]),
    );

    let trivial_squares = Workload::new(["(sqr x)"])
        .plug("x", &app)
        .plug("op", &t_ops)
        .plug("v", &Workload::new(["a"]));

    let two_var = app
        .clone()
        .plug("op", &t_ops)
        .plug("v", &Workload::new(["a", "b", "(+ a b)", "(- a b)"]));
    let sum_two_vars = Workload::new(["(+ x y)", "(- x y)"])
        .plug("x", &two_var)
        .plug("y", &two_var);
    let prod_two_vars = Workload::new(["(* x y)"])
        .plug("x", &two_var)
        .plug("y", &two_var)
        .filter(non_square_filter);

    let sum_of_prod = Workload::new(["(+ x y)", "(- x y)"])
        .plug("x", &prod_two_vars)
        .plug("y", &prod_two_vars)
        .filter(two_x_filter)
        .filter(trivial_trig_filter);

    let shifted_simple = shift.clone().plug("x", &simple);
    let sum_and_prod = Workload::Append(vec![sum_two_vars.clone(), prod_two_vars.clone()]);
    let shifted_simple_sqrs = Workload::Append(vec![shifted_simple, trivial_squares]);
    let scaled_shifted_sqrs = scale_down.clone().plug("x", &shifted_simple_sqrs);

    let scaled_sum_prod = scale_down.clone().plug("x", &sum_and_prod);

    let two_var_no_sub = two_var.clone().filter(trig_no_sub_filter);

    // Coangles
    let wkld1 = Workload::Append(vec![simple, consts.clone()]);
    let rules1 = run_rule_lifting(wkld1.clone(), all.clone(), limits, limits);
    all.extend(rules1.clone());
    new.extend(rules1.clone());

    // Power reduction
    let wkld2 = Workload::Append(vec![scaled_shifted_sqrs, consts.clone()]);
    let rules2 = run_rule_lifting(wkld2.clone(), all.clone(), limits, limits);
    all.extend(rules2.clone());
    new.extend(rules2.clone());

    // Product-to-sum
    let wkld3 = Workload::Append(vec![scaled_sum_prod, consts.clone()]);
    let rules3 = run_rule_lifting(wkld3.clone(), all.clone(), limits, limits);
    all.extend(rules3.clone());
    new.extend(rules3.clone());

    // Sums
    let wkld4 = Workload::Append(vec![two_var_no_sub, sum_of_prod, consts.clone()]);
    let rules4 = run_rule_lifting(wkld4.clone(), all.clone(), limits, limits);
    all.extend(rules4.clone());
    new.extend(rules4.clone());
    new
}
