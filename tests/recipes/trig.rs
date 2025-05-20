use super::*;
use crate::test::prior_rules;
use ruler::{
    enumo::{Filter, Ruleset, Workload},
    recipe_utils::run_fast_forwarding,
    Limits,
};

/// Terms that apply trigonometric functions to (possibly fractional)
/// multiples of pi.
fn workload_consts() -> Workload {
    let op = Workload::new(["sin", "cos", "tan"]);
    let cnst = Workload::new([
        "0", "(/ PI 6)", "(/ PI 4)", "(/ PI 3)", "(/ PI 2)", "PI", "(* PI 2)",
    ]);

    let invalid = Filter::Contains(
        "(tan (/ PI 2))".parse().unwrap(),
    );

    Workload::new(["(op v)"])
        .plug("op", &op)
        .plug("v", &cnst)
        .filter(Filter::Invert(Box::new(invalid)))
}

/// Terms that apply trigonometric functions to a variable,
/// possibly translated by a negation or ±pi; terms may also
/// be negated.
fn workload_symmetry_periodicity() -> Workload {
    let op = Workload::new(["sin", "cos", "tan"]);
    let var = Workload::new(["a"]);

    let t_shift = Workload::new(["x", "(~ x)", "(+ PI x)", "(- PI x)", "(+ x x)"]).plug("x", &var);
    let t_simpl = Workload::new(["(op t)"]).plug("op", &op).plug("t", &t_shift);
    let t_neg = Workload::new(["(~ t)"]).plug("t", &t_simpl);

    workload_consts().append(t_simpl).append(t_neg)
}

/// Terms to prove identitites that sum-of-squares terms, e.g.,
/// sin(x) * sin(x). This includes the Pythagorean identity.
fn workload_sum_of_squares() -> Workload {
    let op = Workload::new(["sin", "cos", "tan"]);
    let var = Workload::new(["a", "b"]);

    let double_angle = Filter::Or(vec![
        Filter::Contains("(sin (+ ?a ?a))".parse().unwrap()),
        Filter::Contains("(cos (+ ?a ?a))".parse().unwrap()),
        Filter::Contains("(tan (+ ?a ?a))".parse().unwrap()),
    ]);

    let t_simpl = Workload::new(["(op t)"]).plug("op", &op).plug("t", &var);
    let t_sqr = Workload::new(["(* t t)"]).plug("t", &t_simpl);
    let t_sos = Workload::new(["(+ t t)", "(- t t)"]).plug("t", &t_sqr);

    workload_symmetry_periodicity()
        .filter(Filter::Invert(Box::new(double_angle)))
        .append(t_sos)
}

/// Terms to prove co-angle identities, e.g., (cos x) => (sin (- (/ PI 2) x))
fn workload_coangle() -> Workload {
    let op = Workload::new(["sin", "cos", "tan"]);
    let var = Workload::new(["a", "b"]);
    let cnst = Workload::new([
        "0", "(/ PI 6)", "(/ PI 4)", "(/ PI 3)", "(/ PI 2)", "PI", "(* PI 2)",
    ]);

    let t_shift = Workload::new(["x", "(~ x)", "(+ PI x)", "(- PI x)", "(+ x x)"]).plug("x", &var);
    let t_simpl = Workload::new(["(op t)"]).plug("op", &op).plug("t", &t_shift);

    t_simpl.append(cnst)
}

/// Terms to prove power reduction identities, e.g.,
/// (* (cos x) (cos x)) => (1 - (sin x) * (sin x)).
// fn workload_power_reduction() -> Workload {
//     let op = Workload::new(["sin", "cos", "tan"]);
//     let var = Workload::new(["a", "b"]);
//     let cnst = Workload::new([
//         "0", "(/ PI 6)", "(/ PI 4)", "(/ PI 3)", "(/ PI 2)", "PI", "(* PI 2)",
//     ]);

//     let t_xform = Workload::new(["a", "(- a)", "(+ x)", "(+ (/ PI 2) x)", "(* 2 x)"]).plug("x", &var);
    
// }


pub fn trig_rules() -> Ruleset<Trig> {
    let limits = Limits {
        iter: 3,
        node: 2000000,
        match_: 200_000,
    };

    // known rules for fast-fowarding
    let mut prior: Ruleset<Trig> = Ruleset::from_file("scripts/oopsla21/trig/complex.rules");
    prior.extend(prior_rules());

    // rulesets
    let mut all = prior.clone();
    let mut new = Ruleset::<Trig>::default();

    /////////////////////////////////////////////////////////////////
    // workload 1: constants
    println!("Starting 1");
    let wkld_consts = workload_consts();
    let rules = run_fast_forwarding(wkld_consts.clone(), all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 2: even/odd symmetry and periodicity
    println!("Starting 2");
    let wkld_sym_per = workload_symmetry_periodicity();
    let rules = run_fast_forwarding(wkld_sym_per.clone(), all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());
    
    /////////////////////////////////////////////////////////////////
    // workload 3: sum of squares
    println!("Starting 3");
    let wkld_sos = workload_sum_of_squares();
    let rules = run_fast_forwarding(wkld_sos, all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 4: coangles
    println!("Starting 4");
    let wkld_coangle = workload_coangle();
    let rules = run_fast_forwarding(wkld_coangle, all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 5: power reduction
    println!("Starting 5");

    /////////////////////////////////////////////////////////////////
    // workload 6: product-to-sum reduction
    println!("Starting 6");

    /////////////////////////////////////////////////////////////////
    // workload 7: sum-to-product reduction
    println!("Starting 7");


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

    // Power reduction
    let wkld2 = Workload::Append(vec![scaled_shifted_sqrs, consts.clone()]);
    let rules2 = run_fast_forwarding(wkld2.clone(), all.clone(), limits, limits);
    all.extend(rules2.clone());
    new.extend(rules2.clone());

    // Product-to-sum
    let wkld3 = Workload::Append(vec![scaled_sum_prod, consts.clone()]);
    let rules3 = run_fast_forwarding(wkld3.clone(), all.clone(), limits, limits);
    all.extend(rules3.clone());
    new.extend(rules3.clone());

    // Sums
    let wkld4 = Workload::Append(vec![two_var_no_sub, sum_of_prod, consts.clone()]);
    let rules4 = run_fast_forwarding(wkld4.clone(), all.clone(), limits, limits);
    all.extend(rules4.clone());
    new.extend(rules4.clone());
    new
}
