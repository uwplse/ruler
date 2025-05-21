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

    let invalid = Filter::Contains("(tan (/ PI 2))".parse().unwrap());

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

    // shift argument to function
    // (and optionally) negate it
    let t_shift = Workload::new(["t", "(~ t)", "(+ PI t)", "(- PI t)", "(+ t t)"]).plug("t", &var);
    let t_simpl = Workload::new(["(op t)"])
        .plug("op", &op)
        .plug("t", &t_shift);
    let t_neg = Workload::new(["(~ t)"]).plug("t", &t_simpl);

    workload_consts().append(t_simpl).append(t_neg)
}

/// Terms to prove identitites that sum-of-squares terms, e.g.,
/// sin(x) * sin(x). This includes the Pythagorean identity.
fn workload_sum_of_squares() -> Workload {
    let op = Workload::new(["sin", "cos", "tan"]);
    let var = Workload::new(["a", "b"]);

    let is_double_angle = Filter::Or(vec![
        Filter::Contains("(sin (+ ?x ?x))".parse().unwrap()),
        Filter::Contains("(cos (+ ?x ?x))".parse().unwrap()),
        Filter::Contains("(tan (+ ?x ?x))".parse().unwrap()),
    ]);

    // sum (or difference) of squares (of trig functions)
    let t_simpl = Workload::new(["(op t)"]).plug("op", &op).plug("t", &var);
    let t_sqr = Workload::new(["(sqr t)"]).plug("t", &t_simpl);
    let t_sos = Workload::new(["(+ t t)", "(- t t)"]).plug("t", &t_sqr);

    workload_symmetry_periodicity()
        .filter(Filter::Invert(Box::new(is_double_angle)))
        .append(t_sos)
}

/// Terms to prove co-angle identities, e.g., (cos x) => (sin (- (/ PI 2) x))
fn workload_coangle() -> Workload {
    let op = Workload::new(["sin", "cos"]);
    let var = Workload::new(["a", "b"]);
    let cnst = Workload::new(["-2", "-1", "0", "1", "2"]);

    let t_shift =
        Workload::new(["t", "(- (/ PI 2) t)", "(+ (/ PI 2) t)", "(* 2 t)"]).plug("t", &var);
    let t_simpl = Workload::new(["(op t)"])
        .plug("op", &op)
        .plug("t", &t_shift);

    t_simpl.append(cnst)
}

/// Terms to prove power reduction identities, e.g.,
/// (* (cos x) (cos x)) => (1 - (sin x) * (sin x)).
fn workload_power_reduction() -> Workload {
    let op = Workload::new(["sin", "cos"]);
    let var = Workload::new(["a"]);
    let cnst = Workload::new(["-2", "-1", "0", "1", "2"]);

    // squared trig functions with variable arguments
    let t_trig = Workload::new(["(op t)"]).plug("op", &op).plug("t", &var);
    let t_sqr = Workload::new(["(sqr t)"]).plug("t", &t_trig);

    // trig functions (with possibly shifted arguments, and shifted output)
    let t_xform =
        Workload::new(["t", "(- (/ PI 2) t)", "(+ (/ PI 2) t)", "(* 2 t)"]).plug("t", &var);
    let t_trig_xform = Workload::new(["(op t)"])
        .plug("op", &op)
        .plug("t", &t_xform);
    let t_shift = Workload::new(["t", "(- 1 t)", "(+ 1 t)"]).plug("t", &t_trig_xform);

    // merge and scale
    let t_prescale = t_shift.append(t_sqr);
    let t_scale = Workload::new(["t", "(/ t 2)"]).plug("t", &t_prescale);

    t_scale.append(cnst)
}

/// Terms to prove product-to-sum identities, e.g.,
/// (* (cos x) (cos y)) => (/ (+ (cos (- x y)) (cos (+ x y))) 2).
fn workload_product_to_sum() -> Workload {
    let op = Workload::new(["sin", "cos"]);
    let cnst = Workload::new(["-2", "-1", "0", "1", "2"]);

    // filter for square terms
    let is_square = Filter::Or(vec![
        Filter::Contains("(* (cos ?x) (cos ?x))".parse().unwrap()),
        Filter::Contains("(* (sin ?x) (sin ?x))".parse().unwrap()),
    ]);

    // simple arguments to trig functions
    let t_simpl = Workload::new(["a", "b", "(+ a b)", "(- a b)"]);

    // trig functions with variable arguments
    let t_2var = Workload::new(["(op t)"])
        .plug("op", &op)
        .plug("t", &t_simpl);

    // product of trig functions (no squares)
    let t_prod = Workload::new(["(* t1 t2)"])
        .plug("t1", &t_2var)
        .plug("t2", &t_2var)
        .filter(Filter::Invert(Box::new(is_square)));

    // sum of trig functions
    let t_sum = Workload::new(["(+ t1 t2)", "(- t1 t2)"])
        .plug("t1", &t_2var)
        .plug("t2", &t_2var);

    // merge and scale
    let t_prescale = t_sum.append(t_prod);
    let t_scale = Workload::new(["t", "(/ t 2)"]).plug("t", &t_prescale);

    t_scale.append(cnst)
}

/// Terms to prove sum-to-product identities, e.g.,
/// (+ (cos x) (cos y)) => (* 2 (cos (/ (+ x y) 2)) (cos (/ (- x y) 2))).
fn workload_sum_to_product() -> Workload {
    let op = Workload::new(["sin", "cos"]);
    let cnst = Workload::new(["-2", "-1", "0", "1", "2"]);

    // filter for non-trivial trig terms
    let is_nontrivial = Filter::Or(vec![
        Filter::Contains("(cos (?op ?x ?y))".parse().unwrap()),
        Filter::Contains("(sin (?op ?x ?y))".parse().unwrap()),
    ]);

    // filter for difference of angles
    let is_diff = Filter::Or(vec![
        Filter::Contains("(cos (- ?x ?y))".parse().unwrap()),
        Filter::Contains("(sin (- ?x ?y))".parse().unwrap()),
    ]);

    // filter for doubling
    let is_double = Filter::Contains("(+ ?x ?x)".parse().unwrap());

    // filter for square terms
    let is_square = Filter::Or(vec![
        Filter::Contains("(* (cos ?x) (cos ?x))".parse().unwrap()),
        Filter::Contains("(* (sin ?x) (sin ?x))".parse().unwrap()),
    ]);

    // simple arguments to trig functions
    let t_simpl = Workload::new(["a", "b", "(+ a b)", "(- a b)"]);

    // trig functions with variable arguments
    let t_2var = Workload::new(["(op t)"])
        .plug("op", &op)
        .plug("t", &t_simpl);

    // product of trig functions (no squares)
    let t_prod = Workload::new(["(* t1 t2)"])
        .plug("t1", &t_2var)
        .plug("t2", &t_2var)
        .filter(Filter::Invert(Box::new(is_square)));

    // sum-of-product terms
    let t_sop = Workload::new(["(+ t1 t2)", "(- t1 t2)"])
        .plug("t1", &t_prod)
        .plug("t2", &t_prod)
        .filter(Filter::Invert(Box::new(is_double)))
        .filter(Filter::Invert(Box::new(is_nontrivial)));

    // remove difference of angles
    let t_2var_no_sub = t_2var.filter(Filter::Invert(Box::new(is_diff)));

    t_2var_no_sub.append(t_sop).append(cnst)
}

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
    println!("starting 1: constants");
    let wkld_consts = workload_consts();
    let rules = run_fast_forwarding(wkld_consts.clone(), all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 2: even/odd symmetry and periodicity
    println!("starting 2: symmetry/periodicity");
    let wkld_sym_per = workload_symmetry_periodicity();
    let rules = run_fast_forwarding(wkld_sym_per.clone(), all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 3: sum of squares
    println!("starting 3: sum of squares");
    let wkld_sos = workload_sum_of_squares();
    let rules = run_fast_forwarding(wkld_sos, all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 4: coangles
    println!("starting 4: coangles");
    let wkld_coangle = workload_coangle();
    let rules = run_fast_forwarding(wkld_coangle, all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 5: power reduction
    println!("starting 5: power reduction");
    let wkld_power = workload_power_reduction();
    let rules = run_fast_forwarding(wkld_power, all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 6: product-to-sum reduction
    println!("starting 6: product-to-sum");
    let wkld_prod_sum = workload_product_to_sum();
    let rules = run_fast_forwarding(wkld_prod_sum, all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    /////////////////////////////////////////////////////////////////
    // workload 7: sum-to-product reduction
    println!("starting 7: sum-to-product");
    let wkld_sum_prod = workload_sum_to_product();
    let rules = run_fast_forwarding(wkld_sum_prod, all.clone(), limits, limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    new
}
