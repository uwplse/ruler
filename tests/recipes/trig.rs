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
        node: 2_000_000,
        match_: 200_000,
    };

    let rational: Ruleset<Trig> = Ruleset::from_file("scripts/oopsla21/trig/complex.rules");
    let handwritten: Ruleset<Trig> = prior_rules();

    let mut prior = Ruleset::default();
    prior.extend(rational);
    prior.extend(handwritten);

    let mut all = prior.clone();
    let mut new = Ruleset::default();

    // Language Basics
    let t_ops = Workload::new(["sin", "cos", "tan"]);
    let t_consts = Workload::new([
        "0", "(/ PI 6)", "(/ PI 4)", "(/ PI 3)", "(/ PI 2)", "PI", "(* PI 2)",
    ]);
    let consts = Workload::new(["-2", "-1", "0", "1", "2"]);
    let valid_trig = Filter::Invert(Box::new(Filter::Contains(
        "(tan (/ PI 2))".parse().unwrap(),
    )));
    let no_tan = Filter::Excludes("tan".parse().unwrap());

    // Phase 1:
    let wkld = Workload::new(["(OP V)"])
        .plug("OP", &t_ops)
        .plug("V", &t_consts)
        .filter(valid_trig.clone());
    let rules = run_rule_lifting(&wkld, all.clone(), limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    // Phase 2:
    let vals = Workload::Append(vec![
        t_consts.clone(),
        Workload::new(["a", "(~ a)", "(+ PI a)", "(- PI a)", "(+ a a)"]),
    ]);
    let simple_ops = Workload::new(["(OP V)"])
        .plug("OP", &t_ops)
        .plug("V", &vals)
        .filter(valid_trig.clone());
    let negations = Workload::new(["(~ V)"]).plug("V", &simple_ops);
    let wkld = Workload::Append(vec![simple_ops, negations]);
    let rules = run_rule_lifting(&wkld, all.clone(), limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    // Phase 3:
    let prev_wkld = wkld; // Re-use workload from previous phase
    let vals = Workload::new(["(OP V)"])
        .plug("OP", &t_ops)
        .plug("V", &Workload::new(["a", "b"]));
    let squares = Workload::new(["(sqr V)"]).plug("V", &vals);
    let sum_of_squares = Workload::new(["(+ V V)", "(- V V)"]).plug("V", &squares);
    let wkld = Workload::Append(vec![prev_wkld, sum_of_squares]);
    let rules = run_rule_lifting(&wkld, all.clone(), limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    // Phase 4:
    let wkld = Workload::Append(vec![
        consts.clone(),
        Workload::new(["(OP V)"]).plug("OP", &t_ops).plug(
            "V",
            &Workload::new(["a", "(- (/ PI 2) a)", "(+ (/ PI 2) a)", "(* 2 a)"]),
        ),
    ]);
    let rules = run_rule_lifting(&wkld, all.clone(), limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    // Phase 5:
    let trig_sqrs = Workload::new(["(sqr (OP V))"])
        .plug("OP", &t_ops)
        .plug("V", &Workload::new(["a"]));
    let base = Workload::Append(vec![
        trig_sqrs,
        Workload::new(["(OP V)"]).plug("OP", &t_ops).plug(
            "V",
            &Workload::new(["a", "(- (/ PI 2) a)", "(+ (/ PI 2) a)", "(* 2 a)"]),
        ),
    ]);
    let wkld = Workload::Append(vec![
        consts.clone(),
        Workload::new([
            "V",
            "(/ V 2)",
            "(+ 1 V)",
            "(- 1 V)",
            "(/ (+ 1 V) 2)",
            "(/ (- 1 V) 2)",
        ])
        .plug("V", &base),
    ])
    .filter(no_tan.clone());
    let rules = run_rule_lifting(&wkld, all.clone(), limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    // Phase 6:
    let base = Workload::new(["(OP V)"])
        .plug("OP", &t_ops)
        .plug("V", &Workload::new(["a", "b", "(+ a b)", "(- a b)"]));
    let sum_and_prod = Workload::new(["(+ V V)", "(- V V)", "(* V V)"]).plug("V", &base);
    let wkld = Workload::Append(vec![
        consts.clone(),
        Workload::new(["V", "(/ V 2)"]).plug("V", &sum_and_prod),
    ])
    .filter(no_tan.clone());
    let rules = run_rule_lifting(&wkld, all.clone(), limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    // Phase 7:
    let base = Workload::new(["(OP V)"])
        .plug("OP", &t_ops)
        .plug("V", &Workload::new(["a", "b"]));
    let sum_of_prod =
        Workload::new(["(+ (* V V) (* V V))", "(- (* V V) (* V V))"]).plug("V", &base);

    let no_square = Filter::And(vec![
        Filter::Excludes("(* (cos ?x) (cos ?x))".parse().unwrap()),
        Filter::Excludes("(* (sin ?x) (sin ?x))".parse().unwrap()),
    ]);
    let wkld = Workload::Append(vec![
        consts,
        Workload::new(["(sin (+ a b))", "(cos (+ a b))"]),
        base,
        sum_of_prod,
    ])
    .filter(no_tan.clone())
    .filter(no_square);
    let rules = run_rule_lifting(&wkld, all.clone(), limits);
    all.extend(rules.clone());
    new.extend(rules.clone());

    let orig = Ruleset::<Trig>::from_file("trig.rules");
    let (can, cannot) = all.derive(DeriveType::LhsAndRhs, &orig, limits);
    println!("{} {}", can.len(), cannot.len());
    cannot.pretty_print();

    new
}
