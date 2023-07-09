use egg::Rewrite;
use num::{
    rational::Ratio, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, Signed, ToPrimitive, Zero,
};
use ruler::{
    enumo::{Rule, Ruleset, Scheduler, Workload},
    *,
};
use std::{ops::*, time::Instant};
use symbolic_expressions::parser::parse_str;
use symbolic_expressions::Sexp;
use z3::ast::Ast;
#[path = "./recipes/rational_best.rs"]
pub mod rational_best;
#[path = "./recipes/rational_replicate.rs"]
pub mod rational_replicate;

/// define `Constant` for rationals.
pub type Constant = Ratio<i64>;

fn mk_rat(n: i64, d: i64) -> Constant {
    if d.is_zero() {
        panic!("mk_rat: denominator is zero!");
    }
    Ratio::new(n, d)
}

egg::define_language! {
  pub enum Math {
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    "~" = Neg(Id),
    "fabs" = Abs(Id),
    "if" = If([Id; 3]),
    Lit(Constant),
    Var(egg::Symbol),
  }
}

impl SynthLanguage for Math {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Add([x, y]) => map!(get_cvec, x, y => x.checked_add(y)),
            Math::Sub([x, y]) => map!(get_cvec, x, y => x.checked_sub(y)),
            Math::Mul([x, y]) => map!(get_cvec, x, y => x.checked_mul(y)),
            Math::Div([x, y]) => map!(get_cvec, x, y => {
                x.checked_div(y)
            }),
            Math::Neg(x) => map!(get_cvec, x => Some(-x)),
            Math::Abs(a) => map!(get_cvec, a => Some(a.abs())),
            Math::Lit(c) => vec![Some(c.clone()); cvec_len],
            Math::Var(_) => vec![],
            Math::If([x, y, z]) => get_cvec(x)
                .iter()
                .zip(get_cvec(y).iter())
                .zip(get_cvec(z).iter())
                .map(|tup| {
                    let ((x, y), z) = tup;
                    if let Some(cond) = x {
                        if !cond.is_zero() {
                            *y
                        } else {
                            *z
                        }
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>(),
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        let mut get_const = |x: &'a Id| {
            let ival = get_interval(x);
            if ival.low == ival.high {
                ival.low.clone()
            } else {
                None
            }
        };
        match self {
            Math::Lit(n) => Some(n.clone()),
            Math::Var(_) => None,
            Math::Neg(x) => get_const(x).map(|c| -c),
            Math::Abs(a) => get_const(a).map(|c| c.abs()),
            Math::Add([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_add(&y),
                _ => None,
            },
            Math::Sub([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_sub(&y),
                _ => None,
            },
            Math::Mul([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_mul(&y),
                _ => None,
            },
            Math::Div([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_div(&y),
                _ => None,
            },
            Math::If([x, y, z]) => {
                if let Some(x) = get_const(x) {
                    if !x.is_zero() {
                        get_const(y)
                    } else {
                        get_const(z)
                    }
                } else {
                    None
                }
            }
        }
        .map(|c| Interval::new(Some(c.clone()), Some(c)))
        .unwrap_or_default()
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let consts = vec![
            Some(mk_rat(-1, 1)),
            Some(mk_rat(0, 1)),
            Some(mk_rat(1, 1)),
            Some(mk_rat(2, 1)),
            Some(mk_rat(-3, 1)),
        ];
        let cvecs = self_product(&consts, vars.len());

        egraph.analysis.cvec_len = cvecs[0].len();

        for (i, v) in vars.iter().enumerate() {
            let id = egraph.add(Math::Var(Symbol::from(v.clone())));
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Math::Var(v) => Some(*v),
            _ => None,
        }
    }

    fn validate(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> ValidationResult {
        // TODO if you drop variables, it's unsound because
        // we may have lost an error
        /*if lhs.vars().into_iter().collect::<HashSet<Var>>()
            != rhs.vars().into_iter().collect::<HashSet<Var>>()
        {
            return ValidationResult::Invalid;
        }*/

        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
        let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
        let lhs_denom = Self::error_conditions(
            &ctx,
            Self::pat_to_sexp(lhs),
            z3::ast::Bool::from_bool(&ctx, true),
        );
        let rhs_denom = Self::error_conditions(
            &ctx,
            Self::pat_to_sexp(rhs),
            z3::ast::Bool::from_bool(&ctx, true),
        );

        let mut assert_equal = lexpr._eq(&rexpr);

        for condition in lhs_denom.iter().chain(rhs_denom.iter()) {
            assert_equal = condition.not().implies(&assert_equal);
        }

        let rhs_errors =
            z3::ast::Bool::or(&ctx, &rhs_denom.iter().collect::<Vec<&z3::ast::Bool>>());
        let lhs_errors =
            z3::ast::Bool::or(&ctx, &lhs_denom.iter().collect::<Vec<&z3::ast::Bool>>());
        let error_preserved = rhs_errors.iff(&lhs_errors);
        let assertion = z3::ast::Bool::and(&ctx, &[&assert_equal, &error_preserved]);

        solver.assert(&assertion.clone().not());
        let res = Self::z3_res_to_validationresult(solver.check());
        /*if let ValidationResult::Valid = res {
            eprintln!("verifying {} => {}", lhs, rhs);
        eprintln!("assertion: {}", assertion);
        }*/
        res
    }

    fn is_constant(&self) -> bool {
        matches!(self, Math::Lit(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Math::Lit(c)
    }
}

impl Math {
    fn _one_of_errors(ctx: &z3::Context, denoms: HashSet<String>) -> z3::ast::Bool {
        let zero_z3 = z3::ast::Real::from_real(&ctx, 0, 1);

        let mut one_of_rhs_errors = z3::ast::Bool::from_bool(ctx, false);
        for d in denoms {
            let expr = egg_to_z3(
                ctx,
                Self::instantiate(&d.to_string().parse::<Pattern<Math>>().unwrap()).as_ref(),
            );
            one_of_rhs_errors = z3::ast::Bool::or(ctx, &[&one_of_rhs_errors, &expr._eq(&zero_z3)]);
        }
        one_of_rhs_errors
    }

    fn z3_res_to_validationresult(res: z3::SatResult) -> ValidationResult {
        match res {
            z3::SatResult::Unsat => ValidationResult::Valid,
            z3::SatResult::Sat => ValidationResult::Invalid,
            z3::SatResult::Unknown => ValidationResult::Unknown,
        }
    }

    fn pat_to_sexp(pat: &Pattern<Math>) -> Sexp {
        parse_str(&pat.to_string()).unwrap()
    }

    fn all_denominators(sexp: Sexp) -> HashSet<String> {
        let mut res = HashSet::<String>::default();
        match sexp {
            Sexp::List(list) => {
                if list[0] == Sexp::String("/".to_string()) {
                    res.insert(list[2].to_string());
                }

                for s in list {
                    res.extend(Self::all_denominators(s));
                }
            }
            _ => (),
        }

        res
    }

    fn add_condition(rule: Rule<Math>) -> Option<Rule<Math>> {
        let lhs_sexp = parse_str(&rule.lhs.to_string()).unwrap();
        let rhs_sexp = parse_str(&rule.rhs.to_string()).unwrap();

        let lhs_denoms = Self::all_denominators(lhs_sexp.clone());
        let rhs_denoms = Self::all_denominators(rhs_sexp.clone());
        let intersection: HashSet<String> = lhs_denoms.intersection(&rhs_denoms).cloned().collect();
        let all_denoms: Vec<String> = lhs_denoms
            .union(&rhs_denoms)
            .cloned()
            .collect::<HashSet<String>>()
            .difference(&intersection)
            .cloned()
            .collect();

        if all_denoms.is_empty() {
            None
        } else {
            let mut iterator = all_denoms.iter();
            let mut condition: Sexp = parse_str(iterator.next().unwrap()).unwrap();

            // TODO doesn't handle multiple denominators
            if let Some(_) = iterator.next() {
                return None;
            }
            for denom in iterator {
                condition = Sexp::List(vec![
                    Sexp::String("and".to_string()),
                    condition,
                    parse_str(denom).unwrap(),
                ]);
            }
            let rhs = Sexp::List(vec![
                Sexp::String("if".to_string()),
                condition,
                rhs_sexp,
                lhs_sexp,
            ])
            .to_string()
            .parse::<Pattern<Math>>()
            .unwrap();

            let name = format!("{} ==> {}", rule.lhs, rhs);
            let rewrite = Rewrite::new(name.clone(), rule.lhs.clone(), rhs.clone()).unwrap();
            Some(Rule {
                lhs: rule.lhs,
                rhs,
                name: name.into(),
                rewrite,
            })
        }
    }

    /// Given an expression, returns a vector
    /// of conditions for when the expression
    /// divides by zero.
    ///
    /// For example, the expression
    /// (/ 1 x) errors when (== x 0)
    ///
    /// The path variable stores the path conditions for reaching this expression.
    ///
    /// For example,
    /// In (if x y z), the expression y
    /// has condition (!= x 0)
    fn error_conditions<'a>(
        ctx: &'a z3::Context,
        sexp: Sexp,
        path: z3::ast::Bool<'a>,
    ) -> Vec<z3::ast::Bool<'a>> {
        let mut res = Vec::<z3::ast::Bool<'a>>::default();
        match sexp {
            Sexp::List(list) => {
                if list[0] == Sexp::String("/".to_string()) {
                    let denom = list[2].to_string();
                    let expr = egg_to_z3(
                        &ctx,
                        Self::instantiate(&denom.to_string().parse::<Pattern<Math>>().unwrap())
                            .as_ref(),
                    );
                    let is_zero = expr._eq(&z3::ast::Real::from_real(ctx, 0, 1));

                    res.push(z3::ast::Bool::and(ctx, &[&is_zero, &path]));
                }

                if list[0] == Sexp::String("if".to_string()) {
                    let cond_real = egg_to_z3(
                        &ctx,
                        Self::instantiate(&list[1].to_string().parse::<Pattern<Math>>().unwrap())
                            .as_ref(),
                    );
                    let zero = z3::ast::Real::from_real(ctx, 0, 1);
                    let new_path_pos = z3::ast::Bool::and(
                        &ctx,
                        &[&path, &z3::ast::Bool::not(&cond_real._eq(&zero))],
                    );
                    let new_path_neg = z3::ast::Bool::and(&ctx, &[&path, &cond_real._eq(&zero)]);
                    res.extend(Self::error_conditions(ctx, list[2].clone(), new_path_pos));
                    res.extend(Self::error_conditions(ctx, list[3].clone(), new_path_neg));
                } else {
                    for s in list {
                        res.extend(Self::error_conditions(ctx, s, path.clone()));
                    }
                };
            }
            Sexp::String(_) => (),
            Sexp::Empty => (),
        }

        res
    }

    fn run_workload_conditional(
        workload: Workload,
        prior: Ruleset<Self>,
        limits: Limits,
        fast_match: bool,
    ) -> Ruleset<Self> {
        let t = Instant::now();

        println!("Compressing workload with {} prior rules", prior.len());
        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let mut candidates = if fast_match {
            Ruleset::fast_cvec_match(&compressed)
        } else {
            Ruleset::cvec_match(&compressed)
        };

        let num_prior = prior.len();
        let (chosen, invalid) = candidates.minimize(prior.clone(), Scheduler::Compress(limits));

        println!(
            "Found {} valid and {} invalid rules",
            chosen.len(),
            invalid.len()
        );
        // here's the conditional stuff
        let mut with_condition = Ruleset::<Math>(
            invalid
                .0
                .iter()
                .filter_map(|r| {
                    if let Some(rewritten) = Self::add_condition(r.1.clone()) {
                        Some((rewritten.name.clone(), rewritten))
                    } else {
                        None
                    }
                })
                .collect(),
        );

        println!(
            "Instrumented {} rules with conditions",
            with_condition.len()
        );

        let chosen_conditional = with_condition
            .minimize(prior.union(&chosen), Scheduler::Compress(limits))
            .0;

        let result = chosen.union(&chosen_conditional);

        let time = t.elapsed().as_secs_f64();
        println!(
            "Learned {} bidirectional rewrites ({} total rewrites) in {} using {} prior rewrites",
            result.bidir_len(),
            result.len(),
            time,
            num_prior
        );

        result.pretty_print();

        result
    }
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Math]) -> z3::ast::Real<'a> {
    let mut buf: Vec<z3::ast::Real> = vec![];
    for node in expr.as_ref().iter() {
        match node {
            Math::Add([x, y]) => buf.push(z3::ast::Real::add(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Math::Sub([x, y]) => buf.push(z3::ast::Real::sub(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Math::Mul([x, y]) => buf.push(z3::ast::Real::mul(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Math::Div([x, y]) => buf.push(z3::ast::Real::div(
                &buf[usize::from(*x)],
                &buf[usize::from(*y)],
            )),
            Math::Neg(x) => buf.push(z3::ast::Real::unary_minus(&buf[usize::from(*x)])),
            Math::Abs(a) => {
                let inner = &buf[usize::from(*a)].clone();
                let zero = z3::ast::Real::from_real(ctx, 0, 1);
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Real::le(inner, &zero),
                    &z3::ast::Real::unary_minus(inner),
                    &inner,
                ));
            }
            Math::Lit(c) => buf.push(z3::ast::Real::from_real(
                ctx,
                (c.numer()).to_i32().unwrap(),
                (c.denom()).to_i32().unwrap(),
            )),
            Math::Var(v) => buf.push(z3::ast::Real::new_const(ctx, v.to_string())),
            Math::If([x, y, z]) => {
                let zero = z3::ast::Real::from_real(ctx, 0, 1);
                let cond = z3::ast::Bool::not(&buf[usize::from(*x)]._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &cond,
                    &buf[usize::from(*y)],
                    &buf[usize::from(*z)],
                ))
            }
        }
    }
    buf.pop().unwrap()
}

// Interval helpers
#[derive(Debug, Clone, PartialEq, Eq)]
enum Sign {
    Positive,
    ContainsZero,
    Negative,
}

fn sign(interval: &Interval<Constant>) -> Sign {
    match (&interval.low, &interval.high) {
        (None, None) => Sign::ContainsZero,
        (Some(x), None) => {
            if x.is_positive() {
                Sign::Positive
            } else {
                Sign::ContainsZero
            }
        }
        (None, Some(y)) => {
            if y.is_negative() {
                Sign::Negative
            } else {
                Sign::ContainsZero
            }
        }
        (Some(x), Some(y)) => {
            if x.is_positive() && y.is_positive() {
                Sign::Positive
            } else if x.is_negative() && y.is_negative() {
                Sign::Negative
            } else {
                Sign::ContainsZero
            }
        }
    }
}

fn neg(interval: &Interval<Constant>) -> Interval<Constant> {
    let low = interval.low.clone();
    let high = interval.high.clone();
    Interval::new(high.map(|x| -x), low.map(|x| -x))
}

fn _abs(interval: &Interval<Constant>) -> Interval<Constant> {
    let low = interval.low.clone();
    let high = interval.high.clone();

    match (low, high) {
        (None, None) => Interval::new(None, None),
        (Some(x), None) => {
            if x.is_negative() {
                Interval::new(Some(-x), None)
            } else {
                Interval::new(Some(x), None)
            }
        }
        (None, Some(y)) => {
            if y.is_negative() {
                Interval::new(None, Some(-y))
            } else {
                Interval::new(None, Some(y))
            }
        }
        (Some(x), Some(y)) => {
            if x.is_negative() {
                if y.is_negative() {
                    Interval::new(Some(-x), Some(-y))
                } else {
                    Interval::new(Some(-x), Some(y))
                }
            } else {
                if y.is_negative() {
                    Interval::new(Some(x), Some(-y))
                } else {
                    Interval::new(Some(x), Some(y))
                }
            }
        }
    }
}

fn add(a: &Interval<Constant>, b: &Interval<Constant>) -> Interval<Constant> {
    let add_opts = |x: Option<Constant>, y: Option<Constant>| x.zip(y).map(|(x, y)| x + y);
    let a = a.clone();
    let b = b.clone();
    Interval::new(add_opts(a.low, b.low), add_opts(a.high, b.high))
}

fn mul(a: &Interval<Constant>, b: &Interval<Constant>) -> Interval<Constant> {
    let mul_opts = |x: Option<Constant>, y: Option<Constant>| x.zip(y).map(|(x, y)| x * y);
    let (sign_a, sign_b) = (sign(a), sign(b));
    let a = a.clone();
    let b = b.clone();
    match (sign_a, sign_b) {
        (Sign::Negative, Sign::Negative) => {
            Interval::new(mul_opts(a.high, b.high), mul_opts(a.low, b.low))
        }
        (Sign::Positive, Sign::Positive) => {
            Interval::new(mul_opts(a.low, b.low), mul_opts(a.high, b.high))
        }
        (Sign::Positive, Sign::Negative) => {
            Interval::new(mul_opts(a.high, b.low), mul_opts(a.low, b.high))
        }
        (Sign::Negative, Sign::Positive) => {
            Interval::new(mul_opts(a.low, b.high), mul_opts(a.high, b.low))
        }

        (Sign::Positive, Sign::ContainsZero) => {
            Interval::new(mul_opts(a.high.clone(), b.low), mul_opts(a.high, b.high))
        }
        (Sign::ContainsZero, Sign::Positive) => {
            Interval::new(mul_opts(a.low, b.high.clone()), mul_opts(a.high, b.high))
        }

        (Sign::Negative, Sign::ContainsZero) => {
            Interval::new(mul_opts(a.low.clone(), b.high), mul_opts(a.low, b.low))
        }
        (Sign::ContainsZero, Sign::Negative) => {
            Interval::new(mul_opts(a.high, b.low.clone()), mul_opts(a.low, b.low))
        }

        (Sign::ContainsZero, Sign::ContainsZero) => {
            let al_bh = mul_opts(a.low.clone(), b.high.clone());
            let ah_bl = mul_opts(a.high.clone(), b.low.clone());
            let min = al_bh.zip(ah_bl).map(|(x, y)| x.min(y));

            let ah_bh = mul_opts(a.high, b.high);
            let al_bl = mul_opts(a.low, b.low);
            let max = ah_bh.zip(al_bl).map(|(x, y)| x.max(y));
            Interval::new(min, max)
        }
    }
}

fn recip(interval: &Interval<Constant>) -> Interval<Constant> {
    let interval = interval.clone();
    let sign = sign(&interval);
    match (interval.low, interval.high) {
        (Some(x), Some(y)) => match sign {
            Sign::ContainsZero => Interval::default(),
            _ => Interval::new(Some(y.recip()), Some(x.recip())),
        },
        _ => Interval::default(),
    }
}

#[cfg(test)]
pub mod test {

    use super::*;
    use crate::rational_best::best_enumo_recipe;
    use crate::rational_replicate::replicate_ruler1_recipe;
    use num::rational::Ratio;
    use ruler::{
        enumo::{Ruleset, Workload},
        recipe_utils::{base_lang, iter_metric, run_workload},
    };

    fn interval(low: Option<i64>, high: Option<i64>) -> Interval<Constant> {
        let i64_to_constant = |x: i64| Ratio::new(x, 1);
        Interval::new(low.map(i64_to_constant), high.map(i64_to_constant))
    }

    #[test]
    fn sign_test() {
        assert_eq!(sign(&interval(None, None)), Sign::ContainsZero);
        assert_eq!(sign(&interval(None, Some(-100))), Sign::Negative);
        assert_eq!(sign(&interval(None, Some(100))), Sign::ContainsZero);
        assert_eq!(sign(&interval(Some(-100), None)), Sign::ContainsZero);
        assert_eq!(sign(&interval(Some(100), None)), Sign::Positive);
        assert_eq!(sign(&interval(Some(-100), Some(-50))), Sign::Negative);
        assert_eq!(sign(&interval(Some(50), Some(100))), Sign::Positive);
        assert_eq!(sign(&interval(Some(-10), Some(100))), Sign::ContainsZero);
    }

    #[test]
    fn neg_interval_test() {
        assert_eq!(neg(&interval(None, None)), interval(None, None));
        assert_eq!(neg(&interval(Some(10), None)), interval(None, Some(-10)));
        assert_eq!(neg(&interval(Some(-10), None)), interval(None, Some(10)));
        assert_eq!(neg(&interval(None, Some(10))), interval(Some(-10), None));
        assert_eq!(neg(&interval(None, Some(-10))), interval(Some(10), None));
        assert_eq!(
            neg(&interval(Some(5), Some(10))),
            interval(Some(-10), Some(-5))
        );
    }

    #[test]
    fn add_interval_test() {
        assert_eq!(
            add(&interval(None, None), &interval(None, None)),
            interval(None, None)
        );
        assert_eq!(
            add(&interval(None, None), &interval(Some(-10), Some(10))),
            interval(None, None)
        );
        assert_eq!(
            add(&interval(Some(-10), Some(10)), &interval(None, None)),
            interval(None, None)
        );
        assert_eq!(
            add(
                &interval(Some(-20), Some(5)),
                &interval(Some(-10), Some(10))
            ),
            interval(Some(-30), Some(15))
        );
    }

    #[test]
    fn mul_interval_test() {
        assert_eq!(
            mul(&interval(None, Some(-3)), &interval(None, Some(-4))),
            interval(Some(12), None)
        );
        assert_eq!(
            mul(
                &interval(Some(-100), Some(-2)),
                &interval(Some(-50), Some(-20))
            ),
            interval(Some(40), Some(5000))
        );
        assert_eq!(
            mul(&interval(Some(2), None), &interval(Some(50), None)),
            interval(Some(100), None)
        );
        assert_eq!(
            mul(&interval(Some(30), Some(50)), &interval(Some(2), Some(3))),
            interval(Some(60), Some(150))
        );
        assert_eq!(
            mul(
                &interval(Some(-10), Some(-5)),
                &interval(Some(6), Some(100))
            ),
            interval(Some(-1000), Some(-30))
        );
        assert_eq!(
            mul(&interval(Some(3), Some(10)), &interval(None, Some(-1))),
            interval(None, Some(-3))
        );
        assert_eq!(
            mul(&interval(Some(2), Some(5)), &interval(Some(-3), Some(4))),
            interval(Some(-15), Some(20))
        );
        assert_eq!(
            mul(&interval(Some(-2), None), &interval(Some(3), Some(4))),
            interval(Some(-8), None)
        );
        assert_eq!(
            mul(&interval(None, None), &interval(Some(-10), Some(-4))),
            interval(None, None)
        );
        assert_eq!(
            mul(&interval(Some(-8), Some(6)), &interval(Some(-3), Some(-2))),
            interval(Some(-18), Some(24))
        );
        assert_eq!(
            mul(&interval(Some(-4), Some(6)), &interval(Some(-8), Some(10))),
            interval(Some(-48), Some(60))
        );
        assert_eq!(
            mul(
                &interval(Some(-100), Some(50)),
                &interval(Some(-5), Some(7))
            ),
            interval(Some(-700), Some(500))
        );
        assert_eq!(
            mul(&interval(Some(-5), Some(6)), &interval(Some(-4), Some(8))),
            interval(Some(-40), Some(48))
        );
        assert_eq!(
            mul(&interval(Some(-4), Some(10)), &interval(Some(-8), Some(6))),
            interval(Some(-80), Some(60))
        );
        assert_eq!(
            mul(&interval(None, Some(10)), &interval(Some(-5), Some(15))),
            interval(None, None)
        );
        assert_eq!(
            mul(&interval(Some(-4), Some(10)), &interval(Some(-8), None)),
            interval(None, None)
        );
    }

    #[test]
    fn recip_interval_test() {
        assert_eq!(recip(&interval(None, None)), interval(None, None));
        assert_eq!(
            recip(&interval(Some(50), Some(100))),
            Interval::new(Some(Ratio::new(1, 100)), Some(Ratio::new(1, 50)),)
        );
        assert_eq!(
            recip(&interval(Some(-10), Some(-5))),
            Interval::new(Some(Ratio::new(1, -5)), Some(Ratio::new(1, -10)),)
        );
    }

    #[test]
    fn just_best() {
        best_enumo_recipe();
    }

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let ruler1: Ruleset<Math> = Ruleset::from_file("baseline/rational.rules");
        let herbie: Ruleset<Math> = Ruleset::from_file("baseline/herbie-rational.rules");

        let start = Instant::now();
        let replicate_rules = replicate_ruler1_recipe();
        let duration = start.elapsed();

        logger::write_baseline(
            &replicate_rules,
            "rational_replicate",
            &ruler1,
            "oopsla",
            duration,
        );
        logger::write_baseline(
            &replicate_rules,
            "rational_replicate",
            &herbie,
            "herbie",
            duration,
        );

        let start = Instant::now();
        let best_rules = best_enumo_recipe();
        let duration = start.elapsed();
        // herbie and ruler can't learn any if rules
        // but crash if you include them
        let without_if: Ruleset<Math> = Ruleset::new(
            best_rules
                .0
                .iter()
                .filter_map(|r| {
                    if r.1.rhs.to_string().starts_with("(if") {
                        None
                    } else {
                        Some(r.0.to_string().clone())
                    }
                })
                .collect::<Vec<_>>(),
        );

        logger::write_baseline(&without_if, "rational_best", &ruler1, "oopsla", duration);
        logger::write_baseline(&without_if, "rational_best", &herbie, "herbie", duration);
        logger::write_baseline(
            &best_rules,
            "rational_best",
            &best_rules,
            "rational_best",
            duration,
        );
    }

    #[test]
    fn cond_div_figure() {
        let mut all_rules: Ruleset<Math> = Ruleset::default();

        let starting_rules = run_workload(
            iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 3)
                .plug("VAL", &Workload::new(["-1", "0", "1"]))
                .plug("VAR", &Workload::new(["a", "b", "c"]))
                .plug("OP1", &Workload::new(["~", "fabs"]))
                .plug("OP2", &Workload::new(["+", "*", "-", "/"])),
            all_rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            false,
        );
        all_rules.extend(starting_rules);

        let basic_if_rules = run_workload(
            Workload::new(["(if e e e)"])
                .plug("e", &Workload::new(["a", "b", "c", "-1", "0", "1"])),
            all_rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            false,
        );
        all_rules.extend(basic_if_rules);

        let terms = Workload::new(["(/ lit var)", "(if var (op lit lit) (/ lit var))"])
            .plug("lit", &Workload::new(["a", "0", "1"]))
            .plug("var", &Workload::new(["a"]))
            .plug("op", &Workload::new(["+", "-", "*", "/"]));
        terms.to_file("guard.terms");

        let guarded_rules = run_workload(
            terms,
            all_rules.clone(),
            Limits::synthesis(),
            Limits::minimize(),
            false,
        );
        guarded_rules.to_file("guard.rules");
        assert!(guarded_rules
            .0
            .contains_key("(if ?a 0 (/ 0 ?a)) ==> (/ 0 ?a)"));
        assert!(guarded_rules
            .0
            .contains_key("(/ 0 ?a) ==> (if ?a 0 (/ ?a ?a))"));
    }

    // TODO write test that catches if cvecs are not initialized

    #[test]
    fn reverse_candidate() {
        // regression test that captures a bug where we were not properly adding both directions
        // when we add candidates. See https://github.com/uwplse/ruler/pull/183

        let limits = Limits {
            iter: 3,
            node: 300000,
            match_: 200_000,
        };
        let test = Workload::new(&["(if a b b)", "b"]);
        let test_rules: Ruleset<Math> =
            run_workload(test, Ruleset::default(), limits, limits, false);
        assert_eq!(test_rules.len(), 1);
    }
}
