use egg::Rewrite;
use num::{rational::Ratio, BigInt, Signed, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use ruler::{
    enumo::{Rule, Ruleset, Scheduler, Workload},
    *,
};
use std::{ops::*, time::Instant};
use symbolic_expressions::parser::parse_str;
use symbolic_expressions::Sexp;
use z3::ast::Ast;

/// define `Constant` for rationals.
pub type Constant = Ratio<BigInt>;

fn mk_rat(n: i64, d: i64) -> Ratio<BigInt> {
    if d.is_zero() {
        panic!("mk_rat: denominator is zero!");
    }
    let n = n
        .to_bigint()
        .unwrap_or_else(|| panic!("could not make bigint from {}", n));
    let d = d
        .to_bigint()
        .unwrap_or_else(|| panic!("could not make bigint from {}", d));

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
    "zero" = Z(Id),
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
            Math::Add([x, y]) => map!(get_cvec, x, y => Some(x + y)),
            Math::Sub([x, y]) => map!(get_cvec, x, y => Some(x - y)),
            Math::Mul([x, y]) => map!(get_cvec, x, y => Some(x * y)),
            Math::Div([x, y]) => map!(get_cvec, x, y => {
              if y.is_zero() {
                None
              } else {
                Some(x / y)
              }
            }),
            Math::Neg(x) => map!(get_cvec, x => Some(-x)),
            Math::Abs(a) => map!(get_cvec, a => Some(a.abs())),
            Math::Lit(c) => vec![Some(c.clone()); cvec_len],
            Math::Var(_) => vec![],
            Math::If([x, y, z]) => {
                map!(get_cvec, x, y, z => Some( if x.is_zero() {z.clone()} else {y.clone()}))
            }

            Math::Z(x) => {
                let zero = mk_rat(0, 1);
                let one = mk_rat(1, 1);
                map!(get_cvec, x => Some(if x.eq(&zero) {one.clone()} else {zero.clone()}))
            }
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        match self {
            Math::Lit(n) => Interval::new(Some(n.clone()), Some(n.clone())),
            Math::Var(_) => Interval::default(),
            Math::Neg(x) => neg(get_interval(x)),
            Math::Abs(a) => abs(get_interval(a)),
            Math::Add([x, y]) => add(get_interval(x), get_interval(y)),
            Math::Sub([x, y]) => add(get_interval(x), &neg(get_interval(y))),
            Math::Mul([x, y]) => mul(get_interval(x), get_interval(y)),
            Math::Div([x, y]) => mul(get_interval(x), &recip(get_interval(y))),
            Math::If(_) => Interval::default(), // TODO?
            Math::Z(_) => Interval::default(),
        }
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let consts = vec![
            Some(mk_rat(-1, 1)),
            Some(mk_rat(0, 1)),
            Some(mk_rat(1, 1)),
            Some(mk_rat(2, 1)),
            Some(mk_rat(-3, 1)),
            Some(mk_rat(1238923, 2)),
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
        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
        let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
        let lhs_denom = Self::all_denominators(Self::pat_to_sexp(lhs));
        let rhs_denom = Self::all_denominators(Self::pat_to_sexp(rhs));
        let denominators = lhs_denom.union(&rhs_denom);

        let mut assert_equal = lexpr._eq(&rexpr);
        let zero_pat = "0".parse::<Pattern<Math>>().unwrap();
        let zero_z3 = egg_to_z3(&ctx, Self::instantiate(&zero_pat).as_ref());

        for d in denominators {
            let expr = egg_to_z3(
                &ctx,
                Self::instantiate(&d.to_string().parse::<Pattern<Math>>().unwrap()).as_ref(),
            );
            assert_equal = expr._eq(&zero_z3).not().implies(&assert_equal);
        }

        let rhs_errors = Self::one_of_errors(&ctx, rhs_denom);
        let lhs_errors = Self::one_of_errors(&ctx, lhs_denom);
        let error_preserved = rhs_errors.iff(&lhs_errors);
        let assertion = z3::ast::Bool::and(&ctx, &[&assert_equal, &error_preserved]);

        solver.assert(&assertion.not());
        Self::z3_res_to_validationresult(solver.check())
    }

    fn is_constant(&self) -> bool {
        matches!(self, Math::Lit(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Math::Lit(c)
    }
}

impl Math {
    fn one_of_errors(ctx: &z3::Context, denoms: HashSet<String>) -> z3::ast::Bool {
        let zero_pat = "0".parse::<Pattern<Math>>().unwrap();
        let zero_z3 = egg_to_z3(ctx, Self::instantiate(&zero_pat).as_ref());

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
            z3::SatResult::Unknown => ValidationResult::Unknown,
            z3::SatResult::Sat => ValidationResult::Invalid,
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
            Sexp::String(atom) => (),
            Empty => (),
        }

        res
    }

    // currently does nothing
    fn wrap_neqzero(sexp: Sexp) -> Sexp {
        sexp
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
            let mut condition: Sexp =
                Self::wrap_neqzero(parse_str(iterator.next().unwrap()).unwrap());

            // TODO doesn't handle multiple denominators
            if let Some(_) = iterator.next() {
                return None;
            }
            for denom in iterator {
                condition = Sexp::List(vec![
                    Sexp::String("and".to_string()),
                    condition,
                    Self::wrap_neqzero(parse_str(denom).unwrap()),
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

    fn run_workload_conditional(
        workload: Workload,
        prior: Ruleset<Self>,
        limits: Limits,
        fast_match: bool,
    ) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let candidates = if fast_match {
            Ruleset::fast_cvec_match(&compressed)
        } else {
            Ruleset::cvec_match(&compressed)
        };

        println!(
            "Partionioning {} candidates into valid/invalid",
            candidates.len()
        );
        let (mut valid, invalid) = candidates.partition(|r| r.is_valid());
        println!(
            "Found {} valid and {} invalid rules",
            valid.len(),
            invalid.len()
        );

        let num_prior = prior.len();
        let chosen = valid.minimize(prior.clone(), Scheduler::Compress(limits));

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

        let chosen_conditional =
            with_condition.minimize(prior.union(&chosen), Scheduler::Compress(limits));

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

    fn run_workload(
        workload: Workload,
        prior: Ruleset<Self>,
        limits: Limits,
        fast_match: bool,
    ) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let mut candidates = if fast_match {
            Ruleset::fast_cvec_match(&compressed)
        } else {
            Ruleset::cvec_match(&compressed)
        };

        let num_prior = prior.len();
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
            Math::Div([x, y]) => {
                // Do NOT assume denominator is non-zero
                buf.push(z3::ast::Real::div(
                    &buf[usize::from(*x)],
                    &buf[usize::from(*y)],
                ))
            }
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
            Math::Z(x) => {
                let l = &buf[usize::from(*x)];
                let zero = z3::ast::Real::from_real(ctx, 0, 1);
                let one = z3::ast::Real::from_real(ctx, 1, 1);
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Real::_eq(l, &zero),
                    &one,
                    &zero,
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

fn abs(interval: &Interval<Constant>) -> Interval<Constant> {
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
    use std::time::Duration;

    use super::*;
    use ruler::enumo::{Filter, Ruleset, Workload};

    fn interval(low: Option<i32>, high: Option<i32>) -> Interval<Constant> {
        let i32_to_constant = |x: i32| Ratio::new(x.to_bigint().unwrap(), 1.to_bigint().unwrap());
        Interval::new(low.map(i32_to_constant), high.map(i32_to_constant))
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
            Interval::new(
                Some(Ratio::new(1.to_bigint().unwrap(), 100.to_bigint().unwrap())),
                Some(Ratio::new(1.to_bigint().unwrap(), 50.to_bigint().unwrap())),
            )
        );
        assert_eq!(
            recip(&interval(Some(-10), Some(-5))),
            Interval::new(
                Some(Ratio::new(
                    1.to_bigint().unwrap(),
                    (-5).to_bigint().unwrap()
                )),
                Some(Ratio::new(
                    1.to_bigint().unwrap(),
                    (-10).to_bigint().unwrap()
                )),
            )
        );
    }

    fn replicate_ruler1_recipe() -> Ruleset<Math> {
        let mut rules = Ruleset::default();
        let limits = Limits::default();

        // Domain
        let lang = Workload::new(&["var", "const", "(uop expr)", "(bop expr expr)"]);
        let vars = &Workload::new(["a", "b", "c"]);
        let consts = &Workload::new(["0", "-1", "1"]);
        let uops = &Workload::new(["~", "fabs"]);
        let bops = &Workload::new(["+", "-", "*", "/"]);

        // Layer 1 (one op)
        println!("layer1");
        let layer1 = lang
            .clone()
            .iter_metric("expr", enumo::Metric::Depth, 2)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug_lang(vars, consts, uops, bops);
        let layer1_rules = Math::run_workload(layer1.clone(), rules.clone(), limits, false);
        rules.extend(layer1_rules);

        // Layer 2 (two ops)
        println!("layer2");
        let layer2 = lang
            .clone()
            .iter_metric("expr", enumo::Metric::Depth, 3)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug_lang(vars, consts, uops, bops);
        layer2.to_file("replicate_layer2_terms");
        let layer2_rules = Math::run_workload(layer2.clone(), rules.clone(), limits, true);
        rules.extend(layer2_rules);

        rules
    }

    pub fn conditional_rational_recipe() -> Ruleset<Math> {
        println!("Learning conditional rational rules");
        let mut rules = Ruleset::default();
        let limits = Limits::default();

        // Domain
        let lang = Workload::new(&["var", "const", "(uop expr)", "(bop expr expr)"]);
        let vars = &Workload::new(["a", "b", "c"]);
        let consts = &Workload::new(["0", "-1", "1"]);
        let uops = &Workload::new(["~", "fabs"]);
        let bops = &Workload::new(["+", "-", "*", "/"]);

        // Layer 1 (one op)
        println!("layer1");
        let layer1 = lang
            .clone()
            .iter_metric("expr", enumo::Metric::Depth, 2)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug_lang(vars, consts, uops, bops);
        let layer1_rules =
            Math::run_workload_conditional(layer1.clone(), rules.clone(), limits, false);
        rules.extend(layer1_rules);

        // Layer 2 (two ops)
        println!("layer2");
        let layer2 = lang
            .clone()
            .iter_metric("expr", enumo::Metric::Depth, 3)
            .filter(Filter::Contains("var".parse().unwrap()))
            .plug_lang(vars, consts, uops, bops);
        layer2.to_file("replicate_layer2_terms");
        let layer2_rules =
            Math::run_workload_conditional(layer2.clone(), rules.clone(), limits, true);
        rules.extend(layer2_rules);

        rules
    }

    pub fn best_enumo_recipe() -> Ruleset<Math> {
        let mut rules = conditional_rational_recipe();
        let limits = Limits::default();

        // Contains var filter
        let contains_var_filter = Filter::Or(vec![
            Filter::Contains("a".parse().unwrap()),
            Filter::Contains("b".parse().unwrap()),
            Filter::Contains("c".parse().unwrap()),
        ]);

        // Safe filter
        let safe_filter = Filter::Invert(Box::new(Filter::Contains("(/ ?x 0)".parse().unwrap())));

        // Contains abs filter
        let contains_abs_filter = Filter::Contains("fabs".parse().unwrap());

        let vars = Workload::new(["a", "b", "c"]);
        let consts = Workload::new(["-1", "0", "1", "2"]);

        // Div
        println!("div");
        let div = Workload::new(["(/ v (/ v v))"]).plug("v", &vars);
        let div_rules = Math::run_workload(div, rules.clone(), limits, false);
        rules.extend(div_rules);

        // Nested fabs
        println!("nested fabs");
        let op_layer = Workload::new(["(uop expr)", "(bop expr expr)"])
            .plug("uop", &Workload::new(&["~", "fabs"]))
            .plug("bop", &Workload::new(&["+", "-", "*", "/"]));
        let layer1 = op_layer.clone().plug("expr", &vars.append(consts));
        let layer2 = op_layer
            .plug("expr", &layer1)
            .filter(safe_filter.clone())
            .filter(contains_var_filter.clone())
            .filter(contains_abs_filter);
        let nested_abs = Workload::new(["(fabs e)"]).plug("e", &layer2);
        let nested_abs_rules = Math::run_workload(nested_abs, rules.clone(), limits, true);
        rules.extend(nested_abs_rules);

        rules
    }

    fn test_against_herbie(rules: &Ruleset<Math>, name: &str, duration: Duration) {
        let herbie: Ruleset<Math> = Ruleset::from_file("baseline/herbie-rational.rules");

        println!("Comparing rational to herbie...");
        rules.write_baseline_row(
            herbie.clone(),
            name,
            "herbie_baseline",
            "herbie.json",
            Limits {
                iter: 2,
                node: 150000,
            },
            duration,
        );
        rules.write_baseline_row_big_object(
            herbie,
            name,
            "herbie_baseline",
            Limits {
                iter: 2,
                node: 150000,
            },
            duration,
        );
    }

    #[test]
    fn run_all() {
        let start = Instant::now();
        let rules = replicate_ruler1_recipe();
        let duration = start.elapsed();

        rules.write_json_rules("rational_replicate.json");
        test_against_ruler1(&rules, "rational_replicate", duration);
        test_against_herbie(&rules, "herbie_rational_replicate", duration);

        let start = Instant::now();
        let rules = best_enumo_recipe();
        let duration = start.elapsed();

        rules.write_json_rules("rational_best.json");
        test_against_ruler1(&rules, "rational_best", duration);
        test_against_herbie(&rules, "herbie_rational_best", duration);
    }

    // todo delete
    #[test]
    fn best_enumo() {
        best_enumo_recipe();
    }

    fn test_against_ruler1(rules: &Ruleset<Math>, name: &str, duration: Duration) {
        let ruler1: Ruleset<Math> = Ruleset::from_file("baseline/rational.rules");

        println!("Comparing rational to ruler1...");
        rules.write_baseline_row(
            ruler1.clone(),
            name,
            "oopsla_rational",
            "baseline.json",
            Limits {
                iter: 2,
                node: 150000,
            },
            duration,
        );
        rules.write_baseline_row_big_object(
            ruler1,
            name,
            "oopsla_rational",
            Limits {
                iter: 2,
                node: 150000,
            },
            duration,
        );
    }

    #[test]
    fn cond_div_figure() {
        let lang = Workload::new(&["var", "const", "(uop expr)", "(bop expr expr)"]);
        let uops = &Workload::new(["~", "fabs"]);
        let bops = &Workload::new(["+", "-", "*", "/"]);

        let mut all_rules = Ruleset::default();

        let starting_rules = Math::run_workload(
            lang.clone()
                .iter_metric("expr", enumo::Metric::Atoms, 3)
                .plug_lang(
                    &Workload::new(["a", "b", "c"]),
                    &Workload::new(["-1", "0", "1"]),
                    uops,
                    bops,
                ),
            all_rules.clone(),
            Limits::default(),
            false,
        );
        all_rules.extend(starting_rules);

        let basic_if_rules = Math::run_workload(
            Workload::new(["(if e e e)"])
                .plug("e", &Workload::new(["a", "b", "c", "-1", "0", "1"])),
            all_rules.clone(),
            Limits::default(),
            false,
        );
        all_rules.extend(basic_if_rules);

        let terms = Workload::new(["(/ lit var)", "(if (zero var) (/ lit var) (op lit lit))"])
            .plug("lit", &Workload::new(["a", "0", "1"]))
            .plug("var", &Workload::new(["a"]))
            .plug("op", &Workload::new(["+", "-", "*", "/"]));
        terms.to_file("guard.terms");

        let guarded_rules = Math::run_workload(terms, all_rules.clone(), Limits::default(), false);
        assert!(guarded_rules
            .0
            .contains_key("(/ ?a ?a) ==> (if (zero ?a) (/ ?a ?a) 1)"));
    }
}
