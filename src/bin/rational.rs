/*!
   Here is an example of how to implement a domain for Ruler to infer rewrites.
   This shows the domain of rationals.
   You need to
   - define the operators in the language with `define_language`,
   - implement `eval` which is the interpreter for your domain,
   - implement `init_synth` to add the variables and important constants to the initial egraph,
   - implement `make_layer` to enumerate terms in the egraph,
   - implement `is_valid` for checking the validity of the rules in your domain.
!*/

use egg::*;
use ruler::*;

use num::bigint::{BigInt, RandBigInt, ToBigInt};
use num::{rational::Ratio, Signed, ToPrimitive, Zero};
use rand::Rng;
use rand_pcg::Pcg64;
use z3::ast::Ast;
use z3::*;

/// define `Constant` for rationals.
pub type Constant = Ratio<BigInt>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Sign {
    Positive,
    ContainsZero,
    Negative,
}

define_language! {
    /// Define the operators for the domain.
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "~" = Neg(Id),
        "fabs" = Abs(Id),
        "pow" = Pow([Id; 2]),
        "recip" = Reciprocal(Id),
        Num(Constant),
        Var(egg::Symbol),
    }
}

/// Return a non-zero constant.
fn mk_constant(n: &BigInt, d: &BigInt) -> Option<Constant> {
    if d.is_zero() {
        None
    } else {
        Some(Ratio::new(n.clone(), d.clone()))
    }
}

fn sign(interval: &Interval<Constant>) -> Sign {
    match (&interval.low, &interval.high) {
        (None, None) => Sign::ContainsZero,
        (Some(a), None) => {
            if a.is_positive() {
                Sign::Positive
            } else {
                Sign::ContainsZero
            }
        }
        (None, Some(b)) => {
            if b.is_negative() {
                Sign::Negative
            } else {
                Sign::ContainsZero
            }
        }
        (Some(a), Some(b)) => {
            if a.is_positive() && b.is_positive() {
                Sign::Positive
            } else if a.is_negative() && b.is_negative() {
                Sign::Negative
            } else {
                Sign::ContainsZero
            }
        }
    }
}

fn neg(interval: Interval<Constant>) -> Interval<Constant> {
    Interval::new(interval.high.map(|x| -x), interval.low.map(|x| -x))
}

fn abs(interval: Interval<Constant>) -> Interval<Constant> {
    let sign = sign(&interval);
    let zero = Ratio::new(0.to_bigint().unwrap(), 1.to_bigint().unwrap());

    let new_min = match (interval.low.clone(), interval.high.clone()) {
        (None, None) => zero,
        (Some(a), None) => a.max(zero).abs(),
        (None, Some(b)) => b.min(zero).abs(),
        (Some(a), Some(b)) => {
            if let Sign::ContainsZero = sign {
                zero
            } else {
                a.abs().min(b.abs())
            }
        }
    };

    let new_max = match (interval.low, interval.high) {
        (Some(a), Some(b)) => Some(a.abs().max(b.abs())),
        _ => None,
    };
    Interval::new(Some(new_min), new_max)
}

fn add(a: Interval<Constant>, b: Interval<Constant>) -> Interval<Constant> {
    let new_min = a.low.zip(b.low).map(|(x, y)| x + y);
    let new_max = a.high.zip(b.high).map(|(x, y)| x + y);
    Interval::new(new_min, new_max)
}

fn mul(a: Interval<Constant>, b: Interval<Constant>) -> Interval<Constant> {
    let mul = |(a, b): (Constant, Constant)| a * b;
    let (sign_a, sign_b) = (sign(&a), sign(&b));

    match (sign_a, sign_b) {
        (Sign::Negative, Sign::Negative) => {
            Interval::new(a.high.zip(b.high).map(mul), a.low.zip(b.low).map(mul))
        }
        (Sign::Positive, Sign::Positive) => {
            Interval::new(a.low.zip(b.low).map(mul), a.high.zip(b.high).map(mul))
        }

        (Sign::Positive, Sign::Negative) => {
            Interval::new(a.high.zip(b.low).map(mul), a.low.zip(b.high).map(mul))
        }
        (Sign::Negative, Sign::Positive) => {
            Interval::new(a.low.zip(b.high).map(mul), a.high.zip(b.low).map(mul))
        }

        (Sign::Positive, Sign::ContainsZero) => Interval::new(
            a.high.clone().zip(b.low).map(mul),
            a.high.zip(b.high).map(mul),
        ),
        (Sign::ContainsZero, Sign::Positive) => Interval::new(
            a.low.zip(b.high.clone()).map(mul),
            a.high.zip(b.high).map(mul),
        ),

        (Sign::Negative, Sign::ContainsZero) => Interval::new(
            a.low.clone().zip(b.high).map(mul),
            a.low.zip(b.low).map(mul),
        ),
        (Sign::ContainsZero, Sign::Negative) => Interval::new(
            a.high.zip(b.low.clone()).map(mul),
            a.low.zip(b.low).map(mul),
        ),

        (Sign::ContainsZero, Sign::ContainsZero) => {
            let al_bh = a.low.clone().zip(b.high.clone()).map(mul);
            let ah_bl = a.high.clone().zip(b.low.clone()).map(mul);
            let min = al_bh.zip(ah_bl).map(|(x, y)| x.min(y));

            let ah_bh = a.high.zip(b.high).map(mul);
            let al_bl = a.low.zip(b.low).map(mul);
            let max = ah_bh.zip(al_bl).map(|(x, y)| x.max(y));
            Interval::new(min, max)
        }
    }
}

fn recip(interval: Interval<Constant>) -> Interval<Constant> {
    let sign = sign(&interval);
    if let (Some(a), Some(b)) = (interval.low, interval.high) {
        if let Sign::ContainsZero = sign {
            Interval::default() // TODO: Can we tighten the interval when it contains zero?
        } else {
            Interval::new(Some(b.recip()), Some(a.recip()))
        }
    } else {
        Interval::default() // TODO: Can we tighten the interval when it contains infinity?
    }
}

impl SynthLanguage for Math {
    type Constant = Constant;

    /// Interpreter for rationals.
    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Neg(a) => map!(v, a => Some(-a)),
            Math::Add([a, b]) => map!(v, a, b => Some(a + b)),
            Math::Sub([a, b]) => map!(v, a, b => Some(a - b)),
            Math::Mul([a, b]) => map!(v, a, b => Some(a * b)),
            Math::Num(n) => vec![Some(n.clone()); cvec_len],
            Math::Var(_) => vec![],
            Math::Div([a, b]) => map!(v, a, b => {
                if b.is_zero() {
                    None
                } else{
                    Some(a / b)
                }
            }),
            Math::Abs(a) => map!(v, a => Some(a.abs())),
            Math::Pow([a, b]) => map!(v, a, b => {
                match b.to_i32() {
                    Some(b_int) => {
                        if a.is_zero() && b_int < 0 {
                            None
                        } else {
                            Some(a.pow(b_int))
                        }
                    }
                    None => None
                }

            }),
            Math::Reciprocal(a) => map!(v, a => {
                if a.is_zero() {
                    None
                } else {
                    Some(a.recip())
                }
            }),
        }
    }

    fn mk_interval(&self, egraph: &EGraph<Self, SynthAnalysis>) -> Interval<Self::Constant> {
        match self {
            Math::Num(n) => Interval::new(Some(n.clone()), Some(n.clone())),
            Math::Var(_) => Interval::default(),
            Math::Neg(a) => neg(egraph[*a].data.interval.clone()),
            Math::Abs(a) => abs(egraph[*a].data.interval.clone()),
            Math::Add([a, b]) => add(
                egraph[*a].data.interval.clone(),
                egraph[*b].data.interval.clone(),
            ),
            Math::Sub([a, b]) => add(
                egraph[*a].data.interval.clone(),
                neg(egraph[*b].data.interval.clone()),
            ),
            Math::Mul([a, b]) => mul(
                egraph[*a].data.interval.clone(),
                egraph[*b].data.interval.clone(),
            ),
            Math::Reciprocal(a) => recip(egraph[*a].data.interval.clone()),
            Math::Div([a, b]) => mul(
                egraph[*a].data.interval.clone(),
                recip(egraph[*b].data.interval.clone()),
            ),

            // TODO
            Math::Pow([_a, _b]) => Interval::default(),
        }
    }

    fn to_var(&self) -> Option<egg::Symbol> {
        if let Math::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::Num(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Math::Num(c)
    }

    /// Initialize an egraph with some constants and variables.
    fn init_synth(synth: &mut Synthesizer<Self>) {
        // disabled constants (TODO: validate input)
        let disabled_consts: Vec<&str> = if let Some(s) = &synth.params.disabled_consts {
            s.split(' ').collect()
        } else {
            vec![]
        };

        // this is for adding to the egraph, not used for cvec.
        let constants: Vec<Constant> = ["1", "0", "-1"]
            .iter()
            .filter(|s| !disabled_consts.iter().any(|x| x.eq(*s)))
            .map(|s| s.parse().unwrap())
            .collect();

        let mut consts: Vec<Option<Constant>> = vec![];

        for i in 0..synth.params.important_cvec_offsets {
            consts.push(mk_constant(
                &i.to_bigint().unwrap(),
                &(1.to_bigint().unwrap()),
            ));
            consts.push(mk_constant(
                &(-i.to_bigint().unwrap()),
                &(1.to_bigint().unwrap()),
            ));
        }

        consts.sort();
        consts.dedup();

        let mut consts = self_product(&consts, synth.params.variables);
        // add the necessary random values, if any
        for row in consts.iter_mut() {
            let n_samples = synth.params.n_samples;
            let svals: Vec<Constant> = sampler(&mut synth.rng, 8, 6, n_samples);
            let mut vals: Vec<Option<Constant>> = vec![];
            for v in svals {
                vals.push(Some(v));
            }
            // let vals: Vec<Option<Constant>> = (0..n_samples)
            //     .map(|_| mk_constant(&synth.rng.gen_bigint(32), &gen_denom(&mut synth.rng, 32)))
            //     .collect();
            row.extend(vals);
        }

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: consts[0].len(),
            constant_fold: if synth.params.no_constant_fold {
                ConstantFoldMethod::NoFold
            } else {
                ConstantFoldMethod::IntervalAnalysis
            },
            rule_lifting: false,
        });

        for (i, item) in consts.iter().enumerate().take(synth.params.variables) {
            let var = egg::Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));
            egraph[id].data.cvec = item.clone();
        }

        for n in &constants {
            egraph.add(Math::Num(n.clone()));
        }

        synth.egraph = egraph;
    }

    /// Term enumeration.
    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        // disabled operators (TODO: validate input)
        let disabled_ops: Vec<&str> = if let Some(s) = &synth.params.disabled_ops {
            s.split(' ').collect()
        } else {
            vec![]
        };

        // predicate if disabled
        let allowedp = |s| !disabled_ops.iter().any(|x| x.eq(&s));

        let extract = Extractor::new(&synth.egraph, NumberOfOps);
        let mut to_add = vec![];

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        log::info!(
            "Previous layer: {} enodes, {} eclasses ",
            synth.egraph.total_number_of_nodes(),
            synth.egraph.number_of_classes()
        );

        for i in synth.ids() {
            for j in synth.ids() {
                // 2ary ops
                if ids[&i] + ids[&j] + 1 != iter {
                    continue;
                }

                if iter > synth.params.no_constants_above_iter {
                    if synth.egraph[i].data.exact || synth.egraph[j].data.exact {
                        continue;
                    }
                } else if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                };

                if allowedp("+") {
                    to_add.push(Math::Add([i, j]));
                }
                if allowedp("-") {
                    to_add.push(Math::Sub([i, j]));
                }
                if allowedp("*") {
                    to_add.push(Math::Mul([i, j]));
                }
                if allowedp("/") {
                    to_add.push(Math::Div([i, j]));
                }
            }

            // 1ary ops
            if ids[&i] + 1 != iter || synth.egraph[i].data.exact {
                continue;
            }

            if allowedp("abs") {
                to_add.push(Math::Abs(i));
            }
            if allowedp("~") {
                to_add.push(Math::Neg(i));
            }
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    /// Check the validity of a rewrite rule.
    /// Depending on the value of `use_smt`, it either uses
    /// Z3 to verify the rules or fuzzing to validate them.
    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &egg::Pattern<Self>,
        rhs: &egg::Pattern<Self>,
    ) -> ValidationResult {
        if synth.params.use_smt {
            let mut cfg = z3::Config::new();
            cfg.set_timeout_msec(1000);
            let ctx = z3::Context::new(&cfg);
            let solver = z3::Solver::new(&ctx);
            let (lexpr, mut lasses) = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
            let (rexpr, mut rasses) = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
            lasses.append(&mut rasses);
            let all = &lasses[..];
            solver.assert(&lexpr._eq(&rexpr).not());
            match solver.check_assumptions(all) {
                // match solver.check() {
                SatResult::Unsat => ValidationResult::Valid,
                SatResult::Sat => {
                    // println!("z3 validation: failed for {} => {}", lhs, rhs);
                    ValidationResult::Invalid
                }
                SatResult::Unknown => {
                    // println!("z3 validation: unknown for {} => {}", lhs, rhs)
                    synth.smt_unknown += 1;
                    ValidationResult::Unknown
                }
            }
        } else {
            let n = synth.params.num_fuzz;
            let mut env = HashMap::default();

            for var in lhs.vars() {
                env.insert(var, vec![]);
            }

            for var in rhs.vars() {
                env.insert(var, vec![]);
            }

            for cvec in env.values_mut() {
                cvec.reserve(n);
                for s in sampler(&mut synth.rng, 8, 6, n) {
                    cvec.push(Some(s));
                }
                // for _ in 0..n {
                //     let numer = synth.rng.gen_bigint(32);
                //     let denom = gen_denom(&mut synth.rng, 32);
                //     cvec.push(Some(Ratio::new(numer, denom)));
                // }
            }

            let lvec = Self::eval_pattern(lhs, &env, n);
            let rvec = Self::eval_pattern(rhs, &env, n);
            ValidationResult::from(lvec == rvec)
        }
    }
}

/// Return a randomply sampled BigInt that is not 0
// Ratio::new will panic if the denom is 0
pub fn gen_pos(rng: &mut Pcg64, bits: u64) -> BigInt {
    let mut res: BigInt;
    loop {
        res = rng.gen_bigint(bits);
        if res != 0.to_bigint().unwrap() {
            break;
        }
    }
    res
}

/// A sampler that generates both big and small rationals.
pub fn sampler(rng: &mut Pcg64, b1: u64, b2: u64, num_samples: usize) -> Vec<Ratio<BigInt>> {
    let mut ret = vec![];
    for _ in 0..num_samples {
        let big = gen_pos(rng, b1);
        let small = gen_pos(rng, b2);
        let flip = rng.gen::<bool>();
        if flip {
            ret.push(Ratio::new(big, small));
        } else {
            ret.push(Ratio::new(small, big))
        }
    }
    ret
}

/// Convert expressions to Z3's syntax for using SMT based rule verification.
fn egg_to_z3<'a>(
    ctx: &'a z3::Context,
    expr: &[Math],
) -> (z3::ast::Real<'a>, Vec<z3::ast::Bool<'a>>) {
    let mut buf: Vec<z3::ast::Real> = vec![];
    let mut assumes: Vec<z3::ast::Bool> = vec![];
    let zero = z3::ast::Real::from_real(ctx, 0, 1);
    for node in expr.as_ref().iter() {
        match node {
            Math::Var(v) => buf.push(z3::ast::Real::new_const(ctx, v.to_string())),
            Math::Num(c) => buf.push(z3::ast::Real::from_real(
                ctx,
                (c.numer()).to_i32().unwrap(),
                (c.denom()).to_i32().unwrap(),
                // to_i32(c.numer()),
                // to_i32(c.denom()),
            )),
            Math::Add([a, b]) => buf.push(z3::ast::Real::add(
                ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Math::Sub([a, b]) => buf.push(z3::ast::Real::sub(
                ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Math::Mul([a, b]) => buf.push(z3::ast::Real::mul(
                ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Math::Div([a, b]) => {
                let denom = &buf[usize::from(*b)];
                let lez = z3::ast::Real::le(denom, &zero);
                let gez = z3::ast::Real::ge(denom, &zero);
                let assume = z3::ast::Bool::not(&z3::ast::Bool::and(ctx, &[&lez, &gez]));
                assumes.push(assume);
                buf.push(z3::ast::Real::div(
                    &buf[usize::from(*a)],
                    &buf[usize::from(*b)],
                ))
            }
            Math::Neg(a) => buf.push(z3::ast::Real::unary_minus(&buf[usize::from(*a)])),
            Math::Abs(a) => {
                let inner = &buf[usize::from(*a)].clone();
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Real::le(inner, &zero),
                    &z3::ast::Real::unary_minus(inner),
                    inner,
                ));
            }
            _ => unimplemented!(),
        }
    }
    (buf.pop().unwrap(), assumes)
}

/// Entry point
fn main() {
    Math::main()
}

#[cfg(test)]
mod test {
    use super::*;

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
    fn abs_interval_test() {
        assert_eq!(abs(interval(None, None)), interval(Some(0), None));
        assert_eq!(
            abs(interval(Some(-4), Some(10))),
            interval(Some(0), Some(10))
        );
        assert_eq!(
            abs(interval(Some(5), Some(10))),
            interval(Some(5), Some(10))
        );
        assert_eq!(
            abs(interval(Some(-10), Some(-1))),
            interval(Some(1), Some(10))
        );
        assert_eq!(abs(interval(None, Some(2))), interval(Some(0), None));
        assert_eq!(abs(interval(None, Some(-4))), interval(Some(4), None));
        assert_eq!(abs(interval(Some(-1), None)), interval(Some(0), None));
        assert_eq!(abs(interval(Some(1), None)), interval(Some(1), None));
    }

    #[test]
    fn neg_interval_test() {
        assert_eq!(neg(interval(None, None)), interval(None, None));
        assert_eq!(neg(interval(Some(10), None)), interval(None, Some(-10)));
        assert_eq!(neg(interval(Some(-10), None)), interval(None, Some(10)));
        assert_eq!(neg(interval(None, Some(10))), interval(Some(-10), None));
        assert_eq!(neg(interval(None, Some(-10))), interval(Some(10), None));
        assert_eq!(
            neg(interval(Some(5), Some(10))),
            interval(Some(-10), Some(-5))
        );
    }

    #[test]
    fn add_interval_test() {
        assert_eq!(
            add(interval(None, None), interval(None, None)),
            interval(None, None)
        );
        assert_eq!(
            add(interval(None, None), interval(Some(-10), Some(10))),
            interval(None, None)
        );
        assert_eq!(
            add(interval(Some(-10), Some(10)), interval(None, None)),
            interval(None, None)
        );
        assert_eq!(
            add(interval(Some(-20), Some(5)), interval(Some(-10), Some(10))),
            interval(Some(-30), Some(15))
        );
    }

    #[test]
    fn mul_interval_test() {
        assert_eq!(
            mul(interval(None, Some(-3)), interval(None, Some(-4))),
            interval(Some(12), None)
        );
        assert_eq!(
            mul(
                interval(Some(-100), Some(-2)),
                interval(Some(-50), Some(-20))
            ),
            interval(Some(40), Some(5000))
        );
        assert_eq!(
            mul(interval(Some(2), None), interval(Some(50), None)),
            interval(Some(100), None)
        );
        assert_eq!(
            mul(interval(Some(30), Some(50)), interval(Some(2), Some(3))),
            interval(Some(60), Some(150))
        );
        assert_eq!(
            mul(interval(Some(-10), Some(-5)), interval(Some(6), Some(100))),
            interval(Some(-1000), Some(-30))
        );
        assert_eq!(
            mul(interval(Some(3), Some(10)), interval(None, Some(-1))),
            interval(None, Some(-3))
        );
        assert_eq!(
            mul(interval(Some(2), Some(5)), interval(Some(-3), Some(4))),
            interval(Some(-15), Some(20))
        );
        assert_eq!(
            mul(interval(Some(-2), None), interval(Some(3), Some(4))),
            interval(Some(-8), None)
        );
        assert_eq!(
            mul(interval(None, None), interval(Some(-10), Some(-4))),
            interval(None, None)
        );
        assert_eq!(
            mul(interval(Some(-8), Some(6)), interval(Some(-3), Some(-2))),
            interval(Some(-18), Some(24))
        );
        assert_eq!(
            mul(interval(Some(-4), Some(6)), interval(Some(-8), Some(10))),
            interval(Some(-48), Some(60))
        );
        assert_eq!(
            mul(interval(Some(-100), Some(50)), interval(Some(-5), Some(7))),
            interval(Some(-700), Some(500))
        );
        assert_eq!(
            mul(interval(Some(-5), Some(6)), interval(Some(-4), Some(8))),
            interval(Some(-40), Some(48))
        );
        assert_eq!(
            mul(interval(Some(-4), Some(10)), interval(Some(-8), Some(6))),
            interval(Some(-80), Some(60))
        );
        assert_eq!(
            mul(interval(None, Some(10)), interval(Some(-5), Some(15))),
            interval(None, None)
        );
        assert_eq!(
            mul(interval(Some(-4), Some(10)), interval(Some(-8), None)),
            interval(None, None)
        );
    }

    #[test]
    fn recip_interval_test() {
        assert_eq!(recip(interval(None, None)), interval(None, None));
        assert_eq!(
            recip(interval(Some(50), Some(100))),
            Interval::new(
                Some(Ratio::new(1.to_bigint().unwrap(), 100.to_bigint().unwrap())),
                Some(Ratio::new(1.to_bigint().unwrap(), 50.to_bigint().unwrap())),
            )
        );
        assert_eq!(
            recip(interval(Some(-10), Some(-5))),
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
}
