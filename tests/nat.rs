use std::time::Instant;

use num::{BigInt, Zero};
use num_bigint::ToBigInt;
use rand::{Rng, SeedableRng};
use rand_pcg::Pcg64;
use ruler::{
    enumo::{Ruleset, Scheduler, Workload},
    *,
};
use z3::ast::Ast;

egg::define_language! {
  pub enum Nat {
    "Z" = Z,
    "S" = S(Id),
    "+" = Add([Id; 2]),
    "*" = Mul([Id; 2]),
    Var(egg::Symbol),
  }
}

impl Nat {
    fn mk_constant_id(c: BigInt, egraph: &mut EGraph<Self, SynthAnalysis>) -> Id {
        if c.is_zero() {
            egraph.add(Nat::Z)
        } else {
            let pred = Self::mk_constant_id(c - 1, egraph);
            egraph.add(Nat::S(pred))
        }
    }
}

impl SynthLanguage for Nat {
    type Constant = BigInt;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Nat::Z => vec![Some(0.to_bigint().unwrap()); cvec_len],
            Nat::S(x) => map!(get_cvec, x => Some(x + 1)),
            Nat::Add([x, y]) => map!(get_cvec, x, y => Some(x + y)),
            Nat::Mul([x, y]) => map!(get_cvec, x, y => Some(x * y)),
            Nat::Var(_) => vec![],
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        match self {
            Nat::Z => Interval::new(Some(0.to_bigint().unwrap()), Some(0.to_bigint().unwrap())),
            Nat::S(x) => {
                let x_int = get_interval(x);
                let low = x_int
                    .low
                    .clone()
                    .expect("There shouldn't be infinite lower bounds for Nat");
                Interval::new(Some(low + 1), x_int.high.clone().map(|h| h + 1))
            }
            Nat::Add([x, y]) => {
                let x_int = get_interval(x);
                let y_int = get_interval(y);

                let low = x_int
                    .low
                    .clone()
                    .zip(y_int.low.clone())
                    .map(|(a, b)| a + b)
                    .expect("There shouldn't be infinite lower bounds for Nat");
                let high = x_int
                    .high
                    .clone()
                    .zip(y_int.high.clone())
                    .map(|(a, b)| a + b);

                Interval::new(Some(low), high)
            }
            Nat::Mul([x, y]) => {
                let x_int = get_interval(x);
                let y_int = get_interval(y);

                let low = x_int
                    .low
                    .clone()
                    .zip(y_int.low.clone())
                    .map(|(a, b)| a * b)
                    .expect("There shouldn't be infinite lower bounds for Nat");
                let high = x_int
                    .high
                    .clone()
                    .zip(y_int.high.clone())
                    .map(|(a, b)| a + b);

                Interval::new(Some(low), high)
            }
            Nat::Var(_) => Interval::new(Some(0.to_bigint().unwrap()), None),
        }
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let mut rng = Pcg64::seed_from_u64(0);
        for v in vars {
            let id = egraph.add(Nat::Var(Symbol::from(v)));
            let mut vals = vec![];
            for _ in 0..egraph.analysis.cvec_len {
                vals.push(Some(rng.gen::<u64>().to_bigint().unwrap()));
            }
            egraph[id].data.cvec = vals.clone();
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Nat::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Nat::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Nat::Z)
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        if c.is_zero() {
            Nat::Z
        } else {
            Nat::S(Self::mk_constant_id(c - 1.to_bigint().unwrap(), egraph))
        }
    }

    fn validate(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> ValidationResult {
        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
        let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
        solver.assert(&lexpr._eq(&rexpr).not());
        match solver.check() {
            z3::SatResult::Unsat => ValidationResult::Valid,
            z3::SatResult::Unknown => ValidationResult::Unknown,
            z3::SatResult::Sat => ValidationResult::Invalid,
        }
    }
}

impl Nat {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let mut candidates = Ruleset::cvec_match(&compressed);

        let num_prior = prior.len();
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

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Nat]) -> z3::ast::Int<'a> {
    let mut buf = vec![];
    let zero = z3::ast::Int::from_i64(ctx, 0);
    let one = z3::ast::Int::from_i64(ctx, 1);
    for node in expr.as_ref().iter() {
        match node {
            Nat::Z => buf.push(zero.clone()),
            Nat::S(x) => buf.push(z3::ast::Int::add(ctx, &[&buf[usize::from(*x)], &one])),
            Nat::Add([x, y]) => buf.push(z3::ast::Int::add(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Nat::Mul([x, y]) => buf.push(z3::ast::Int::mul(
                ctx,
                &[&buf[usize::from(*x)], &buf[usize::from(*y)]],
            )),
            Nat::Var(v) => buf.push(z3::ast::Int::new_const(ctx, v.to_string())),
        }
    }
    buf.pop().unwrap()
}

#[cfg(test)]
mod test {

    use ruler::enumo::{Ruleset, Workload};

    use super::*;

    fn iter_nat(n: usize) -> Workload {
        Workload::iter_lang(n, &["Z"], &["a", "b", "c"], &["S"], &["+", "*"])
    }

    #[test]
    fn simple() {
        let mut all_rules = Ruleset::default();
        let atoms3 = iter_nat(3);
        assert_eq!(atoms3.force().len(), 39);

        let rules3 = Nat::run_workload(
            atoms3,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
                derive_type: DeriveType::Lhs,
            },
        );
        all_rules.extend(rules3);

        let atoms4 = iter_nat(4);
        assert_eq!(atoms4.force().len(), 132);

        let rules4 = Nat::run_workload(
            atoms4,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
                derive_type: DeriveType::Lhs,
            },
        );
        all_rules.extend(rules4);

        let atoms5 = iter_nat(5);
        assert_eq!(atoms5.force().len(), 819);

        let rules5 = Nat::run_workload(
            atoms5,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
                derive_type: DeriveType::Lhs,
            },
        );
        all_rules.extend(rules5);

        let expected: Ruleset<Nat> = Ruleset::new(&[
            "(+ ?b ?a) ==> (+ ?a ?b)",
            "(* ?b ?a) ==> (* ?a ?b)",
            "(+ Z ?a) <=> ?a",
            "(* Z ?a) ==> Z",
            "?a <=> (* ?a (S Z))",
            "(+ ?b (S ?a)) <=> (S (+ ?b ?a))",
            "(+ ?b (* ?b ?a)) ==> (* ?b (S ?a))",
            "(* ?c (* ?b ?a)) ==> (* ?a (* ?b ?c))",
            "(+ ?c (+ ?b ?a)) ==> (+ ?a (+ ?b ?c))",
        ]);
        let (can, cannot) = all_rules.derive(&expected, Limits::default());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }
}
