use num::{BigInt, Zero};
use num_bigint::ToBigInt;
use rand::{Rng, SeedableRng};
use rand_pcg::Pcg64;
use ruler::*;
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

    fn initialize_vars(synth: &mut Synthesizer<Self>, vars: Vec<String>) {
        let mut rng = Pcg64::seed_from_u64(0);
        let cvec_len = 10;
        let mut egraph: EGraph<Nat, SynthAnalysis> = EGraph::new(SynthAnalysis { cvec_len });
        for v in vars {
            let id = egraph.add(Nat::Var(Symbol::from(v)));
            let mut vals = vec![];
            for _ in 0..cvec_len {
                vals.push(Some(rng.gen::<u64>().to_bigint().unwrap()));
            }
            egraph[id].data.cvec = vals.clone();
        }

        synth.egraph = egraph;
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

    fn validate(
        _synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
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

        let rules3 = Nat::run_workload_with_limits(atoms3, all_rules.clone(), 3, 30, 1000000);
        assert_eq!(rules3.len(), 4);
        all_rules.extend(rules3);

        let atoms4 = iter_nat(4);
        assert_eq!(atoms4.force().len(), 132);

        let rules4 = Nat::run_workload_with_limits(atoms4, all_rules.clone(), 3, 30, 1000000);
        assert_eq!(rules4.len(), 3);
        all_rules.extend(rules4);

        let atoms5 = iter_nat(5);
        assert_eq!(atoms5.force().len(), 819);

        let rules5 = Nat::run_workload_with_limits(atoms5, all_rules.clone(), 3, 30, 1000000);
        assert_eq!(rules5.len(), 5);
        all_rules.extend(rules5);

        assert_eq!(all_rules.len(), 12);
    }
}
