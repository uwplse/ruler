use rand::Rng;
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
    fn mk_constant_id(c: usize, egraph: &mut EGraph<Self, SynthAnalysis>) -> Id {
        match c {
            0 => egraph.add(Nat::Z),
            _ => {
                let pred = Self::mk_constant_id(c - 1, egraph);
                egraph.add(Nat::S(pred))
            }
        }
    }
}

impl SynthLanguage for Nat {
    type Constant = usize;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Nat::Z => vec![Some(0); cvec_len],
            Nat::S(x) => map!(get_cvec, x => Some(*x + 1)),
            Nat::Add([x, y]) => map!(get_cvec, x, y => Some(*x + *y)),
            Nat::Mul([x, y]) => map!(get_cvec, x, y => Some(*x * *y)),
            Nat::Var(_) => vec![],
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        match self {
            Nat::Z => Interval::new(Some(0), Some(0)),
            Nat::S(x) => {
                let x_int = get_interval(x);
                let low = x_int
                    .low
                    .expect("There shouldn't be infinite lower bounds for Nat");
                Interval::new(Some(low + 1), x_int.high.map(|h| h + 1))
            }
            Nat::Add([x, y]) => {
                let x_int = get_interval(x);
                let y_int = get_interval(y);

                let low = x_int
                    .low
                    .zip(y_int.low)
                    .map(|(a, b)| a + b)
                    .expect("There shouldn't be infinite lower bounds for Nat");
                let high = x_int.high.zip(y_int.high).map(|(a, b)| a + b);

                Interval::new(Some(low), high)
            }
            Nat::Mul([x, y]) => {
                let x_int = get_interval(x);
                let y_int = get_interval(y);

                let low = x_int
                    .low
                    .zip(y_int.low)
                    .map(|(a, b)| a * b)
                    .expect("There shouldn't be infinite lower bounds for Nat");
                let high = x_int.high.zip(y_int.high).map(|(a, b)| a + b);

                Interval::new(Some(low), high)
            }
            Nat::Var(_) => Interval::new(Some(0), None),
        }
    }

    fn initialize_vars(synth: &mut Synthesizer<Self>, vars: Vec<String>) {
        let cvec_len = 10;
        let mut egraph: EGraph<Nat, SynthAnalysis> = EGraph::new(SynthAnalysis { cvec_len });
        for v in vars {
            let id = egraph.add(Nat::Var(Symbol::from(v)));
            let mut vals = vec![];
            for _ in 0..cvec_len {
                vals.push(Some(synth.rng.gen::<usize>()));
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
        match c {
            0 => Nat::Z,
            _ => Nat::S(Self::mk_constant_id(c - 1, egraph)),
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

fn main() {
    Nat::run_synth()
}
