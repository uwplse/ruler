use egg::*;
use rand::Rng;
use ruler::{HashMap, *};

define_language! {
    pub enum Nat {
        "Z" = Z,
        "S" = S(Id),
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),
        Var(egg::Symbol),
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Nat,
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

    type Type = Type;

    fn get_type(&self) -> Self::Type {
        Type::Nat
    }

    fn induction_principle(&self, egraph: &mut EGraph<Self, SynthAnalysis>) -> Option<Vec<Self>> {
        if let Self::Var(var) = self {
            let x = egraph.add(Nat::Var(format!("{var}-S.0").into()));
            Some(vec![Nat::Z, Nat::S(x)])
        } else {
            None
        }
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Nat::Z => vec![Some(0); cvec_len],
            Nat::S(x) => map!(v, x => Some(x.checked_add(1).unwrap())),
            Nat::Add([a, b]) => map!(v, a, b => Some(a.checked_add(*b).unwrap())),
            Nat::Mul([a, b]) => map!(v, a, b => Some(a.checked_mul(*b).unwrap())),
            Nat::Var(_) => vec![],
        }
    }

    fn mk_interval(&self, egraph: &EGraph<Self, SynthAnalysis>) -> Interval<Self::Constant> {
        match self {
            Nat::Z => Interval::new(Some(0), Some(0)),
            Nat::S(x) => {
                let x_int = egraph[*x].data.interval.clone();
                if let Some(l) = x_int.low {
                    Interval::new(Some(l + 1), x_int.high.map(|v| v + 1))
                } else {
                    panic!("There shouldn't be infinite lower bounds for Nat")
                }
            }
            Nat::Add([x, y]) => {
                let x_int = egraph[*x].data.interval.clone();
                let y_int = egraph[*y].data.interval.clone();

                let low = x_int
                    .low
                    .zip(y_int.low)
                    .map(|(a, b)| a + b)
                    .expect("There shouldn't be infinite lower bounds for Nat");

                let high = x_int.high.zip(y_int.high).map(|(a, b)| a + b);

                Interval::new(Some(low), high)
            }
            Nat::Mul([x, y]) => {
                let x_int = egraph[*x].data.interval.clone();
                let y_int = egraph[*y].data.interval.clone();

                let low = x_int
                    .low
                    .zip(y_int.low)
                    .map(|(a, b)| a * b)
                    .expect("There shouldn't be infinite lower bounds for Nat");

                let high = x_int.high.zip(y_int.high).map(|(a, b)| a * b);

                Interval::new(Some(low), high)
            }
            Nat::Var(_) => Interval::new(Some(0), None),
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Nat::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Nat::Var(sym)
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        match c {
            0 => Nat::Z,
            _ => Nat::S(Self::mk_constant_id(c - 1, egraph)),
        }
    }

    fn is_constant(&self) -> bool {
        matches!(self, Nat::Z)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let cvec_len = 10;
        let mut egraph: EGraph<Nat, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len,
            constant_fold: ConstantFoldMethod::IntervalAnalysis,
            rule_lifting: false,
        });

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Nat::Var(var));
            let mut vals = vec![];
            for _ in 0..cvec_len {
                vals.push(Some(synth.rng.gen::<usize>()));
            }
            egraph[id].data.cvec = vals.clone();
        }

        synth.egraph = egraph;
    }

    fn make_layer(_synth: &Synthesizer<Self>, _iter: usize) -> Vec<Self> {
        todo!()
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
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
            let mut vals = vec![];
            for _ in 0..n {
                vals.push(synth.rng.gen::<usize>());
            }
            for v in vals {
                cvec.push(Some(v));
            }
        }
        let lvec = Self::eval_pattern(lhs, &env, n);
        let rvec = Self::eval_pattern(rhs, &env, n);
        ValidationResult::from(lvec == rvec)
    }
}

fn main() {
    Nat::main()
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[test]
    fn test_nat() {
        env_logger::init();
        let args: &[String] = &[];
        let mut params = SynthParams::parse_from(args);
        params.eqsat_iter_limit = 5;
        params.no_constant_fold = true;
        let mut synth = Synthesizer::<Nat>::new(params);
        synth.egraph = EGraph::new(SynthAnalysis {
            cvec_len: 0,
            constant_fold: ConstantFoldMethod::NoFold,
            rule_lifting: false,
        });
        let eqs = ["(+ Z n) <=> n", "(+ (S x) n) <=> (S (+ x n))"];
        for eq in eqs.iter() {
            synth
                .all_eqs
                .insert(eq.to_string().into(), eq.parse().unwrap());
        }

        let left = "(+ a (+ b c))".parse().unwrap();
        let right = "(+ (+ a b) c)".parse().unwrap();
        println!("Goal: {left} = {right}");
        let var = "a".into();

        assert!(synth.prove_by_induction(&left, &right, var));
    }
}
