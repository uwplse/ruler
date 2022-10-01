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

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Nat::Z => vec![Some(0); cvec_len],
            Nat::S(x) => map!(v, x => Some(*x + 1)),
            Nat::Add([a, b]) => map!(v, a, b => Some(*a + *b)),
            Nat::Mul([a, b]) => map!(v, a, b => Some(*a * *b)),
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
    ) -> ValidationResult<Self> {
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
