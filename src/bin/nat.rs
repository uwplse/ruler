use egg::*;
use rand::RngCore;
use ruler::*;

define_language! {
  pub enum Nat {
    "Z" = Z,
    "S" = S(Id),
    "+" = Add([Id; 2]),
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

    fn convert_parse(s: &str) -> RecExpr<Self> {
        s.parse().unwrap()
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Nat::Z => vec![Some(0); cvec_len],
            Nat::S(x) => map!(v, x => Some(*x + 1)),
            Nat::Add([a, b]) => map!(v, a, b => Some(*a + *b)),
            Nat::Var(_) => vec![],
        }
    }

    fn mk_interval(&self, egraph: &EGraph<Self, SynthAnalysis>) -> Interval<Self::Constant> {
        match self {
            Nat::Var(_) => Interval::new(Some(0), None),
            Nat::Z => Interval::new(Some(0), Some(0)),
            Nat::S(x) => {
                let x_int = egraph[*x].data.interval.clone();
                if let Some(l) = x_int.low {
                    Interval::new(Some(l + 1), x_int.high.map(|v| v + 1))
                } else {
                    panic!("There shouldn't be infinite lower bounds for Nats")
                }
            }
            Nat::Add([x, y]) => {
                let x_int = egraph[*x].data.interval.clone();
                let y_int = egraph[*y].data.interval.clone();

                let low = match (x_int.low, y_int.low) {
                    (Some(a), Some(b)) => Some(a + b),
                    (_, _) => panic!("There shouldn't be infinite lower bounds for Nats"),
                };

                let high = match (x_int.high, y_int.high) {
                    (Some(a), Some(b)) => Some(a + b),
                    (_, _) => None,
                };
                Interval::new(low, high)
            }
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

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Nat::Z = self {
            Some(&0)
        } else {
            // TODO: Should we try to do something smarter here to get constants like S (S Z)?
            None
        }
    }

    fn mk_constant(c: Self::Constant, egraph: &EGraph<Self, SynthAnalysis>) -> Self {
        match c {
            0 => Nat::Z,
            _ => {
                let pred = Self::mk_constant_id(c - 1, &mut egraph.clone());
                Nat::S(pred)
            }
        }
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut egraph: EGraph<Nat, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 10,
            constant_fold: ConstantFoldMethod::IntervalAnalysis,
            rule_lifting: false,
        });

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Nat::Var(var));
            let mut vals = vec![];
            let rng = &mut synth.rng;

            for _ in 0..10 {
                vals.push(Some(rng.next_u32() as usize));
            }
            egraph[id].data.cvec = vals.clone();
        }
        synth.egraph = egraph;
    }

    fn make_layer(_synth: &Synthesizer<Self>, _iter: usize) -> Vec<Self> {
        // TODO - Do we need this if we'll always be using workloads?
        vec![]
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult {
        // TODO
        ValidationResult::Valid
    }
}

fn main() {
    Nat::main()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mk_constant_id() {
        let mut egraph: EGraph<Nat, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 1,
            constant_fold: ConstantFoldMethod::NoFold,
            rule_lifting: false,
        });
        let zero = egraph.add(Nat::Z);
        let one = egraph.add(Nat::S(zero));
        let two = egraph.add(Nat::S(one));
        let three = egraph.add(Nat::S(two));

        let zero_c = Nat::mk_constant_id(0, &mut egraph.clone());
        assert_eq!(zero, zero_c);

        let one_c = Nat::mk_constant_id(1, &mut egraph.clone());
        assert_eq!(one, one_c);

        let two_c = Nat::mk_constant_id(2, &mut egraph.clone());
        assert_eq!(two, two_c);

        let three_c = Nat::mk_constant_id(3, &mut egraph.clone());
        assert_eq!(three, three_c);
    }
}
