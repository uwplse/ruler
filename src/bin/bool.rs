use egg::*;
use ruler::*;
use std::ops::*;

define_language! {
  pub enum Bool {
    "~" = Not(Id),
    "&" = And([Id; 2]),
    "|" = Or([Id; 2]),
    "^" = Xor([Id; 2]),
    "->" = Implies([Id; 2]),
    Lit(bool),
    Var(egg::Symbol),
  }
}

impl SynthLanguage for Bool {
    type Constant = bool;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Bool::Not(x) => map!(get_cvec, x => Some(x.not())),
            Bool::And([x, y]) => map!(get_cvec, x, y => Some(*x & *y)),
            Bool::Or([x, y]) => map!(get_cvec, x, y => Some(*x | *y)),
            Bool::Xor([x, y]) => map!(get_cvec, x, y => Some(*x ^ *y)),
            Bool::Implies([x, y]) => map!(get_cvec, x, y => Some(!(*x) | *y)),
            Bool::Lit(c) => vec![Some(*c); cvec_len],
            Bool::Var(_) => vec![],
        }
    }

    fn mk_interval(&self, egraph: &EGraph<Self, SynthAnalysis>) -> Interval<Self::Constant> {
        let get_interval = |x: &Id| {
            let interval = egraph[*x].data.interval.clone();
            (
                interval
                    .low
                    .expect("Bool shouldn't have infinite intervals"),
                interval
                    .high
                    .expect("Bool shouldn't have infinite intervals"),
            )
        };
        match self {
            Bool::Lit(c) => Interval::new(Some(*c), Some(*c)),
            Bool::Var(_) => Interval::new(Some(false), Some(true)),
            Bool::Not(x) => {
                let (low, high) = get_interval(x);
                Interval::new(Some(!high), Some(!low))
            }
            Bool::And([x, y]) => {
                let (x_low, x_high) = get_interval(x);
                let (y_low, y_high) = get_interval(y);
                Interval::new(Some(x_low && y_low), Some(x_high && y_high))
            }
            Bool::Or([x, y]) => {
                let (x_low, x_high) = get_interval(x);
                let (y_low, y_high) = get_interval(y);
                Interval::new(Some(x_low || y_low), Some(x_high || y_high))
            }
            Bool::Xor([x, y]) => {
                let (x_low, x_high) = get_interval(x);
                let (y_low, y_high) = get_interval(y);
                if x_low == x_high && y_low == y_high {
                    Interval::new(Some(x_low != y_low), Some(x_low != y_low))
                } else {
                    Interval::new(Some(false), Some(true))
                }
            }
            Bool::Implies([x, y]) => {
                let (x_low, x_high) = get_interval(x);
                let (y_low, y_high) = get_interval(y);
                Interval::new(Some(!x_high || y_low), Some(!x_low || y_high))
            }
        }
    }

    fn initialize_vars(synth: &mut Synthesizer<Self>, vars: Vec<String>) {
        println!("initializing vars: {:?}", vars);

        let consts = vec![Some(true), Some(false)];
        let cvecs = self_product(&consts, vars.len());

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: cvecs[0].len(),
        });

        for (i, v) in vars.iter().enumerate() {
            let id = egraph.add(Bool::Var(Symbol::from(v.clone())));
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }

        synth.egraph = egraph;
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Bool::Var(sym)
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Bool::Var(v) => Some(*v),
            _ => None,
        }
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult {
        ValidationResult::Valid
    }

    fn is_constant(&self) -> bool {
        matches!(self, Bool::Lit(_))
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Bool::Lit(c)
    }
}

fn main() {
    Bool::run_synth()
}
