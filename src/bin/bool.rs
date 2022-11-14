use egg::*;
use ruler::*;
use std::ops::*;
use SynthLanguage;

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
            Bool::Not(a) => {
                let cvec = get_cvec(a);
                cvec.iter()
                    .map(|a| match a {
                        Some(x) => Some(x.not()),
                        None => None,
                    })
                    .collect()
            }
            Bool::And([x, y]) => {
                let x_cvec = get_cvec(x);
                let y_cvec = get_cvec(y);
                x_cvec
                    .iter()
                    .zip(y_cvec.iter())
                    .map(|(x, y)| match (x, y) {
                        (Some(x), Some(y)) => Some(*x & *y),
                        _ => None,
                    })
                    .collect()
            }
            Bool::Or([x, y]) => {
                let x_cvec = get_cvec(x);
                let y_cvec = get_cvec(y);
                x_cvec
                    .iter()
                    .zip(y_cvec.iter())
                    .map(|(x, y)| match (x, y) {
                        (Some(x), Some(y)) => Some(*x | *y),
                        _ => None,
                    })
                    .collect()
            }
            Bool::Xor([x, y]) => {
                let x_cvec = get_cvec(x);
                let y_cvec = get_cvec(y);
                x_cvec
                    .iter()
                    .zip(y_cvec.iter())
                    .map(|(x, y)| match (x, y) {
                        (Some(x), Some(y)) => Some(*x ^ *y),
                        _ => None,
                    })
                    .collect()
            }
            Bool::Implies([x, y]) => {
                let x_cvec = get_cvec(x);
                let y_cvec = get_cvec(y);
                x_cvec
                    .iter()
                    .zip(y_cvec.iter())
                    .map(|(x, y)| match (x, y) {
                        (Some(x), Some(y)) => Some(!(*x) || *y),
                        _ => None,
                    })
                    .collect()
            }
            Bool::Lit(c) => vec![Some(*c); cvec_len],
            Bool::Var(_) => vec![],
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
}

fn main() {
    Bool::run_synth()
}
