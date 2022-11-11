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

    fn mk_var(sym: egg::Symbol) -> Self {
        Bool::Var(sym)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        todo!()
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
