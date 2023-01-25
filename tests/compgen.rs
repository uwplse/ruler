use std::{fmt::Display, str::FromStr};

use itertools::Itertools;
use rand::{Rng, SeedableRng};
use rand_pcg::Pcg32;
use ruler::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub enum Value {
    Int(i64),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl FromStr for Value {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let int = str::parse::<i64>(s).map_err(|e| e.to_string())?;
        Ok(Value::Int(int))
    }
}

impl Value {
    fn int2<F>(lhs: &Self, rhs: &Self, f: F) -> Option<Self>
    where
        F: Fn(i64, i64) -> Option<i64>,
    {
        if let Value::Int(i) = lhs {
            if let Value::Int(j) = rhs {
                if let Some(x) = f(*i, *j) {
                    return Some(Value::Int(x));
                }
            }
        }
        None
    }

    fn sample(rng: &mut Pcg32, min: i64, max: i64, num_samples: usize) -> Vec<Value> {
        (0..num_samples)
            .map(|_| Value::Int(rng.gen_range(min, max)))
            .collect::<Vec<_>>()
    }
}

egg::define_language! {
  pub enum VecLang {
    "+" = Add([Id; 2]),
    "*" = Mul([Id; 2]),
    "-" = Minus([Id; 2]),
    "/" = Div([Id; 2]),
    Const(Value),
    Symbol(egg::Symbol),
  }
}

impl SynthLanguage for VecLang {
    type Constant = Value;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            VecLang::Add([l, r]) => map!(get_cvec, l, r => Value::int2(l, r, |x, y| Some(x + y))),
            VecLang::Mul([l, r]) => map!(get_cvec, l, r => Value::int2(l, r, |x, y| Some(x * y))),
            VecLang::Minus([l, r]) => map!(get_cvec, l, r => Value::int2(l, r, |x, y| Some(x - y))),
            VecLang::Div([l, r]) => map!(get_cvec, l, r => Value::int2(l, r, |x, y| {
              if y == 0 {
                None
              } else if x == 0 {
                Some(0)
              } else {
                Some(x / y)
              }
            })),
            VecLang::Const(n) => vec![Some(n.clone()); cvec_len],
            VecLang::Symbol(_) => vec![],
        }
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let mut rng = Pcg32::seed_from_u64(0);
        let cvec_size = vars.len() * 10;
        egraph.analysis.cvec_len = cvec_size;

        for v in vars.iter() {
            let id = egraph.add(VecLang::Symbol(egg::Symbol::from(v)));
            let mut cvec = vec![];

            cvec.extend(
                Value::sample(&mut rng, -100, 100, cvec_size)
                    .into_iter()
                    .map(Some),
            );

            egraph[id].data.cvec = cvec;
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            VecLang::Symbol(v) => Some(*v),
            _ => None,
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        VecLang::Symbol(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, VecLang::Const(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        VecLang::Const(c)
    }

    fn validate(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> ValidationResult {
        let mut rng = Pcg32::seed_from_u64(0);
        let mut env: HashMap<Var, Vec<Option<Value>>> = HashMap::default();

        for v in lhs.vars() {
            env.insert(v, vec![]);
        }
        for v in rhs.vars() {
            env.insert(v, vec![]);
        }

        let possibilities = vec![-10, -5, -2, -1, 0, 1, 2, 5, 10];
        for perms in possibilities.iter().permutations(env.keys().len()) {
            for (i, cvec) in perms.iter().zip(env.values_mut()) {
                cvec.push(Some(Value::Int(**i)));
            }
        }

        let mut length = 0;
        for cvec in env.values_mut() {
            cvec.extend(Value::sample(&mut rng, -100, 100, 10).into_iter().map(Some));
            length = cvec.len();
        }

        let lvec = Self::eval_pattern(lhs, &env, length);
        let rvec = Self::eval_pattern(rhs, &env, length);

        if lvec.iter().all(|x| x.is_none()) || rvec.iter().all(|x| x.is_none()) {
            return ValidationResult::Invalid;
        } else {
            let res = lvec.iter().zip(rvec.iter()).all(|tup| match tup {
                (Some(l), Some(r)) => l == r,
                (None, None) => true,
                _ => false,
            });
            match res {
                true => ValidationResult::Valid,
                false => ValidationResult::Invalid,
            }
        }
    }
}
