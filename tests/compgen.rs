use itertools::Itertools;
use rand::{Rng, SeedableRng};
use rand_pcg::Pcg32;
use ruler::*;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub enum Value {
    // starts with i
    Int(i64),
    // starts with [
    List(Vec<Value>),
    // starts with <
    Vec(Vec<Value>),
    // starts with b
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::List(l) => write!(f, "{:?}", l),
            Value::Vec(v) => {
                write!(
                    f,
                    "(Vec {})",
                    v.iter().map(|x| format!("{}", x)).collect_vec().join(" ")
                )
            }
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
    fn int1<F>(arg: &Self, f: F) -> Option<Value>
    where
        F: Fn(i64) -> Value,
    {
        if let Value::Int(val) = arg {
            Some(f(*val))
        } else {
            None
        }
    }

    fn int2<F>(lhs: &Self, rhs: &Self, f: F) -> Option<Value>
    where
        F: Fn(i64, i64) -> Value,
    {
        if let (Value::Int(lv), Value::Int(rv)) = (lhs, rhs) {
            Some(f(*lv, *rv))
        } else {
            None
        }
    }

    fn bool2<F>(lhs: &Self, rhs: &Self, f: F) -> Option<Value>
    where
        F: Fn(bool, bool) -> Value,
    {
        if let (Value::Bool(lv), Value::Bool(rv)) = (lhs, rhs) {
            Some(f(*lv, *rv))
        } else {
            None
        }
    }

    fn vec1<F>(val: &Self, f: F) -> Option<Value>
    where
        F: Fn(&[Value]) -> Option<Value>,
    {
        if let Value::Vec(v) = val {
            f(v)
        } else {
            None
        }
    }

    fn vec2<F>(lhs: &Self, rhs: &Self, f: F) -> Option<Value>
    where
        F: Fn(&[Value], &[Value]) -> Option<Value>,
    {
        if let (Value::Vec(v1), Value::Vec(v2)) = (lhs, rhs) {
            if v1.len() == v2.len() {
                f(v1, v2)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn vec3<F>(v1: &Self, v2: &Self, v3: &Self, f: F) -> Option<Value>
    where
        F: Fn(&[Value], &[Value], &[Value]) -> Option<Value>,
    {
        if let (Value::Vec(v1), Value::Vec(v2), Value::Vec(v3)) = (v1, v2, v3) {
            if v1.len() == v2.len() && v2.len() == v3.len() {
                f(v1, v2, v3)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn vec2_op<F>(lhs: &Self, rhs: &Self, f: F) -> Option<Value>
    where
        F: Fn(&Value, &Value) -> Option<Value>,
    {
        Self::vec2(lhs, rhs, |lhs, rhs| {
            lhs.iter()
                .zip(rhs)
                .map(|(l, r)| f(l, r))
                .collect::<Option<Vec<Value>>>()
                .map(Value::Vec)
        })
    }

    fn sample(rng: &mut Pcg32, min: i64, max: i64, num_samples: usize) -> Vec<Value> {
        (0..num_samples)
            .map(|_| Value::Int(rng.gen_range(min, max)))
            .collect::<Vec<_>>()
    }
}

fn integer_division(a: i64, b: i64) -> Option<i64> {
    if b == 0 {
        None
    } else if a == 0 {
        Some(0)
    } else {
        Some(a / b)
    }
    // if a / b != 0
}

egg::define_language! {
  pub enum VecLang {
        // Id is a key to identify EClasses within an EGraph, represents
        // children nodes
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),

        "neg" = Neg([Id; 1]),

        // Lists have a variable number of elements
        "List" = List(Box<[Id]>),

        // Vectors have width elements
        "Vec" = Vec(Box<[Id]>),

        // Vector with all literals
        "LitVec" = LitVec(Box<[Id]>),

        "Get" = Get([Id; 2]),

        // Used for partitioning and recombining lists
        "Concat" = Concat([Id; 2]),

        // Vector operations that take 2 vectors of inputs
        "VecAdd" = VecAdd([Id; 2]),
        "VecMinus" = VecMinus([Id; 2]),
        "VecMul" = VecMul([Id; 2]),
        "VecDiv" = VecDiv([Id; 2]),
        "VecMulSgn" = VecMulSgn([Id; 2]),

        // Vector operations that take 1 vector of inputs
        "VecNeg" = VecNeg([Id; 1]),

        // MAC takes 3 lists: acc, v1, v2
        "VecMAC" = VecMAC([Id; 3]),

    // Comment
        Const(Value),

        // language items are parsed in order, and we want symbol to
        // be a fallback, so we put it last.
        // `Symbol` is an egg-provided interned string type
        Symbol(egg::Symbol),
    }
}

impl SynthLanguage for VecLang {
    type Constant = Value;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            VecLang::Const(i) => vec![Some(i.clone()); cvec_len],
            VecLang::Add([l, r]) => map!(get, l, r => {
                Value::int2(l, r, |l, r| Value::Int(l + r))
            }),
            VecLang::Mul([l, r]) => map!(get, l, r => {
                Value::int2(l, r, |l, r| Value::Int(l * r))
            }),
            VecLang::Neg([x]) => {
                map!(get, x => Value::int1(x, |x| Value::Int(-x)))
            }
            VecLang::List(l) => l
                .iter()
                .fold(vec![Some(vec![]); cvec_len], |mut acc, item| {
                    acc.iter_mut().zip(get(item)).for_each(|(mut v, i)| {
                        if let (Some(v), Some(i)) = (&mut v, i) {
                            v.push(i.clone());
                        } else {
                            *v = None;
                        }
                    });
                    acc
                })
                .into_iter()
                .map(|acc| acc.map(Value::List))
                .collect::<Vec<_>>(),
            VecLang::Vec(l) => l
                .iter()
                .fold(vec![Some(vec![]); cvec_len], |mut acc, item| {
                    acc.iter_mut().zip(get(item)).for_each(|(mut v, i)| {
                        if let (Some(v), Some(i)) = (&mut v, i) {
                            v.push(i.clone());
                        } else {
                            *v = None;
                        }
                    });
                    acc
                })
                .into_iter()
                .map(|acc| acc.map(Value::Vec))
                .collect::<Vec<_>>(),
            VecLang::LitVec(l) => l
                .iter()
                .fold(vec![Some(vec![]); cvec_len], |mut acc, item| {
                    acc.iter_mut().zip(get(item)).for_each(|(mut v, i)| {
                        if let (Some(v), Some(i)) = (&mut v, i) {
                            v.push(i.clone());
                        } else {
                            *v = None;
                        }
                    });
                    acc
                })
                .into_iter()
                .map(|acc| acc.map(Value::Vec))
                .collect::<Vec<_>>(),
            #[rustfmt::skip]
            VecLang::Get([l, i]) => map!(get, l, i => {
                if let (Value::Vec(v), Value::Int(idx)) = (l, i) {
                    // get index and clone the inner Value if there is one
                    v.get(*idx as usize).cloned()
                } else {
                    None
                }
            }),
            #[rustfmt::skip]
            VecLang::Concat([l, r]) => map!(get, l, r => {
                Value::vec2(l, r, |l, r| {
                    Some(Value::List(
                        l.iter().chain(r).cloned().collect::<Vec<_>>(),
                    ))
                })
            }),
            #[rustfmt::skip]
            VecLang::VecAdd([l, r]) => map!(get, l, r => {
                Value::vec2_op(l, r, |l, r| {
                    Value::int2(l, r, |l, r| Value::Int(l + r))
                })
            }),
            #[rustfmt::skip]
            VecLang::VecMinus([l, r]) => map!(get, l, r => {
                Value::vec2_op(l, r, |l, r| {
                    Value::int2(l, r, |l, r| Value::Int(l - r))
                })
            }),
            #[rustfmt::skip]
            VecLang::VecMul([l, r]) => map!(get, l, r => {
                Value::vec2_op(l, r, |l, r| {
                    Value::int2(l, r, |l, r| Value::Int(l * r))
                })
            }),
            #[rustfmt::skip]
            VecLang::VecDiv([l, r]) => map!(get, l, r => {
                Value::vec2_op(l, r, |l, r| match (l, r) {
                    (Value::Int(a), Value::Int(b)) => {
                        integer_division(*a, *b).map(Value::Int)
                    }
                    _ => None,
                })
            }),
            VecLang::VecMulSgn(_) => todo!(),
            #[rustfmt::skip]
            VecLang::VecNeg([l]) => map!(get, l => {
                Value::vec1(l, |l| {
                    if l.iter().all(|x| matches!(x, Value::Int(_))) {
                        Some(Value::Vec(
                            l.iter()
                                .map(|tup| match tup {
                                    Value::Int(a) => Value::Int(-a),
                                    x => panic!("NEG: Ill-formed: {}", x),
                                })
                                .collect::<Vec<_>>(),
                        ))
                    } else {
                        None
                    }
                })
            }),
            #[rustfmt::skip]
            VecLang::VecMAC([acc, v1, v2]) => map!(get, v1, v2, acc => {
                Value::vec3(v1, v2, acc, |v1, v2, acc| {
                    v1.iter()
                        .zip(v2.iter())
                        .zip(acc.iter())
                        .map(|tup| match tup {
                            ((Value::Int(v1), Value::Int(v2)), Value::Int(acc))
				=> Some(Value::Int((v1 * v2) + acc)),
                            _ => None,
                        })
                        .collect::<Option<Vec<Value>>>()
                        .map(Value::Vec)
                })
            }),
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

mod test {
    use ruler::enumo::{Ruleset, Workload};

    use super::*;

    fn iter_rat(n: usize) -> Workload {
        Workload::iter_lang(n, &[], &["a", "b", "c", "d"], &[], &["+"])
    }

    fn iter_vec(n: usize) -> Workload {
        Workload::iter_lang(n, &[], &[], &[], &["VecAdd"])
    }

    #[test]
    fn simple() {
        let mut rules: Ruleset<VecLang> = Ruleset::default();
        let atoms3 = iter_rat(3);
        let rules3 = VecLang::run_workload(atoms3.clone(), rules.clone(), Limits::default());
        rules.extend(rules3);
        let atoms4 = iter_rat(4);
        let rules4 = VecLang::run_workload(atoms4, rules.clone(), Limits::default());
        rules.extend(rules4);
        let atoms5 = iter_rat(5);
        let rules5 = VecLang::run_workload(atoms5, rules.clone(), Limits::default());
        rules.extend(rules5);

        // Vec (+ _ _) (+ _ _)
        // VecAdd (+ _ _) (+ _ _)

        // Vec (+ 1 2) (+ 3 4) --> VecAdd (Vec 1 3) (Vec 2 4)

        let vec_of_adds = Workload::from_vec(vec!["(Vec x x)"]);
        let wkld2 = vec_of_adds.plug("x", &atoms3);

        let vec_add = Workload::from_vec(vec!["(VecAdd x x)"]);
        let lit_vecs = Workload::from_vec(vec!["(Vec y y)"])
            .plug("y", &Workload::from_vec(vec!["a", "b", "c", "d"]));
        let wkld3 = vec_add.plug("x", &lit_vecs);

        let my_wkld = wkld2.append(&wkld3);

        println!("Starting rules: ");
        for (name, _) in rules.0.clone() {
            println!("{}", name);
        }

        let wkld = iter_vec(3);
        // let actual_wkld = wkld.append(&wkld2);
        for term in my_wkld.force() {
            println!("{}", term);
        }

        println!("New rules:");
        let vec_rules = VecLang::run_workload(my_wkld, rules.clone(), Limits::default());
        for (name, _) in vec_rules.0 {
            println!("{}", name);
        }
    }
}
