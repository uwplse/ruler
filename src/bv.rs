use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::*;

/// General bitvector implementation.
#[derive(Copy, Clone, Hash, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BV<const N: u32>(pub Inner);

type Inner = u32;
const INNER_N: u32 = 32;

impl<const N: u32> BV<N> {
    pub const ZERO: Self = Self(0);
    pub const ALL_ONES: Self = Self((!(0)) >> (INNER_N - N));
    pub const NEG_ONE: Self = Self::ALL_ONES;
    pub const MIN: Self = Self(1 << (N - 1));
    pub const MAX: Self = Self(Self::ALL_ONES.0 >> 1);

    pub fn new(n: impl Into<Inner>) -> Self {
        Self(n.into() & Self::ALL_ONES.0)
    }

    pub fn wrapping_add(self, rhs: Self) -> Self {
        Self::new(self.0.wrapping_add(rhs.0))
    }

    pub fn wrapping_sub(self, rhs: Self) -> Self {
        Self::new(self.0.wrapping_sub(rhs.0))
    }

    pub fn wrapping_mul(self, rhs: Self) -> Self {
        Self::new(self.0.wrapping_mul(rhs.0))
    }

    pub fn wrapping_neg(self) -> Self {
        Self::new(self.0.wrapping_neg())
    }

    pub fn my_shl(self, rhs: Self) -> Self {
        if rhs.0 >= N {
            Self::ZERO
        } else {
            Self::new(self.0 << rhs.0)
        }
    }

    pub fn my_shr(self, rhs: Self) -> Self {
        if rhs.0 >= N {
            Self::ZERO
        } else {
            Self::new(self.0 >> rhs.0)
        }
    }
}

impl<const N: u32> Not for BV<N> {
    type Output = Self;
    fn not(self) -> Self {
        Self::new(self.0.not())
    }
}

impl<const N: u32> BitAnd for BV<N> {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Self::new(self.0.bitand(rhs.0))
    }
}

impl<const N: u32> BitOr for BV<N> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self::new(self.0.bitor(rhs.0))
    }
}

impl<const N: u32> BitXor for BV<N> {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self {
        Self::new(self.0.bitxor(rhs.0))
    }
}

impl<const N: u32> fmt::Debug for BV<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<const N: u32> fmt::Display for BV<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl<const N: u32> Distribution<BV<N>> for rand::distributions::Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> BV<N> {
        let inner: Inner = rng.gen();
        inner.into()
    }
}

impl<const N: u32> std::str::FromStr for BV<N> {
    type Err = std::num::ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(stripped) = s.strip_prefix("#b") {
            let i = Inner::from_str_radix(stripped, 2).unwrap();
            return Ok(Self::new(i));
        }
        s.parse::<Inner>().map(Self::new)
    }
}

impl<const N: u32> From<Inner> for BV<N> {
    fn from(t: Inner) -> Self {
        Self::new(t)
    }
}

/// Macro for specializing `BV` to different sized bitvectors.
#[macro_export]
macro_rules! impl_bv {
    ($n:literal) => {
        use egg::*;
        use $crate::*;

        use std::ops::*;

        use rand_pcg::Pcg64;

        use rand::prelude::*;
        use serde::{Deserialize, Serialize};
        use std::fmt;

        pub type BV = $crate::BV::<$n>;

        egg::define_language! {
            /// Define the operators for the domain.
            pub enum Math {
                "+" = Add([Id; 2]),
                "--" = Sub([Id; 2]),
                "*" = Mul([Id; 2]),
                "-" = Neg(Id),
                "~" = Not(Id),
                "<<" = Shl([Id; 2]),
                ">>" = Shr([Id; 2]),
                "&" = And([Id; 2]),
                "|" = Or([Id; 2]),
                "^" = Xor([Id; 2]),
                Num(BV),
                Var(egg::Symbol),
            }
        }

        impl SynthLanguage for Math {
            type Constant = BV;

            /// Converting CVC4's rewrites to Ruler's BV grammar syntax.
            fn convert_parse(s: &str) -> RecExpr<Self> {
                let s = s
                    .replace("bvadd", "+")
                    .replace("bvsub", "--")
                    .replace("bvmul", "*")
                    .replace("bvand", "&")
                    .replace("bvor", "|")
                    .replace("bvneg", "-")
                    .replace("bvnot", "~")
                    .replace("bvlshr", ">>")
                    .replace("bvshl", "<<")
                    .replace("and", "&")
                    .replace("xor", "^")
                    .replace("or", "|")
                    .replace("not", "~");
                s.parse().unwrap()
            }

            fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
            where
                F: FnMut(&'a Id) -> &'a CVec<Self>,
            {
                match self {
                    Math::Neg(a) => map!(v, a => Some(a.wrapping_neg())),
                    Math::Not(a) => map!(v, a => Some(a.not())),

                    Math::Add([a, b]) => map!(v, a, b => Some(a.wrapping_add(*b))),
                    Math::Sub([a, b]) => map!(v, a, b => Some(a.wrapping_sub(*b))),
                    Math::Mul([a, b]) => map!(v, a, b => Some(a.wrapping_mul(*b))),

                    Math::Shl([a, b]) => map!(v, a, b => Some(a.my_shl(*b))),
                    Math::Shr([a, b]) => map!(v, a, b => Some(a.my_shr(*b))),

                    Math::And([a, b]) => map!(v, a, b => Some(*a & *b)),
                    Math::Or([a, b]) => map!(v, a, b => Some(*a | *b)),
                    Math::Xor([a, b]) => map!(v, a, b => Some(*a ^ *b)),

                    Math::Num(n) => vec![Some(n.clone()); cvec_len],
                    Math::Var(_) => vec![],
                }
            }

            fn to_var(&self) -> Option<Symbol> {
                if let Math::Var(sym) = self {
                    Some(*sym)
                } else {
                    None
                }
            }

            fn mk_var(sym: Symbol) -> Self {
                Math::Var(sym)
            }

            fn to_constant(&self) -> Option<&Self::Constant> {
                if let Math::Num(n) = self {
                    Some(n)
                } else {
                    None
                }
            }

            fn mk_constant(c: Self::Constant) -> Self {
                Math::Num(c)
            }

            fn init_synth(synth: &mut Synthesizer<Self>, _workload: Vec<RecExpr<Self>>) {
                let mut consts: Vec<Option<BV>> = vec![];
                if synth.params.complete_cvec {
                    consts = (0..1u64 << $n).map(|i| Some((i as u32).into())).collect();
                } else {
                    for i in 0..synth.params.important_cvec_offsets {
                        let i = BV::from(i);
                        consts.push(Some(BV::MIN.wrapping_add(i)));
                        consts.push(Some(BV::MAX.wrapping_sub(i)));
                        consts.push(Some(i));
                        consts.push(Some(i.wrapping_neg()));
                    }
                }
                consts.sort();
                consts.dedup();

                let mut consts = self_product(&consts, synth.params.variables);
                // add the necessary random values, if any
                for row in consts.iter_mut() {
                    let n_samples = synth.params.n_samples;
                    let vals = std::iter::repeat_with(|| synth.rng.gen::<BV>());
                    row.extend(vals.take(n_samples).map(Some));
                }
                // println!("cvec len: {}", consts[0].len());

                let mut egraph = EGraph::new(SynthAnalysis {
                    cvec_len: consts[0].len(),
                    constant_fold: if synth.params.no_constant_fold {
                        ConstantFoldMethod::NoFold
                    } else {
                        ConstantFoldMethod::CvecMatching
                    },
                    rule_lifting: false,
                });

                // egraph.add(Math::Num(BV::ZERO));
                // egraph.add(Math::Num(1.into()));
                // egraph.add(Math::Num(2.into()));
                // egraph.add(Math::Num(BV::MIN));
                // egraph.add(Math::Num(BV::MAX));

                for i in 0..synth.params.variables {
                    let var = Symbol::from(letter(i));
                    let id = egraph.add(Math::Var(var));
                    egraph[id].data.cvec = consts[i].clone();
                }

                synth.egraph = egraph;
            }

            fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
                let extract = Extractor::new(&synth.egraph, NumberOfOps);

                // maps ids to n_ops
                let ids: HashMap<Id, usize> = synth
                    .ids()
                    .map(|id| (id, extract.find_best_cost(id)))
                    .collect();

                let mut to_add = vec![];
                for i in synth.ids() {
                    for j in synth.ids() {
                        if ids[&i] + ids[&j] + 1 != iter {
                            continue;
                        }

                        to_add.push(Math::Add([i, j]));
                        to_add.push(Math::Sub([i, j]));
                        to_add.push(Math::Mul([i, j]));

                        if !synth.params.no_shift {
                            to_add.push(Math::Shl([i, j]));
                            to_add.push(Math::Shr([i, j]));
                        }

                        to_add.push(Math::And([i, j]));
                        to_add.push(Math::Or([i, j]));
                        // if !synth.params.no_xor {
                        //     to_add.push(Math::Xor([i, j]));
                        // }
                    }
                    if ids[&i] + 1 != iter {
                        continue;
                    }

                    to_add.push(Math::Not(i));
                    to_add.push(Math::Neg(i));
                }

                log::info!("Made a layer of {} enodes", to_add.len());
                to_add
            }

            fn validate(
                synth: &mut Synthesizer<Self>,
                lhs: &Pattern<Self>,
                rhs: &Pattern<Self>
            ) -> ValidationResult {
                use z3::{*, ast::Ast};

                fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Math]) -> z3::ast::BV<'a> {
                    let mut buf: Vec<z3::ast::BV> = vec![];
                    for node in expr.as_ref().iter() {
                        match node {
                            Math::Var(v) => buf.push(z3::ast::BV::new_const(&ctx, v.to_string(), $n)),
                            Math::Num(c) => buf.push(z3::ast::BV::from_u64(&ctx, c.0 as u64, $n)),
                            Math::Add([a, b]) => buf.push(buf[usize::from(*a)].bvadd(&buf[usize::from(*b)])),
                            Math::Sub([a, b]) => buf.push(buf[usize::from(*a)].bvsub(&buf[usize::from(*b)])),
                            Math::Mul([a, b]) => buf.push(buf[usize::from(*a)].bvmul(&buf[usize::from(*b)])),
                            Math::Shl([a, b]) => buf.push(buf[usize::from(*a)].bvshl(&buf[usize::from(*b)])),
                            Math::Shr([a, b]) => buf.push(buf[usize::from(*a)].bvlshr(&buf[usize::from(*b)])),
                            Math::And([a, b]) => buf.push(buf[usize::from(*a)].bvand(&buf[usize::from(*b)])),
                            Math::Or([a, b]) => buf.push(buf[usize::from(*a)].bvor(&buf[usize::from(*b)])),
                            Math::Xor([a, b]) => buf.push(buf[usize::from(*a)].bvxor(&buf[usize::from(*b)])),
                            Math::Not(a) => buf.push(buf[usize::from(*a)].bvnot()),
                            Math::Neg(a) => buf.push(buf[usize::from(*a)].bvneg()),
                        }
                    }
                    buf.pop().unwrap()
                }

                if synth.params.use_smt {
                    let mut cfg = z3::Config::new();
                    cfg.set_timeout_msec(1000);
                    let ctx = z3::Context::new(&cfg);
                    let solver = z3::Solver::new(&ctx);
                    let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
                    let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
                    solver.assert(&lexpr._eq(&rexpr).not());
                    match solver.check() {
                        SatResult::Sat => ValidationResult::Invalid,
                        SatResult::Unsat => {
                            // println!("z3 validation: failed for {} => {}", lhs, rhs);
                            ValidationResult::Valid
                        },
                        SatResult::Unknown => {
                            // println!("z3 validation: unknown for {} => {}", lhs, rhs);
                            synth.smt_unknown += 1;
                            ValidationResult::Unknown
                        },
                    }
                } else {
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
                        for _ in 0..n {
                            let v = synth.rng.gen::<BV>();
                            cvec.push(Some(v));
                        }
                    }

                    let lvec = Self::eval_pattern(lhs, &env, n);
                    let rvec = Self::eval_pattern(rhs, &env, n);
                    ValidationResult::from(lvec == rvec)
                }
            }
        }
    };
}

#[cfg(test)]
pub mod tests {
    use super::*;

    type BV4 = BV<4>;

    #[test]
    fn test_bv() {
        assert_eq!(BV4::ALL_ONES.0, 0b1111);
        assert_eq!(BV4::MAX.0, 0b0111);
        assert_eq!(BV4::MIN.0, 0b1000);

        let one = BV4::from(1);

        assert_eq!(BV4::MAX.wrapping_add(one), BV::MIN);
        assert_eq!(BV4::NEG_ONE.wrapping_neg(), one);
        assert_eq!(BV4::MIN.wrapping_mul(BV::NEG_ONE), BV::MIN);
        assert_eq!(BV4::MIN.wrapping_neg(), BV::MIN);
    }
}
