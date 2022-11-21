use std::fmt;
use std::ops::*;

use rand::prelude::Distribution;
use rand::Rng;
use serde::Deserialize;
use serde::Serialize;

// General bitvector implementation
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

    fn not(self) -> Self::Output {
        Self::new(self.0.not())
    }
}

impl<const N: u32> BitAnd for BV<N> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::new(self.0.bitand(rhs.0))
    }
}

impl<const N: u32> BitOr for BV<N> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::new(self.0.bitor(rhs.0))
    }
}

impl<const N: u32> BitXor for BV<N> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::new(self.0.bitxor(rhs.0))
    }
}

impl<const N: u32> fmt::Debug for BV<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
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

// Macro for specializing BV to different sized bitvectors
#[macro_export]
macro_rules! impl_bv {
    ($n:literal) => {
        use $crate::*;

        use rand::prelude::*;
        use rand_pcg::Pcg64;
        use serde::{Deserialize, Serialize};
        use std::fmt;
        use std::ops::*;

        pub type BV = $crate::BV::<$n>;

        egg::define_language! {
          pub enum Bv {
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
                  Lit(BV),
                  Var(egg::Symbol),
              }
        }

        impl SynthLanguage for Bv {
            type Constant = BV;

            fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
            where
                F: FnMut(&'a Id) -> &'a CVec<Self>,
            {
                match self {
                    Bv::Neg(a) => map!(get_cvec, a => Some(a.wrapping_neg())),
                    Bv::Not(a) => map!(get_cvec, a => Some(a.not())),

                    Bv::Add([a, b]) => map!(get_cvec, a, b => Some(a.wrapping_add(*b))),
                    Bv::Sub([a, b]) => map!(get_cvec, a, b => Some(a.wrapping_sub(*b))),
                    Bv::Mul([a, b]) => map!(get_cvec, a, b => Some(a.wrapping_mul(*b))),

                    Bv::Shl([a, b]) => map!(get_cvec, a, b => Some(a.my_shl(*b))),
                    Bv::Shr([a, b]) => map!(get_cvec, a, b => Some(a.my_shr(*b))),

                    Bv::And([a, b]) => map!(get_cvec, a, b => Some(*a & *b)),
                    Bv::Or([a, b]) => map!(get_cvec, a, b => Some(*a | *b)),
                    Bv::Xor([a, b]) => map!(get_cvec, a, b => Some(*a ^ *b)),

                    Bv::Lit(n) => vec![Some(n.clone()); cvec_len],
                    Bv::Var(_) => vec![],
                }
            }

            fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
            where
                F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
            {
                match self {
                    Bv::Lit(c) => Interval::new(Some(*c), Some(*c)),
                    // Todo- proper interval analysis. For now it's just constant folding
                    _ => Interval::default()
                }
            }

            fn to_var(&self) -> Option<Symbol> {
                if let Bv::Var(sym) = self {
                    Some(*sym)
                } else {
                    None
                }
            }

            fn mk_var(sym: Symbol) -> Self {
                Bv::Var(sym)
            }

            fn is_constant(&self) -> bool {
                matches!(self, Bv::Lit(_))
            }

            fn mk_constant(c: Self::Constant) -> Self {
                Bv::Lit(c)
            }

            fn initialize_vars(synth: &mut Synthesizer<Self>, vars: Vec<String>) {
                //   let mut consts: Vec<Option<BV>> = (0..1u64 << $n).map(|i| Some((i as u32).into())).collect();
                let mut consts = vec![];

                for i in 0..2 {
                    let i = BV::from(i);
                    consts.push(Some(BV::MIN.wrapping_add(i)));
                    consts.push(Some(BV::MAX.wrapping_sub(i)));
                    consts.push(Some(i));
                    consts.push(Some(i.wrapping_neg()));
                }
                consts.sort();
                consts.dedup();

                println!("{}", consts.len());

                let mut cvecs = self_product(&consts, vars.len());

                let mut egraph = EGraph::new(SynthAnalysis {
                    cvec_len: cvecs[0].len()
                });

                for (i, v) in vars.iter().enumerate() {
                    let id = egraph.add(Bv::Var(Symbol::from(v.clone())));
                    egraph[id].data.cvec = cvecs[i].clone()
                }

                synth.egraph = egraph;
            }

            fn validate(
                _synth: &mut Synthesizer<Self>,
                _lhs: &Pattern<Self>,
                _rhs: &Pattern<Self>,
            ) -> ValidationResult {
                ValidationResult::Valid
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
