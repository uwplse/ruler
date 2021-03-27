use egg::*;
use ruler::*;

use std::ops::*;

use rand_pcg::Pcg64;

use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;

macro_rules! impl_bits {
    ($inner:ty, $name:ident, $n:literal) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Hash, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $name($inner);

        impl $name {
            pub const ZERO: Self = Self(0);
            pub const MAX: Self = Self(<$inner>::MAX).mask();
            pub const WIDTH: u32 = $n;

            const fn mask(self) -> Self {
                $name(self.0 & (((1 as $inner) << $n).overflowing_sub(1).0))
            }

            pub fn wrapping_add(self, rhs: Self) -> Self {
                $name(self.0.wrapping_add(rhs.0)).mask()
            }

            pub fn wrapping_sub(self, rhs: Self) -> Self {
                $name(self.0.wrapping_sub(rhs.0)).mask()
            }

            pub fn wrapping_mul(self, rhs: Self) -> Self {
                $name(self.0.wrapping_mul(rhs.0)).mask()
            }

            pub fn wrapping_neg(self) -> Self {
                $name(self.0.wrapping_neg()).mask()
            }

            pub fn my_shl(self, rhs: Self) -> Self {
                if rhs.0 >= $n {
                    Self::ZERO
                } else {
                    $name(self.0 << rhs.0).mask()
                }
            }

            pub fn my_shr(self, rhs: Self) -> Self {
                if rhs.0 >= $n {
                    Self::ZERO
                } else {
                    $name(self.0 >> rhs.0).mask()
                }
            }
        }

        impl Not for $name {
            type Output = Self;
            fn not(self) -> Self {
                $name(self.0.not()).mask()
            }
        }

        impl BitAnd for $name {
            type Output = Self;
            fn bitand(self, rhs: Self) -> Self {
                $name(self.0.bitand(rhs.0)).mask()
            }
        }

        impl BitOr for $name {
            type Output = Self;
            fn bitor(self, rhs: Self) -> Self {
                $name(self.0.bitor(rhs.0)).mask()
            }
        }

        impl BitXor for $name {
            type Output = Self;
            fn bitxor(self, rhs: Self) -> Self {
                $name(self.0.bitxor(rhs.0)).mask()
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Debug::fmt(&self.0, f)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Display::fmt(&self.0, f)
            }
        }

        impl Distribution<$name> for rand::distributions::Standard {
            fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> $name {
                let inner: $inner = rng.gen();
                $name(inner).mask()
            }
        }

        impl std::str::FromStr for $name {
            type Err = std::num::ParseIntError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                s.parse().map(|inner| Self(inner).mask())
            }
        }

        impl From<$inner> for $name {
            fn from(t: $inner) -> Self {
                Self(t).mask()
            }
        }
    };
}

impl_bits!(std::primitive::u8, u4, 4);

define_language! {
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
        Num(u4),
        Var(egg::Symbol),
    }
}

impl SynthLanguage for Math {
    type Constant = u4;

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

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let consts: Vec<Option<u4>> = (0..1 << 4).map(|i| Some(i.into())).collect();

        let consts = self_product(&consts, synth.params.variables);
        println!("cvec len: {}", consts[0].len());

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: consts[0].len(),
        });

        egraph.add(Math::Num(0.into()));
        egraph.add(Math::Num(0x7.into()));
        egraph.add(Math::Num(0x8.into()));

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));
            egraph[id].data.cvec = consts[i].clone();
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>) -> Vec<Self> {
        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                }
                to_add.push(Math::Add([i, j]));
                to_add.push(Math::Sub([i, j]));
                to_add.push(Math::Mul([i, j]));

                to_add.push(Math::Shl([i, j]));
                to_add.push(Math::Shr([i, j]));

                to_add.push(Math::And([i, j]));
                to_add.push(Math::Or([i, j]));
                to_add.push(Math::Xor([i, j]));
            }
            if synth.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Not(i));
            to_add.push(Math::Neg(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(_rng: &mut Pcg64, _lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> bool {
        true
    }
}

fn main() {
    Math::main()
}
