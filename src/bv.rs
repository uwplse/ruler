use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{BitAnd, BitOr, BitXor, Not};

macro_rules! impl_bits {
    ($inner:ty, $name:ident, $n:literal) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Hash, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $name($inner);

        impl $name {
            pub const ZERO: Self = Self(0);
            pub const MAX: Self = Self(<$inner>::MAX).mask();

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bvmul() {
        assert_eq!(u4(4).wrapping_mul(u4(4)), u4(0));
        assert_eq!(u4(4).wrapping_mul(u4(5)), u4((4 * 5) & 0b1111));
    }
}
