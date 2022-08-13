use num::{rational::Ratio, BigInt, Zero};

/// Helper function to cross product a list of values `ts` across `n` variables.
pub fn self_product<T: Clone>(ts: &[T], n: usize) -> Vec<Vec<T>> {
    (0..n)
        .map(|i| {
            let mut res = vec![];
            let nc = ts.len();
            let nrows = nc.pow(n as u32);
            while res.len() < nrows {
                for c in ts {
                    for _ in 0..nc.pow(i as u32) {
                        res.push(c.clone())
                    }
                }
            }
            res
        })
        .collect()
}

// Division the rounds up
// Hack from https://www.reddit.com/r/rust/comments/bk7v15/my_next_favourite_way_to_divide_integers_rounding/
pub fn div_up(a: usize, b: usize) -> usize {
    (0..a).step_by(b).size_hint().0
}

// Make a Ratio whose denominator is not zero.
pub fn mk_constant(n: &BigInt, d: &BigInt) -> Ratio<BigInt> {
    if d.is_zero() {
        panic!("mk_constant: denominator is zero!")
    } else {
        Ratio::new(n.clone(), d.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_product() {
        let ts = &[4, 5, 6];
        assert_eq!(
            self_product(ts, 2),
            vec![
                vec![4, 5, 6, 4, 5, 6, 4, 5, 6],
                vec![4, 4, 4, 5, 5, 5, 6, 6, 6]
            ],
        );
    }
}
