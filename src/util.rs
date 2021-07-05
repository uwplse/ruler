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
