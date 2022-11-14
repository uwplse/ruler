/// Return the `i`th letter from the English alphabet.
pub fn letter(i: usize) -> &'static str {
    let alpha = "abcdefghijklmnopqrstuvwxyz";
    &alpha[i..i + 1]
}

/// Helper function to cross product a list of values `ts` across `n` variables.
pub fn self_product<T: Clone>(ts: &[T], n: usize) -> Vec<Vec<T>> {
    let num_consts = ts.len();
    let num_rows = num_consts.pow(n as u32);
    let mut res = vec![];
    for i in 0..n {
        let mut entry = vec![];
        while entry.len() < num_rows {
            for c in ts {
                for _ in 0..num_consts.pow(i as u32) {
                    entry.push(c.clone());
                }
            }
        }
        res.push(entry);
    }
    res
}
