use egg::{ENodeOrVar, RecExpr};

use crate::SynthLanguage;

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

/// Cvecs for variables must be explicitly initialized, so this helper function
/// makes it easy to find the variables in a set of expressions that will be
/// added to an e-graph.
pub fn get_vars_from_recexprs<L: SynthLanguage>(recexprs: &[RecExpr<L>]) -> Vec<String> {
    let mut vars = vec![];
    for expr in recexprs {
        for node in expr.as_ref() {
            if let ENodeOrVar::Var(v) = node.clone().to_enode_or_var() {
                let mut v = v.to_string();
                v.remove(0);
                if !vars.contains(&v) {
                    vars.push(v);
                }
            }
        }
    }
    vars
}

#[macro_export]
macro_rules! map {
    ($get:ident, $a:ident => $body:expr) => {
        $get($a)
            .iter()
            .map(|a| match a {
                Some($a) => $body,
                _ => None,
            })
            .collect::<Vec<_>>()
    };

    ($get:ident, $a:ident, $b:ident => $body:expr) => {
        $get($a)
            .iter()
            .zip($get($b).iter())
            .map(|tup| match tup {
                (Some($a), Some($b)) => $body,
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    ($get:ident, $a:ident, $b:ident, $c:ident => $body:expr) => {
        $get($a)
            .iter()
            .zip($get($b).iter())
            .zip($get($c).iter())
            .map(|tup| match tup {
                ((Some($a), Some($b)), Some($c)) => $body,
                _ => None,
            })
            .collect::<Vec<_>>()
    };
}
