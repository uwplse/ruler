use crate::HashMap;

pub use filter::*;
pub use metric::*;
pub use pattern::*;
pub use sexp::*;
pub use workload::*;

mod filter;
mod metric;
mod pattern;
mod sexp;
mod workload;

#[macro_export]
macro_rules! s {
        (( $($x:tt)* )) => { Sexp::List(vec![ $(s!($x)),* ]) };
        ($x:tt) => { Sexp::Atom(format!(stringify!($x))) };
        ($($x:tt)*) => { s!(( $($x)* )) };
    }
