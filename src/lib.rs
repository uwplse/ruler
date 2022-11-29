use clap::Parser;
use serde::{Deserialize, Serialize};

pub use bv::*;
pub use derive::*;
pub use equality::*;
pub use interval::*;
pub use language::*;
pub use synth::*;
pub use util::*;

mod bv;
mod derive;
mod equality;
mod interval;
mod language;
mod synth;
mod util;

pub type Id = egg::Id;
pub type Symbol = egg::Symbol;
pub type Var = egg::Var;
pub type EGraph<L, N> = egg::EGraph<L, N>;
pub type Pattern<L> = egg::Pattern<L>;

#[derive(Parser)]
#[clap(rename_all = "kebab-case")]
pub enum Command {
    Synth(SynthParams),
    Derive(DeriveParams),
}

/// All parameters for rule synthesis.
#[derive(Parser, Deserialize, Serialize)]
#[clap(rename_all = "kebab-case")]
pub struct SynthParams {
    /// Output file name
    #[clap(long, default_value = "out.json")]
    pub outfile: String,

    #[clap(long)]
    pub prior_rules: Option<String>,

    #[clap(long)]
    pub workload: Option<String>,

    ////////////////
    // eqsat args //
    ////////////////
    /// node limit for all the eqsats
    #[clap(long, default_value = "300000")]
    pub node_limit: usize,
    /// iter limit for all the eqsats
    #[clap(long, default_value = "2")]
    pub iter_limit: usize,
    /// time limit (seconds) for all the eqsats
    #[clap(long, default_value = "60")]
    pub time_limit: u64,
}

/// All parameters for rule synthesis.
#[derive(Parser, Deserialize, Serialize)]
#[clap(rename_all = "kebab-case")]
pub struct DeriveParams {
    in1: String,
    in2: String,
    /// Output file name
    #[clap(long, default_value = "out.json")]
    outfile: String,

    #[clap(long, default_value = "10")]
    iter_limit: usize,

    #[clap(long)]
    ci: bool,
}

#[derive(Serialize, Deserialize)]
#[serde(bound = "L: SynthLanguage")]
pub struct Report<L: SynthLanguage> {
    pub params: SynthParams,
    pub time: f64,
    pub num_rules: usize,
    pub prior_rws: Vec<Equality<L>>,
    pub new_rws: Vec<Equality<L>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Sexp {
    Atom(String),
    List(Vec<Self>),
}

impl Sexp {
    fn plug(&self, name: &str, pegs: &[Self]) -> Vec<Sexp> {
        use itertools::Itertools;
        match self {
            Sexp::Atom(s) if s == name => pegs.to_vec(),
            Sexp::Atom(_) => vec![self.clone()],
            Sexp::List(sexps) => sexps
                .iter()
                .map(|x| x.plug(name, pegs))
                .multi_cartesian_product()
                .map(Sexp::List)
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Metric {
    Atoms,
    List,
    Depth,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum EnumoPattern {
    Wild,
    Var(String),
    Lit(String),
    List(Vec<EnumoPattern>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Filter {
    Met(Metric, usize),
    Contains(EnumoPattern),
    Canon(Vec<String>),
    And(Box<Self>, Box<Self>),
}

impl Filter {
    fn test(&self, sexp: &Sexp) -> bool {
        todo!()
    }

    fn is_monotonic(&self) -> bool {
        todo!()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Workload {
    Set(Vec<Sexp>),
    Plug(Box<Self>, String, Box<Self>),
    Filter(Filter, Box<Self>),
    Append(Vec<Self>),
}

impl Workload {
    fn force(&self) -> Vec<Sexp> {
        match self {
            Workload::Set(set) => set.clone(),
            Workload::Plug(tgt, name, workload) => {
                let mut res = vec![];
                let workload = workload.force();
                for sexp in tgt.force() {
                    res.extend(sexp.plug(&name, &workload));
                }
                res
            }
            Workload::Filter(f, workload) => {
                let mut set = workload.force();
                set.retain(|sexp| f.test(sexp));
                set
            }
            Workload::Append(workloads) => {
                let mut set = vec![];
                for w in workloads {
                    set.extend(w.force());
                }
                set
            }
        }
    }

    fn plug(self, name: impl Into<String>, workload: Workload) -> Self {
        Workload::Plug(Box::new(self), name.into(), Box::new(workload))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! s {
        (( $($x:tt)* )) => { Sexp::List(vec![ $(s!($x)),* ]) };
        ($x:tt) => { Sexp::Atom(format!(stringify!($x))) };
        ($($x:tt)*) => { s!(( $($x)* )) };
    }

    #[test]
    fn simple_plug() {
        let x = s!(x);
        let expected = vec![x.clone()];
        let actual = x.plug("a", &[s!(1), s!(2)]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn simple_plug2() {
        let atom = s!(x);
        let pegs = vec![s!(1), s!(2)];
        let expected = pegs.clone();
        let actual = atom.plug("x", &pegs);
        assert_eq!(actual, expected);
    }

    #[test]
    fn plug_cross_product() {
        let term = s!(x x);
        let pegs = vec![s!(1), s!(2), s!(3)];
        let expected = vec![
            s!(1 1),
            s!(1 2),
            s!(1 3),
            s!(2 1),
            s!(2 2),
            s!(2 3),
            s!(3 1),
            s!(3 2),
            s!(3 3),
        ];
        let actual = term.plug("x", &pegs);
        assert_eq!(actual, expected);
    }
}
