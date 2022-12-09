pub use bv::*;
use enumo::{Ruleset, Workload};
pub use equality::*;
pub use interval::*;
pub use language::*;
pub use synth::*;
pub use util::*;

mod bv;
pub mod enumo;
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

pub struct SynthParams<L: SynthLanguage> {
    pub prior_rules: Ruleset<L>,

    pub workload: Workload,

    ////////////////
    // eqsat args //
    ////////////////
    pub node_limit: usize,
    pub iter_limit: usize,
    pub time_limit: u64,
}
