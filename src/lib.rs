use std::{
    fmt::{self, Display},
    hash::BuildHasherDefault,
};

pub use bv::*;
use enumo::{Ruleset, Scheduler};
pub use language::*;
use serde::Serialize;
pub use util::*;

mod bv;
pub mod enumo;
mod language;
pub mod llm;
pub mod logger;
pub mod recipe_utils;
mod util;

/// Egg Id
pub type Id = egg::Id;

/// Egg Symbol
pub type Symbol = egg::Symbol;

/// Egg Var
pub type Var = egg::Var;

/// E-graph
pub type EGraph<L, N> = egg::EGraph<L, N>;

/// Egg Pattern
pub type Pattern<L> = egg::Pattern<L>;

/// Faster hashMap implementation used in rustc
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
/// Faster hashSet implementation used in rustc
pub type HashSet<K> = rustc_hash::FxHashSet<K>;
/// IndexMap data implementation used in rustc
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<rustc_hash::FxHasher>>;

/// Whether a rule is sound.
#[derive(Debug, Clone)]
pub enum ValidationResult {
    /// The rule is sound.
    Valid,
    /// The rule is unsound.
    Invalid,
    /// The soundness of the rule is unknown, for example, the SMT query timed out.
    Unknown,
}

// Cost function for ast size in the domain
// Penalizes ops not in the domain
pub struct ExtractableAstSize;
impl<L: SynthLanguage> egg::CostFunction<L> for ExtractableAstSize {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &L, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if enode.is_allowed_op() {
            enode.fold(1, |sum, id| sum.saturating_add(costs(id)))
        } else {
            usize::MAX
        }
    }
}

/// Resource limits for equality saturation.
#[derive(Debug, Clone, Copy)]
pub struct Limits {
    pub iter: usize,
    pub node: usize,
    pub match_: usize,
}

/// What metric for derivability to measure. Derivability tests whether a
/// ruleset, R, can recover the proving power of a specified rule, lhs->rhs
#[derive(Debug, Clone, Copy, Serialize)]
pub enum DeriveType {
    /// Tests whether the equivalence between lhs and rhs can be discovered
    /// from an e-graph initialized only with lhs.
    Lhs,
    /// Tests whether the equivalence between lhs and rhs can be discovered
    /// from an e-graph initialized with lhs and rhs.
    LhsAndRhs,
}

impl Limits {
    // match oopsla21 limits
    pub fn synthesis() -> Self {
        Self {
            iter: 2,
            node: 300_000,
            match_: 200_000,
        }
    }

    // match oopsla21 limits (oopsla21 did not distinguish between rulefinding and minimize limits)
    pub fn minimize() -> Self {
        Self {
            iter: 2,
            node: 300_000,
            match_: 200_000,
        }
    }

    // match oopsla21 limits
    pub fn deriving() -> Self {
        Self {
            iter: 5,
            node: 100_000,
            match_: 1000,
        }
    }

    pub fn trig_deriving() -> Self {
        Self {
            iter: 8,
            node: 300_000,
            match_: 200_000,
        }
    }
}

/// Used for interval analysis and constant folding
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interval<T> {
    pub low: Option<T>,
    pub high: Option<T>,
}

impl<T: Ord + Display> Interval<T> {
    pub fn new(low: Option<T>, high: Option<T>) -> Self {
        if let (Some(a), Some(b)) = (&low, &high) {
            assert!(
                a.le(b),
                "Invalid interval: low must be less than or equal to high\n{} >= {}",
                a,
                b
            );
        }
        Self { low, high }
    }
}

impl<T> Default for Interval<T> {
    fn default() -> Self {
        Self {
            low: None,
            high: None,
        }
    }
}

/// A scheduling phase for equality saturationg
pub struct Phase<L: SynthLanguage> {
    /// The rules to run
    pub rules: Ruleset<L>,
    /// A name for the ruleset. Primarily used for logging and debugging.
    pub rules_name: String,
    /// The schedule to use
    pub scheduler: Scheduler,
}

impl<L: SynthLanguage> fmt::Display for Phase<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let scheduler_name = match self.scheduler {
            Scheduler::Simple(_) => "eqsat",
            Scheduler::Saturating(_) => "sat",
            Scheduler::Compress(_) => "compress",
        };
        write!(f, "{} ({})", scheduler_name, self.rules_name)
    }
}

/// Very minimal implementation of SynthLanguage for SymbolLang just so that we
/// can write domain-agnostic tests
impl SynthLanguage for egg::SymbolLang {
    type Constant = usize;

    fn eval<'a, F>(&'a self, _cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    fn initialize_vars(_egraph: &mut EGraph<Self, SynthAnalysis>, _vars: &[String]) {
        todo!()
    }

    fn to_var(&self) -> Option<Symbol> {
        None
    }

    fn mk_var(sym: Symbol) -> Self {
        Self {
            op: sym,
            children: vec![],
        }
    }

    fn is_constant(&self) -> bool {
        false
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Self {
            op: Symbol::from(c.to_string()),
            children: vec![],
        }
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Invalid
    }
}
