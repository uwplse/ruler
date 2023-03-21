use std::{fmt::Display, hash::BuildHasherDefault};

pub use bv::*;
use enumo::Ruleset;
pub use language::*;
pub use util::*;

mod bv;
pub mod enumo;
mod language;
mod util;

pub type Id = egg::Id;
pub type Symbol = egg::Symbol;
pub type Var = egg::Var;
pub type EGraph<L, N> = egg::EGraph<L, N>;
pub type Pattern<L> = egg::Pattern<L>;

/// Faster hashMap implementation used in rustc
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
/// Faster hashSet implementation used in rustc
pub type HashSet<K> = rustc_hash::FxHashSet<K>;
/// IndexMap data implementation used in rustc
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<rustc_hash::FxHasher>>;

/// Validation result
#[derive(Debug, Clone)]
pub enum ValidationResult {
    Valid,
    Invalid,
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
            usize::max_value()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Limits {
    pub iter: usize,
    pub node: usize,
    pub derive_type: DeriveType,
}

#[derive(Debug, Clone, Copy)]
pub enum DeriveType {
    Lhs,
    LhsAndRhs,
    AllRules,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            iter: 3,
            node: 300_000,
            derive_type: DeriveType::AllRules,
        }
    }
}

impl Display for DeriveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeriveType::Lhs => write!(f, "lhs"),
            DeriveType::LhsAndRhs => write!(f, "lhs_and_rhs"),
            DeriveType::AllRules => write!(f, "all_rules"),
        }
    }
}

impl Limits {
    fn max() -> Self {
        Self {
            iter: usize::MAX,
            node: usize::MAX,
            derive_type: DeriveType::Lhs,
        }
    }
}

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

// Very minimal implementation of SynthLanguage for SymbolLang just so that we
// can write domain-agnostic tests
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
