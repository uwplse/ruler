use crate::*;

// This is an extremely minimal implementation of SynthLanguage
// It is not intended for any real uses.
// Workloads of arbitrary s-expressions can use EnumoSym as a
// default type parameter.
egg::define_language! {
  pub enum EnumoSym {
    Sym(egg::Symbol),
  }
}

impl SynthLanguage for EnumoSym {
    type Constant = usize;

    fn eval<'a, F>(&'a self, _cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    fn initialize_vars(_egraph: &mut EGraph<Self, SynthAnalysis>, _vars: &[String]) {}

    fn to_var(&self) -> Option<Symbol> {
        None
    }

    fn mk_var(sym: Symbol) -> Self {
        EnumoSym::Sym(sym)
    }

    fn is_constant(&self) -> bool {
        false
    }

    fn mk_constant(_c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        EnumoSym::Sym("".into())
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Unknown
    }
}
