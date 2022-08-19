use egg::*;
use ruler::*;

define_language! {
    pub enum Pos {
        // Nat
        "Z" = Z,
        "S" = S(Id),

        // Pos
        "XH" = XH,
        "XO" = XO(Id),
        "XI" = XI(Id),

        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),

        Var(egg::Symbol),
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Pos,
}

impl SynthLanguage for Pos {
    type Constant = usize;

    type Type = Type;

    fn get_type(&self) -> Self::Type {
        todo!()
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, f: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        todo!()
    }

    fn to_var(&self) -> Option<Symbol> {
        todo!()
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        todo!()
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        todo!()
    }

    fn is_constant(&self) -> bool {
        todo!()
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        todo!()
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        todo!()
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        todo!()
    }
}

fn main() {
    Pos::main()
}
