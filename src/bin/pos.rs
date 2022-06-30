use egg::*;
use rand::RngCore;
use ruler::*;

define_language! {
  pub enum Pos {
    "XH" = XH,
    "XO" = XO(Id),
    "XI" = XI(Id),
    "+" = Add([Id;2]),
    "*" = Mul([Id;2]),
    Var(egg::Symbol),
  }
}

impl Pos {
    fn mk_constant_id(c: usize, egraph: &mut EGraph<Self, SynthAnalysis>) -> Id {
        if c == 1 {
            egraph.add(Pos::XH)
        } else if c < 1 {
            panic!("invalid pos")
        } else if c % 2 == 0 {
            let pred = Self::mk_constant_id(c / 2, egraph);
            egraph.add(Pos::XO(pred))
        } else {
            let pred = Self::mk_constant_id((c - 1) / 2, egraph);
            egraph.add(Pos::XI(pred))
        }
    }
}

impl SynthLanguage for Pos {
    type Constant = usize;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Pos::XH => vec![Some(1); cvec_len],
            Pos::XO(p) => map!(v, p => Some(*p * 2)),
            Pos::XI(p) => map!(v, p => Some(*p * 2 + 1)),
            Pos::Add([a, b]) => map!(v, a, b => Some(*a + *b)),
            Pos::Mul([a, b]) => map!(v, a, b => Some(*a * *b)),
            Pos::Var(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Pos::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Pos::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Pos::XH = self {
            Some(&1)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        if c == 1 {
            Pos::XH
        } else if c < 1 {
            panic!("invalid constant");
        } else {
            if c % 2 == 0 {
                Pos::XO(Self::mk_constant_id(c / 2, egraph))
            } else {
                Pos::XI(Self::mk_constant_id((c - 1) / 2, egraph))
            }
        }
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut egraph: EGraph<Pos, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 10,
            constant_fold: ConstantFoldMethod::IntervalAnalysis,
            rule_lifting: false,
        });

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Pos::Var(var));
            let mut vals = vec![];
            let rng = &mut synth.rng;

            for _ in 0..10 {
                vals.push(Some(rng.next_u32() as usize));
            }
            egraph[id].data.cvec = vals.clone();
        }
        synth.egraph = egraph;
    }

    fn make_layer(_synth: &Synthesizer<Self>, _iter: usize) -> Vec<Self> {
        vec![]
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult {
        // TODO
        ValidationResult::Valid
    }
}

fn main() {
    Pos::main()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mk_constant_id() {
        let mut egraph: EGraph<Pos, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 1,
            constant_fold: ConstantFoldMethod::NoFold,
            rule_lifting: false,
        });

        let one = egraph.add(Pos::XH);
        let two = egraph.add(Pos::XO(one));
        let three = egraph.add(Pos::XI(one));
        let four = egraph.add(Pos::XO(two));
        let five = egraph.add(Pos::XI(two));
        let six = egraph.add(Pos::XO(three));
        let seven = egraph.add(Pos::XI(three));

        assert_eq!(one, Pos::mk_constant_id(1, &mut egraph.clone()));
        assert_eq!(two, Pos::mk_constant_id(2, &mut egraph.clone()));
        assert_eq!(three, Pos::mk_constant_id(3, &mut egraph.clone()));
        assert_eq!(four, Pos::mk_constant_id(4, &mut egraph.clone()));
        assert_eq!(five, Pos::mk_constant_id(5, &mut egraph.clone()));
        assert_eq!(six, Pos::mk_constant_id(6, &mut egraph.clone()));
        assert_eq!(seven, Pos::mk_constant_id(7, &mut egraph.clone()));
    }
}
