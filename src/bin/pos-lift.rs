use egg::*;
use ruler::*;

define_language! {
  pub enum Math {
    // Nat
    "Z" = Z,
    "S" = S(Id),

    // TODO (find equivs between Nat ops and Pos ops)
    // Nat Ops
    "PlusN" = PlusN([Id;2]),
    "TimesN" = TimesN([Id;2]),

    // Pos
    "XH" = XH,
    "XO" = XO(Id),
    "XI" = XI(Id),

    // Pos Ops
    "+" = Add([Id; 2]),
    "*" = Mul([Id; 2]),

    Var(egg::Symbol),
  }
}

impl Math {
    fn mk_constant_id(c: usize, egraph: &mut EGraph<Self, SynthAnalysis>) -> Id {
        if c == 0 {
            egraph.add(Math::Z)
        } else if c == 1 {
            egraph.add(Math::XH)
        } else if c < 1 {
            panic!("invalid c")
        } else if c % 2 == 0 {
            let pred = Self::mk_constant_id(c / 2, egraph);
            egraph.add(Math::XO(pred))
        } else {
            let pred = Self::mk_constant_id((c - 1) / 2, egraph);
            egraph.add(Math::XI(pred))
        }
    }
}

impl SynthLanguage for Math {
    type Constant = usize;

    // No eval
    fn eval<'a, F>(&'a self, _cvec_len: usize, _f: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Math::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        match self {
            Math::Z => Some(&0),
            Math::XH => Some(&1),
            _ => None,
        }
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        if c == 1 {
            Math::XH
        } else if c < 1 {
            panic!("invalid constant");
        } else {
            if c % 2 == 0 {
                Math::XO(Self::mk_constant_id(c / 2, egraph))
            } else {
                Math::XI(Self::mk_constant_id((c - 1) / 2, egraph))
            }
        }
    }

    fn is_allowed(&self) -> bool {
        matches!(
            self,
            Math::XH | Math::XO(_) | Math::XI(_) | Math::Add(_) | Math::Mul(_) | Math::Var(_)
        )
    }

    fn is_extractable(&self) -> bool {
        matches!(
            self,
            Math::XH | Math::XO(_) | Math::XI(_) | Math::Add(_) | Math::Mul(_) | Math::Var(_)
        )
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut egraph: EGraph<Math, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 0,
            constant_fold: ConstantFoldMethod::NoFold,
            rule_lifting: true,
        });

        synth.lifting_rewrites = vec![
            rewrite!("def-xh"; "XH" <=> "(S Z)"),
            // rewrite!("def-xo-mul"; "(XO ?a)" <=> "(* ?a (S (S Z)))"),
            rewrite!("def-xo-add"; "(XO ?a)" <=> "(+ ?a ?a)"),
            // rewrite!("def-xi-mul"; "(XI ?a)" <=> "(+ (* ?a (S (S Z))) (S Z))"),
            rewrite!("def-xi-add"; "(XI ?a)" <=> "(+ ?a (+ ?a (S Z)))"),
        ]
        .concat();

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        vec![]
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        ValidationResult::Valid
    }

    fn is_allowed_rewrite(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool {
        let contains_pos_node = |pat: &Pattern<Self>| {
            pat.ast.as_ref().iter().any(|n| {
                matches!(
                    n,
                    ENodeOrVar::ENode(Math::XH)
                        | ENodeOrVar::ENode(Math::XO(_))
                        | ENodeOrVar::ENode(Math::XI(_))
                )
            })
        };

        let pattern_is_extractable = |pat: &Pattern<Self>| {
            pat.ast.as_ref().iter().all(|n| {
                println!("n: {}", n);
                match n {
                    ENodeOrVar::Var(_) => true,
                    ENodeOrVar::ENode(n) => n.is_extractable(),
                }
            })
        };

        pattern_is_extractable(lhs)
            && pattern_is_extractable(rhs)
            && (contains_pos_node(lhs) || contains_pos_node(rhs))
    }
}

fn main() {
    Math::main()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn is_extractable() {
        let mut egraph: EGraph<Math, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len: 1,
            constant_fold: ConstantFoldMethod::NoFold,
            rule_lifting: true,
        });
        let z = Math::Z;
        assert!(!z.is_extractable());
        let z_id = egraph.add(z);

        let sz = Math::S(z_id);
        assert!(!sz.is_extractable());
        let sz_id = egraph.add(sz);

        let one = Math::XH;
        assert!(one.is_extractable());
        let one_id = egraph.add(one);

        let add = Math::Add([sz_id, one_id]);
        assert!(!add.is_extractable());
    }
}
