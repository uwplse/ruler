/**
 * Pos is a datatype representing the strictly positive integers in a binary way.
 * XH represents 1
 * XO and XI represent adding a new least significant digit
 * That is, (XO n) represents 2n and (XI n) represents 2n + 1
 * Example: 6 is represented as (XO (XI XH))
 * See https://coq.inria.fr/library/Coq.Numbers.BinNums.html
 */
use egg::rewrite;
use ruler::*;

egg::define_language! {
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

impl Pos {
    fn mk_constant_id(c: usize, egraph: &mut EGraph<Self, SynthAnalysis>) -> Id {
        match c {
            0 => egraph.add(Pos::Z),
            1 => egraph.add(Pos::XH),
            c if c % 2 == 0 => {
                let pred = Self::mk_constant_id(c / 2, egraph);
                egraph.add(Pos::XO(pred))
            }
            _ => {
                let pred = Self::mk_constant_id((c - 1) / 2, egraph);
                egraph.add(Pos::XI(pred))
            }
        }
    }
}

impl SynthLanguage for Pos {
    type Constant = usize;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rewrites() -> Vec<egg::Rewrite<Self, SynthAnalysis>> {
        vec![
            rewrite!("def-xh"; "XH" <=> "(S Z)"),
            rewrite!("def-xo"; "(XO ?a)" <=> "(+ ?a ?a)"),
            rewrite!("def-xi"; "(XI ?a)" <=> "(+ ?a (+ ?a (S Z)))"),
        ]
        .concat()
    }

    fn is_allowed_op(&self) -> bool {
        matches!(
            self,
            Pos::XH | Pos::XO(_) | Pos::XI(_) | Pos::Add(_) | Pos::Mul(_) | Pos::Var(_)
        )
    }

    // No eval needed for rule lifting
    fn eval<'a, F>(&'a self, _cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    // Nov variable initialization needed
    fn initialize_vars(_synth: &mut Synthesizer<Self>, _vars: Vec<String>) {}

    fn to_var(&self) -> Option<Symbol> {
        if let Pos::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Pos::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Pos::Z | Pos::XH)
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        match c {
            0 => Pos::Z,
            1 => Pos::XH,
            c if c % 2 == 0 => Pos::XO(Self::mk_constant_id(c / 2, egraph)),
            _ => Pos::XI(Self::mk_constant_id((c - 1) / 2, egraph)),
        }
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult {
        ValidationResult::Valid
    }
}

#[cfg(test)]
mod tests {
    use ruler::enumo::{Ruleset, Workload};

    use super::*;

    fn iter_pos(n: usize) -> Workload {
        Workload::iter_lang(n, &["XH"], &["a", "b", "c"], &["XO", "XI"], &["+", "*"])
    }

    #[test]
    fn rule_lifting() {
        let nat_rules: Vec<Equality<Pos>> = vec![
            "(+ ?b ?a) ==> (+ ?a ?b)",
            "(* ?b ?a) ==> (* ?a ?b)",
            "(+ Z ?a) ==> ?a",
            "(* ?a Z) ==> Z",
            "(S (+ ?b ?a)) ==> (+ ?b (S ?a))",
            "(* ?a (S Z)) ==> ?a",
            "(+ ?a (S Z)) ==> (S ?a)",
            "(+ ?c (+ ?b ?a)) ==> (+ ?a (+ ?b ?c))",
            "(* (* ?c ?b) ?a) ==> (* ?b (* ?c ?a))",
            "(+ ?b (* ?b ?a)) ==> (* ?b (S ?a))",
            "(* (+ ?b ?b) ?a) ==> (* ?b (+ ?a ?a))",
            "(+ ?a ?a) ==> (* ?a (S (S Z)))",
        ]
        .iter()
        .map(|s| s.parse().unwrap())
        .collect();

        let mut all_rules = Ruleset::default();
        all_rules.extend(Ruleset(nat_rules));

        let atoms3 = iter_pos(3);
        assert_eq!(atoms3.force().len(), 51);

        let rules3 = Pos::run_workload_with_limits(atoms3, all_rules.clone(), 3, 30, 1000000);
        assert_eq!(rules3.len(), 3);
        all_rules.extend(rules3);

        let atoms4 = iter_pos(4);
        assert_eq!(atoms4.force().len(), 255);

        let rules4 = Pos::run_workload_with_limits(atoms4, all_rules.clone(), 3, 30, 1000000);
        assert_eq!(rules4.len(), 4);
        all_rules.extend(rules4);

        let atoms5 = iter_pos(5);
        assert_eq!(atoms5.force().len(), 1527);

        let rules4 = Pos::run_workload_with_limits(atoms5, all_rules.clone(), 3, 30, 1000000);
        assert_eq!(rules4.len(), 8);
    }
}
