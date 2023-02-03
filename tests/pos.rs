/**
 * Pos is a datatype representing the strictly positive integers in a binary way.
 * XH represents 1
 * XO and XI represent adding a new least significant digit
 * That is, (XO n) represents 2n and (XI n) represents 2n + 1
 * Example: 6 is represented as (XO (XI XH))
 * See https://coq.inria.fr/library/Coq.Numbers.BinNums.html
 */
use ruler::{enumo::Ruleset, *};

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

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::from_str_vec(&[
            "XH ==> (S Z)",
            "(S Z) ==> XH",
            "(XO ?a) ==> (+ ?a ?a)",
            "(+ ?a ?a) ==> (XO ?a)",
            "(XI ?a) ==> (+ ?a (+ ?a (S Z)))",
            "(+ ?a (+ ?a (S Z))) ==> (XI ?a)",
        ])
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
    fn initialize_vars(_egraph: &mut EGraph<Self, SynthAnalysis>, _vars: &[String]) {}

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

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
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
    fn rule_lifting_recipe() {
        let nat_rules = [
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
        ];

        let prior = Ruleset::from_str_vec(&nat_rules);

        let atoms3 = iter_pos(3);
        assert_eq!(atoms3.force().len(), 51);

        let limits = Limits {
            iter: 3,
            node: 1000000,
        };

        let eg_init = atoms3.to_egraph();
        // Allowed rules: run on clone, apply unions, no candidates
        let (allowed, _) = prior.partition(|eq| Pos::is_allowed_rewrite(&eq.lhs, &eq.rhs));
        let eg_allowed = allowed.compress(&eg_init, limits);

        // Translation rules: grow egraph, extract candidates, assert!(saturated)
        let lifting_rules = Pos::get_lifting_rules();
        let eg_denote = lifting_rules.crunch(&eg_allowed, limits);
        let mut candidates = Ruleset::extract_candidates(&eg_allowed, &eg_denote);

        // All rules: clone/no clone doesn't matter, extract candidates
        let mut all_rules = prior;
        all_rules.extend(lifting_rules);
        let eg_final = all_rules.compress(&eg_denote, limits);
        candidates.extend(Ruleset::extract_candidates(&eg_denote, &eg_final));

        let rules = candidates;
        for r in rules.0.values() {
            println!("{}", r.name)
        }
    }

    #[test]
    fn rule_lifting() {
        let nat_rules = [
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
        ];

        let mut all_rules = Ruleset::default();
        all_rules.extend(Ruleset::from_str_vec(&nat_rules));

        let atoms3 = iter_pos(3);
        assert_eq!(atoms3.force().len(), 51);

        let rules3 = Pos::run_workload(
            atoms3,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
            },
        );
        assert_eq!(rules3.len(), 3);
        all_rules.extend(rules3);

        let atoms4 = iter_pos(4);
        assert_eq!(atoms4.force().len(), 255);

        let rules4 = Pos::run_workload(
            atoms4,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
            },
        );
        assert_eq!(rules4.len(), 4);
        all_rules.extend(rules4);

        let atoms5 = iter_pos(5);
        assert_eq!(atoms5.force().len(), 1527);

        let rules4 = Pos::run_workload(
            atoms5,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
            },
        );
        assert_eq!(rules4.len(), 8);
    }
}
