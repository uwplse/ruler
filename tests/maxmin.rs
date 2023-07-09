// An attempt to infer rules about max/min, e.g. max(min(a, b), a) ==> a
// by lowering to if.
// Status: Very experimental. Doesn't work.

#![allow(unused_imports)]
#![allow(unused_variables)]
use num::{rational::Ratio, BigInt, Signed, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use rayon::vec;
use ruler::{
    enumo::{Ruleset, Scheduler, Workload},
    *,
};
use std::time::Instant;

pub type Constant = Ratio<BigInt>;

// fn mk_rat(n: i64, d: i64) -> Ratio<BigInt> {
//     if d.is_zero() {
//         panic!("mk_rat: denominator is zero!");
//     }
//     let n = n
//         .to_bigint()
//         .unwrap_or_else(|| panic!("could not make bigint from {}", n));
//     let d = d
//         .to_bigint()
//         .unwrap_or_else(|| panic!("could not make bigint from {}", d));

//     Ratio::new(n, d)
// }

fn get_nat_rules() -> Vec<&'static str> {
    [
        "(& ?a ?b) ==> (binop & ?a ?b)",
        "(binop & ?a ?b) ==> (& ?a ?b)",
        "(| ?a ?b) ==> (binop | ?a ?b)",
        "(binop | ?a ?b) ==> (| ?a ?b)",
        "(< ?a ?b) ==> (binop < ?a ?b)",
        "(binop < ?a ?b) ==> (< ?a ?b)",
        "(! ?a) ==> (unop ! ?a)",
        "(unop ! ?a) ==> (! ?a)",

        "(& false ?a) ==> false",
        "(& true ?a) ==> ?a",
        "(& ?a (! ?a)) ==> false",
        "(| true ?a) ==> true",
        "(| false ?a) ==> ?a",
        "(| ?a (! ?a)) ==> true",
        "(! true) ==> false",
        "(! false) ==> true",

        "(& ?b ?a) ==> (& ?a ?b)",
        "(& ?a ?a) ==> ?a",
        "(& ?c (& ?b ?a)) ==> (& ?a (& ?b ?c))",

        "(| ?b ?a) ==> (| ?a ?b)",
        "(| ?a ?a) ==> ?a",
        "(| ?c (| ?b ?a)) ==> (| ?a (| ?b ?c))",

        "(! (! ?a)) ==> ?a",
        "?a ==> (! (! ?a))",

        "(! ?a) ==> (bool (dummy2 ! ?a))",
        "(bool (dummy2 ! ?a)) ==> (! ?a)",
        "(< ?a ?b) ==> (bool (dummy3 < ?a ?b))",
        "(bool (dummy3 < ?a ?b)) ==> (< ?a ?b)",
        "(& ?a ?b) ==> (bool (dummy3 & ?a ?b))",
        "(bool (dummy3 & ?a ?b)) ==> (& ?a ?b)",
        "(| ?a ?b) ==> (bool (dummy3 | ?a ?b))",
        "(bool (dummy3 | ?a ?b)) ==> (| ?a ?b)",

        // Lift case through binop
        "(binop ?op ?a (case ?c ?t ?rest)) ==> (case ?c (binop ?op ?a ?t) (binop ?op ?a ?rest))",
        "(binop ?op (case ?c ?t ?rest) ?a) ==> (case ?c (binop ?op ?t ?a) (binop ?op ?rest ?a))",
        "(binop ?op ?a endcase) ==> endcase",
        "(binop ?op endcase ?a) ==> endcase",

        "(case false ?t ?rest) ==> ?rest",
        "(case true ?t ?rest) ==> ?t",

        // Reorder
        "(case ?c1 ?t1 (case ?c2 ?t2 ?rest)) ==> (case ?c2 ?t2 (case ?c1 ?t1 ?rest))",
        // Raise case
        "(case ?x (case ?c ?t ?rest) ?outer) ==> (case (& ?x ?c) ?t (case (& ?x (! ?c)) ?rest ?outer))",
        // Prune
        "(case ?x endcase ?outer) ==> ?outer",
        // Merge
        "(case ?c1 ?t (case ?c2 ?t ?rest)) ==> (case (| ?c1 ?c2) ?t ?rest)",
        // Create knowledge
        "(case ?c ?t ?rest) ==> (case ?c (know ?c ?t) ?rest)",
        // Forget
        "(know ?k ?e) ==> ?e",
        // Knowledge flow
        "(know ?k (case ?c ?t ?rest)) ==> (know ?k (case (know ?k ?c) (know ?k ?t) ?rest))",
        "(know ?k (binop ?op ?a ?b)) ==> (know ?k (binop ?op (know ?k ?a) (know ?k ?b)))",
        "(know ?k (unop ?op ?a)) ==> (know ?k (unop ?op (know ?k ?a)))",
        // Merge and split
        "(know (& ?k1 ?k2) ?e) ==> (know ?k1 (know ?k2 ?e))",
        "(know ?k1 (know ?k2 ?e)) ==> (know (& ?k1 ?k2) ?e)",
        // Use knowledge
        "(know ?k (bool ?a)) ==> (know ?k (| (! ?k) (bool ?a)))",
        // "(know ?k (bool ?a)) ==> (know ?k (& ?k (bool ?a)))",

/*
b
!(!b & a)
b | !a



!b
!(b & a) & a
(!b | !a) & a

!b & a
case {
    case {
        x: true
        y: true
        !x | !y: false
    }: x | y
    !x | !y : true
} x

 */

    ].into()
}

egg::define_language! {
 pub enum CaddyAndFRep  {
    // bool/int language
    "<" = Less([Id; 2]),
    "case" = Case([Id; 3]),
    "endcase" = EndCase,
    "!" = Not(Id),
    "&" = And([Id; 2]),
    "|" = Or([Id; 2]),
    "true" = True,
    "false" = False,

    // rule utility
    "know" = Know([Id; 2]),
    "binop" = Binop([Id; 3]),
    "unop" = Unop([Id; 2]),
    "bool" = Bool([Id; 1]),
    "dummy2" = Dummy2([Id; 2]),
    "dummy3" = Dummy3([Id; 3]),

    // lowering utility
    "ite" = If([Id; 3]),

    // max/min
    "max" = Max([Id; 2]),
    "min" = Min([Id; 2]),

    "cheat" = Cheat([Id; 2]),

    Lit(Constant),
    Var(egg::Symbol),
 }
}

impl SynthLanguage for CaddyAndFRep {
    type Constant = Constant;

    fn is_rule_lifting() -> bool {
        true
    }

    /*

    (Inter (FRep ?a) (Union (FRep ?a) (FRep ?b)))
    (Intersect (FRep ?a) (FRep (max ?a ?b))

    max(min(a, b), a)
    (ite (< ?a (ite (< ?a ?b) ?a ?b)) (ite (< ?a ?b) ?a ?b) ?a)

    if (< ?a (if (< ?a ?b) then ?a else ?b))
    then (if (< ?a ?b) then ?a else ?b)
    else ?a

    (case
        (< ?a
            (if (< ?a ?b) then ?a else ?b)
        )
        (if (< ?a ?b) then ?a else ?b)
    (case
        (! (< ?a
            (if (< ?a ?b) then ?a else ?b)
        ))
        ?a
    endcase))

    (case
        (< ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        )
        (case (< ?a ?b)
            ?a
        (case (! (< ?a ?b))
            ?b
        endcase))
    (case
        (! (< ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        ))
        ?a
    endcase))


    fold min


    (case
        (< ?a (min a b))
        (case (< ?a ?b)
            ?a
        (case (! (< ?a ?b))
            ?b
        endcase))
    (case
        (! (< ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        ))
        ?a
    endcase))

    lift

    (case
        (& (< ?a (min a b)) (< ?a ?b))
        ?a
    (case
        (& (< ?a (min a b)) (! (< ?a ?b)))
        (case (! (< ?a ?b))
            ?b
        endcase)
    (case
        (! (< ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        ))
        ?a
    endcase)))



    (case
        (& (< ?a (min a b)) (< ?a ?b))
        ?a
    (case
        (& (& (< ?a (min a b)) (! (< ?a ?b))) (! (< ?a ?b)))
        ?b
    (case
        (& (& (< ?a (min a b)) (! (< ?a ?b))) (! (! (< ?a ?b))))
        endcase
    (case
        (! (< ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        ))
        ?a
    endcase))))


    (case
        (& (< ?a (min a b)) (< ?a ?b))
        ?a
    (case
        (& (& (< ?a (min a b)) (! (< ?a ?b))) (! (< ?a ?b)))
        ?b
    (case
        (! (< ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        ))
        ?a
    endcase))))

    detour -->

    (case
        (& (< ?a (min a b)) (< ?a ?b))
        ?a
    (case
        (& (& (< ?a (min a b)) (! (< ?a ?b))) (! (< ?a ?b)))
        ?b
    (case
        (! (< ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        ))
        ?a
    endcase))))







    detour <--

    (& (& (< ?a (min a b)) (! (< ?a ?b))) (! (< ?a ?b)))

    (&
        (&
            (< ?a (min a b))
            (! (< ?a ?b))
        )
        (! (< ?a ?b))
    )


    (&
        (< ?a (min a b))
        (! (< ?a ?b))
    )

    (&
        (<
            ?a
            (case (< ?a ?b)
                ?a
            (case (! (< ?a ?b))
                ?b
            endcase))
        )
        (! (< ?a ?b))
    )


    (&
        (case (< ?a ?b)
            (< ?a ?a)
        (case (! (< ?a ?b))
            (< ?a ?b)
        endcase))
        (! (< ?a ?b))
    )

    (&
        (case (< ?a ?b)
            (know (< ?a ?b) (< ?a ?b))
        (case (! (< ?a ?b))
            (< ?a ?b)
        endcase))
        (! (< ?a ?b))
    )

    (know ?a (! ?a))
    (| (! ?a) (! ?a))

    (&
        (case (< ?a ?b)
            (| (! (< ?a ?b)) (< ?a ?b)))
        (case (! (< ?a ?b))
            (< ?a ?b)
        endcase))
        (! (< ?a ?b))
    )


    (case (< ?a ?b)
        ! (< ?a ?b)
    (case (! (< ?a ?b))
        false
    endcase))
     */

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::new(&[
            "(ite ?c ?t ?e) ==> (case ?c ?t (case (! ?c) ?e endcase))",
            "(max ?a ?b) ==> (ite (< ?a ?b) ?b ?a)",
            "(min ?a ?b) ==> (ite (< ?a ?b) ?a ?b)",
            // "(Cheat ?a ?b) ==> (Inter (FRep ?a) (Union (FRep ?a) (FRep ?b)))",
        ])
    }

    fn is_allowed_op(&self) -> bool {
        matches!(
            self,
            CaddyAndFRep::Max(_)
                | CaddyAndFRep::Min(_)
                | CaddyAndFRep::Var(_)
                | CaddyAndFRep::Cheat(_)
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
        if let CaddyAndFRep::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        CaddyAndFRep::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, CaddyAndFRep::Lit(_))
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        CaddyAndFRep::Lit(c)
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}

#[cfg(test)]
mod tests {
    use ruler::{
        enumo::{Filter, Metric, Ruleset, Scheduler, Workload},
        recipe_utils::{base_lang, iter_metric, run_rule_lifting},
    };

    use super::*;

    fn iter_pos(n: usize) -> Workload {
        iter_metric(base_lang(2), "EXPR", Metric::Atoms, n)
            .filter(Filter::Contains("VAR".parse().unwrap()))
            .plug("VAL", &Workload::empty())
            .plug("VAR", &Workload::new(["a", "b"]))
            .plug("OP1", &Workload::empty())
            .plug("OP2", &Workload::new(["max", "min"]))
    }

    #[test]
    fn rule_lifting_recipe() {
        let nat_rules = get_nat_rules();

        let prior = Ruleset::new(&nat_rules);

        let atoms3 = iter_pos(3);
        // assert_eq!(atoms3.force().len(), 51);

        let limits = Limits {
            iter: 3,
            node: 10000000,
            match_: 200_000,
        };

        let eg_init = atoms3.to_egraph();
        // Allowed rules: run on clone, apply unions, no candidates
        let (allowed, _) = prior.partition(|eq| CaddyAndFRep::is_allowed_rewrite(&eq.lhs, &eq.rhs));
        let eg_allowed = Scheduler::Compress(limits).run(&eg_init, &allowed);

        // Translation rules: grow egraph, extract candidates, assert!(saturated)
        let lifting_rules = CaddyAndFRep::get_lifting_rules();
        let eg_denote = Scheduler::Simple(limits).run(&eg_allowed, &lifting_rules);
        let mut candidates = Ruleset::extract_candidates(&eg_allowed, &eg_denote);

        // All rules: clone/no clone doesn't matter, extract candidates
        let mut all_rules = prior;
        all_rules.extend(lifting_rules);
        let eg_final = Scheduler::Compress(limits).run(&eg_denote, &all_rules);
        candidates.extend(Ruleset::extract_candidates(&eg_denote, &eg_final));

        let rules = candidates;
        for r in rules.0.values() {
            println!("{}", r.name)
        }
    }

    #[test]
    fn rule_lifting() {
        let limits = Limits {
            iter: 3,
            node: 1000000,
            match_: 200_000,
        };
        let nat_rules = get_nat_rules();

        let mut all_rules: Ruleset<CaddyAndFRep> = Ruleset::default();
        all_rules.extend(Ruleset::new(&nat_rules));

        let atoms3 = iter_pos(5);
        // assert_eq!(atoms3.force().len(), 51);

        let rules3 = run_rule_lifting(atoms3, all_rules.clone(), limits, limits);
        // assert_eq!(rules3.len(), 6);
        all_rules.extend(rules3);

        let atoms4 = iter_pos(5);
        // assert_eq!(atoms4.force().len(), 255);

        let rules4 = run_rule_lifting(atoms4, all_rules.clone(), limits, limits);
        // assert_eq!(rules4.len(), 2);
        all_rules.extend(rules4);

        let atoms5 = iter_pos(5);
        // assert_eq!(atoms5.force().len(), 1527);

        let rules4 = run_rule_lifting(atoms5, all_rules.clone(), limits, limits);
        // assert_eq!(rules4.len(), 1);
    }
}
