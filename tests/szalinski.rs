// Learn Szalinski rules by lowering to FRep
// Status: work-in-progress.

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

fn get_nat_rules() -> Vec<&'static str> {
    [
        // Small subset of rat rules
        "(+ ?b ?a) ==> (+ ?a ?b)",
        "(* ?b ?a) ==> (* ?a ?b)",
        "(+ 0 ?a) ==> ?a",
        "?a ==> (+ 0 ?a)",
        // "(* ?a 0) ==> 0",
        "(* 1 ?a) ==> ?a",
        "?a ==> (* 1 ?a)",
        "(+ ?c (+ ?b ?a)) ==> (+ ?a (+ ?b ?c))",
        "(* (* ?c ?b) ?a) ==> (* ?b (* ?c ?a))",
        "(* (+ ?b ?b) ?a) ==> (* ?b (+ ?a ?a))",
        "(+ ?a (max ?b ?c)) ==> (max (+ ?a ?b) (+ ?a ?c))",
        "(max (+ ?a ?b) (+ ?a ?c)) ==> (+ ?a (max ?b ?c))",
        "(+ ?a (min ?b ?c)) ==> (min (+ ?a ?b) (+ ?a ?c))",
        "(min (+ ?a ?b) (+ ?a ?c)) ==> (+ ?a (min ?b ?c))",
        "(max ?b ?a) ==> (max ?a ?b)",
        "(min ?b ?a) ==> (min ?a ?b)",
        "(min (max ?a ?b) ?a) ==> ?a",
        "(max (min ?a ?b) ?a) ==> ?a",
        "(max ?a ?a) ==> ?a",
        "(min ?a ?a) ==> ?a",
    ]
    .into()
}

egg::define_language! {
 pub enum CaddyAndFRep  {
    // FRep
    "FRep" = FRep([Id; 1]),
    "max" = Max([Id; 2]),
    "min" = Min([Id; 2]),
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    // "~" = Neg(Id),
    "x" = DimX,
    "y" = DimY,
    "z" = DimZ,
    "subst" = Subst([Id; 3]),

    // "binop" = Binop([Id; 3]),

    // Caddy
    "Caddy" = Caddy([Id; 1]),
    "Cuboid" = Cuboid([Id; 3]),
    "Spheroid" = Spheroid([Id; 3]),
    "Trans" = Trans([Id; 4]),
    "Scale" = Scale([Id; 4]),
    "Union" = Union([Id; 2]),
    "Inter" = Inter([Id; 2]),
    "Empty" = Empty,

    "Cheat" = Cheat([Id; 2]),

    Lit(Constant),
    Var(egg::Symbol),
 }
}

impl SynthLanguage for CaddyAndFRep {
    type Constant = Constant;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::new(&[
            "(/ x ?a) ==> (subst x x (/ x ?a))",
            "(/ y ?a) ==> (subst y y (/ y ?a))",
            "(/ z ?a) ==> (subst z z (/ z ?a))",
            "(min (subst ?e ?from ?to) ?a) ==> (subst (min ?e ?a) ?from ?to)",
            "(- ?a (subst ?e ?from ?to)) ==> (subst (- ?a ?e) ?from ?to)",
            "(Cuboid ?a ?b ?c) ==> (FRep (- 1 (min (min (/ x ?a) (/ y ?b)) (/ z ?c))))",
            "(Union (FRep ?a) (FRep ?b)) ==> (FRep (max ?a ?b))",
            "(FRep (max ?a ?b)) ==> (Union (FRep ?a) (FRep ?b))",
            "(Inter (FRep ?a) (FRep ?b)) ==> (FRep (min ?a ?b))",
            "(FRep (min ?a ?b)) ==> (Inter (FRep ?a) (FRep ?b))",
            // "(Empty) ==> (FRep (~ 1))",
            "(Scale (FRep ?e) ?a ?b ?c)", // "(Cheat ?a ?b) ==> (Inter (FRep ?a) (Union (FRep ?a) (FRep ?b)))",
        ])
    }

    fn is_allowed_op(&self) -> bool {
        matches!(
            self,
            CaddyAndFRep::FRep(_)
                | CaddyAndFRep::Cuboid(_)
                | CaddyAndFRep::Spheroid(_)
                | CaddyAndFRep::Trans(_)
                | CaddyAndFRep::Scale(_)
                | CaddyAndFRep::Var(_)
                | CaddyAndFRep::Union(_)
                | CaddyAndFRep::Inter(_)
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

impl CaddyAndFRep {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let num_prior = prior.len();
        let mut candidates = Ruleset::allow_forbid_actual(egraph, prior.clone(), limits);

        let chosen = candidates.minimize(prior, Scheduler::Compress(limits)).0;
        let time = t.elapsed().as_secs_f64();

        println!(
            "Learned {} bidirectional rewrites ({} total rewrites) in {} using {} prior rewrites",
            chosen.bidir_len(),
            chosen.len(),
            time,
            num_prior
        );

        chosen.pretty_print();

        chosen
    }
}

#[cfg(test)]
mod tests {
    use ruler::enumo::{Ruleset, Scheduler, Workload};

    use super::*;

    fn iter_pos(n: usize) -> Workload {
        Workload::iter_lang(n, &[], &["Empty", "a", "b"], &["FRep"], &["Union", "Inter"])
    }

    #[test]
    fn rule_lifting_recipe() {
        let nat_rules = get_nat_rules();

        let prior = Ruleset::new(&nat_rules);

        let atoms3 = iter_pos(3);
        // assert_eq!(atoms3.force().len(), 51);

        let limits = Limits {
            iter: 4,
            node: 10000000,
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

    // #[test]
    fn rule_lifting() {
        let nat_rules = get_nat_rules();

        let mut all_rules = Ruleset::default();
        all_rules.extend(Ruleset::new(&nat_rules));

        let atoms3 = iter_pos(8);
        // assert_eq!(atoms3.force().len(), 51);

        let rules3 = CaddyAndFRep::run_workload(
            atoms3,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
            },
        );
        // assert_eq!(rules3.len(), 6);
        all_rules.extend(rules3);

        let atoms4 = iter_pos(4);
        // assert_eq!(atoms4.force().len(), 255);

        let rules4 = CaddyAndFRep::run_workload(
            atoms4,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
            },
        );
        // assert_eq!(rules4.len(), 2);
        all_rules.extend(rules4);

        let atoms5 = iter_pos(5);
        // assert_eq!(atoms5.force().len(), 1527);

        let rules4 = CaddyAndFRep::run_workload(
            atoms5,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 1000000,
            },
        );
        // assert_eq!(rules4.len(), 1);
    }
}
