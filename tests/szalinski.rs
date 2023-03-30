// Learn Szalinski rules by lowering to FRep
// Status: work-in-progress.

#![allow(unused_imports)]
#![allow(unused_variables)]
use num::{rational::Ratio, BigInt, Signed, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use rayon::vec;
use ruler::{
    enumo::{Filter, Ruleset, Metric, Scheduler, Workload},
    *,
};
use std::time::Instant;
use std::fs;
use std::time::SystemTime;

pub type Constant = usize;


fn get_nat_rules() -> Vec<&'static str> {
    [
        // Small subset of rat rules
        // "(+ ?b ?a) ==> (+ ?a ?b)",
        // "(* ?b ?a) ==> (* ?a ?b)",
        // "(+ 0 ?a) ==> ?a",
        // "?a ==> (+ 0 ?a)",
        // "(* ?a 0) ==> 0",
        // "(* 1 ?a) ==> ?a",
        // "?a ==> (* 1 ?a)",
        // "(+ ?c (+ ?b ?a)) ==> (+ ?a (+ ?b ?c))",
        // "(* (* ?c ?b) ?a) ==> (* ?b (* ?c ?a))",
        // "(* (+ ?b ?b) ?a) ==> (* ?b (+ ?a ?a))",

        // "(+ ?a (max ?b ?c)) ==> (max (+ ?a ?b) (+ ?a ?c))",
        // "(max (+ ?a ?b) (+ ?a ?c)) ==> (+ ?a (max ?b ?c))",
        // "(+ ?a (min ?b ?c)) ==> (min (+ ?a ?b) (+ ?a ?c))",
        // "(min (+ ?a ?b) (+ ?a ?c)) ==> (+ ?a (min ?b ?c))",
        // "(max ?b ?a) ==> (max ?a ?b)",
        // "(min ?b ?a) ==> (min ?a ?b)",
        "(min (max ?a ?b) ?a) ==> ?a",
        "(max (min ?a ?b) ?a) ==> ?a",
        "(max ?a ?a) ==> ?a",
        "(min ?a ?a) ==> ?a",


        // Subst computing
        "(subst x x ?to) ==> ?to",
        "(subst x y ?to) ==> x",
        "(subst x z ?to) ==> x",
        "(subst y x ?to) ==> y",
        "(subst y y ?to) ==> ?to",
        "(subst y z ?to) ==> y",
        "(subst z x ?to) ==> z",
        "(subst z y ?to) ==> z",
        "(subst z z ?to) ==> ?to",
        "(subst (min ?a ?b) ?from ?to) ==> (min (subst ?a ?from ?to) (subst ?b ?from ?to))",
        "(subst (- ?a ?b) ?from ?to) ==> (- (subst ?a ?from ?to) (subst ?b ?from ?to))",
        "(subst (/ ?a ?b) ?from ?to) ==> (/ (subst ?a ?from ?to) (subst ?b ?from ?to))",

        "(subst 1 ?from ?to) ==> 1",
    ]
    .into()
}

fn get_lifting_rules() -> Vec<&'static str> {
    [
        // Subst lifting
        // "(/ x ?a) ==> (subst x x (/ x ?a))",
        // "(/ y ?a) ==> (subst y y (/ y ?a))",
        // "(/ z ?a) ==> (subst z z (/ z ?a))",
        // "(min (subst ?e ?from ?to) ?a) ==> (subst (min ?e ?a) ?from ?to)",
        // "(- ?a (subst ?e ?from ?to)) ==> (subst (- ?a ?e) ?from ?to)",

        
        "(Union (FRep ?a) (FRep ?b)) ==> (FRep (max ?a ?b))",
        "(FRep (max ?a ?b)) ==> (Union (FRep ?a) (FRep ?b))",
        "(Inter (FRep ?a) (FRep ?b)) ==> (FRep (min ?a ?b))",
        "(FRep (min ?a ?b)) ==> (Inter (FRep ?a) (FRep ?b))",
        // "(Empty) ==> (FRep (~ 1))",


        "(Scale (FRep ?e) ?a ?b ?c) ==> (FRep (subst (subst (subst ?e x (/ x ?a)) y (/ y ?b)) z (/ z ?c)))",
        "(FRep (subst (subst (subst ?e x (/ x ?a)) y (/ y ?b)) z (/ z ?c))) ==> (Scale (FRep ?e) ?a ?b ?c)",

        "(Cuboid ?a ?b ?c) ==> (FRep (- 1 (min (min (/ x ?a) (/ y ?b)) (/ z ?c))))",
        "(FRep (- 1 (min (min (/ x ?a) (/ y ?b)) (/ z ?c)))) ==> (Cuboid ?a ?b ?c)",
        // "(Cheat ?a ?b) ==> (Inter (FRep ?a) (Union (FRep ?a) (FRep ?b)))",
        
        // "(Scale (Cuboid ?w ?l? ?h) ?a ?b ?c) ==> (FRep (- 1 (min (min (/ x (* ?w ?a)) (/ y (* ?l ?b))) (/ z (* ?h ?c)))))",
        // "(Scale (Cuboid ?w ?l? ?h) ?a ?b ?c) ==> (FRep (- 1 (min (min (/ x (* ?w ?a)) (/ y (* ?l ?b))) (/ z (* ?h ?c)))))",
        // "(Scale (Cuboid 1 1 1) ?a ?b ?c) ==> (FRep (- 1 (min (min (/ x ?a) (/ y ?b)) (/ z ?c))))",

        "(Scale (Cuboid ?w ?l? ?h) ?a ?b ?c) ==> (Cheat ?a ?b ?c)",
        "(Cheat ?a ?b ?c) ==> (Cuboid ?a ?b ?c)",
    ].into()
}

egg::define_language! {
 pub enum CF  {
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

    // Caddy
    "Cuboid" = Cuboid([Id; 3]),
    "Spheroid" = Spheroid([Id; 3]),
    "Trans" = Trans([Id; 4]),
    "Scale" = Scale([Id; 4]),
    "Union" = Union([Id; 2]),
    "Inter" = Inter([Id; 2]),
    "Empty" = Empty,

    "Cheat" = Cheat([Id; 3]),

    Lit(Constant),
    Var(egg::Symbol),
 }
}

fn compute_subst(egraph: &mut EGraph<CF, SynthAnalysis>, e: Id, from: &CF, to: &Id) -> Option<CF>{
    // Choose arbitrary node to continue computation on
    let nodes = egraph[e].nodes.clone();
    for n in nodes {
        match n {
            CF::Min([a, b]) => {
                if let Some(a_substed) = compute_subst(egraph, a, from, to) {
                    if let Some(b_substed) = compute_subst(egraph, b, from, to) {
                        // TODO: Only add to egraph if entire computation
                        // is successful by maintaining a tree of terms only.
                        let a_id = egraph.add(a_substed);
                        let b_id = egraph.add(b_substed);
                        return Some(CF::Min([a_id, b_id]));
                    }
                }
            }

            CF::Div([a, b]) => {
                if let Some(a_substed) = compute_subst(egraph, a, from, to) {
                    if let Some(b_substed) = compute_subst(egraph, b, from, to) {
                        let a_id = egraph.add(a_substed);
                        let b_id = egraph.add(b_substed);
                        return Some(CF::Div([a_id, b_id]));
                    }
                }
            }

            CF::Sub([a, b]) => {
                if let Some(a_substed) = compute_subst(egraph, a, from, to) {
                    if let Some(b_substed) = compute_subst(egraph, b, from, to) {
                        let a_id = egraph.add(a_substed);
                        let b_id = egraph.add(b_substed);
                        return Some(CF::Sub([a_id, b_id]));
                    }
                }
            }

            CF::DimX | CF::DimY | CF::DimZ => {
                if *from == n {
                    return Some(egraph[*to].nodes[0].clone());
                } else {
                    return Some(n);
                }
            }

            CF::Lit(i) => {
                return Some(n);
            }
            _ => (),
        }
    }
    None
}

impl SynthLanguage for CF {
    type Constant = Constant;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::new(&get_lifting_rules())
    }

    fn is_allowed_op(&self) -> bool {
        matches!(
            self,
            CF::FRep(_)
                | CF::Cuboid(_)
                | CF::Spheroid(_)
                | CF::Trans(_)
                | CF::Scale(_)
                | CF::Var(_)
                | CF::Union(_)
                | CF::Inter(_)
                | CF::Cheat(_)
                | CF::Lit(_)
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
        if let CF::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        CF::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, CF::Lit(_))
    }

    fn mk_constant(c: Self::Constant, egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        CF::Lit(c)
    }

    fn custom_modify(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {

        let get_dim_var = |nodes: &[CF]| {
            for n in nodes {
                if let CF::DimX = n {
                    return Some(CF::DimX);
                } else if let CF::DimY = n {
                    return Some(CF::DimY);
                } else if let CF::DimZ = n {
                    return Some(CF::DimZ);
                }
            }
            None
        };


        for n in &egraph[id].nodes {
            if let CF::Subst([e, from, to]) = egraph[id].nodes[0] {
                if let Some(from_var) = get_dim_var(&egraph[from].nodes) {
                    if let Some(substed) = compute_subst(egraph, e, &from_var, &to) {
                        let id2 = egraph.add(substed);
                        egraph.union(id, id2);
                    }
                    return;
                }
            }
        }
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}

impl CF {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let num_prior = prior.len();
        let mut candidates = Ruleset::allow_forbid_actual(egraph, prior.clone(), limits);

        let chosen = candidates.minimize(prior, Scheduler::Compress(limits));
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


fn time() -> String {
    let mut t = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs() - 1680159600;
    let days = t / (60 * 60 * 24);
    t %= 60 * 60 * 24;
    let hours = t / (60 * 60);
    t %= 60 * 60;
    let minutes = t / 60;
    t %= 60;
    format!("{}d {}:{:02}:{:02}", days, hours, minutes, t)
}
#[cfg(test)]
mod tests {
    use ruler::enumo::{Ruleset, Scheduler, Workload};

    use super::*;

    fn iter_szalinski(n: usize) -> Workload {
        let lang = Workload::new([
            // For union, inter
            // "(FRep var)",
            // "(bop shape shape)",

            // "(Scale shape var var var)",
            // "(Cuboid cnst cnst cnst)",
            "(Scale (Cuboid cnst cnst cnst) var var var)",
        ]);

        let consts: &[&str] = &["1"];
        let vars: &[&str] = &["a", "b", "c"];
        let bops: &[&str] = &["Union", "Inter"];
        
        let w = lang.iter_metric("shape", Metric::Atoms, n)
            // .filter(Filter::Contains("var".parse().unwrap()))
            .plug("cnst", &consts.into())
            .plug("var", &vars.into())
            .plug("bop", &bops.into());
        
        let mut data = w.force().iter().map(|x| x.to_string()).collect::<Vec<String>>().join("\n");
        data = format!("{}\n{}\n", time(), data);
        fs::write("wl.txt", data).expect("Unable to write file");
        w
    }

    #[test]
    fn rule_lifting_recipe() {
        let nat_rules = get_nat_rules();

        let prior = Ruleset::new(&nat_rules);

        let atoms3 = iter_szalinski(8);
        // assert_eq!(atoms3.force().len(), 51);

        let limits = Limits {
            iter: 4,
            node: 10000000,
        };

        let eg_init = atoms3.to_egraph();
        // Allowed rules: run on clone, apply unions, no candidates
        let (allowed, _) = prior.partition(|eq| CF::is_allowed_rewrite(&eq.lhs, &eq.rhs));
        let eg_allowed = Scheduler::Compress(limits).run(&eg_init, &allowed);

        // Translation rules: grow egraph, extract candidates, assert!(saturated)
        let lifting_rules = CF::get_lifting_rules();
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
        let nat_rules = get_nat_rules();

        let mut all_rules = Ruleset::default();
        all_rules.extend(Ruleset::new(&nat_rules));

        let atoms3 = iter_szalinski(8);
        // assert_eq!(atoms3.force().len(), 51);

        let rules3 = CF::run_workload(
            atoms3,
            all_rules.clone(),
            Limits {
                iter: 20,
                node: 10000000,
            },
        );
        all_rules.extend(rules3);

        // let atoms4 = iter_szalinski(8);

        // let rules4 = CF::run_workload(
        //     atoms4,
        //     all_rules.clone(),
        //     Limits {
        //         iter: 3,
        //         node: 1000000,
        //     },
        // );
        // all_rules.extend(rules4);

    }
}
