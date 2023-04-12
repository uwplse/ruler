// Learn Szalinski rules by lowering to FRep
// Status: work-in-progress.

// To see the language rules, look at the first ~130 lines, to see the
// recipe attempts, look at the #[test]'s

#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]
use egg::{AstSize, Extractor, RecExpr, Language, CostFunction, Runner};
use itertools::enumerate;
use num::{rational::Ratio, BigInt, Signed, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use rayon::vec;
use ruler::{
    enumo::{Filter, Metric, Ruleset, Scheduler, Workload, Rule},
    *,
};
use std::{collections::HashMap, usize, io::{self, Write}};
use std::collections::HashSet;
use std::fs;
use std::time::SystemTime;
use std::{hash::Hash, time::Instant};

pub type Constant = i64;

egg::define_language! {
 pub enum CF  {
    // FRep
    "max" = Max([Id; 2]),
    "min" = Min([Id; 2]),
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    "x" = DimX,
    "y" = DimY,
    "z" = DimZ,
    "subst" = Subst([Id; 4]),
    // "Cheat" = Cheat([Id; 7]),
    // Indicate that the containing expr cannot have x, y, or z
    "Scalar" = Scalar(Id),
    Lit(Constant),

    // Caddy
    "Cube" = Cube([Id; 2]),
    "Cylinder" = Cylinder([Id; 3]),
    "Sphere" = Sphere([Id; 2]),
    "Trans" = Trans([Id; 2]),
    "Scale" = Scale([Id; 2]),
    "Union" = Union([Id; 2]),
    "Inter" = Inter([Id; 2]),
    "Vec3" = Vec3([Id; 3]),
    "true" = True,
    "false" = False,
    "Empty" = Empty,

    Var(egg::Symbol),
 }
}


fn get_subst_rules() -> Vec<&'static str> {
    [
        "0 ==> (Scalar 0)",
        "1 ==> (Scalar 1)",
        "2 ==> (Scalar 2)",
        "(Scalar 0) ==> 0",
        "(Scalar 1) ==> 1",
        "(Scalar 2) ==> 2",
        "(subst ?x ?y ?z (Scalar ?a)) ==> (Scalar ?a)",
        "(subst ?x ?y ?z x) ==> ?x",
        "(subst ?x ?y ?z y) ==> ?y",
        "(subst ?x ?y ?z z) ==> ?z",
        "(subst ?x ?y ?z (min ?a ?b)) ==> (min (subst ?x ?y ?z ?a) (subst ?x ?y ?z ?b))",
        "(subst ?x ?y ?z (max ?a ?b)) ==> (max (subst ?x ?y ?z ?a) (subst ?x ?y ?z ?b))",
        "(subst ?x ?y ?z (- ?a ?b)) ==> (- (subst ?x ?y ?z ?a) (subst ?x ?y ?z ?b))",
        "(subst ?x ?y ?z (+ ?a ?b)) ==> (+ (subst ?x ?y ?z ?a) (subst ?x ?y ?z ?b))",
        "(subst ?x ?y ?z (* ?a ?b)) ==> (* (subst ?x ?y ?z ?a) (subst ?x ?y ?z ?b))",
        "(subst ?x ?y ?z (/ ?a ?b)) ==> (/ (subst ?x ?y ?z ?a) (subst ?x ?y ?z ?b))",
        "(subst ?x ?y ?z (subst ?x2 ?y2 ?z2 ?a)) ==>
            (subst (subst ?x ?y ?z ?x2)
                   (subst ?x ?y ?z ?y2)
                   (subst ?x ?y ?z ?z2)
                   ?a)",
        "(subst x y z ?a) ==> ?a",
        "(Scalar (/ ?a ?b)) ==> (/ (Scalar ?a) (Scalar ?b))",
        "(Scalar (* ?a ?b)) ==> (* (Scalar ?a) (Scalar ?b))",
        "(Scalar (+ ?a ?b)) ==> (+ (Scalar ?a) (Scalar ?b))",
    ].into()
}

fn get_frep_rules() -> Vec<&'static str> {
    [
        "(/ ?a 1) ==> ?a",
        "(- ?a 0) ==> ?a",
        "(- (/ ?a ?c) (/ ?b ?c)) ==> (/ (- ?a ?b) ?c)",
        "(- (/ ?a ?c) ?b) ==> (/ (- ?a (* ?b ?c)) ?c)",
        "(/ (/ ?a ?b) ?c) ==> (/ ?a (* ?b ?c))",        
        "(- ?a (+ ?b ?c)) ==> (- (- ?a ?b) ?c)",
        "(min (max ?a ?b) ?a) ==> ?a",
        "(max (min ?a ?b) ?a) ==> ?a",
        "(max ?a ?a) ==> ?a",
        "(min ?a ?a) ==> ?a",
    ]
    .into()
}

fn get_subst_and_frep_rules() -> Vec<&'static str>{
    let mut m = get_frep_rules();
    m.extend(get_subst_rules());
    m
}

fn get_lifting_rules() -> Vec<&'static str> {
    [
        // "(Union ?a ?b) ==> (max ?a ?b)",
        // "(Inter ?a ?b) ==> (min ?a ?b)",
        "(Scale (Vec3 ?w ?h ?l) ?e) ==> (subst (/ x (Scalar ?w)) (/ y (Scalar ?h)) (/ z (Scalar ?l)) ?e)",
        "(Cube (Vec3 ?a ?b ?c) false) ==>
         (min (min (min (/ x (Scalar ?a))
                        (/ y (Scalar ?b)))
                   (/ z (Scalar ?c)))
              (min (min (- 1 (/ x (Scalar ?a)))
                        (- 1 (/ y (Scalar ?b))))
                   (- 1 (/ z (Scalar ?c)))))",
        "(Cylinder (Vec3 ?h ?r ?r) ?params true) ==>
         (min (- (- 1 (* (/ x (Scalar ?r)) (/ x (Scalar ?r))))
                 (* (/ y (Scalar ?r)) (/ y (Scalar ?r))))
              (min (- (/ 1 2) (/ z (Scalar ?h)))
                   (+ (/ 1 2) (/ z (Scalar ?h)))))",
        "(Sphere ?r ?params) ==> (- (- (- 1 (* (/ x (Scalar ?r)) (/ x (Scalar ?r))))
                               (* (/ y (Scalar ?r)) (/ y (Scalar ?r))))
                            (* (/ z (Scalar ?r)) (/ z (Scalar ?r))))",
        "(Trans (Vec3 ?a ?b ?c) ?e) ==> (subst (- x (Scalar ?a)) (- y (Scalar ?b)) (- z (Scalar ?c)) ?e)"
    ].into()
}


fn is_caddy(node: &CF) -> bool {
    match node {
        CF::Max(_) => false,
        CF::Add(_) => false,
        CF::Min(_) => false,
        CF::Sub(_) => false,
        CF::Mul(_) => false,
        CF::Div(_) => false,
        CF::DimX => false,
        CF::DimY => false,
        CF::DimZ => false,
        CF::Subst(_) => false,
        CF::Scalar(_) => false,
        CF::Lit(_) => false,
        // CF::Cheat(_) => false,
        CF::Cube(_) => true,
        CF::Cylinder(_) => true,
        CF::Sphere(_) => true,
        CF::Trans(_) => true,
        CF::Scale(_) => true,
        CF::Union(_) => true,
        CF::Inter(_) => true,
        CF::Empty => true,
        CF::Var(_) => false,
        CF::Vec3(_) => true,
        CF::True => true,
        CF::False => true,
    }
}

fn op_cost (enode: &CF) -> f64 {
    if is_caddy(enode) {
        return 1000.0;
    }
    if matches!(enode, CF::Subst(_)) {
        return 100.0;
    }
    return 0.;
}

struct SzalinskiCostFunction;
impl CostFunction<CF> for SzalinskiCostFunction {
    type Cost = f64;
    fn cost<C>(&mut self, enode: &CF, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost
    {
        enode.fold(op_cost(enode), |sum, id| sum + costs(id))
    }
}

const DEBUG: bool = true;

impl SynthLanguage for CF {
    type Constant = Constant;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::new(&get_lifting_rules())
    }

    fn is_allowed_op(&self) -> bool {
        is_caddy(self) || matches!(
            self, CF::Var(_)
                | CF::Lit(_)
                | CF::Div(_)
                | CF::Mul(_)
                | CF::Sub(_)
                | CF::Add(_)
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

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}

fn flush() {
    if let Err(error) = io::stdout().flush() {
        println!("couldn't flush {}", error);
    }
}

fn timekeep(msg: String) {
    println!("{} {}", time(), msg);
    flush();
}

fn paste_print(rs: &Ruleset<CF>) {
    println!("\npaste these vvv\n");
    let mut i = 1;

    fn fix(s: String) -> String {
        s.replace("Scale", "Affine Scale")
         .replace("Trans", "Affine Trans")
    }

    for (s, rule) in rs {
        println!(
            "rw!(\"ruler{}\"; \"{}\" => \"{}\"),",
            i,
            fix(rule.lhs.to_string()),
            fix(rule.rhs.to_string())
        );
        i += 1;
    }
    println!("\n");
}


fn time() -> String {
    let mut t = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs()
        - 1680159600;
    let days = t / (60 * 60 * 24) - 1;
    t %= 60 * 60 * 24;
    let hours = t / (60 * 60);
    t %= 60 * 60;
    let minutes = t / 60;
    t %= 60;
    format!("4-{} {}:{:02}:{:02}", days, hours, minutes, t)
}

fn dump_workload(fname: String, w: &Workload) {
    let mut data = w
        .force()
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join("\n");
    data = format!("{}\n{}\n", time(), data);
    fs::write(fname, data).expect("Unable to write file");
}


impl CF {
    pub fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let num_prior = prior.len();
        
        timekeep("starting allow_forbid_actual".into());
        
        let mut candidates = Ruleset::allow_forbid_actual(egraph, prior.clone(), limits);


        timekeep("allow_forbid_actual done".into());
        
        candidates.pretty_print();

        timekeep("starting minimize".into());

        let chosen = candidates.minimize(prior, Scheduler::Compress(limits));

        timekeep("minimize done".into());

        let time = t.elapsed().as_secs_f64();

        println!();

        println!(
            "Learned {} bidirectional rewrites ({} total rewrites) in {} using {} prior rewrites",
            chosen.bidir_len(),
            chosen.len(),
            time,
            num_prior
        );

        chosen.pretty_print();

        paste_print(&chosen);
        chosen
    }
}

#[cfg(test)]
mod tests {
    use rayon::iter;
    use ruler::enumo::{Ruleset, Scheduler, Workload};

    use super::*;

    // This gives all the rules, but is overfit
    #[test]
    fn rule_lifting_overfit() {
        let mut all_rules = Ruleset::new(&get_subst_and_frep_rules());
        let atoms3 = Workload::new([
            "(Scale (Vec3 sa sb sc) (Cube (Vec3 1 1 1) false))", "(Cube (Vec3 sa sb sc) false)",
            "(Scale (Vec3 sa sa sa) (Sphere 1 ?params))",  "(Sphere sa ?params)",
            "(Scale (Vec3 r r h) (Cylinder (Vec3 1 1 1) params true))", "(Cylinder (Vec3  h r r) params true)",
            "(Scale (Vec3 sa sb sc) (Trans (Vec3 ta tb tc) a))", "(Trans (Vec3 (* ta sa) (* tb sb) (* tc sc)) (Scale (Vec3 sa sb sc) a))",
            "(Trans (Vec3 ta tb tc) (Scale (Vec3 sa sb sc) a))", "(Scale (Vec3 sa sb sc) (Trans (Vec3 (/ ta sa) (/ tb sb) (/ tc sc)) a))",
            "(Trans (Vec3 0 0 0) a)", "a",
            "(Scale (Vec3 1 1 1) a)", "a",
            "(Scale (Vec3 sa sb sc) (Scale (Vec3 ta tb tc) a))", "(Scale (Vec3 (* sa ta) (* sb tb) (* sc tc)) a)",
            "(Trans (Vec3 sa sb sc) (Trans (Vec3 ta tb tc) a))", "(Trans (Vec3 (+ sa ta) (+ sb tb) (+ sc tc)) a)",
            // "(Union (Inter a b) a)", "a",
            // "(Inter (Union a b) a)", "a",
        ]);

        let rules3 = CF::run_workload(
            atoms3,
            all_rules.clone(),
            Limits {
                iter: 5,
                node: 10000000000,
            },
        );
        all_rules.extend(rules3);
    }

    // Draft of a better workload
    fn iter_szalinski(n: usize) -> Workload {
        let lang = Workload::new([
            "(transformation v3 shape)",
            "(Cube v3 false)",
            "(Cylinder v3 params true)",
            "(Sphere scalar params)",
        ]);
        let scalars: &[&str] = &["a", "b", "c", "1", "0"];
        let transformations: &[&str] = &["Scale", "Trans"];
        let v3s: &[&str] = &["(Vec3 scalar scalar scalar)"];
        let w = lang
            .iter_metric("shape", Metric::Depth, n)
            .plug("v3", &v3s.into())
            .plug("transformation", &transformations.into())
            .plug("scalar", &scalars.into());


        let lang = Workload::new([
            "(transformation v3 shape)",
            "(Cube v3 false)",
            "(Cylinder v3 params true)",
            "(Sphere scalar params)",
            "s",
        ]);
        let scalars: &[&str] = &["sa", "1"];
        let transformations: &[&str] = &["Scale", "Trans"];
        let bops: &[&str] = &["+", "*", "/"];
        let v3s: &[&str] = &[
            "(Vec3 0 0 0)",
            "(Vec3 1 1 1)",
            "(Vec3 a a a)",
            "(Vec3 a b c)",
            "(Vec3 d e f)",
            "(Vec3 (bop a d) (bop b e) (bop c f))"
        ];
        let w = lang
            .iter_metric("shape", Metric::Depth, n)
            .plug("v3", &v3s.into())
            .plug("transformation", &transformations.into())
            .plug("scalar", &scalars.into())
            .plug("bop", &bops.into());
        dump_workload("wl.txt".into(), &w);
        w
    }

    // Simple attempt to use iter_szalinski.
    #[test]
    fn rule_lifting() {
        let mut learned_rules = Ruleset::default();
        let mut all_rules: Ruleset<CF> = Ruleset::new(&get_subst_and_frep_rules());

        for i in 2..4 {
            println!("MAX DEPTH OF {}", i);

            let atoms = iter_szalinski(i);
            let t = Instant::now();

            let limits = Limits {iter: 4, node: 10_000_000};

            let egraph = atoms.to_egraph::<CF>();
            
            timekeep("starting allow_forbid_actual".into());
            let mut candidates = Ruleset::allow_forbid_actual(egraph, all_rules.clone(), limits);

            timekeep("allow_forbid_actual done".into());
            candidates.pretty_print();

            timekeep("starting minimize".into());
            let chosen = candidates.minimize(all_rules.clone(), Scheduler::Compress(limits));

            timekeep("minimize done".into());
            println!();
            all_rules.extend(chosen.clone());
            learned_rules.extend(chosen);
            learned_rules.pretty_print();
            paste_print(&learned_rules);  
        }
    }

    // An attempt to help w scaling by separating translation/subst rules.
    // Status: doesn't get the right rules for small iters
    #[test]
    fn rule_lifting_recipe_fine() {
        let subst_ruleset: Ruleset<CF> = Ruleset::new(&get_subst_rules());
        let frep_ruleset: Ruleset<CF> = Ruleset::new(&get_frep_rules());

        let translational_ruleset = CF::get_lifting_rules();

        let mut learned_rules: Ruleset<CF> = Ruleset::default();

        for i in 2..3 {
            timekeep(format!("Size is {i}!!!!!").into());

            let atoms3 = iter_szalinski(i);
            dump_workload(format!("{} wl.txt", i), &atoms3);

            let eg_init = atoms3.to_egraph();

            timekeep("Running translation rules".into());
            let eg_translated = Scheduler::Simple(Limits {iter: 2, node: 10_000_000}).run(&eg_init, &translational_ruleset);
            
            timekeep("Running subst rules".into());
            let eg_substed = Scheduler::Simple(Limits {iter: 10, node: 10_000_000}).run(&eg_translated, &subst_ruleset);
            
            timekeep("Running frep rules".into()); 
            let eg_final = Scheduler::Simple(Limits {
                iter: 3,
                node: 10_000_000,
            }).run(&eg_substed, &frep_ruleset);

            let mut candidates = Ruleset::extract_candidates(&eg_init, &eg_final);

            timekeep("Candidates".into());
            candidates.pretty_print();

            let chosen = candidates.minimize(learned_rules.clone(), Scheduler::Compress(Limits {
                iter: 5,
                node: 10_000_000,
            }));
            
            timekeep("minimized".into());

            chosen.pretty_print();
            println!("");
            learned_rules.extend(chosen.clone());
        }

        learned_rules.pretty_print();
        paste_print(&learned_rules);

    }

    // A coarser recipe than the above. status: not working
    #[test]
    fn rule_lifting_recipe2() {
        let mut all_rules: Ruleset<CF> = Ruleset::new(&get_subst_and_frep_rules());
        
        let mut learned_rules = Ruleset::default();

        for i in 2..4 {
            println!("MAX DEPTH OF {}", i);

            let atoms = iter_szalinski(i);
            let egraph = atoms.to_egraph::<CF>();
            
            timekeep("starting allow_forbid_actual".into());

            let eg_init = egraph;

            // Allowed rules: run on clone, apply unions, no candidates
            let (allowed, _) = all_rules.partition(|rule| CF::is_allowed_rewrite(&rule.lhs, &rule.rhs));
            let eg_allowed = Scheduler::Compress(Limits {
                iter: 5,
                node: 10000000000,
            }).run(&eg_init, &allowed);

            // Translation rules: grow egraph, extract candidates, assert!(saturated)
            let lifting_rules = CF::get_lifting_rules();
            let eg_denote = Scheduler::Simple(Limits {
                iter: 5,
                node: 10000000000,
            }).run(&eg_allowed, &lifting_rules);
            let mut candidates = Ruleset::extract_candidates(&eg_allowed, &eg_denote);

            // All rules: clone/no clone doesn't matter, extract candidates
            all_rules.extend(lifting_rules);
            let eg_final = Scheduler::Compress(Limits {
                iter: 5,
                node: 10000000000,
            }).run(&eg_denote, &all_rules);
            candidates.extend(Ruleset::extract_candidates(&eg_denote, &eg_final));

            timekeep("allow_forbid_actual done".into());
            candidates.pretty_print();

            timekeep("starting minimize".into());
            let chosen = candidates.minimize(learned_rules.clone(), Scheduler::Compress(Limits {
                iter: 5,
                node: 10000000000,
            }));

            timekeep("minimize done".into());
            println!();

            chosen.pretty_print();

            all_rules.extend(chosen.clone());

            learned_rules.extend(chosen);
            learned_rules.pretty_print();
            paste_print(&learned_rules);  
        }
    }
}
