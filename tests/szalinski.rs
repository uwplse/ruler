// Learn Szalinski rules by lowering to FRep
// Status: work-in-progress.

#![allow(unused_imports)]
#![allow(unused_variables)]
use egg::{AstSize, Extractor, RecExpr, Language, CostFunction};
use itertools::enumerate;
use num::{rational::Ratio, BigInt, Signed, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use rayon::vec;
use ruler::{
    enumo::{Filter, Metric, Ruleset, Scheduler, Workload},
    *,
};
use std::{collections::HashMap, usize};
use std::collections::HashSet;
use std::fs;
use std::time::SystemTime;
use std::{hash::Hash, time::Instant};

pub type Constant = usize;

egg::define_language! {
 pub enum CF  {
    // FRep
    "max" = Max([Id; 2]),
    "min" = Min([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    "x" = DimX,
    "y" = DimY,
    "z" = DimZ,
    "subst" = Subst([Id; 4]),
    "Cheat" = Cheat([Id; 7]),
    // Indicate that the containing expr cannot have x, y, or z
    "Scalar" = Scalar(Id),
    Lit(Constant),

    // Caddy
    "Cube" = Cube([Id; 3]),
    "Cylinder" = Cylinder([Id; 3]),
    "Sphere" = Sphere(Id),
    "Trans" = Trans([Id; 4]),
    "Scale" = Scale([Id; 4]),
    "Union" = Union([Id; 2]),
    "Inter" = Inter([Id; 2]),
    "Empty" = Empty,


    Var(egg::Symbol),
 }
}


fn is_caddy(node: &CF) -> bool {
    match node {
        CF::Max(_) => false,
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
        CF::Cheat(_) => false,
        CF::Cube(_) => true,
        CF::Cylinder(_) => true,
        CF::Sphere(_) => true,
        CF::Trans(_) => true,
        CF::Scale(_) => true,
        CF::Union(_) => true,
        CF::Inter(_) => true,
        CF::Empty => true,
        CF::Var(_) => false,
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

#[test]
fn custom_modify() {
    let term_expected_pairs = [
        (
            "(subst (/ x ?a) y z (- 1 (- (- x y) z)))",
            "(- 1 (- (- (/ x ?a) y) z))",
        ),
        ("(subst (/ x a) y z x)", "(/ x a)"),

        ("(subst (/ x a) y z (min x x))", "(min (/ x a) (/ x a))"),
        (
            "(subst (/ x a) y z (- x y))",
            "(- (/ x a) y)",
        ),
        (
            "(subst (/ x a) y z (* x x))",
            "(* (/ x a) (/ x a))",
        ),

        (
            "(subst (/ x ?a) (/ y ?b) (/ z ?c)
                
            (min (/ x 1)
            (min (- 1 (/ x 1))
               (min y
                  (min (- 1 y)
                     (min z
                        (- 1 z))))))

              )",


            "(min (/ (/ x ?a) 1)
            (min (- 1 (/ (/ x ?a) 1))
               (min (/ y ?b)
                  (min (- 1 (/ y ?b))
                     (min (/ z ?c)
                        (- 1 (/ z ?c)))))))",
        ),
        (
            "(subst (/ x (Scalar sa)) (/ y (Scalar sb)) (/ z (Scalar sc))
             (subst (- x (Scalar ta)) (- y (Scalar tb)) (- z (Scalar tc))
                x
            ))",
            "(- (/ x (Scalar sa)) (Scalar ta))",
        ),

        (
            "(subst (/ x (Scalar sa)) (/ y (Scalar sb)) (/ z (Scalar sc))
             (subst (- x (Scalar ta)) (- y (Scalar tb)) (- z (Scalar tc))
                ?b
            ))",
            "
            (subst (- (/ x (Scalar sa)) (Scalar ta)) (- (/ y (Scalar sb)) (Scalar tb)) (- (/ z (Scalar sc)) (Scalar tc))
            ?b
            )
            ",
        ),
    ];

    for (term, expected) in term_expected_pairs {
        let mut egraph: EGraph<CF, SynthAnalysis> = EGraph::default();
        let id = egraph.add_expr(&term.parse().unwrap());

        CF::custom_modify(&mut egraph, id);

        let id2 = egraph.add_expr(&expected.parse().unwrap());

        assert_eq!(id, id2);
    }
}

fn get_frep_rules() -> Vec<&'static str> {
    [
        "(Scalar 1) ==> 1",
        "(/ ?a 1) ==> ?a",
        "(- (/ ?a ?b) (/ ?c ?b)) ==> (/ (- ?a ?c) ?b)",
        "(/ (- ?a ?b) ?c) ==> (- (/ ?a ?c) (/ ?b ?c))",
        "(- (/ ?a ?b) ?c) ==> (/ (- ?a (* ?b ?c)) ?b)",
        "(/ (- ?a (* ?b ?c)) ?b) ==> (- (/ ?a ?b) ?c)",
        "(/ ?a (Scalar ?a)) ==> 1",
        "(/ (Scalar ?a) (Scalar ?b)) ==> (Scalar (/ (Scalar ?a) (Scalar ?b)))",
        "(- (Scalar ?a) (Scalar ?b)) ==> (Scalar (- (Scalar ?a) (Scalar ?b)))",
        "(* (Scalar ?a) (Scalar ?b)) ==> (Scalar (* (Scalar ?a) (Scalar ?b)))",
        "(- (* ?a ?c) (* ?b ?c)) ==> (* (- ?a ?b) ?c)",
        "(min (max ?a ?b) ?a) ==> ?a",
        "(max (min ?a ?b) ?a) ==> ?a",
        "(max ?a ?a) ==> ?a",
        "(min ?a ?a) ==> ?a",
        "(- ?a 0) ==> ?a",
        "(* ?a ?b) ==> (* ?b ?a)",
        // "(/ (- ?a (* ?b ?c)) ?b) ="


        // "(Scale ?sa ?sb ?sc (Trans ?ta ?tb ?tc ?a)) ==>
        //     // (subst (- (/ x (Scalar ?sa)) (Scalar ?ta)) (- (/ y (Scalar ?sb)) (Scalar ?tb)) (- (/ z (Scalar ?sc)) (Scalar ?tc)) ?a)",

        // "(Trans (* ?ta ?sa) (* ?tb ?sb) (* ?tc ?sc) (Scale ?sa ?sb ?sc ?a)) ==>
        // (Cheat ?ta ?sa ?tb ?sb ?tc ?sc ?a)",

        // "(Cheat ?ta ?sa ?tb ?sb ?tc ?sc ?a) ==>
        //  (Scale ?sa ?sb ?sc (Trans ?ta ?tb ?tc ?a))",
        // (subst (- (/ x (Scalar ?sa)) (Scalar ?ta)) (- (/ y (Scalar ?sb)) (Scalar ?tb)) (- (/ z (Scalar ?sc)) (Scalar ?tc)) ?a)",
        // "(Trans (* 
        // (subst (- (/ x ?sa) ?ta) (- (/ y ?sb) ?tb) (- (/ z ?sc) ?tc) ?a)",?ta ?sa) (* ?tb ?sb) (* ?tc ?sc) (Scale ?sa ?sb ?sc ?a)) ==>
        // "(Trans ta tb tc (Scale sa sb sc a))",
        // "(Scale sa sb sc (Trans (/ ta sa) (/ tb sb) (/ tc sc) a))",
    ]
    .into()
}

fn get_lifting_rules() -> Vec<&'static str> {
    [
        "(Union ?a ?b) ==> (max ?a ?b)",
        "(Inter ?a ?b) ==> (min ?a ?b)",
        "(Scale ?w ?h ?l ?e) ==> (subst (/ x (Scalar ?w)) (/ y (Scalar ?h)) (/ z (Scalar ?l)) ?e)",
        "(Cube ?a ?b ?c) ==> (min (/ x (Scalar ?a))
                                  (min (- 1 (/ x (Scalar ?a)))
                                       (min (/ y (Scalar ?b))
                                            (min (- 1 (/ y (Scalar ?b)))
                                                 (min (/ z (Scalar ?c))
                                                      (- 1 (/ z (Scalar ?c))))))))",
        
        "(Sphere ?r) ==> (- (- (- 1 (* (/ x (Scalar ?r)) (/ x (Scalar ?r))))
                               (* (/ y (Scalar ?r)) (/ y (Scalar ?r))))
                            (* (/ z (Scalar ?r)) (/ z (Scalar ?r))))",
        "(Trans ?a ?b ?c ?e) ==> (subst (- x (Scalar ?a)) (- y (Scalar ?b)) (- z (Scalar ?c)) ?e)"
    ].into()
}

const DEBUG: bool = true;

fn to_idx(from_var: CF) -> usize {
    if from_var == CF::DimX {
        return 0;
    } else if from_var == CF::DimY {
        return 1;
    } else if from_var == CF::DimZ {
        return 2;
    } else {
        println!("not a dim var!");
        return 0;
    }
}

fn compute_substs<'a, const N: usize>(
    egraph: &mut EGraph<CF, SynthAnalysis>,
    ids: &[Id; N],
    mapping: [Id; 3],
    cache: &mut HashMap<(Id, [Id; 3]), Option<CF>>,
    depth: usize,
) -> Option<[Id; N]> {
    let mut v = vec![];
    for id in ids {
        if let Some(e_substed) = compute_subst(egraph, *id, mapping.clone(), cache, depth) {
            v.push(e_substed);
        } else {
            return None;
        }
    }
    let mut ids_substed = ids.clone();
    for (i, n) in enumerate(v.into_iter()) {
        ids_substed[i] = egraph.add(n);
    }
    Some(ids_substed)
}

fn compute_subst(
    egraph: &mut EGraph<CF, SynthAnalysis>,
    id: Id,
    mapping: [Id; 3],
    cache: &mut HashMap<(Id, [Id; 3]), Option<CF>>,
    depth: usize,
) -> Option<CF> {

    if DEBUG {
        for ii in 0..depth { print!(" "); }
        println!("cs");
        let extractor = Extractor::new(&egraph, SzalinskiCostFunction);
        let (_, idex) = extractor.find_best(id);
        for ii in 0..depth { print!(" "); }
        println!("id: {} {}", id, idex);
    }
    if cache.contains_key(&(id, mapping)) {
        if DEBUG {     
            for ii in 0..depth { print!(" "); }
            println!("cached")
        }
        return cache[&(id, mapping)].clone();
    }
    // We will replace this is we can successfully subst, but this just prevents
    // infinite cycles.
    cache.insert((id, mapping), None);

    // If something is a scalar, then substitution doesn't change it
    for n in egraph[id].nodes.clone() {
        if matches!(n, CF::Scalar(_)) {
            return Some(n);
        }
    }
    // If it's a non-scalar var, then we can't hope to perform this substitution
    for n in egraph[id].nodes.clone() {
        if matches!(n, CF::Var(_)) {
            return None;
        }
    }

    for n in egraph[id].nodes.clone() {
        if DEBUG {
            for ii in 0..depth { print!(" "); }
            println!("anticipate node: {}", n.to_string());
        }

    }
    for n in egraph[id].nodes.clone() {
        // println!("node: {}", n.to_string());
        match n {
            CF::Subst([x2, y2, z2, e]) => {
                if let Some([x3, y3, z3]) = compute_substs(egraph, &[x2, y2, z2], mapping.clone(), cache, depth + 1) {
                    if let Some(e_double_substed) = compute_subst(egraph, e, [x3, y3, z3], cache, depth + 1) {
                        // println!("did double!");
                        cache.insert((id, mapping), Some(e_double_substed));
                        return cache[&(id, mapping)].clone();
                    } else {
                        if DEBUG {
                            
                            for ii in 0..depth { print!(" "); }
                            println!("combined");
                            let extractor = Extractor::new(&egraph, SzalinskiCostFunction);
                            let (_, eex) = extractor.find_best(e);
                            let (_, x3ex) = extractor.find_best(x3);
                            let (_, y3ex) = extractor.find_best(y3);
                            let (_, z3ex) = extractor.find_best(z3);
                            for ii in 0..depth { print!(" "); }
                            println!("e: {}", eex);
                            for ii in 0..depth { print!(" "); }
                            println!("x3: {}", x3ex);
                            for ii in 0..depth { print!(" "); }
                            println!("y3: {}", y3ex);
                            for ii in 0..depth { print!(" "); }
                            println!("z3: {}", z3ex);
                        }


                        // We couldn't compute through everything, at least combine the substs
                        cache.insert((id, mapping), Some(CF::Subst([x3, y3, z3, e])));
                        return cache[&(id, mapping)].clone();
                    }
                }
            }
            CF::Min(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache, depth + 1) {
                    cache.insert((id, mapping), Some(CF::Min(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::Max(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache, depth + 1) {
                    cache.insert((id, mapping), Some(CF::Max(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::Div(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache, depth + 1) {
                    cache.insert((id, mapping), Some(CF::Div(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::Mul(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache, depth + 1) {
                    cache.insert((id, mapping), Some(CF::Mul(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::Sub(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache, depth + 1) {
                    cache.insert((id, mapping), Some(CF::Sub(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::DimX | CF::DimY | CF::DimZ => {
                let i = to_idx(n);
                cache.insert((id, mapping), Some(egraph[mapping[i]].nodes[0].clone()));
                return cache[&(id, mapping)].clone();
            }
            CF::Lit(i) => {
                cache.insert((id, mapping), Some(n));
                return cache[&(id, mapping)].clone();
            }
            CF::Scalar(_) => {
                return Some(n);
            }
            _ => {},
        }
    }
    cache.insert((id, mapping), None);
    return cache[&(id, mapping)].clone();
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
        is_caddy(self) || matches!(
            self, CF::Var(_)
                | CF::Lit(_)
                | CF::Div(_)
                | CF::Mul(_)
                | CF::Sub(_)
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
        // return;
        // println!("TOP");
        let mut all_subst_ids: Vec<Id> = vec![];
        for class in egraph.classes() {
            let mut all_subst = true;
            let mut at_least_one_subst = false;
            for node in &egraph[class.id].nodes {
                if is_caddy(node) {
                    continue;
                }
                // Check if there is any non-subst FRep node
                if let CF::Subst(_) = node {
                    at_least_one_subst = true;
                } else {
                    all_subst = false;
                }
            }
            if at_least_one_subst && all_subst {
                all_subst_ids.push(class.id.clone());
            }
        }

        // println!("num is {}", all_subst_ids.len());
        let x = egraph.add(CF::DimX);
        let y = egraph.add(CF::DimY);
        let z = egraph.add(CF::DimZ);
        for id in all_subst_ids {
            // println!("cm");
            for node in &egraph[id].nodes.clone() {
                if let CF::Subst([x2, y2, z2, e]) = node.clone() {
                    let mapping = [x2, y2, z2];
                    if mapping == [x, y, z] {
                        if egraph.find(id) != egraph.find(e) {
                            egraph.union(id, e);
                            if DEBUG { println!("redo1"); }
                            return CF::custom_modify(egraph, id);
                        } else {
                            continue;
                        }
                    }
                    let mut cache: HashMap<(Id, [Id; 3]), Option<CF>> = HashMap::new();
                    
                    if DEBUG {
                        let extractor = Extractor::new(&egraph, SzalinskiCostFunction);
                        let (_, eex) = extractor.find_best(e);
                        let (_, x2ex) = extractor.find_best(x2);
                        let (_, y2ex) = extractor.find_best(y2);
                        let (_, z2ex) = extractor.find_best(z2);
                        let (_, idex) = extractor.find_best(id);
                        println!("custom modify start");
                        println!("id: {} {}", id, idex); 
                        println!("e: {} {}", e, eex);
                        println!("x2: {}", x2ex);
                        println!("y2: {}", y2ex);
                        println!("z2: {}", z2ex);
                    }

                    if let Some(e_substed) = compute_subst(egraph, e, mapping, &mut cache, 0) {
                        
                        let id2 = egraph.add(e_substed.clone());
                        
                        if DEBUG {
                            println!("custom modify SUCCESS");
                            let extractor = Extractor::new(&egraph, SzalinskiCostFunction);
                            let (_, eex) = extractor.find_best(e);
                            let (_, x2ex) = extractor.find_best(x2);
                            let (_, y2ex) = extractor.find_best(y2);
                            let (_, z2ex) = extractor.find_best(z2);
                            let (_, id2ex) = extractor.find_best(id2);
                            let (_, idex) = extractor.find_best(id);
                            println!("id: {} {}", id, idex); 
                            println!("e: {} {}", e, eex);
                            println!("x2: {}", x2ex);
                            println!("y2: {}", y2ex);
                            println!("z2: {}", z2ex);
       
                            println!("id2: {} {}", id2, id2ex);
                        }


                        // This algorithm runs until fixed-point
                        // Only restart if we introduced a non-subst term
                        if egraph.find(id) != egraph.find(id2) {
                            egraph.union(id, id2);
                            if DEBUG { println!("redo2"); }
                            return CF::custom_modify(egraph, id);
                        }
                    } else {
                        // println!("Custom modify fail");
                    }
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
    let mut t = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs()
        - 1680159600;
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
    use rayon::iter;
    use ruler::enumo::{Ruleset, Scheduler, Workload};

    use super::*;

    fn iter_szalinski(n: usize) -> Workload {
        let lang = Workload::new([
            // "var",
            // "(bop solid solid)",
            // "(Scale scalar scalar scalar solid)",
            // "(Cube scalar scalar scalar)",
            // "(Sphere scalar)",
            // "(Cylinder scalar scalar scalar)",
            // "(Cube scalar scalar scalar)", "(Scale scalar scalar scalar (Cube 1 1 1))",
            
            // "(Scale sa sb sc (Cube 1 1 1))", "(Cube sa sb sc)",
            // "(Scale sa sa sa (Sphere 1))", "(Sphere sa)",
            "(Scale sa sb sc (Trans ta tb tc a))", "(Trans (* ta sa) (* tb sb) (* tc sc) (Scale sa sb sc a))",
            "(Trans ta tb tc (Scale sa sb sc a))", "(Scale sa sb sc (Trans (/ ta sa) (/ tb sb) (/ tc sc) a))",
            // "(Trans 0 0 0 a)", "a",
            // "(Scale 1 1 1 a)", "a",
            // "(Trans 0 0 0 a)", "(Scale ta tb tc a)",

        ]);
        let scalars: &[&str] = &["sa", "sb", "sc", "1"];
        let vars: &[&str] = &["a", "b", "c"];
        let bops: &[&str] = &["Union", "Inter"];

        let w = lang
            .iter_metric("solid", Metric::Atoms, n)
            // .filter(Filter::Contains("var".parse().unwrap()))
            .plug("scalar", &scalars.into())
            .plug("var", &vars.into())
            .plug("bop", &bops.into());

        let mut data = w
            .force()
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        data = format!("{}\n{}\n", time(), data);
        fs::write("wl.txt", data).expect("Unable to write file");
        w
    }

    #[test]
    fn rule_lifting_recipe() {
        let frep_rules = get_frep_rules();

        let prior = Ruleset::new(&frep_rules);

        let atoms3 = iter_szalinski(15);
        // assert_eq!(atoms3.force().len(), 51);

        let limits = Limits {
            iter: 3,
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
        let nat_rules = get_frep_rules();

        let mut all_rules = Ruleset::default();
        all_rules.extend(Ruleset::new(&nat_rules));
        
        let atoms3 = iter_szalinski(20);

        let rules3 = CF::run_workload(
            atoms3,
            all_rules.clone(),
            Limits {
                iter: 6,
                node: 10000000000,
            },
        );
        all_rules.extend(rules3);
    }
}
