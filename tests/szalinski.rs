// Learn Szalinski rules by lowering to FRep
// Status: work-in-progress.

#![allow(unused_imports)]
#![allow(unused_variables)]
use egg::{AstSize, Extractor, RecExpr};
use itertools::enumerate;
use num::{rational::Ratio, BigInt, Signed, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use rayon::vec;
use ruler::{
    enumo::{Filter, Metric, Ruleset, Scheduler, Workload},
    *,
};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::time::SystemTime;
use std::{hash::Hash, time::Instant};

pub type Constant = usize;

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
    "x" = DimX,
    "y" = DimY,
    "z" = DimZ,
    "subst" = Subst([Id; 3]),

    // Caddy
    "Cube" = Cube([Id; 3]),
    "Cylinder" = Cylinder([Id; 3]),
    "Sphere" = Sphere(Id),
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

#[test]
fn custom_modify() {
    let term_expected_pairs = [
        (
            "(subst (- 1 (min (min x y) z)) x (/ x a))",
            "(- 1 (min (min (/ x a) y) z))",
        ),
        ("(FRep (subst x x (/ x a)))", "(FRep (/ x a))"),
        (
            "(FRep (subst (min x y) x (/ x a)))",
            "(FRep (min (/ x a) y))",
        ),
        (
            "(FRep (subst (min x x) x (/ x a)))",
            "(FRep (min (/ x a) (/ x a)))",
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
        "(/ ?a 1) ==> ?a",
        "(min (max ?a ?b) ?a) ==> ?a",
        "(max (min ?a ?b) ?a) ==> ?a",
        "(max ?a ?a) ==> ?a",
        "(min ?a ?a) ==> ?a",
    ]
    .into()
}

fn get_lifting_rules() -> Vec<&'static str> {
    [
        "(Union (FRep ?a) (FRep ?b)) ==> (FRep (max ?a ?b))",
        "(Inter (FRep ?a) (FRep ?b)) ==> (FRep (min ?a ?b))",
        "(Scale (FRep ?e) ?a ?b ?c) ==> (FRep (subst (subst (subst ?e x (/ x ?a)) y (/ y ?b)) z (/ z ?c)))",
        "(Cube ?a ?b ?c) ==> (FRep (min (/ x ?a)
                                   (min (- 1 (/ x ?a))
                                        (min (/ y ?b)
                                             (min (- 1 (/ y ?b))
                                                  (min (/ z ?c)
                                                       (- 1 (/ z ?c))))))))",
        "(Sphere ?r) ==> (FRep (- (- (- 1 (* (/ x ?r) (/ x ?r))) (* (/ y ?r) (/ y ?r))) (* (/ z ?r) (/ z ?r))))",
        "(Cylinder ?r1 ?r2 ?h) ==> (FRep (min (/ z ?h)
                                              (min (- 1 (/ z ?h))
                                                   (- (- 1 (* (/ x ?r1) (/ x ?r1))) (* (/ y ?r2) (/ y ?r2))))))",
    ].into()
}

fn get_dim_var(nodes: &[CF]) -> Option<CF> {
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
}

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
) -> Option<[Id; N]> {
    let mut v = vec![];
    for id in ids {
        if let Some(e_substed) = compute_subst(egraph, *id, mapping.clone(), cache) {
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
) -> Option<CF> {
    if cache.contains_key(&(id, mapping)) {
        return cache[&(id, mapping)].clone();
    }
    // We will replace this is we can successfully subst, but this just prevents
    // infinite cycles.
    cache.insert((id, mapping), None);

    for n in egraph[id].nodes.clone() {
        match n {
            CF::Min(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache) {
                    cache.insert((id, mapping), Some(CF::Min(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::Div(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache) {
                    cache.insert((id, mapping), Some(CF::Div(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::Mul(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache) {
                    cache.insert((id, mapping), Some(CF::Mul(ids_substed)));
                    return cache[&(id, mapping)].clone();
                }
            }
            CF::Sub(ids) => {
                if let Some(ids_substed) = compute_substs(egraph, &ids, mapping.clone(), cache) {
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
            CF::Subst([e, from, to]) => {
                if let Some(from_var) = get_dim_var(&egraph[from].nodes) {
                    let mut mapping2 = [mapping[0], mapping[1], mapping[2]];
                    let i = to_idx(from_var);
                    mapping2[i] = to;
                    if let Some(e_substed) = compute_subst(egraph, e, mapping2, cache) {
                        let x = egraph.add(CF::DimX);
                        let y = egraph.add(CF::DimY);
                        let z = egraph.add(CF::DimZ);
                        if mapping == [x, y, z] {
                            let id2 = egraph.add(e_substed.clone());
                            egraph.union(id, id2);
                        }
                        cache.insert((id, mapping), Some(e_substed.clone()));
                        return cache[&(id, mapping)].clone();
                    }
                } else {
                    println!("Subst wo var");
                }
            }
            _ => (),
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
        matches!(
            self,
            CF::FRep(_)
                | CF::Cube(_)
                | CF::Sphere(_)
                | CF::Cylinder(_)
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
        let mut cache: HashMap<(Id, [Id; 3]), Option<CF>> = HashMap::new();
        let x = egraph.add(CF::DimX);
        let y = egraph.add(CF::DimY);
        let z = egraph.add(CF::DimZ);
        compute_subst(egraph, id, [x, y, z], &mut cache);
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
    use ruler::enumo::{Ruleset, Scheduler, Workload};

    use super::*;

    fn iter_szalinski(n: usize) -> Workload {
        let lang = Workload::new([
            "(FRep var)",
            "(bop solid solid)",
            "(Scale solid var var var)",
            "(Cube scalar scalar scalar)",
            "(Sphere scalar)",
            "(Cylinder scalar scalar scalar)",
        ]);
        let scalars: &[&str] = &["var", "1", "0"];
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
        let nat_rules = get_frep_rules();

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
        let nat_rules = get_frep_rules();

        let mut all_rules = Ruleset::default();
        all_rules.extend(Ruleset::new(&nat_rules));

        let atoms3 = iter_szalinski(8);

        let rules3 = CF::run_workload(
            atoms3,
            all_rules.clone(),
            Limits {
                iter: 3,
                node: 10000000,
            },
        );
        all_rules.extend(rules3);
    }
}
