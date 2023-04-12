// Learn Szalinski rules by lowering to FRep
// Status: frozen. Develop on szalinski-dev.rs instead.
use ruler::{
    enumo::{Metric, Ruleset},
    recipe_utils::iter_metric,
    *,
};
use std::hash::Hash;
use std::usize;

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
    ]
    .into()
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

fn get_subst_and_frep_rules() -> Vec<&'static str> {
    let mut m = get_frep_rules();
    m.extend(get_subst_rules());
    m
}

fn get_lifting_rules() -> Vec<&'static str> {
    [
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

impl SynthLanguage for CF {
    type Constant = Constant;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rules() -> Ruleset<Self> {
        Ruleset::new(&get_lifting_rules())
    }

    fn is_allowed_op(&self) -> bool {
        is_caddy(self)
            || matches!(
                self,
                CF::Var(_) | CF::Lit(_) | CF::Div(_) | CF::Mul(_) | CF::Sub(_) | CF::Add(_)
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

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        CF::Lit(c)
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}

fn paste_print(rs: &Ruleset<CF>) {
    println!("\nPaste these vvv\n");
    let mut i = 1;

    fn fix(s: String) -> String {
        s.replace("Scale", "Affine Scale")
            .replace("Trans", "Affine Trans")
    }

    for (_, rule) in rs {
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

#[cfg(test)]
mod tests {
    use ruler::enumo::{Ruleset, Scheduler, Workload};

    use super::*;

    fn iter_szalinski(n: usize) -> Workload {
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
            "(Vec3 (bop a d) (bop b e) (bop c f))",
        ];
        iter_metric(lang, "shape", Metric::Depth, n)
            .plug("v3", &v3s.into())
            .plug("transformation", &transformations.into())
            .plug("scalar", &scalars.into())
            .plug("bop", &bops.into())
    }

    #[test]
    fn rule_lifting() {
        let mut learned_rules = Ruleset::default();
        let mut all_rules: Ruleset<CF> = Ruleset::new(&get_subst_and_frep_rules());
        let limits = Limits {
            iter: 4,
            node: 10_000_000,
        };

        for i in 2..4 {
            let atoms = iter_szalinski(i);
            let egraph = atoms.to_egraph::<CF>();
            let mut candidates = Ruleset::allow_forbid_actual(egraph, all_rules.clone(), limits);
            let (chosen, _) = candidates.minimize(learned_rules.clone(), Scheduler::Compress(limits));

            all_rules.extend(chosen.clone());
            learned_rules.extend(chosen);
        }

        learned_rules.pretty_print();
        paste_print(&learned_rules);

        let expected: Ruleset<CF> = Ruleset::new(&[
            "(Trans (Vec3 0 0 0) ?a) <=> ?a",
            "?a <=> (Scale (Vec3 1 1 1) ?a)",
            "(Scale (Vec3 ?b ?b ?b) (Cylinder (Vec3 1 1 1) ?a true)) <=> (Cylinder (Vec3 ?b ?b ?b) ?a true)",
            "(Scale (Vec3 ?f ?e ?d) (Cube (Vec3 ?c ?b ?a) false)) ==> (Scale (Vec3 (* ?f ?c) (* ?e ?b) (* ?d ?a)) (Cube (Vec3 1 1 1) false))",
            "(Scale (Vec3 ?f ?e ?d) (Cube (Vec3 ?c ?b ?a) false)) ==> (Cube (Vec3 (* ?f ?c) (* ?e ?b) (* ?d ?a)) false)",
            "(Scale (Vec3 ?f ?d ?b) (Trans (Vec3 (/ ?g ?f) (/ ?e ?d) (/ ?c ?b)) ?a)) ==> (Trans (Vec3 ?g ?e ?c) (Scale (Vec3 ?f ?d ?b) ?a))",
            "(Scale (Vec3 ?g ?f ?e) (Trans (Vec3 ?d ?c ?b) ?a)) ==> (Trans (Vec3 (* ?d ?g) (* ?c ?f) (* ?b ?e)) (Scale (Vec3 ?g ?f ?e) ?a))",
            "(Trans (Vec3 ?g ?f ?e) (Trans (Vec3 ?d ?c ?b) ?a)) ==> (Trans (Vec3 (+ ?g ?d) (+ ?f ?c) (+ ?e ?b)) ?a)",
            "(Scale (Vec3 ?g ?f ?e) (Scale (Vec3 ?d ?c ?b) ?a)) ==> (Scale (Vec3 (* ?g ?d) (* ?f ?c) (* ?e ?b)) ?a)",
        ]);
        let (can, cannot) = all_rules.derive(DeriveType::Lhs, &expected, Limits::deriving());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }
}
