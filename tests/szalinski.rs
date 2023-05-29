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
    "max" = Max,
    "min" = Min,
    "+" = Add,
    "-" = Sub,
    "*" = Mul,
    "/" = Div,
    "x" = DimX,
    "y" = DimY,
    "z" = DimZ,
    "subst" = Subst([Id; 4]),
    "op" = Op([Id; 3]),

    // Indicate that the containing expr cannot have x, y, or z
    "Scalar" = Scalar(Id),
    "Lit" = Lit(Id),
    Literal(Constant),

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
        "(Lit ?x) ==> (Scalar (Lit ?x))",
        "(subst ?x ?y ?z (Scalar ?a)) ==> (Scalar ?a)",
        "(subst ?x ?y ?z x) ==> ?x",
        "(subst ?x ?y ?z y) ==> ?y",
        "(subst ?x ?y ?z z) ==> ?z",
        "(subst ?x ?y ?z (op ?op ?a ?b)) ==> (op ?op (subst ?x ?y ?z ?a) (subst ?x ?y ?z ?b))",
        "(subst ?x ?y ?z (subst ?x2 ?y2 ?z2 ?a)) ==>
            (subst (subst ?x ?y ?z ?x2)
                   (subst ?x ?y ?z ?y2)
                   (subst ?x ?y ?z ?z2)
                   ?a)",
        "(subst x y z ?a) ==> ?a",
        "(Scalar (op ?op ?a ?b)) ==> (op ?op (Scalar ?a) (Scalar ?b))",
    ]
    .into()
}

fn get_frep_rules() -> Vec<&'static str> {
    [
        "(op / ?a (Lit 1)) ==> ?a",
        "(op - ?a (Lit 0)) ==> ?a",
        "(op - (op / ?a ?c) (op / ?b ?c)) ==> (op / (op - ?a ?b) ?c)",
        "(op - (op / ?a ?c) ?b) ==> (op / (op - ?a (op * ?b ?c)) ?c)",
        "(op / (op / ?a ?b) ?c) ==> (op / ?a (op * ?b ?c))",
        "(op - ?a (op + ?b ?c)) ==> (op - (op - ?a ?b) ?c)",
        // "(op min (op max ?a ?b) ?a) ==> ?a",
        // "(op max (op min ?a ?b) ?a) ==> ?a",
        // "(op max ?a ?a) ==> ?a",
        // "(op min ?a ?a) ==> ?a",
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
        "(Scale (Vec3 ?w ?h ?l) ?e) ==> (subst (op / x (Scalar ?w)) (op / y (Scalar ?h)) (op / z (Scalar ?l)) ?e)",
        "(Cube (Vec3 ?a ?b ?c) false) ==>
         (op min (op min (op min (op / x (Scalar ?a))
                        (op / y (Scalar ?b)))
                   (op / z (Scalar ?c)))
              (op min (op min (op - (Lit 1) (op / x (Scalar ?a)))
                        (op - (Lit 1) (op / y (Scalar ?b))))
                   (op - (Lit 1) (op / z (Scalar ?c)))))",
        "(Cylinder (Vec3 ?h ?r ?r) ?params true) ==>
         (op min (op - (op - (Lit 1) (op * (op / x (Scalar ?r)) (op / x (Scalar ?r))))
                 (op * (op / y (Scalar ?r)) (op / y (Scalar ?r))))
              (op min (op - (op / (Lit 1) (Lit 2)) (op / z (Scalar ?h)))
                   (op + (op / (Lit 1) (Lit 2)) (op / z (Scalar ?h)))))",
        "(Sphere ?r ?params) ==> (op - (op - (op - (Lit 1) (op * (op / x (Scalar ?r)) (op / x (Scalar ?r))))
                               (op * (op / y (Scalar ?r)) (op / y (Scalar ?r))))
                            (op * (op / z (Scalar ?r)) (op / z (Scalar ?r))))",
        "(Trans (Vec3 ?a ?b ?c) ?e) ==> (subst (op - x (Scalar ?a)) (op - y (Scalar ?b)) (op - z (Scalar ?c)) ?e)"
    ].into()
}

fn is_caddy(node: &CF) -> bool {
    match node {
        CF::Max => false,
        CF::Add => false,
        CF::Min => false,
        CF::Sub => false,
        CF::Mul => false,
        CF::Div => false,
        CF::Op(_) => false,
        CF::DimX => false,
        CF::DimY => false,
        CF::DimZ => false,
        CF::Subst(_) => false,
        CF::Scalar(_) => false,
        CF::Literal(_) => false,
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
                CF::Var(_)
                    | CF::Literal(_)
                    | CF::Lit(_)
                    | CF::Op(_)
                    | CF::Div
                    | CF::Mul
                    | CF::Sub
                    | CF::Add
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
        matches!(self, CF::Literal(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        CF::Literal(c)
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}

fn export_print(rs: &Ruleset<CF>) {
    println!("RULER RULES START");
    fn fix(s: String) -> String {
        s.replace("Scale", "Affine Scale")
            .replace("Trans", "Affine Trans")
            .replace("op ", "")
            .replace("(Lit 0)", "0") // TODO: regex or smth
            .replace("(Lit 1)", "1")
    }

    let mut i = 1;
    for (_, rule) in rs {
        println!(
            "rw!(\"ruler{}\"; \"{}\" => \"{}\"),",
            i,
            fix(rule.lhs.to_string()),
            fix(rule.rhs.to_string())
        );
        i += 1;
    }
    println!("RULER RULES END");
    println!("\n");
}

#[cfg(test)]
mod tests {
    use ruler::enumo::{Ruleset, Scheduler, Workload};

    use super::*;

    fn iter_szalinski(n: usize) -> Workload {
        let lang = Workload::new([
            "(TRANSFORMATION V3 SOLID)",
            "(Cube V3 false)",
            "(Cylinder V3 params true)",
            "(Sphere SCALAR params)",
            "s",
        ]);
        let scalars: &[&str] = &["a", "(Lit 1)"];
        let transformations: &[&str] = &["Scale", "Trans"];
        let bops: &[&str] = &["+", "*", "/"];
        let v3s: &[&str] = &[
            "(Vec3 (Lit 0) (Lit 0) (Lit 0))",
            "(Vec3 (Lit 1) (Lit 1) (Lit 1))",
            "(Vec3 a a a)",
            "(Vec3 a b c)",
            "(Vec3 d e f)",
            "(Vec3 (op BOP a d) (op BOP b e) (op BOP c f))",
        ];
        iter_metric(lang, "SOLID", Metric::Depth, n)
            .plug("V3", &v3s.into())
            .plug("TRANSFORMATION", &transformations.into())
            .plug("SCALAR", &scalars.into())
            .plug("BOP", &bops.into())
    }

    #[test]
    #[ignore]
    fn rule_lifting() {
        let mut learned_rules = Ruleset::default();
        let mut all_rules: Ruleset<CF> = Ruleset::new(&get_subst_and_frep_rules());
        let limits = Limits {
            iter: 4,
            node: 10_000_000,
            match_: 1_000_000,
        };

        for i in 2..4 {
            let atoms = iter_szalinski(i);
            let egraph = atoms.to_egraph::<CF>();
            let mut candidates = Ruleset::allow_forbid_actual(egraph, all_rules.clone(), limits);
            let (chosen, _) =
                candidates.minimize(learned_rules.clone(), Scheduler::Compress(limits));

            all_rules.extend(chosen.clone());
            learned_rules.extend(chosen);
        }

        learned_rules.pretty_print();
        export_print(&learned_rules);

        let expected: Ruleset<CF> = Ruleset::new(&[
            "?a <=> (Trans (Vec3 (Lit 0) (Lit 0) (Lit 0)) ?a)",
            "?a <=> (Scale (Vec3 (Lit 1) (Lit 1) (Lit 1)) ?a)",
            "(Scale (Vec3 ?b ?b ?b) (Cylinder (Vec3 (Lit 1) (Lit 1) (Lit 1)) ?a true)) <=> (Cylinder (Vec3 ?b ?b ?b) ?a true)",
            "(Scale (Vec3 (op * ?f ?e) (op * ?d ?c) (op * ?b ?a)) (Cube (Vec3 (Lit 1) (Lit 1) (Lit 1)) false)) <=> (Scale (Vec3 ?f ?d ?b) (Cube (Vec3 ?e ?c ?a) false))",
            "(Scale (Vec3 ?g ?e ?c) (Scale (Vec3 ?f ?d ?b) ?a)) <=> (Scale (Vec3 (op * ?g ?f) (op * ?e ?d) (op * ?c ?b)) ?a)",
            "(Trans (Vec3 (op + ?g ?d) (op + ?f ?c) (op + ?e ?b)) ?a) <=> (Trans (Vec3 ?g ?f ?e) (Trans (Vec3 ?d ?c ?b) ?a))",
            "(Scale (Vec3 ?d ?c ?b) (Trans (Vec3 ?g ?f ?e) ?a)) <=> (Trans (Vec3 (op * ?g ?d) (op * ?f ?c) (op * ?e ?b)) (Scale (Vec3 ?d ?c ?b) ?a))",
            "(Trans (Vec3 ?g ?e ?c) (Scale (Vec3 ?f ?d ?b) ?a)) <=> (Scale (Vec3 ?f ?d ?b) (Trans (Vec3 (op / ?g ?f) (op / ?e ?d) (op / ?c ?b)) ?a))",
            "(Scale (Vec3 ?f ?d ?b) (Cube (Vec3 ?e ?c ?a) false)) <=> (Cube (Vec3 (op * ?f ?e) (op * ?d ?c) (op * ?b ?a)) false)",
        ]);
        let (can, cannot) = all_rules.derive(DeriveType::Lhs, &expected, Limits::deriving());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }
}
