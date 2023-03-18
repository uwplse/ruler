use egg::Subst;
use ruler::{
    enumo::{Rule, Ruleset},
    *,
};
use std::ops::*;

egg::define_language! {
  pub enum BvBool {
    // boolean domain
        "~" = Not(Id),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "^" = Xor([Id; 2]),
        "->" = Implies([Id; 2]),
        Lit(bool),

        // bitvector domain
        "and" = Band([Id; 2]),
        "or" = Bor([Id; 2]),
        "xor" = Bxor([Id; 2]),
        "not" = Bnot(Id),
        Num(BV<2>),
        Var(egg::Symbol),

        // conversions
        "bv" = Make([Id; 2]),
        "first" = First(Id),
        "second" = Second(Id),
  }
}

fn is_bv_str(s: &'static str) -> impl Fn(&mut EGraph<BvBool, SynthAnalysis>, Id, &Subst) -> bool {
    let var = s.parse().unwrap();
    move |egraph, _, subst| egraph[subst[var]].nodes.iter().any(BvBool::is_allowed_op)
}

impl SynthLanguage for BvBool {
    type Constant = BV<2>;

    fn is_rule_lifting() -> bool {
        true
    }

    fn get_lifting_rules() -> Ruleset<Self> {
        let mut rules = Ruleset::new(&[
            "(first (not ?a)) ==> (~ (first ?a))",
            "(second (not ?a)) ==> (~ (second ?a))",
            "(first (and ?a ?b)) ==> (& (first ?a) (first ?b))",
            "(second (and ?a ?b)) ==> (& (second ?a) (second ?b))",
            "(first (or ?a ?b)) ==> (| (first ?a) (first ?b))",
            "(second (or ?a ?b)) ==> (| (second ?a) (second ?b))",
            "(first (xor ?a ?b)) ==> (^ (first ?a) (first ?b))",
            "(second (xor ?a ?b)) ==> (^ (second ?a) (second ?b))",
        ]);

        rules.add(Rule {
            name: "def-bv".into(),
            lhs: "?a".parse().unwrap(),
            rhs: "(bv (first ?a) (second ?a))".parse().unwrap(),
            rewrite: egg::rewrite!("def-bv"; "?a" => "(bv (first ?a) (second ?a))" if is_bv_str("?a")),
        });
        rules
    }

    fn is_allowed_op(&self) -> bool {
        matches!(
            self,
            BvBool::Band(_)
                | BvBool::Bxor(_)
                | BvBool::Bnot(_)
                | BvBool::Bor(_)
                | BvBool::Num(_)
                | BvBool::Var(_)
                | BvBool::Make(_)
        )
    }

    // No eval needed for rule lifting
    fn eval<'a, F>(&'a self, _cvec_len: usize, _get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        for var in vars {
            let var_id = egraph.add(BvBool::Var(Symbol::from(var)));
            let fst_id = egraph.add(BvBool::First(var_id));
            let sec_id = egraph.add(BvBool::Second(var_id));
            let mk_id = egraph.add(BvBool::Make([fst_id, sec_id]));
            egraph.union(var_id, mk_id);
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let BvBool::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        BvBool::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, BvBool::Lit(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        BvBool::Num(c)
    }

    fn custom_modify(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if egraph[id].nodes.iter().any(|n| matches!(n, BvBool::Num(_))) {
            // e-class is already a constant
            return;
        }

        let get_bool_const = |nodes: &[BvBool]| {
            // let nodes = &egraph[id].nodes;
            for n in nodes {
                if let BvBool::Lit(v) = n {
                    return Some(*v);
                }
            }
            None
        };

        for n in &egraph[id].nodes {
            if let BvBool::Make([i, j]) = n {
                if let Some(x) = get_bool_const(&egraph[*i].nodes) {
                    if let Some(y) = get_bool_const(&egraph[*j].nodes) {
                        let cnst = match (x, y) {
                            (true, true) => BV::<2>::from(3),
                            (true, false) => BV::<2>::from(2),
                            (false, true) => BV::<2>::from(1),
                            (false, false) => BV::<2>::from(0),
                        };

                        let c_id = egraph.add(BvBool::Num(cnst));
                        egraph.union(id, c_id);
                        return;
                    }
                }
            }
        }
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }
}
