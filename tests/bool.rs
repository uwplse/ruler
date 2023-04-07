use ruler::{
    enumo::{Ruleset, Scheduler, Workload},
    *,
};
use std::{ops::*, time::Instant};
#[path = "./recipes/bool.rs"]
pub mod bool;

egg::define_language! {
  pub enum Bool {
    "~" = Not(Id),
    "&" = And([Id; 2]),
    "|" = Or([Id; 2]),
    "^" = Xor([Id; 2]),
    "->" = Implies([Id; 2]),
    Lit(bool),
    Var(egg::Symbol),
  }
}

impl SynthLanguage for Bool {
    type Constant = bool;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Bool::Not(x) => map!(get_cvec, x => Some(x.not())),
            Bool::And([x, y]) => map!(get_cvec, x, y => Some(*x & *y)),
            Bool::Or([x, y]) => map!(get_cvec, x, y => Some(*x | *y)),
            Bool::Xor([x, y]) => map!(get_cvec, x, y => Some(*x ^ *y)),
            Bool::Implies([x, y]) => map!(get_cvec, x, y => Some(!(*x) | *y)),
            Bool::Lit(c) => vec![Some(*c); cvec_len],
            Bool::Var(_) => vec![],
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        let unwrap_interval = |interval: &Interval<Self::Constant>| {
            (
                interval
                    .low
                    .expect("Bool shouldn't have infinite intervals"),
                interval
                    .high
                    .expect("Bool shouldn't have infinite intervals"),
            )
        };
        match self {
            Bool::Lit(c) => Interval::new(Some(*c), Some(*c)),
            Bool::Var(_) => Interval::new(Some(false), Some(true)),
            Bool::Not(x) => {
                let (low, high) = unwrap_interval(get_interval(x));
                Interval::new(Some(!high), Some(!low))
            }
            Bool::And([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                Interval::new(Some(x_low && y_low), Some(x_high && y_high))
            }
            Bool::Or([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                Interval::new(Some(x_low || y_low), Some(x_high || y_high))
            }
            Bool::Xor([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                if x_low == x_high && y_low == y_high {
                    Interval::new(Some(x_low != y_low), Some(x_low != y_low))
                } else {
                    Interval::new(Some(false), Some(true))
                }
            }
            Bool::Implies([x, y]) => {
                let (x_low, x_high) = unwrap_interval(get_interval(x));
                let (y_low, y_high) = unwrap_interval(get_interval(y));
                Interval::new(Some(!x_high || y_low), Some(!x_low || y_high))
            }
        }
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let consts = vec![Some(true), Some(false)];
        let cvecs = self_product(&consts, vars.len());

        egraph.analysis.cvec_len = cvecs[0].len();

        for (i, v) in vars.iter().enumerate() {
            let id = egraph.add(Bool::Var(Symbol::from(v.clone())));
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Bool::Var(sym)
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Bool::Var(v) => Some(*v),
            _ => None,
        }
    }

    fn validate(_lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> ValidationResult {
        ValidationResult::Valid
    }

    fn is_constant(&self) -> bool {
        matches!(self, Bool::Lit(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Bool::Lit(c)
    }
}

impl Bool {
    fn run_workload(workload: Workload, prior: Ruleset<Self>, limits: Limits) -> Ruleset<Self> {
        let t = Instant::now();

        let egraph = workload.to_egraph::<Self>();
        let compressed = Scheduler::Compress(limits).run(&egraph, &prior);

        let mut candidates = Ruleset::cvec_match(&compressed);

        let num_prior = prior.len();
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::bool::bool_rules;
    use ruler::enumo::{Ruleset, Workload};
    use std::time::Instant;

    fn iter_bool(n: usize) -> Workload {
        Workload::iter_lang(
            n,
            &["true", "false"],
            &["a", "b", "c"],
            &["~"],
            &["&", "|", "^", "->"],
        )
    }

    #[test]
    fn dsl() {
        let mut all_rules: Ruleset<Bool> = Ruleset::default();
        let atoms3 = iter_bool(3);
        assert_eq!(atoms3.force().len(), 93);

        let scheduler = Scheduler::Compress(Limits::default());

        let egraph = scheduler.run(&atoms3.to_egraph(), &all_rules);
        let mut candidates = Ruleset::cvec_match(&egraph);
        let rules3 = candidates.minimize(all_rules.clone(), scheduler);
        all_rules.extend(rules3);

        let atoms4 = iter_bool(4);
        assert_eq!(atoms4.force().len(), 348);

        let egraph = scheduler.run(&atoms4.to_egraph(), &all_rules);
        candidates = Ruleset::cvec_match(&egraph);
        let rules4 = candidates.minimize(all_rules.clone(), scheduler);
        all_rules.extend(rules4);

        let atoms5 = iter_bool(5);
        assert_eq!(atoms5.force().len(), 4599);

        let egraph = scheduler.run(&atoms5.to_egraph(), &all_rules);
        candidates = Ruleset::cvec_match(&egraph);
        let rules5 = candidates.minimize(all_rules.clone(), scheduler);
        all_rules.extend(rules5);

        let expected: Ruleset<Bool> = Ruleset::new(&[
            "(^ ?b ?a) ==> (^ ?a ?b)",
            "(& ?b ?a) ==> (& ?a ?b)",
            "(| ?b ?a) ==> (| ?a ?b)",
            "(& ?a ?a) <=> ?a",
            "?a <=> (~ (~ ?a))",
            "?a <=> (| ?a ?a)",
            "(-> ?a ?a) ==> true",
            "(^ ?a ?a) ==> false",
            "(| ?a false) <=> ?a",
            "?a <=> (-> true ?a)",
            "?a <=> (& ?a true)",
            "?a <=> (^ false ?a)",
            "(~ ?a) <=> (-> ?a false)",
            "(~ ?a) <=> (^ ?a true)",
            "(-> ?b (~ ?a)) <=> (~ (& ?b ?a))",
            "(~ (^ ?b ?a)) ==> (^ ?b (~ ?a))",
            "(-> (~ ?b) ?a) ==> (| ?a ?b)",
            "(-> ?b (~ ?a)) ==> (^ ?a (-> ?a ?b))",
            "(| ?b ?a) ==> (-> (-> ?b ?a) ?a)",
            "(-> ?b ?a) <=> (-> (| ?a ?b) ?a)",
            "(| ?b ?a) <=> (| ?a (^ ?b ?a))",
            "(& ?b ?a) ==> (& ?b (-> ?b ?a))",
            "(| ?b (& ?b ?a)) ==> ?b",
            "(& ?a (| ?b ?a)) ==> ?a",
            "(& ?a (-> ?b ?a)) ==> ?a",
            "(-> (-> ?a ?b) ?a) ==> ?a",
            "(-> ?c (-> ?b ?a)) <=> (-> (& ?c ?b) ?a)",
            "(| ?c (| ?b ?a)) ==> (| ?a (| ?b ?c))",
            "(-> ?c (-> ?b ?a)) ==> (-> ?b (-> ?c ?a))",
            "(^ ?c (^ ?b ?a)) ==> (^ ?a (^ ?c ?b))",
        ]);
        let (can, cannot) = all_rules.derive(DeriveType::LhsAndRhs, &expected, Limits::default());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }

    #[test]
    fn simple() {
        let mut all_rules = Ruleset::default();
        let atoms3 = iter_bool(3);
        assert_eq!(atoms3.force().len(), 93);

        let rules3 = Bool::run_workload(atoms3, all_rules.clone(), Limits::default());
        all_rules.extend(rules3);

        let atoms4 = iter_bool(4);
        assert_eq!(atoms4.force().len(), 348);

        let rules4 = Bool::run_workload(atoms4, all_rules.clone(), Limits::default());
        all_rules.extend(rules4);

        let atoms5 = iter_bool(5);
        assert_eq!(atoms5.force().len(), 4599);

        let rules5 = Bool::run_workload(atoms5, all_rules.clone(), Limits::default());
        all_rules.extend(rules5);

        let expected: Ruleset<Bool> = Ruleset::new(&[
            "(^ ?b ?a) ==> (^ ?a ?b)",
            "(& ?b ?a) ==> (& ?a ?b)",
            "(| ?b ?a) ==> (| ?a ?b)",
            "(& ?a ?a) <=> ?a",
            "?a <=> (~ (~ ?a))",
            "?a <=> (| ?a ?a)",
            "(-> ?a ?a) ==> true",
            "(^ ?a ?a) ==> false",
            "(| ?a false) <=> ?a",
            "?a <=> (-> true ?a)",
            "?a <=> (& ?a true)",
            "?a <=> (^ false ?a)",
            "(~ ?a) <=> (-> ?a false)",
            "(~ ?a) <=> (^ ?a true)",
            "(-> ?b (~ ?a)) <=> (~ (& ?b ?a))",
            "(~ (^ ?b ?a)) ==> (^ ?b (~ ?a))",
            "(-> (~ ?b) ?a) ==> (| ?a ?b)",
            "(-> ?b (~ ?a)) ==> (^ ?a (-> ?a ?b))",
            "(| ?b ?a) ==> (-> (-> ?b ?a) ?a)",
            "(-> ?b ?a) <=> (-> (| ?a ?b) ?a)",
            "(| ?b ?a) <=> (| ?a (^ ?b ?a))",
            "(& ?b ?a) ==> (& ?b (-> ?b ?a))",
            "(| ?b (& ?b ?a)) ==> ?b",
            "(& ?a (| ?b ?a)) ==> ?a",
            "(& ?a (-> ?b ?a)) ==> ?a",
            "(-> (-> ?a ?b) ?a) ==> ?a",
            "(-> ?c (-> ?b ?a)) <=> (-> (& ?c ?b) ?a)",
            "(| ?c (| ?b ?a)) ==> (| ?a (| ?b ?c))",
            "(-> ?c (-> ?b ?a)) ==> (-> ?b (-> ?c ?a))",
            "(^ ?c (^ ?b ?a)) ==> (^ ?a (^ ?c ?b))",
        ]);
        let (can, cannot) = all_rules.derive(DeriveType::LhsAndRhs, &expected, Limits::default());
        assert_eq!(can.len(), expected.len());
        assert_eq!(cannot.len(), 0);
    }

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bool_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<_>::from_file("baseline/bool.rules");

        logger::write_output(
            &rules,
            &baseline,
            "bool",
            "oopsla",
            Limits {
                iter: 3,
                node: 200000,
            },
            duration,
        );
    }

    #[test]
    fn round_trip_to_file() {
        let rules: Ruleset<Bool> = Ruleset::new(&[
            "(^ ?b ?a) ==> (^ ?a ?b)",
            "(& ?b ?a) ==> (& ?a ?b)",
            "(| ?b ?a) ==> (| ?a ?b)",
            "(& ?a ?a) ==> ?a",
            "?a ==> (~ (~ ?a))",
        ]);

        rules.to_file("out.txt");

        let read: Ruleset<Bool> = Ruleset::from_file("out.txt");

        assert_eq!(rules, read)
    }

    #[test]
    fn derive_rules() {
        let three = Bool::run_workload(
            iter_bool(3),
            Ruleset::default(),
            Limits {
                iter: 4,
                node: 1000000,
            },
        );
        three.to_file("three.txt");

        let four = Bool::run_workload(
            iter_bool(4),
            Ruleset::default(),
            Limits {
                iter: 4,
                node: 1000000,
            },
        );
        four.to_file("four.txt");

        let (can, cannot) = three.derive(
            DeriveType::LhsAndRhs,
            &four,
            Limits {
                iter: 10,
                node: 1000000,
            },
        );
        assert!(can.len() > 0);
        assert!(cannot.len() > 0);
    }
}
