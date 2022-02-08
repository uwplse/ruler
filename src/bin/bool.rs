/*!
Domain of Booleans.
The rewrites for this small domain are correct by construction as
they are model checked.
!*/

use egg::*;
use ruler::*;

use std::ops::*;

define_language! {
    /// Define the operators for the domain.
    pub enum Math {
        "~" = Not(Id),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "^" = Xor([Id; 2]),
        Lit(bool),
        Var(egg::Symbol),
    }
}

impl SynthLanguage for Math {
    type Constant = bool;

    fn convert_parse(s: &str) -> RecExpr<Self> {
        let s = s
            .replace("and", "&")
            .replace("xor", "^")
            .replace("or", "|")
            .replace("not", "~");
        s.parse().unwrap()
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Not(a) => map!(v, a => Some(a.not())),

            Math::And([a, b]) => map!(v, a, b => Some(*a & *b)),
            Math::Or([a, b]) => map!(v, a, b => Some(*a | *b)),
            Math::Xor([a, b]) => map!(v, a, b => Some(*a ^ *b)),

            Math::Lit(n) => vec![Some(n.clone()); cvec_len],
            Math::Var(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Math::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::Lit(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        // let consts: Vec<Option<bool>> = vec![];
        let consts: Vec<Option<bool>> = vec![Some(false), Some(true)];

        let consts = self_product(&consts, synth.params.variables);
        // println!("cvec len: {}", consts[0].len());

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: consts[0].len(),
            constant_fold: if synth.params.no_constant_fold {
                ConstantFoldMethod::NoFold
            } else {
                ConstantFoldMethod::CvecMatching
            },
            rule_lifting: false,
        });

        egraph.add(Math::Lit(false));
        egraph.add(Math::Lit(true));

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));
            egraph[id].data.cvec = consts[i].clone();
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let extract = Extractor::new(&synth.egraph, NumberOfOps);

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                if ids[&i] + ids[&j] + 1 != iter {
                    continue;
                }
                to_add.push(Math::And([i, j]));
                to_add.push(Math::Or([i, j]));
                if !synth.params.no_xor {
                    to_add.push(Math::Xor([i, j]));
                }
            }
            if ids[&i] + 1 != iter {
                continue;
            }
            to_add.push(Math::Not(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> bool {
        true
    }
}

/// Entry point.
fn main() {
    Math::main()
}
#[cfg(test)]
mod test {
    use super::*;

    fn get_params(num_iters: usize) -> SynthParams {
        SynthParams {
            seed: 0,
            n_samples: 2,
            variables: 3,
            outfile: String::from("out.json"),
            no_constant_fold: true,
            iters: num_iters,
            rules_to_take: 0,
            node_chunk_size: 0,
            eq_chunk_size: 0,
            no_constants_above_iter: 999999,
            no_conditionals: true,
            no_run_rewrites: false,
            linear_cvec_matching: false,
            ema_above_iter: 999999,
            disabled_ops: None,
            disabled_consts: None,
            filtered_consts: None,
            eqsat_node_limit: 300000,
            eqsat_iter_limit: 2,
            eqsat_time_limit: 60,
            important_cvec_offsets: 5,
            str_int_variables: 1,
            complete_cvec: false,
            no_xor: false,
            no_shift: false,
            num_fuzz: 0,
            use_smt: false,
            do_final_run: true,
            prior_rules: None,
        }
    }

    #[test]
    fn iter1_rules() {
        let syn = ruler::Synthesizer::<Math>::new(get_params(1));
        let report = syn.run();
        let expected = vec![
            "(& ?b ?a) <=> (& ?a ?b)",
            "(| ?b ?a) <=> (| ?a ?b)",
            "(^ ?b ?a) <=> (^ ?a ?b)",
            "?a <=> (| ?a ?a)",
            "?a <=> (& ?a ?a)",
            "(^ ?a ?a) => false",
            "?a <=> (& true ?a)",
            "?a <=> (| false ?a)",
            "?a <=> (^ false ?a)",
            "(~ ?a) <=> (^ true ?a)",
            "(& false ?a) => false",
            "(| true ?a) => true",
        ];
        assert_eq!(report.num_rules, expected.len());
        report
            .all_eqs
            .iter()
            .for_each(|rule| assert!(expected.contains(&rule.to_string().as_str())));
    }

    #[test]
    fn iter2_rules() {
        let syn = ruler::Synthesizer::<Math>::new(get_params(2));
        let report = syn.run();
        let expected = vec![
            "(& ?c (& ?b ?a)) <=> (& ?a (& ?b ?c))",
            "(^ ?c (^ ?b ?a)) <=> (^ ?a (^ ?b ?c))",
            "(| ?c (| ?b ?a)) <=> (| ?b (| ?a ?c))",
            "(& ?b ?a) <=> (& ?a ?b)",
            "(| ?b ?a) <=> (| ?a ?b)",
            "(^ ?b ?a) <=> (^ ?a ?b)",
            "(& ?b (| ?b ?a)) => ?b",
            "(| ?a (& ?b ?a)) => ?a",
            "(| ?b ?a) <=> (| ?b (^ ?b ?a))",
            "(& ?b (~ ?a)) <=> (& ?b (^ ?a ?b))",
            "(^ ?b (& ?b ?a)) <=> (& ?b (~ ?a))",
            "(^ ?a (| ?b ?a)) <=> (& ?b (~ ?a))",
            "?a <=> (| ?a ?a)",
            "?a <=> (& ?a ?a)",
            "(^ ?a ?a) => false",
            "?a <=> (& true ?a)",
            "?a <=> (| false ?a)",
            "?a <=> (^ false ?a)",
            "(~ ?a) <=> (^ true ?a)",
            "(& false ?a) => false",
            "(| true ?a) => true",
        ];
        assert_eq!(report.num_rules, expected.len());
        report
            .all_eqs
            .iter()
            .for_each(|rule| assert!(expected.contains(&rule.to_string().as_str())));
    }
}
