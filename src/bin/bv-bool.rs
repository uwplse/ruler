/*!
    2 bit implementation of Bitvectors.
!*/

use egg::*;
use ruler::*;

use std::ops::*;

define_language! {
    /// Define the operators for the domain.
    pub enum Math {
        // boolean domain
        "~" = Not(Id),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "^" = Xor([Id; 2]),
        Lit(bool),

        // bitvector domain
        "and" = Band([Id; 2]),
        "or" = Bor([Id; 2]),
        "xor" = Bxor([Id; 2]),
        "not" = Bnot(Id),
        Var(egg::Symbol),
        Num(BV<2>),

        // conversions
        "bv" = Make([Id; 2]),
        "first" = First(Id),
        "second" = Second(Id),
    }
}

fn is_bv_str(s: &'static str) -> impl Fn(&mut EGraph<Math, SynthAnalysis>, Id, &Subst) -> bool {
    let var = s.parse().unwrap();
    move |egraph, _, subst| egraph[subst[var]].data.is_allowed
}

// BV-bool language
impl SynthLanguage for Math {
    type Constant = BV<2>;

    fn convert_parse(s: &str) -> RecExpr<Self> {
        let s = s
            .replace("and", "&")
            .replace("xor", "^")
            .replace("or", "|")
            .replace("not", "~");
        s.parse().unwrap()
    }

    // no evaluation needed
    fn eval<'a, F>(&'a self, _cvec_len: usize, mut _v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
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
        if let Math::Num(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Num(c)
    }

    // override default behavior
    fn is_allowed(&self) -> bool {
        matches!(
            self,
            Math::Band(_)
                | Math::Bor(_)
                | Math::Bxor(_)
                | Math::Bnot(_)
                | Math::Num(_)
                | Math::Var(_)
                | Math::Make(_)
        )
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: 0,
            constant_fold: if synth.params.no_constant_fold {
                ConstantFoldMethod::NoFold
            } else {
                ConstantFoldMethod::Lang
            },
            rule_lifting: true,
        });

        for i in 0..synth.params.variables {
            let var = egg::Symbol::from(letter(i));
            let var_id = egraph.add(Math::Var(var));
            let fst_id = egraph.add(Math::First(var_id));
            let sec_id = egraph.add(Math::Second(var_id));
            let mk_id = egraph.add(Math::Make([fst_id, sec_id]));
            egraph.union(var_id, mk_id);
        }

        synth.lifting_rewrites = vec![
            rewrite!("def-bv"; "?a" => "(bv (first ?a) (second ?a))" if is_bv_str("?a")),
            rewrite!("def-not-first"; "(first (not ?a))" => "(~ (first ?a))"),
            rewrite!("def-not-second"; "(second (not ?a))" => "(~ (second ?a))"),
            rewrite!("def-and-first"; "(first (and ?a ?b))" => "(& (first ?a) (first ?b))"),
            rewrite!("def-and-second"; "(second (and ?a ?b))" => "(& (second ?a) (second ?b))"),
            rewrite!("def-or-first"; "(first (or ?a ?b))" => "(| (first ?a) (first ?b))"),
            rewrite!("def-or-second"; "(second (or ?a ?b))" => "(| (second ?a) (second ?b))"),
            rewrite!("def-xor-first"; "(first (xor ?a ?b))" => "(^ (first ?a) (first ?b))"),
            rewrite!("def-xor-second"; "(second (xor ?a ?b))" => "(^ (second ?a) (second ?b))"),
        ];

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let extract = Extractor::new(&synth.egraph, NumberOfAllowedOps);
        let mut to_add = vec![];

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        for i in synth.ids() {
            for j in synth.ids() {
                if (ids[&i] + ids[&j] + 1 != iter)
                    || !synth.egraph[i].data.is_allowed
                    || !synth.egraph[j].data.is_allowed
                {
                    continue;
                }

                if iter > synth.params.no_constants_above_iter {
                    if synth.egraph[i].data.exact || synth.egraph[j].data.exact {
                        continue;
                    }
                } else if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                };

                to_add.push(Math::Band([i, j]));
                to_add.push(Math::Bor([i, j]));
                to_add.push(Math::Bxor([i, j]));
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.is_allowed
            {
                continue;
            }

            to_add.push(Math::Bnot(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult<Self> {
        ValidationResult::Valid
    }
}

/// Entry point.
fn main() {
    Math::main()
}
