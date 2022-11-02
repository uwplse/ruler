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

fn is_bv_str(s: &'static str) -> impl Fn(&mut EGraph<Math, SynthAnalysis>, Id, &Subst) -> bool {
    let var = s.parse().unwrap();
    move |egraph, _, subst| egraph[subst[var]].nodes.iter().any(Math::is_allowed)
}

fn extract_bool_constant(nodes: &[Math]) -> Option<bool> {
    for n in nodes {
        if let Math::Lit(v) = n {
            return Some(*v);
        }
    }

    None
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Top,
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
// BV-bool language
impl SynthLanguage for Math {
    type Constant = BV<2>;
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        Type::Top
    }

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

    fn is_constant(&self) -> bool {
        matches!(self, Math::Num(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
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

    fn is_extractable(&self) -> bool {
        matches!(
            self,
            Math::Band(_)
                | Math::Bor(_)
                | Math::Bxor(_)
                | Math::Bnot(_)
                | Math::Num(_)
                | Math::Var(_)
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

        // seed egraph with bool constants
        egraph.add(Math::Lit(true));
        egraph.add(Math::Lit(false));

        // seed egraph with bv constants
        egraph.add(Math::Num(BV::<2>::from(3)));
        egraph.add(Math::Num(BV::<2>::from(2)));
        egraph.add(Math::Num(BV::<2>::from(1)));
        egraph.add(Math::Num(BV::<2>::from(0)));

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
                    || !synth.egraph[i].nodes.iter().any(Math::is_allowed)
                    || !synth.egraph[j].nodes.iter().any(Math::is_allowed)
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

            if ids[&i] + 1 != iter
                || synth.egraph[i].data.exact
                || !synth.egraph[i].nodes.iter().any(Math::is_allowed)
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
    ) -> ValidationResult {
        ValidationResult::Valid
    }

    fn constant_fold(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if egraph[id].nodes.iter().any(|n| matches!(n, Math::Num(_))) {
            return;
        }

        for n in &egraph[id].nodes {
            if let Math::Make([i, j]) = n {
                if let Some(v) = extract_bool_constant(&egraph[*i].nodes) {
                    if let Some(w) = extract_bool_constant(&egraph[*j].nodes) {
                        let cnst = match (v, w) {
                            (true, true) => BV::<2>::from(3),
                            (true, false) => BV::<2>::from(2),
                            (false, true) => BV::<2>::from(1),
                            (false, false) => BV::<2>::from(0),
                        };

                        let c_id = egraph.add(Math::Num(cnst));
                        egraph.union(id, c_id);
                        return;
                    }
                }
            }
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
