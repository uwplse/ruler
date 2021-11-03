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
        "not" = Not(Id),
        "and" = And([Id; 2]),
        "or" = Or([Id; 2]),
        "xor" = Xor([Id; 2]),
        Lit(bool),

        // bitvector domain
        "bv" = Make([Id; 2]), 
        "&" = Band([Id; 2]),
        "|" = Bor([Id; 2]),
        "^" = Bxor([Id; 2]),
        "~" = Bnot(Id),
        Var(egg::Symbol),
        Num(BV<2>),

        // conversions
        "first" = First(Id),
        "second" = Second(Id),
    }
}

// transcription of `egraph::add_expr_rec`
fn add_domain_expr_rec(
    synth: &mut Synthesizer<Math>,
    expr: &[Math]
) -> Id {
    let e = expr.last().unwrap().clone().map_children(|i| {
        let child = &expr[..usize::from(i) + 1];
        add_domain_expr_rec(synth, child)
    });
    Math::add_domain_node(synth, e)
}

fn first_bool_id(
    egraph: &EGraph<Math, SynthAnalysis>,
    id: Id
) -> Id {
    egraph[id].nodes
        .iter()
        .find_map(|x| {
            match x {
                Math::Make([i, _]) => Some(*i),
                _ => None
            }})
        .unwrap()
}

fn second_bool_id(
    egraph: &EGraph<Math, SynthAnalysis>,
    id: Id
) -> Id {
    egraph[id].nodes
        .iter()
        .find_map(|x| {
            match x {
                Math::Make([_, i]) => Some(*i),
                _ => None
            }})
        .unwrap()
}

impl SynthLanguage for Math {
    type Constant = BV::<2>;

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
    fn is_in_domain(&self) -> bool {
        match self {
            Math::Band([_, _]) => true,
            Math::Bor([_, _]) => true,
            Math::Bxor([_, _]) => true,
            Math::Bnot(_) => true,
            Math::Num(_) => true,
            Math::Var(_) => true,
            _ => false
        }
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: 0,
            foldable: !synth.params.no_constant_fold
        });

        for i in 0..synth.params.variables {
            let var = egg::Symbol::from(letter(i));
            let var_id = egraph.add(Math::Var(var));
            let fst_id = egraph.add(Math::First(var_id));
            let sec_id = egraph.add(Math::Second(var_id));
            let mk_id = egraph.add(Math::Make([fst_id, sec_id]));
            egraph.union(var_id, mk_id);
        }

        synth.egraph = egraph;
        synth.lifting = true;

        for id in synth.ids() {
            log::info!("{}: {:?}", id, synth.egraph[id]);
        }
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let mut extract = Extractor::new(&synth.egraph, NumberOfDomainOps);
        let mut to_add = vec![];

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        for i in synth.ids() {
            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.in_domain {
                continue;
            }
            
            to_add.push(Math::Bnot(i));
        }

        for id in synth.ids() {
            log::info!("{}: {:?}", id, synth.egraph[id]);
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

    fn add_domain_expr(synth: &mut Synthesizer<Self>, expr: &RecExpr<Self>) -> Id {
        add_domain_expr_rec(synth, expr.as_ref())
    }

    fn add_domain_node(synth: &mut Synthesizer<Self>, node: Self) -> Id {
        match node {
            Math::Bnot(i) => {
                let op_id = synth.egraph.add(node);
                let fst_id = first_bool_id(&synth.egraph, i);
                let sec_id = second_bool_id(&synth.egraph, i);
                let nfst_id = synth.egraph.add(Math::Not(fst_id));
                let nsec_id = synth.egraph.add(Math::Not(sec_id));
                let mk_id = synth.egraph.add(Math::Make([nfst_id, nsec_id]));
                let (uid, _) = synth.egraph.union(op_id, mk_id);
                uid
            },
            _ => {
                panic!("Not implemented {:?}", node);
            }
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
