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


// BV-bool language
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
            for j in synth.ids() {
                if (ids[&i] + ids[&j] + 1 != iter) ||
                    !synth.egraph[i].data.in_domain ||
                    !synth.egraph[j].data.in_domain {
                    continue;
                }

                if iter > synth.params.no_constants_above_iter {
                    if synth.egraph[i].data.exact || synth.egraph[j].data.exact {
                        continue;
                    }
                } else {
                    if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                        continue;
                    }
                };

                to_add.push(Math::Band([i, j]));
                to_add.push(Math::Bor([i, j]));
                to_add.push(Math::Bxor([i, j]));
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.in_domain {
                continue;
            }
            
            to_add.push(Math::Bnot(i));
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
            Math::Band([i, j]) => {
                let op_id = synth.egraph.add(node);
                let fsti_id = first_bool_id(&synth.egraph, i);
                let seci_id = second_bool_id(&synth.egraph, i);
                let fstj_id = first_bool_id(&synth.egraph, j);
                let secj_id = second_bool_id(&synth.egraph, j);
                let nfst_id = synth.egraph.add(Math::And([fsti_id, fstj_id]));
                let nsec_id = synth.egraph.add(Math::And([seci_id, secj_id]));
                let mk_id = synth.egraph.add(Math::Make([nfst_id, nsec_id]));
                let (uid, _) = synth.egraph.union(op_id, mk_id);
                uid
            },
            Math::Bor([i, j]) => {
                let op_id = synth.egraph.add(node);
                let fsti_id = first_bool_id(&synth.egraph, i);
                let seci_id = second_bool_id(&synth.egraph, i);
                let fstj_id = first_bool_id(&synth.egraph, j);
                let secj_id = second_bool_id(&synth.egraph, j);
                let nfst_id = synth.egraph.add(Math::Or([fsti_id, fstj_id]));
                let nsec_id = synth.egraph.add(Math::Or([seci_id, secj_id]));
                let mk_id = synth.egraph.add(Math::Make([nfst_id, nsec_id]));
                let (uid, _) = synth.egraph.union(op_id, mk_id);
                uid
            },
            Math::Bxor([i, j]) => {
                let op_id = synth.egraph.add(node);
                let fsti_id = first_bool_id(&synth.egraph, i);
                let seci_id = second_bool_id(&synth.egraph, i);
                let fstj_id = first_bool_id(&synth.egraph, j);
                let secj_id = second_bool_id(&synth.egraph, j);
                let nfst_id = synth.egraph.add(Math::Xor([fsti_id, fstj_id]));
                let nsec_id = synth.egraph.add(Math::Xor([seci_id, secj_id]));
                let mk_id = synth.egraph.add(Math::Make([nfst_id, nsec_id]));
                let (uid, _) = synth.egraph.union(op_id, mk_id);
                uid
            },
            _ => {
                panic!("Not a bitvector node {:?}", node);
            }
        }
    }

    /// @Override
    /// Heuristics for ranking rewrites based on number of variables,
    /// constants, size of the `lhs` and `rhs`, total size of `lhs` and `rhs`,
    /// and number of ops.
    fn score(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> [i32; 5] {
        let lhs_recpat = Self::recpat_instantiate(&lhs.ast);
        let rhs_recpat = Self::recpat_instantiate(&rhs.ast);
        let sz_lhs = DomainAstSize.cost_rec(&lhs_recpat) as i32;
        let sz_rhs = DomainAstSize.cost_rec(&rhs_recpat) as i32;
        // let sz_max_pattern = sz_lhs.max(sz_rhs);

        // lhs.vars() and rhs.vars() is deduping
        // examples
        //   (- x x) => 0 --- 1 b/c x only var
        //   (- x 0) => x --- 1 b/c x only var
        //   (+ x y) => (+ y x) --- 2 b/c x, y only vars
        let mut var_set: HashSet<Var> = Default::default();
        var_set.extend(lhs.vars());
        var_set.extend(rhs.vars());
        let n_vars_rule = var_set.len() as i32;

        let mut op_set: HashSet<String> = Default::default();
        for node in lhs.ast.as_ref().iter().chain(rhs.ast.as_ref()) {
            if !node.is_leaf() {
                op_set.insert(node.display_op().to_string());
            }
        }
        let n_ops = op_set.len() as i32;

        let n_consts = lhs
            .ast
            .as_ref()
            .iter()
            .chain(rhs.ast.as_ref())
            .filter(|n| match n {
                ENodeOrVar::ENode(n) => n.is_constant(),
                ENodeOrVar::Var(_) => false,
            })
            .count() as i32;

        // (-sz_max_pattern, n_vars_rule)
        [
            n_vars_rule,
            -n_consts,
            -i32::max(sz_lhs, sz_rhs),
            // -i32::min(sz_lhs, sz_rhs),
            -(sz_lhs + sz_rhs),
            -n_ops,
            // 0
        ]
    }
}

/// Entry point.
fn main() {
    Math::main()
}
