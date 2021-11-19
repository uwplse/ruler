/*!
    Real from rationals
!*/

use num::bigint::{BigInt};
use num::{rational::Ratio, Zero};
use std::ops::*;

use egg::*;
use ruler::*;

/// define `Constant` as rationals
pub type Rational = Ratio<BigInt>;

/// macro for constant folding
macro_rules! constant_fold {
    ($i:ident, $egraph:ident, $to_add:ident, $op:tt) => {     // unary
        for n in &$egraph[*$i].nodes {
            match n {
                Math::Rat(v) => {
                    let r = $op v;
                    let s = r.to_string();
                    $to_add = Some((Math::Rat(r), s));
                    break;
                },
                _ => (),
            }
        }
    };
    ($i:ident, $j:ident, $egraph:ident, $to_add:ident, $op:tt) => {     // binary
        for n in &$egraph[*$i].nodes {
            match n {
                Math::Rat(v) => {
                    for n in &$egraph[*$j].nodes {
                        match n {
                            Math::Rat(w) => {
                                let r = v $op w;
                                let s = r.to_string();
                                $to_add = Some((Math::Rat(r), s));
                                break;
                            },
                            _ => (),
                        }
                    }
                },
                _ => (),
            }
        }
    };
}



define_language! {
    pub enum Math {
        // rational domain
        "~" = Neg(Id),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "fabs" = Abs(Id),
        Rat(Rational),

        // real domain
        "~R" = RNeg(Id),
        "+R" = RAdd([Id; 2]),
        "-R" = RSub([Id; 2]),
        "*R" = RMul([Id; 2]),
        "/R" = RDiv([Id; 2]),
        Var(egg::Symbol),
        Real(egg::Symbol),           // TODO: this is dumb

        // conversions
        "lim" = Lim(Id),
        "seq" = Seq(Id),
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

// returns the sequence associated with the eclass
fn sequence_ids(egraph: &EGraph<Math, SynthAnalysis>, id: &Id) -> Vec<Id> {
    egraph[*id].nodes
        .iter()
        .filter_map(|x| {
            match x {
                Math::Lim(i) => Some(*i),
                _ => None,
            }
        })
        .collect()
}

fn real_const_symbol(s: &str) -> egg::Symbol {
    egg::Symbol::from(s.to_owned() + "R")
}

impl SynthLanguage for Math {
    type Constant = Rational;  // not used

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
        if let Math::Rat(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Rat(c)
    }

    // override default behavior
    fn is_constant(&self) -> bool {
        match self {
            Math::Real(_) => true,
            _ => false,
        }
    }

    // override default behavior
    fn is_in_domain(&self) -> bool {
        match self {
            Math::RNeg(_) => true,
            Math::RAdd([_, _]) => true,
            Math::RSub([_, _]) => true,
            Math::RMul([_, _]) => true,
            Math::RDiv([_, _]) => true,
            Math::Var(_) => true,
            Math::Real(_) => true,
            _ => false
        }
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        // disabled constants (TODO: validate input)
        let disabled_consts: Vec<&str> =
            if let Some(s) = &synth.params.disabled_consts {
                s.split(" ").collect()
            } else {
                vec![]
            };

        // this is for adding to the egraph, not used for cvec.
        let constants: Vec<(Rational, &str)> = ["1", "0", "-1"]
            .iter()
            .filter(|s| disabled_consts.iter().find(|x| x.eq(s)).is_none())
            .map(|s| (s.parse().unwrap(), *s))
            .collect();


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
            let seq_id = egraph.add(Math::Seq(var_id));
            let lim_id = egraph.add(Math::Lim(seq_id));
            egraph.union(var_id, lim_id);
        }

        for (c, s) in constants {
            let c_id = egraph.add(Math::Rat(c.clone()));
            let lim_id = egraph.add(Math::Lim(c_id));
            let seq_id = egraph.add(Math::Seq(lim_id));
            let re_id = egraph.add(Math::Real(real_const_symbol(s)));
            egraph.union(seq_id, c_id);
            egraph.union(lim_id, re_id);
        }

        synth.egraph = egraph;
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

                to_add.push(Math::RAdd([i, j]));
                to_add.push(Math::RSub([i, j]));
                to_add.push(Math::RMul([i, j]));

                if !synth.egraph[j].nodes.iter().any(|x| {
                    match x {
                        Math::Real(v) => *v == real_const_symbol("0"),
                        _ => false
                    }
                }) {
                    to_add.push(Math::RDiv([i, j]));
                }
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.in_domain {
                continue;
            }

            to_add.push(Math::RNeg(i));
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
            Math::RNeg(i) => {
                let mut op_id = synth.egraph.add(node);
                for seq_id in sequence_ids(&synth.egraph, &i) {
                    let neg_id = synth.egraph.add(Math::Neg(seq_id));
                    let lim_id = synth.egraph.add(Math::Lim(neg_id));
                    let (uid, _) = synth.egraph.union(op_id, lim_id);
                    op_id = uid;
                }
                op_id
            },
            Math::RAdd([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let seqi_ids = sequence_ids(&synth.egraph, &i);
                let seqj_ids = sequence_ids(&synth.egraph, &j);
                for (&seqi_id, &seqj_id) in seqi_ids.iter().zip(seqj_ids.iter()) {
                    let add_id = synth.egraph.add(Math::Add([seqi_id, seqj_id]));
                    let lim_id = synth.egraph.add(Math::Lim(add_id));
                    let (uid, _) = synth.egraph.union(op_id, lim_id);
                    op_id = uid;
                }
                op_id
            },
            Math::RSub([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let seqi_ids = sequence_ids(&synth.egraph, &i);
                let seqj_ids = sequence_ids(&synth.egraph, &j);
                for (&seqi_id, &seqj_id) in seqi_ids.iter().zip(seqj_ids.iter()) {
                    let sub_id = synth.egraph.add(Math::Sub([seqi_id, seqj_id]));
                    let lim_id = synth.egraph.add(Math::Lim(sub_id));
                    let (uid, _) = synth.egraph.union(op_id, lim_id);
                    op_id = uid;
                }
                op_id
            },
            Math::RMul([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let seqi_ids = sequence_ids(&synth.egraph, &i);
                let seqj_ids = sequence_ids(&synth.egraph, &j);
                for (&seqi_id, &seqj_id) in seqi_ids.iter().zip(seqj_ids.iter()) {
                    let mul_id = synth.egraph.add(Math::Mul([seqi_id, seqj_id]));
                    let lim_id = synth.egraph.add(Math::Lim(mul_id));
                    let (uid, _) = synth.egraph.union(op_id, lim_id);
                    op_id = uid;
                }
                op_id
            },
            Math::RDiv([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let seqi_ids = sequence_ids(&synth.egraph, &i);
                let seqj_ids = sequence_ids(&synth.egraph, &j);
                for (&seqi_id, &seqj_id) in seqi_ids.iter().zip(seqj_ids.iter()) {
                    let div_id = synth.egraph.add(Math::Div([seqi_id, seqj_id]));
                    let lim_id = synth.egraph.add(Math::Lim(div_id));
                    let (uid, _) = synth.egraph.union(op_id, lim_id);
                    op_id = uid;
                }
                op_id
            },
            Math::Var(_) => {
                synth.egraph.add(node)
            }
            Math::Real(_) => {
                synth.egraph.add(node)
            }
            _ => {
                panic!("Not a real node {:?}", node);
            }
        }
    }

    // custom constant folder
    fn constant_fold(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if !egraph[id].data.in_domain { // lower domain
            if egraph[id].nodes.iter().any(|x| {
                match x {
                    Math::Rat(_) => true,
                    _ => false,
                }
            }) {        // early exit if constant exists
                return;
            }

            let mut to_add: Option<(Self, String)> = None;
            for x in &egraph[id].nodes {
                match x {
                    Math::Neg(i) => constant_fold!(i, egraph, to_add, -),
                    Math::Add([i, j]) => constant_fold!(i, j, egraph, to_add, +),
                    Math::Sub([i, j]) => constant_fold!(i, j, egraph, to_add, -),
                    Math::Mul([i, j]) => constant_fold!(i, j, egraph, to_add, *),
                    Math::Div([i, j]) => {  // explicit because of zero check
                        for n in &egraph[*i].nodes {
                            match n {
                                Math::Rat(v) => {
                                    for n in &egraph[*j].nodes {
                                        match n {
                                            Math::Rat(w) => {
                                                if !w.is_zero() {
                                                    let r = v / w;
                                                    let s = r.to_string();
                                                    to_add = Some((Math::Rat(r), s));
                                                    break;
                                                }
                                            },
                                            _ => (),
                                        }
                                    }
                                },
                                _ => (),
                            }
                        }
                    }
                    _ => (),
                }
            }

            if let Some((n, s)) = to_add {
                // log::info!("constant fold lower: {}", s);
                let mut to_update = vec![];
                for id in egraph.classes().map(|c| c.id) {
                    for n in &egraph[id].nodes {
                        match n {
                            Math::Lim(i) => {
                                if *i == id {
                                    to_update.push(*i);
                                }
                            },
                            _ => (),
                        }
                    }
                }
    
                // C = lim c
                // c = id = seq C
                let c_id = egraph.add(n);
                let r_id = egraph.add(Math::Real(real_const_symbol(&s)));
                let seq_id = egraph.add(Math::Seq(r_id));
                let lim_id = egraph.add(Math::Lim(c_id));
    
                egraph.union(r_id, lim_id);
                egraph.union(c_id, id);
                egraph.union(c_id, seq_id);
                for i in to_update {
                    egraph.union(lim_id, i);
                }
    
                let r_id = egraph.find(r_id);
                egraph[r_id].data.exact = true;  
            }
        } else {
            let lim_ids: Vec<Id> = egraph[id].nodes.iter()
                .filter_map(|n| match n {
                    Math::Lim(v) => Some(*v),
                    _ => None,
                })
                .collect();

            // if !lim_ids.is_empty() {
            //     log::info!("constant fold higher");
            // }

            for id in lim_ids {
                Self::constant_fold(egraph, id);
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
