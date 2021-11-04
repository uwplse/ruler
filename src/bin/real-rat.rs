/*!
    Real from rationals
!*/

use num::bigint::{BigInt};
use num::{rational::Ratio};

use egg::*;
use ruler::*;

/// define `Constant` as rationals
pub type Rational = Ratio<BigInt>;

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
        Real(Rational),     // TODO: this is dumb

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
fn sequence_id(egraph: &EGraph<Math, SynthAnalysis>, id: &Id) -> Id {
    egraph[*id].nodes
        .iter()
        .find_map(|x| {
            match x {
                Math::Lim(i) => Some(*i),
                _ => None
            }
        }).unwrap()
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

    // TODO: implement constants
    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::Rat(n) = self {
            Some(n)
        } else {
            None
        }
    }

    // TODO: implement constants
    fn mk_constant(c: Self::Constant) -> Self {
        Math::Rat(c)
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
        let constants: Vec<Rational> = ["1", "0", "-1"]
            .iter()
            .filter(|s| disabled_consts.iter().find(|x| x.eq(s)).is_none())
            .map(|s| s.parse().unwrap())
            .collect();


            let mut egraph = EGraph::new(SynthAnalysis {
                cvec_len: 0,
                constant_fold: if synth.params.no_constant_fold {
                    ConstantFoldMethod::NoFold
                } else {
                    ConstantFoldMethod::Lang
                },
            });

        for i in 0..synth.params.variables {
            let var = egg::Symbol::from(letter(i));
            let var_id = egraph.add(Math::Var(var));
            let seq_id = egraph.add(Math::Seq(var_id));
            let lim_id = egraph.add(Math::Lim(seq_id));
            egraph.union(var_id, lim_id);
        }

        for c in constants {
            let c_id = egraph.add(Math::Rat(c.clone()));
            let lim_id = egraph.add(Math::Lim(c_id));
            let seq_id = egraph.add(Math::Seq(lim_id));
            let re_id = egraph.add(Math::Real(c.clone()));
            egraph.union(seq_id, c_id);
            egraph.union(lim_id, re_id);
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

                to_add.push(Math::RAdd([i, j]));
                to_add.push(Math::RSub([i, j]));
                to_add.push(Math::RMul([i, j]));
                to_add.push(Math::RDiv([i, j]));
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
                let op_id = synth.egraph.add(node);
                let seqi_id = sequence_id(&synth.egraph, &i);
                let neg_id = synth.egraph.add(Math::Neg(seqi_id));
                let lim_id = synth.egraph.add(Math::Lim(neg_id));
                let (uid, _) = synth.egraph.union(op_id, lim_id);
                uid
            },
            Math::RAdd([i, j]) => {
                let op_id = synth.egraph.add(node);
                let seqi_id = sequence_id(&synth.egraph, &i);
                let seqj_id = sequence_id(&synth.egraph, &j);
                let neg_id = synth.egraph.add(Math::Add([seqi_id, seqj_id]));
                let lim_id = synth.egraph.add(Math::Lim(neg_id));
                let (uid, _) = synth.egraph.union(op_id, lim_id);
                uid
            },
            Math::RSub([i, j]) => {
                let op_id = synth.egraph.add(node);
                let seqi_id = sequence_id(&synth.egraph, &i);
                let seqj_id = sequence_id(&synth.egraph, &j);
                let neg_id = synth.egraph.add(Math::Sub([seqi_id, seqj_id]));
                let lim_id = synth.egraph.add(Math::Lim(neg_id));
                let (uid, _) = synth.egraph.union(op_id, lim_id);
                uid
            },
            Math::RMul([i, j]) => {
                let op_id = synth.egraph.add(node);
                let seqi_id = sequence_id(&synth.egraph, &i);
                let seqj_id = sequence_id(&synth.egraph, &j);
                let neg_id = synth.egraph.add(Math::Mul([seqi_id, seqj_id]));
                let lim_id = synth.egraph.add(Math::Lim(neg_id));
                let (uid, _) = synth.egraph.union(op_id, lim_id);
                uid
            },
            Math::RDiv([i, j]) => {
                let op_id = synth.egraph.add(node);
                let seqi_id = sequence_id(&synth.egraph, &i);
                let seqj_id = sequence_id(&synth.egraph, &j);
                let neg_id = synth.egraph.add(Math::Div([seqi_id, seqj_id]));
                let lim_id = synth.egraph.add(Math::Lim(neg_id));
                let (uid, _) = synth.egraph.union(op_id, lim_id);
                uid
            },
            Math::Var(_) => {
                synth.egraph.add(node)
            }
            _ => {
                panic!("Not a real node {:?}", node);
            }
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
