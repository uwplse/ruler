/*!
    Complex from Reals
!*/

use num::bigint::{BigInt};
use num::{rational::Ratio};

use egg::*;
use ruler::*;

/// define `Constant` as rationals
pub type Rational = Ratio<BigInt>;

define_language! {
    pub enum Math {
        // real domain
        "~R" = RNeg(Id),
        "+R" = RAdd([Id; 2]),
        "-R" = RSub([Id; 2]),
        "*R" = RMul([Id; 2]),
        "/R" = RDiv([Id; 2]),
        Real(Rational),     // TODO: this is dumb

        // real domain
        "~C" = CNeg(Id),
        "+C" = CAdd([Id; 2]),
        "-C" = CSub([Id; 2]),
        "*C" = CMul([Id; 2]),
        "/C" = CDiv([Id; 2]),
        "conj" = Conj(Id),
        Complex(egg::Symbol),
        Var(egg::Symbol),

        // conversions
        "complex" = Make([Id; 2]),
        "re" = Re(Id),
        "im" = Im(Id),
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

// returns a vector of (id, id) corresponding to the real and
// imaginary components of numbers associated with the eclass
fn component_ids(egraph: &EGraph<Math, SynthAnalysis>, id: &Id) -> Vec<(Id, Id)> {
    egraph[*id].nodes
        .iter()
        .filter_map(|x| {
            match x {
                Math::Make([i, j]) => Some((*i, *j)),
                _ => None,
            }
        })
        .collect()
}

impl SynthLanguage for Math {
    type Constant = egg::Symbol;  // not used

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
        if let Math::Complex(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Complex(c)
    }

    // override default behavior
    fn is_constant(&self) -> bool {
        match self {
            Math::Complex(_) => true,
            _ => false,
        }
    }

    // override default behavior
    fn is_in_domain(&self) -> bool {
        match self {
            Math::CNeg(_) => true,
            Math::CAdd([_, _]) => true,
            Math::CSub([_, _]) => true,
            Math::CMul([_, _]) => true,
            Math::CDiv([_, _]) => true,
            Math::Conj(_) => true,
            Math::Var(_) => true,
            Math::Complex(_) => true,
            _ => false
        }
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
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
            let re_id = egraph.add(Math::Re(var_id));
            let im_id = egraph.add(Math::Im(var_id));
            let cx_id = egraph.add(Math::Make([re_id, im_id]));
            egraph.union(var_id, cx_id);
        }

        // rational constants
        let zero: Rational = "0".parse().unwrap();
        let one: Rational = "1".parse().unwrap();
        let re_zero = egraph.add(Math::Real(zero));
        let re_one = egraph.add(Math::Real(one));

        // zero constant
        let cx_zero = egraph.add(Math::Complex(Symbol::from("0")));
        let zero_mk = egraph.add(Math::Make([re_zero, re_zero]));
        let zero_mk_re = egraph.add(Math::Re(cx_zero));
        let zero_mk_im = egraph.add(Math::Im(cx_zero));
        egraph.union(cx_zero, zero_mk);
        egraph.union(zero_mk_re, re_zero);
        egraph.union(zero_mk_im, re_zero);

        // one constant
        let cx_one = egraph.add(Math::Complex(Symbol::from("1")));
        let one_mk = egraph.add(Math::Make([re_one, re_zero]));
        let one_mk_re = egraph.add(Math::Re(cx_one));
        let one_mk_im = egraph.add(Math::Im(cx_one));
        egraph.union(cx_one, one_mk);
        egraph.union(one_mk_re, re_one);
        egraph.union(one_mk_im, re_zero);

        // i constant
        let cx_i = egraph.add(Math::Complex(Symbol::from("i")));
        let i_mk = egraph.add(Math::Make([re_zero, re_one]));
        let i_mk_re = egraph.add(Math::Re(cx_i));
        let i_mk_im = egraph.add(Math::Im(cx_i));
        egraph.union(cx_i, i_mk);
        egraph.union(i_mk_re, re_zero);
        egraph.union(i_mk_im, re_one);

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

                to_add.push(Math::CAdd([i, j]));
                to_add.push(Math::CSub([i, j]));
                // to_add.push(Math::CMul([i, j]));
                // to_add.push(Math::CDiv([i, j]));
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.in_domain {
                continue;
            }
            
            to_add.push(Math::Conj(i));
            to_add.push(Math::CNeg(i));
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
            Math::CNeg(i) => {
                let mut op_id = synth.egraph.add(node);
                for (rei_id, imi_id) in component_ids(&synth.egraph, &i) {
                    let re_id = synth.egraph.add(Math::RNeg(rei_id));
                    let im_id = synth.egraph.add(Math::RNeg(imi_id));
                    let cx_id = synth.egraph.add(Math::Make([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }
                op_id
            },
            Math::Conj(i) => {
                let mut op_id = synth.egraph.add(node);
                for (rei_id, imi_id) in component_ids(&synth.egraph, &i) {
                    let im_id = synth.egraph.add(Math::RNeg(imi_id));
                    let cx_id = synth.egraph.add(Math::Make([rei_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }
                op_id
            },
            Math::CAdd([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let compi_ids = component_ids(&synth.egraph, &i);
                let compj_ids = component_ids(&synth.egraph, &j);
                for (&(rei_id, imi_id), &(rej_id, imj_id)) in compi_ids.iter().zip(compj_ids.iter()) {
                    let re_id = synth.egraph.add(Math::RAdd([rei_id, rej_id]));
                    let im_id = synth.egraph.add(Math::RAdd([imi_id, imj_id]));
                    let cx_id = synth.egraph.add(Math::Make([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }
                op_id
            },
            Math::CSub([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let compi_ids = component_ids(&synth.egraph, &i);
                let compj_ids = component_ids(&synth.egraph, &j);
                for (&(rei_id, imi_id), &(rej_id, imj_id)) in compi_ids.iter().zip(compj_ids.iter()) {
                    let re_id = synth.egraph.add(Math::RSub([rei_id, rej_id]));
                    let im_id = synth.egraph.add(Math::RSub([imi_id, imj_id]));
                    let cx_id = synth.egraph.add(Math::Make([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }
                op_id
            },
            // Math::CMul([i, j]) => {
            //     let mut op_id = synth.egraph.add(node);
            //     let compi_ids = component_ids(&synth.egraph, &i);
            //     let compj_ids = component_ids(&synth.egraph, &j);
            //     for (&(rei_id, imi_id), &(rej_id, imj_id)) in compi_ids.iter().zip(compj_ids.iter()) {
            //         let mult_rei_rej_id = synth.egraph.add(Math::RMul([rei_id, rej_id]));
            //         let mult_rei_imj_id = synth.egraph.add(Math::RMul([rei_id, imj_id]));
            //         let mult_imi_rej_id = synth.egraph.add(Math::RMul([imi_id, rej_id]));
            //         let mult_imi_imj_id = synth.egraph.add(Math::RMul([imi_id, imj_id]));
            //         let re_id = synth.egraph.add(Math::RSub([mult_rei_rej_id, mult_imi_imj_id]));
            //         let im_id = synth.egraph.add(Math::RAdd([mult_rei_imj_id, mult_imi_rej_id]));
            //         let cx_id = synth.egraph.add(Math::Make([re_id, im_id]));
            //         let (uid, _) = synth.egraph.union(op_id, cx_id);
            //         op_id = uid;
            //     }
            //     op_id
            // },
            // Math::CMul([i, j]) => {
            //     // let op_id = synth.egraph.add(node);
            //     // let rei_id = re_id(&synth.egraph, &i);  // (Re i)
            //     // let imi_id = im_id(&synth.egraph, &i);  // (Im i)
            //     // let rej_id = re_id(&synth.egraph, &j);  // (Re j)
            //     // let imj_id = im_id(&synth.egraph, &j);  // (Im j)
            //     // let mrirj_id = synth.egraph.add(Math::RMul([rei_id, rej_id]));      // (* (Re i) (Re j))
            //     // let miiij_id = synth.egraph.add(Math::RMul([imi_id, imj_id]));      // (* (Im i) (Im j))
            //     // let mriij_id = synth.egraph.add(Math::RMul([rei_id, imj_id]));      // (* (Re i) (Im j))
            //     // let miirj_id = synth.egraph.add(Math::RMul([imi_id, rej_id]));      // (* (Im i) (Re j))
            //     // let sre_id = synth.egraph.add(Math::RSub([mrirj_id, miiij_id]));
            //     // let aim_id = synth.egraph.add(Math::RAdd([mriij_id, miirj_id]));
            //     // let cx_id = synth.egraph.add(Math::Make([sre_id, aim_id]));
            //     // let (uid, _) = synth.egraph.union(op_id, cx_id);
            //     // uid
            // },
            // Math::CDiv([i, j]) => {
            //     let op_id = synth.egraph.add(node);
            //     let rei_id = re_id(&synth.egraph, &i);
            //     let imi_id = im_id(&synth.egraph, &i);
            //     let rej_id = re_id(&synth.egraph, &j);
            //     let imj_id = im_id(&synth.egraph, &j);
            //     let nre_id = synth.egraph.add(Math::RDiv([rei_id, rej_id]));
            //     let nim_id = synth.egraph.add(Math::RDiv([imi_id, imj_id]));
            //     let cx_id = synth.egraph.add(Math::Make([nre_id, nim_id]));
            //     let (uid, _) = synth.egraph.union(op_id, cx_id);
            //     uid
            // },
            Math::Var(_) => {
                synth.egraph.add(node)
            }
            Math::Complex(_) => {
                log::info!("complex constant");
                synth.egraph.add(node)
            }
            _ => {
                panic!("Not a complex node {:?}", node);
            }
        }
    }

    // Constant folding for complex numbers
    fn constant_fold(_egraph: &mut EGraph<Self, SynthAnalysis>, _id: Id) {

    }
}

/// Entry point.
fn main() {
    Math::main()
}