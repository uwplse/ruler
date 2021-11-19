/*!
    Complex from Reals
!*/

use num::bigint::{BigInt};
use num::rational::{Ratio, ParseRatioError};
use num::Zero;

use egg::*;
use ruler::*;

pub type Rational = Ratio<BigInt>;

define_language! {
    pub enum Math {
        // real domain
        "~R" = RNeg(Id),
        "+R" = RAdd([Id; 2]),
        "-R" = RSub([Id; 2]),
        "*R" = RMul([Id; 2]),
        "/R" = RDiv([Id; 2]),
        Real(egg::Symbol),     // TODO: this is dumb

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
        "Cart" = Cart([Id; 2]),
        "Polar" = Polar([Id; 2]),
        "Re" = Re(Id),
        "Im" = Im(Id),
        "abs" = Abs(Id),
        "Arg" = Arg(Id),
    }
}

// Match for folding real constants
macro_rules! fold_real {
    ($i:ident, $egraph:ident, $to_add:ident, $op:tt) => {{   // unary
        let n = $egraph[*$i].nodes.iter()
            .find_map(|x| match x {
                Math::Real(v) => Some(v),
                _ => None,
            });
        if let Some(v) = n {
            let r = $op(v);
            if let Some(v) = r {
                $to_add = Some(Math::Real(v));
            } else {
                $to_add = None;
            }
            break;
        }
    }};
    ($i:ident, $j:ident, $egraph:ident, $to_add:ident, $op:tt) => {{     // binary
        let ns = $egraph[*$i].nodes.iter()
            .zip($egraph[*$j].nodes.iter())
            .find_map(|x| match x {
                (Math::Real(v), Math::Real(w)) => Some((v, w)),
                _ => None,
            });
        if let Some((v, w)) = ns {
            let r = $op(v, w);
            if let Some(v) = r {
                $to_add = Some(Math::Real(v));
            } else {
                $to_add = None;
            }
            break;
        }
    }};
}

fn real_const_symbol(s: &str) -> egg::Symbol {
    egg::Symbol::from(s.to_owned() + "R")
}

fn complex_const_symbol(s: &str) -> egg::Symbol {
    egg::Symbol::from(s.to_owned() + "C")
}

fn fold_real_neg(v: &egg::Symbol) -> Option<egg::Symbol> {
    let n: Result<Rational, ParseRatioError> = v.as_str().parse();
    match n {
        Ok(x) => Some(real_const_symbol(&(-x).to_string())),
        _ => None,
    }
}

fn fold_real_add(v1: &egg::Symbol, v2: &egg::Symbol) -> Option<egg::Symbol> {
    let n1: Result<Rational, ParseRatioError> = v1.as_str().parse();
    let n2: Result<Rational, ParseRatioError> = v2.as_str().parse();
    match (n1, n2) {
        (Ok(x), Ok(y)) => Some(real_const_symbol(&(x + y).to_string())),
        _ => None,
    }
}

fn fold_real_sub(v1: &egg::Symbol, v2: &egg::Symbol) -> Option<egg::Symbol> {
    let n1: Result<Rational, ParseRatioError> = v1.as_str().parse();
    let n2: Result<Rational, ParseRatioError> = v2.as_str().parse();
    match (n1, n2) {
        (Ok(x), Ok(y)) => Some(real_const_symbol(&(x - y).to_string())),
        _ => None,
    }
}

fn fold_real_mul(v1: &egg::Symbol, v2: &egg::Symbol) -> Option<egg::Symbol> {
    let n1: Result<Rational, ParseRatioError> = v1.as_str().parse();
    let n2: Result<Rational, ParseRatioError> = v2.as_str().parse();
    match (n1, n2) {
        (Ok(x), Ok(y)) => Some(real_const_symbol(&(x * y).to_string())),
        _ => None,
    }
}

fn fold_real_div(v1: &egg::Symbol, v2: &egg::Symbol) -> Option<egg::Symbol> {
    let n1: Result<Rational, ParseRatioError> = v1.as_str().parse();
    let n2: Result<Rational, ParseRatioError> = v2.as_str().parse();
    match (n1, n2) {
        (Ok(x), Ok(y)) => if y.is_zero() {
            None
        } else {
            Some(real_const_symbol(&(x / y).to_string()))
        },
        _ => None,
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
fn cartesian_ids(egraph: &EGraph<Math, SynthAnalysis>, id: &Id) -> Vec<(Id, Id)> {
    egraph[*id].nodes
        .iter()
        .filter_map(|x| {
            match x {
                Math::Cart([i, j]) => Some((*i, *j)),
                _ => None,
            }
        })
        .collect()
}

// returns a vector of (id, id) corresponding to the modulus
// and argument of numbers associated with the eclass
fn polar_ids(egraph: &EGraph<Math, SynthAnalysis>, id: &Id) -> Vec<(Id, Id)> {
    egraph[*id].nodes
        .iter()
        .filter_map(|x| {
            match x {
                Math::Polar([i, j]) => Some((*i, *j)),
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
            // Math::RNeg(_) => true,
            // Math::RAdd([_, _]) => true,
            // Math::RMul([_, _]) => true,

            Math::CNeg(_) => true,
            Math::CAdd([_, _]) => true,
            Math::CSub([_, _]) => true,
            Math::CMul([_, _]) => true,
            Math::CDiv([_, _]) => true,
            Math::Conj(_) => true,
            Math::Var(_) => true,
            Math::Complex(_) => true,

            // Math::Re(_) => true,
            // Math::Im(_) => true,
            // Math::Abs(_) => true,
            // Math::Arg(_) => true,

            Math::Cart([_, _]) => true,
            Math::Polar([_, _]) => true,

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
            rule_lifting: true,
        });

        for i in 0..synth.params.variables {
            let var = egg::Symbol::from(letter(i));
            let var_id = egraph.add(Math::Var(var));
            // real
            let re_id = egraph.add(Math::Re(var_id));
            let im_id = egraph.add(Math::Im(var_id));

            // complex
            let abs_id = egraph.add(Math::Abs(var_id));
            let arg_id = egraph.add(Math::Arg(var_id));
            let cx_id = egraph.add(Math::Cart([re_id, im_id]));
            let px_id = egraph.add(Math::Polar([abs_id, arg_id]));

            let (var_id, _) = egraph.union(var_id, cx_id);
            egraph.union(var_id, px_id);
        }

        // symbolic constants
        let zero = real_const_symbol("0");
        let one = real_const_symbol("1");
        let pi_2 = real_const_symbol("pi/2");
        let pi = real_const_symbol("pi");

        let complex_zero = complex_const_symbol("0");
        let complex_one = complex_const_symbol("1");
        let complex_i = complex_const_symbol("i");

        // rational constants
        let re_zero = egraph.add(Math::Real(zero));
        let re_one = egraph.add(Math::Real(one));
        let re_pi_2 = egraph.add(Math::Real(pi_2));
        let re_pi = egraph.add(Math::Real(pi));

        let pi_2_pi_2 = egraph.add(Math::RAdd([re_pi_2, re_pi_2]));
        egraph.union(re_pi, pi_2_pi_2);

        // zero constant
        let cx_zero = egraph.add(Math::Complex(complex_zero));
        let zero_mk = egraph.add(Math::Cart([re_zero, re_zero]));
        let zero_mk_re = egraph.add(Math::Re(cx_zero));
        let zero_mk_im = egraph.add(Math::Im(cx_zero));
        let zero_mk_abs = egraph.add(Math::Abs(cx_zero));
        let zero_mk_arg = egraph.add(Math::Arg(cx_zero));
        egraph.union(cx_zero, zero_mk);
        egraph.union(zero_mk_re, re_zero);
        egraph.union(zero_mk_im, re_zero);
        egraph.union(zero_mk_abs, re_zero);
        egraph.union(zero_mk_arg, re_zero);

        // one constant
        let cx_one = egraph.add(Math::Complex(complex_one));
        let one_mk = egraph.add(Math::Cart([re_one, re_zero]));
        let one_mk_re = egraph.add(Math::Re(cx_one));
        let one_mk_im = egraph.add(Math::Im(cx_one));
        let one_mk_abs = egraph.add(Math::Abs(cx_one));
        let one_mk_arg = egraph.add(Math::Arg(cx_one));
        egraph.union(cx_one, one_mk);
        egraph.union(one_mk_re, re_one);
        egraph.union(one_mk_im, re_zero);
        egraph.union(one_mk_abs, re_one);
        egraph.union(one_mk_arg, re_zero);

        // i constant
        let cx_i = egraph.add(Math::Complex(complex_i));
        let i_mk = egraph.add(Math::Cart([re_zero, re_one]));
        let i_mk_re = egraph.add(Math::Re(cx_i));
        let i_mk_im = egraph.add(Math::Im(cx_i));
        let i_mk_abs = egraph.add(Math::Abs(cx_i));
        let i_mk_arg = egraph.add(Math::Arg(cx_i));
        egraph.union(cx_i, i_mk);
        egraph.union(i_mk_re, re_zero);
        egraph.union(i_mk_im, re_one);
        egraph.union(i_mk_abs, re_one);
        egraph.union(i_mk_arg, re_pi_2);

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

                to_add.push(Math::CAdd([i, j]));
                to_add.push(Math::CSub([i, j]));
                to_add.push(Math::CMul([i, j]));
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
            // (~C x) ~ (Cart (~R (Re x)) (~R (Im x))
            //
            //          don't do this just yet
            //        ~ (Polar (Abs x) (+ (Arg x) pi))
            Math::CNeg(i) => {
                let mut op_id = synth.egraph.add(node);
                let recx_id = synth.egraph.add(Math::Re(op_id));
                let imcx_id = synth.egraph.add(Math::Im(op_id));
                // let abscx_id = synth.egraph.add(Math::Abs(op_id));
                // let argcx_id = synth.egraph.add(Math::Arg(op_id));

                // cartesian
                for (rei_id, imi_id) in cartesian_ids(&synth.egraph, &i) {
                    // real operators
                    let re_id = synth.egraph.add(Math::RNeg(rei_id));
                    let im_id = synth.egraph.add(Math::RNeg(imi_id));
                    let (re_id, _) = synth.egraph.union(re_id, recx_id);
                    let (im_id, _) = synth.egraph.union(im_id, imcx_id);
                    
                    // complex opertors
                    let cx_id = synth.egraph.add(Math::Cart([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }
                
                // // polar
                // for (absi_id, argi_id) in polar_ids(&synth.egraph, &i) {
                //     // real operators
                //     let re_pi = synth.egraph.add(Math::Real(Symbol::from("pi")));
                //     let arg_id = synth.egraph.add(Math::RAdd([argi_id, re_pi]));
                //     let (abs_id, _) = synth.egraph.union(absi_id, abscx_id);
                //     let (arg_id, _) = synth.egraph.union(arg_id, argcx_id);
                    
                //     // complex opertors
                //     let cx_id = synth.egraph.add(Math::Polar([abs_id, arg_id]));
                //     let (uid, _) = synth.egraph.union(op_id, cx_id);
                //     op_id = uid;
                // }
                op_id
            },
            // (conj x) ~ (Cart (Re x)) (~R (Im x))
            //          ~ (Polar (Abs x) (~R (Arg x)))
            Math::Conj(i) => {
                let mut op_id = synth.egraph.add(node);
                let recx_id = synth.egraph.add(Math::Re(op_id));
                let imcx_id = synth.egraph.add(Math::Im(op_id));
                let abscx_id = synth.egraph.add(Math::Abs(op_id));
                let argcx_id = synth.egraph.add(Math::Arg(op_id));

                // cartesian
                for (rei_id, imi_id) in cartesian_ids(&synth.egraph, &i) {
                    let im_id = synth.egraph.add(Math::RNeg(imi_id));
                    let (re_id, _) = synth.egraph.union(rei_id, recx_id);
                    let (im_id, _) = synth.egraph.union(im_id, imcx_id);
                    let cx_id = synth.egraph.add(Math::Cart([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }

                // polar
                for (absi_id, argi_id) in polar_ids(&synth.egraph, &i) {
                    // real operators
                    let arg_id = synth.egraph.add(Math::RNeg(argi_id));
                    let (abs_id, _) = synth.egraph.union(absi_id, abscx_id);
                    let (arg_id, _) = synth.egraph.union(arg_id, argcx_id);
                    
                    // complex opertors
                    let cx_id = synth.egraph.add(Math::Polar([abs_id, arg_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }

                op_id
            },
            // (+C x y) ~ (Cart (+R (Re x) (Re y)) (+R (Im x) (Im y)))
            //          ~ (Polar (Abs (+C x y)) (Arg (+C x y)))
            Math::CAdd([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let recx_id = synth.egraph.add(Math::Re(op_id));
                let imcx_id = synth.egraph.add(Math::Im(op_id));
                let compi_ids = cartesian_ids(&synth.egraph, &i);
                let compj_ids = cartesian_ids(&synth.egraph, &j);

                // cartesian
                for (&(rei_id, imi_id), &(rej_id, imj_id)) in compi_ids.iter().zip(compj_ids.iter()) {
                    // real operators
                    let re_id = synth.egraph.add(Math::RAdd([rei_id, rej_id]));
                    let im_id = synth.egraph.add(Math::RAdd([imi_id, imj_id]));
                    let (re_id, _) = synth.egraph.union(re_id, recx_id);
                    let (im_id, _) = synth.egraph.union(im_id, imcx_id);
                    
                    // complex operators
                    let cx_id = synth.egraph.add(Math::Cart([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }
                
                // polar
                let abs_id = synth.egraph.add(Math::Abs(op_id));
                let arg_id = synth.egraph.add(Math::Arg(op_id));
                let pr_id = synth.egraph.add(Math::Polar([abs_id, arg_id]));
                let (op_id, _) = synth.egraph.union(op_id, pr_id);

                op_id
            },
            // (-C x y) ~ (Cart (-R (Re x) (Re y)) (-R (Im x) (Im y))
            //          ~ (Polar (Abs (-C x y)) (Arg (-C x y)))
            Math::CSub([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let recx_id = synth.egraph.add(Math::Re(op_id));
                let imcx_id = synth.egraph.add(Math::Im(op_id));
                let compi_ids = cartesian_ids(&synth.egraph, &i);
                let compj_ids = cartesian_ids(&synth.egraph, &j);

                // cartesian
                for (&(rei_id, imi_id), &(rej_id, imj_id)) in compi_ids.iter().zip(compj_ids.iter()) {
                    // real operators
                    let re_id = synth.egraph.add(Math::RSub([rei_id, rej_id]));
                    let im_id = synth.egraph.add(Math::RSub([imi_id, imj_id]));
                    let (re_id, _) = synth.egraph.union(re_id, recx_id);
                    let (im_id, _) = synth.egraph.union(im_id, imcx_id);
                    
                    // complex operators
                    let cx_id = synth.egraph.add(Math::Cart([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }

                // polar
                let abs_id = synth.egraph.add(Math::Abs(op_id));
                let arg_id = synth.egraph.add(Math::Arg(op_id));
                let pr_id = synth.egraph.add(Math::Polar([abs_id, arg_id]));
                let (op_id, _) = synth.egraph.union(op_id, pr_id);

                op_id
            },
            // (*C x y) ~ (Cart (-R (*R (Re x) (Re y)) (*R (Im x) (Im y)))
            //                  (+R (*R (Re x) (Im y)) (*R (Im x) (Re y))))
            //          ~ (Polar (*R (Abs x) (Abs y)) (+R (Arg x) (Arg y)))
            Math::CMul([i, j]) => {
                let mut op_id = synth.egraph.add(node);
                let recx_id = synth.egraph.add(Math::Re(op_id));
                let imcx_id = synth.egraph.add(Math::Im(op_id));
                let abscx_id = synth.egraph.add(Math::Abs(op_id));
                let argcx_id = synth.egraph.add(Math::Arg(op_id));

                // cartesian
                let compi_ids = cartesian_ids(&synth.egraph, &i);
                let compj_ids = cartesian_ids(&synth.egraph, &j);
                for (&(rei_id, imi_id), &(rej_id, imj_id)) in compi_ids.iter().zip(compj_ids.iter()) {
                    // complex operators
                    let mult_rei_rej_id = synth.egraph.add(Math::RMul([rei_id, rej_id]));
                    let mult_rei_imj_id = synth.egraph.add(Math::RMul([rei_id, imj_id]));
                    let mult_imi_rej_id = synth.egraph.add(Math::RMul([imi_id, rej_id]));
                    let mult_imi_imj_id = synth.egraph.add(Math::RMul([imi_id, imj_id]));
                    let re_id = synth.egraph.add(Math::RSub([mult_rei_rej_id, mult_imi_imj_id]));
                    let im_id = synth.egraph.add(Math::RAdd([mult_rei_imj_id, mult_imi_rej_id]));
                    let (re_id, _) = synth.egraph.union(re_id, recx_id);
                    let (im_id, _) = synth.egraph.union(im_id, imcx_id);

                    // complex operators
                    let cx_id = synth.egraph.add(Math::Cart([re_id, im_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }

                // polar
                let compi_ids = polar_ids(&synth.egraph, &i);
                let compj_ids = polar_ids(&synth.egraph, &j);
                for (&(absi_id, argi_id), &(absj_id, argj_id)) in compi_ids.iter().zip(compj_ids.iter()) {
                    // real
                    let abs_id = synth.egraph.add(Math::RMul([absi_id, absj_id]));
                    let arg_id = synth.egraph.add(Math::RAdd([argi_id, argj_id]));
                    let (abs_id, _) = synth.egraph.union(abs_id, abscx_id);
                    let (arg_id, _) = synth.egraph.union(arg_id, argcx_id);
                    
                    // complex opertors
                    let cx_id = synth.egraph.add(Math::Polar([abs_id, arg_id]));
                    let (uid, _) = synth.egraph.union(op_id, cx_id);
                    op_id = uid;
                }

                op_id
            },
            // Math::CDiv([i, j]) => {
            //     let op_id = synth.egraph.add(node);
            //     let rei_id = re_id(&synth.egraph, &i);
            //     let imi_id = im_id(&synth.egraph, &i);
            //     let rej_id = re_id(&synth.egraph, &j);
            //     let imj_id = im_id(&synth.egraph, &j);
            //     let nre_id = synth.egraph.add(Math::RDiv([rei_id, rej_id]));
            //     let nim_id = synth.egraph.add(Math::RDiv([imi_id, imj_id]));
            //     let cx_id = synth.egraph.add(Math::Cart([nre_id, nim_id]));
            //     let (uid, _) = synth.egraph.union(op_id, cx_id);
            //     uid
            // },
            Math::Var(_) => {
                synth.egraph.add(node)
            }
            Math::Complex(_) => {
                synth.egraph.add(node)
            }
            _ => {
                panic!("Not a complex node {:?}", node);
            }
        }
    }

    // Constant folding for complex numbers
    fn constant_fold(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if !egraph[id].data.in_domain { // lower domain
            if egraph[id].nodes.iter().any(|x| match x {
                    Math::Real(_) => true,
                    _ => false,
                }) {        // early exit if constant exists
                return;
            }

            let mut to_add: Option<Self> = None;
            for x in &egraph[id].nodes {
                match x {
                    Math::RNeg(i) => fold_real!(i, egraph, to_add, fold_real_neg),
                    Math::RAdd([i, j]) => fold_real!(i, j, egraph, to_add, fold_real_add),
                    Math::RSub([i, j]) => fold_real!(i, j, egraph, to_add, fold_real_sub),
                    Math::RMul([i, j]) => fold_real!(i, j, egraph, to_add, fold_real_mul),
                    Math::RDiv([i, j]) => fold_real!(i, j, egraph, to_add, fold_real_div),
                    _ => (),
                }
            }

            if let Some(n) = to_add {
                let r_id = egraph.add(n);
                egraph.union(r_id, id);
            }
        } else {
            if egraph[id].nodes.iter().any(|x| match x {
                    Math::Complex(_) => true,
                    _ => false,
                }) {        // early exit if constant exists
                return;
            }
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
