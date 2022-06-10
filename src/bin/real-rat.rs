/*!
    Real from rationals
!*/

use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::*;
use std::str::FromStr;

use num::bigint::BigInt;
use num::{rational::Ratio, Zero};

use egg::*;
use ruler::*;

/// define `Constant` as rationals
pub type Rational = Ratio<BigInt>;

// custom implementation of real value
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Real {
    val: Symbol,
}

impl Real {
    pub fn as_str(self) -> &'static str {
        self.val.as_str()
    }
}

impl<S: AsRef<str>> From<S> for Real {
    fn from(s: S) -> Self {
        let val = Symbol::from(s.as_ref());
        Real { val }
    }
}

impl From<Real> for &'static str {
    fn from(s: Real) -> Self {
        s.as_str()
    }
}

impl FromStr for Real {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.is_empty() && !s.starts_with('?') {
            Ok(s.into())
        } else {
            Err("not real")
        }
    }
}

impl Display for Real {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for Real {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

// custom implementation of a variable
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable(Symbol);

impl Variable {
    fn as_str(self) -> &'static str {
        self.0.as_str()
    }
}

impl<S: AsRef<str>> From<S> for Variable {
    fn from(s: S) -> Self {
        Variable(Symbol::from(s.as_ref()))
    }
}

impl From<Variable> for &'static str {
    fn from(s: Variable) -> Self {
        s.as_str()
    }
}

impl FromStr for Variable {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 1 && s.chars().next().unwrap().is_alphabetic() {
            Ok(s.into())
        } else if s.len() == 2 && s.starts_with('?') && s.chars().nth(1).unwrap().is_alphabetic() {
            Ok((&s[1..2]).into())
        } else {
            Err("not variable")
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

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
        Real(Real),
        Var(Variable),

        // conversions
        "Lim" = Lim(Id),
        "Seq" = Seq(Id),
    }
}

fn real_const_symbol(s: &str) -> Real {
    Real::from(s.to_owned() + "R")
}

fn allowed_nodes(nodes: &[Math]) -> bool {
    nodes.iter().any(Math::is_allowed)
}

fn is_real_str(s: &'static str) -> impl Fn(&mut EGraph<Math, SynthAnalysis>, Id, &Subst) -> bool {
    let var = s.parse().unwrap();
    move |egraph, _, subst| allowed_nodes(&egraph[subst[var]].nodes)
}

fn is_rational_zero(n: &Math) -> bool {
    match n {
        Math::Rat(v) => v.is_zero(),
        _ => false,
    }
}

fn is_real_zero(n: &Math) -> bool {
    match n {
        Math::Real(v) => *v == real_const_symbol("0"),
        _ => false,
    }
}

impl SynthLanguage for Math {
    type Constant = Rational; // not used

    // no evaluation needed
    fn eval<'a, F>(&'a self, _cvec_len: usize, mut _v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Math::Var(Variable(sym)) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Math::Var(Variable(sym))
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::Rat(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Math::Rat(c)
    }

    // override default behavior
    fn is_constant(&self) -> bool {
        matches!(self, Math::Real(_))
    }

    // override default behavior
    fn is_allowed(&self) -> bool {
        matches!(
            self,
            Math::RNeg(_)
                | Math::RAdd([_, _])
                | Math::RSub([_, _])
                | Math::RMul([_, _])
                | Math::RDiv([_, _])
                | Math::Var(_)
                | Math::Real(_)
                | Math::Lim(_)
        )
    }

    fn is_extractable(&self) -> bool {
        matches!(
            self,
            Math::RNeg(_)
                | Math::RAdd([_, _])
                | Math::RSub([_, _])
                | Math::RMul([_, _])
                | Math::RDiv([_, _])
                | Math::Var(_)
                | Math::Real(_)
        )
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        // disabled constants (TODO: validate input)
        let disabled_consts: Vec<&str> = if let Some(s) = &synth.params.disabled_consts {
            s.split(' ').collect()
        } else {
            vec![]
        };

        // this is for adding to the egraph, not used for cvec.
        let constants: Vec<(Rational, &str)> = ["1", "0", "-1"]
            .iter()
            .filter(|s| !disabled_consts.iter().any(|x| x.eq(*s)))
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
            egraph.add(Math::Var(Variable(var)));
        }

        for (c, s) in constants {
            let c_id = egraph.add(Math::Rat(c.clone()));
            let lim_id = egraph.add(Math::Lim(c_id));
            let seq_id = egraph.add(Math::Seq(lim_id));
            let re_id = egraph.add(Math::Real(real_const_symbol(s)));
            egraph.union(seq_id, c_id);
            egraph.union(lim_id, re_id);
        }

        synth.lifting_rewrites = vec![
            rewrite!("def-real"; "?a" => "(Lim (Seq ?a))" if is_real_str("?a")),
            rewrite!("def-neg"; "(Seq (~R ?a))" => "(~ (Seq ?a))"),
            rewrite!("def-add"; "(Seq (+R ?a ?b))" => "(+ (Seq ?a) (Seq ?b))"),
            rewrite!("def-sub"; "(Seq (-R ?a ?b))" => "(- (Seq ?a) (Seq ?b))"),
            rewrite!("def-mul"; "(Seq (*R ?a ?b))" => "(* (Seq ?a) (Seq ?b))"),
            rewrite!("def-div"; "(Seq (/R ?a ?b))" => "(/ (Seq ?a) (Seq ?b))"),
        ];

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let extract = Extractor::new(&synth.egraph, NumberOfAllowedOps);
        let mut to_add = vec![];

        // disabled operators from command line
        // (TODO: validate input)
        let disabled_ops: Vec<&str> = if let Some(s) = &synth.params.disabled_ops {
            s.split(' ').collect()
        } else {
            vec![]
        };

        // predicate if disabled
        let allowedp = |s| !disabled_ops.iter().any(|x| x.eq(&s));

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        for i in synth.ids() {
            for j in synth.ids() {
                if (ids[&i] + ids[&j] + 1 != iter)
                    || !allowed_nodes(&synth.egraph[i].nodes)
                    || !allowed_nodes(&synth.egraph[j].nodes)
                {
                    continue;
                }

                if iter > synth.params.no_constants_above_iter
                    && (synth.egraph[i].data.exact || synth.egraph[j].data.exact)
                {
                    continue;
                }

                if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                };

                if allowedp("+") {
                    to_add.push(Math::RAdd([i, j]));
                }
                if allowedp("-") {
                    to_add.push(Math::RSub([i, j]));
                }
                if allowedp("*") {
                    to_add.push(Math::RMul([i, j]));
                }
                if allowedp("/") && !synth.egraph[j].nodes.iter().any(is_real_zero) {
                    to_add.push(Math::RDiv([i, j]));
                }
            }

            if ids[&i] + 1 != iter
                || synth.egraph[i].data.exact
                || !allowed_nodes(&synth.egraph[i].nodes)
            {
                continue;
            }

            if allowedp("~") {
                to_add.push(Math::RNeg(i));
            }
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn validate(
        _synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        let valid_pattern = |pat: &Pattern<Self>| {
            pat.ast.as_ref().iter().all(|n| match n {
                ENodeOrVar::ENode(Math::Div([_, j])) => match pat.ast.index(*j) {
                    ENodeOrVar::Var(_) => true,
                    ENodeOrVar::ENode(n) => !is_rational_zero(n),
                },
                ENodeOrVar::ENode(Math::RDiv([_, j])) => match pat.ast.index(*j) {
                    ENodeOrVar::Var(_) => true,
                    ENodeOrVar::ENode(n) => !is_real_zero(n),
                },
                _ => true,
            })
        };

        ValidationResult::from(valid_pattern(lhs) && valid_pattern(rhs))
    }

    fn is_valid_rewrite(
        egraph: &EGraph<Self, SynthAnalysis>,
        rhs: &Pattern<Self>,
        subst: &Subst,
    ) -> bool {
        rhs.ast.as_ref().iter().all(|n| match n {
            ENodeOrVar::ENode(Math::Div([_, j])) => match rhs.ast.index(*j) {
                ENodeOrVar::Var(v) => !egraph[subst[*v]].iter().any(is_rational_zero),
                ENodeOrVar::ENode(n) => !is_rational_zero(n),
            },
            ENodeOrVar::ENode(Math::RDiv([_, j])) => match rhs.ast.index(*j) {
                ENodeOrVar::Var(v) => !egraph[subst[*v]].iter().any(is_real_zero),
                ENodeOrVar::ENode(n) => !is_real_zero(n),
            },
            _ => true,
        })
    }

    // custom constant folder
    fn constant_fold(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if !allowed_nodes(&egraph[id].nodes) {
            // lower domain
            if egraph[id].nodes.iter().any(|x| matches!(x, Math::Rat(_))) {
                // early exit if constant exists
                return;
            }

            let mut to_add: Option<(Self, String)> = None;
            for x in &egraph[id].nodes {
                match x {
                    Math::Neg(i) => constant_fold!(i, egraph, to_add, -),
                    Math::Add([i, j]) => constant_fold!(i, j, egraph, to_add, +),
                    Math::Sub([i, j]) => constant_fold!(i, j, egraph, to_add, -),
                    Math::Mul([i, j]) => constant_fold!(i, j, egraph, to_add, *),
                    Math::Div([i, j]) => {
                        // explicit because of zero check
                        for n in &egraph[*i].nodes {
                            if let Math::Rat(v) = n {
                                for n in &egraph[*j].nodes {
                                    if let Math::Rat(w) = n {
                                        if !w.is_zero() {
                                            let r = v / w;
                                            let s = r.to_string();
                                            to_add = Some((Math::Rat(r), s));
                                            break;
                                        }
                                    }
                                }
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
                        if let Math::Lim(i) = n {
                            if *i == id {
                                to_update.push(*i);
                            }
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
            let lim_ids: Vec<Id> = egraph[id]
                .nodes
                .iter()
                .filter_map(|n| match n {
                    Math::Lim(v) => Some(*v),
                    _ => None,
                })
                .collect();

            for id in lim_ids {
                Self::constant_fold(egraph, id);
            }
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
