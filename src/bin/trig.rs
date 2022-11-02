/*!
    Trigonometry from complex
!*/

use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::*;
use std::str::FromStr;

use num::{
    bigint::BigInt,
    rational::{ParseRatioError, Ratio},
    {Signed, Zero},
};

use egg::*;
use ruler::*;

pub type Rational = Ratio<BigInt>;

// custom implementation of real value
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Real(Symbol);

impl Real {
    pub fn as_str(self) -> &'static str {
        self.0.as_str()
    }
}

impl<S: AsRef<str>> From<S> for Real {
    fn from(s: S) -> Self {
        Real(Symbol::from(s.as_ref()))
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
        if !s.is_empty() && s.chars().all(|c| c.is_numeric() || c == '-' || c == '/') {
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

// custom implementation of a complex value
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

fn is_zero(n: &Math) -> bool {
    match n {
        Math::RealConst(v) => *v == Real::from("0"),
        _ => false,
    }
}

fn real_to_rational(r: &Real) -> Result<Rational, ParseRatioError> {
    r.as_str().parse()
}

fn extract_constant(nodes: &[Math]) -> Option<Real> {
    for n in nodes {
        if let Math::RealConst(v) = n {
            return Some(*v);
        }
    }

    None
}

fn trig_node(ns: &[Math], egraph: &EGraph<Math, SynthAnalysis>) -> bool {
    ns.iter().any(|n| match n {
        Math::Sin(_) | Math::Cos(_) | Math::Tan(_) | Math::Csc(_) | Math::Sec(_) | Math::Cot(_) => {
            true
        }
        Math::Sqr(i) => trig_node(&egraph[*i].nodes, egraph),
        _ => false,
    })
}

define_language! {
    pub enum Math {
        // trig operators
        "sin" = Sin(Id),
        "cos" = Cos(Id),
        "tan" = Tan(Id),
        "csc" = Csc(Id),
        "sec" = Sec(Id),
        "cot" = Cot(Id),

        // complex exponetial
        "cis" = Cis(Id),

        // arithmetic operators
        "~" = Neg(Id),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "sqr" = Sqr(Id),

        // constants
        "I" = Imag,
        "PI" = Pi,
        RealConst(Real),
        Var(Variable),
    }
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

impl SynthLanguage for Math {
    type Constant = Real;
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        Type::Top
    }

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
        Math::Var(Variable::from(sym.as_str()))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Math::RealConst(c)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Math::RealConst(_))
    }

    fn is_allowed(&self) -> bool {
        matches!(
            self,
            Math::Sin(_)
                | Math::Cos(_)
                | Math::Tan(_)
                | Math::Csc(_)
                | Math::Sec(_)
                | Math::Cot(_)
                | Math::Pi
        )
    }

    fn is_extractable(&self) -> bool {
        !matches!(self, Math::Imag | Math::Cis(_),)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let egraph = EGraph::new(SynthAnalysis {
            cvec_len: 0,
            constant_fold: if synth.params.no_constant_fold {
                ConstantFoldMethod::NoFold
            } else {
                ConstantFoldMethod::Lang
            },
            rule_lifting: true,
        });

        // rewrites and extra rewrites
        synth.lifting_rewrites = vec![
            // definition of sine, cosine, tangent
            rewrite!("def-sin"; "(sin ?a)" <=> "(/ (- (cis ?a) (cis (~ ?a))) (* 2 I))"),
            rewrite!("def-cos"; "(cos ?a)" <=> "(/ (+ (cis ?a) (cis (~ ?a))) 2)"),
            rewrite!("def-tan"; "(tan ?a)" <=> "(* I (/ (- (cis (~ ?a)) (cis ?a)) (+ (cis (~ ?a)) (cis ?a))))"),

            // definition of cosecant, secant, cotangent
            rewrite!("def-csc"; "(csc ?a)" <=> "(/ 1 (sin ?a))"),
            rewrite!("def-sec"; "(sec ?a)" <=> "(/ 1 (cos ?a))"),
            rewrite!("def-cot"; "(cot ?a)" <=> "(/ 1 (tan ?a))"),

            // relating tangent to sine and cosine
            rewrite!("def-tan-ratio"; "(tan ?a)" <=> "(/ (sin ?a) (cos ?a))"),

            // // definition of cos(a) * cos(b)
            // rewrite!("def-prod-cos"; "(* (cos ?a) (cos ?b))" <=>
            //          "(/ (+ (* (cis ?a) (cis ?b))                   \
            //                 (+ (* (cis ?a) (cis (~ ?b)))            \
            //                    (+ (* (cis (~ ?a)) (cis ?b))         \
            //                       (* (cis (~ ?a)) (cis (~ ?b))))))  \
            //              4)"),
            // // definition of sin(a) * sin(b)
            // rewrite!("def-prod-sin"; "(* (sin ?a) (sin ?b))" <=>
            //          "(~ (/ (+ (* (cis ?a) (cis ?b))                \
            //                 (+ (~ (* (cis ?a) (cis (~ ?b))))        \
            //                    (+ (~ (* (cis (~ ?a)) (cis ?b)))     \
            //                       (* (cis (~ ?a)) (cis (~ ?b))))))  \
            //                 4))"),
            // // definition of sin(a) * cos(b)
            // rewrite!("def-prod-sin-cos"; "(* (sin ?a) (cos ?b))" <=>
            //                 "(/ (+ (* (cis ?a) (cis ?b))                        \
            //                        (+ (* (cis ?a) (cis (~ ?b)))                 \
            //                           (+ (~ (* (cis (~ ?a)) (cis ?b)))          \
            //                              (~ (* (cis (~ ?a)) (cis (~ ?b)))))))   \
            //                     (* 4 I))"),

            // // definition of cos(a) * cos(b)
            // rewrite!("def-prod-cos"; "(* (cos ?a) (cos ?b))" <=>
            //          "(/ (+ (* (cis ?a) (cis ?b)) (+ (* (cis (~ ?a)) (cis (~ ?b))) \
            //             (+ (* (cis ?a) (cis (~ ?b))) (* (cis (~ ?a)) (cis ?b))))) 4)"),
            // // definition of sin(a) * sin(b)
            // rewrite!("def-prod-sin"; "(* (sin ?a) (sin ?b))" <=>
            //          "(~ (/ (+ (* (cis ?a) (cis ?b)) (- (* (cis (~ ?a)) (cis (~ ?b))) \
            //             (+ (* (cis ?a) (cis (~ ?b))) (* (cis (~ ?a)) (cis ?b))))) 4))"),
            // // definition of cos(a) * sin(b)
            // rewrite!("def-prod-cos-sin"; "(* (cos ?a) (sin ?b))" <=>
            //          "(/ (+ (* (cis ?a) (cis ?b)) (- (* (cis (~ ?a)) (cis b)) \
            //             (+ (* (cis (~ ?a)) (cis (~ ?b))) (* (cis ?a) (cis (~ ?b)))))) (* 4 I))"),

            // definition of cos^2(a) and sin^2(a)
            rewrite!("def-cos-sq"; "(* (cos ?a) (cos ?a))" <=>
                     "(/ (+ (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4)"),
            rewrite!("def-sin-sq"; "(* (sin ?a) (sin ?a))" <=>
                     "(~ (/ (- (+ (sqr (cis ?a)) (sqr (cis (~ ?a)))) 2) 4))"),

            // definition of square
            rewrite!("def-sqr"; "(sqr ?a)" <=> "(* ?a ?a)"),
            vec![
                // constant folding for PI
                rewrite!("add-pi"; "(+ PI PI)" => "(* 2 PI)"),

                // constant folding for cis
                rewrite!("cis-0"; "(cis 0)" => "1"),
                rewrite!("cis-pi"; "(cis (/ PI 2))" => "I"),

                // cis identities
                // rewrite!("inv-cis"; "(cis (~ ?a))" => "(/ 1 (cis ?a))"),
                rewrite!("add-cis"; "(cis (+ ?a ?b))" => "(* (cis ?a) (cis ?b))"),
                rewrite!("sub-cis"; "(cis (- ?a ?b))" => "(* (cis ?a) (cis (~ ?b)))"),
                rewrite!("cancel-cis"; "(* (cis ?a) (cis (~ ?a)))" => "1"),

                // definition of cis
                rewrite!("square-i"; "(* I I)" => "-1"),
            ],
        ].concat();

        let extra_rewrites = vec![
            // reverse of cis identities above
            ("(* (cis ?a) (cis ?b))", "(cis (+ ?a ?b))", false),
            ("(* (cis ?a) (cis (~ ?b)))", "(cis (- ?a ?b))", false),
        ];

        for (l, r, bi) in extra_rewrites {
            let lrec: RecExpr<Math> = l.parse().unwrap();
            let rrec: RecExpr<Math> = r.parse().unwrap();
            if let Some(mut e) = Equality::new(&lrec, &rrec) {
                if !bi && e.rewrites.len() == 2 {
                    e.rewrites.pop();
                }

                synth.old_eqs.insert(e.name.clone(), e.clone());
            }
        }
        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        // workload
        if synth.params.workload.is_some() {
            return vec![];
        }

        // disabled iters
        if iter == 4 || iter == 6 {
            return vec![];
        }

        // disabled operators (TODO: validate input)
        let disabled_ops: Vec<&str> = if let Some(s) = &synth.params.disabled_ops {
            s.split(' ').collect()
        } else {
            vec![]
        };

        // predicate if disabled
        let allowedp = |s: &str| !disabled_ops.iter().any(|x| x.eq(&s));

        // predicate for enumerating arithmetic ops
        let arithmetic = |ns: &Vec<Self>| {
            ns.iter().all(|n| {
                matches!(
                    n,
                    Math::Neg(_)
                        | Math::Add(_)
                        | Math::Sub(_)
                        | Math::Mul(_)
                        | Math::Div(_)
                        | Math::Sqr(_)
                        | Math::RealConst(_)
                        | Math::Var(_)
                ) && !matches!(n, Math::Imag)
            })
        };

        // maps ids to n_ops
        let extract = Extractor::new(&synth.egraph, NumberOfOps);
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

                if iter > synth.params.no_constants_above_iter {
                    if synth.egraph[i].data.exact || synth.egraph[j].data.exact {
                        continue;
                    }
                } else if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                };

                if iter <= 3 {
                    // either both are trig or neither are trig
                    if trig_node(&synth.egraph[i].nodes, &synth.egraph)
                        == trig_node(&synth.egraph[j].nodes, &synth.egraph)
                    {
                        if allowedp("+") {
                            to_add.push(Math::Add([i, j]));
                        }

                        if allowedp("-") {
                            to_add.push(Math::Sub([i, j]));
                        }

                        if allowedp("*") {
                            to_add.push(Math::Mul([i, j]));
                        }
                    }
                } else if trig_node(&synth.egraph[i].nodes, &synth.egraph)
                    && trig_node(&synth.egraph[j].nodes, &synth.egraph)
                {
                    if allowedp("+") {
                        to_add.push(Math::Add([i, j]));
                    }

                    if allowedp("-") {
                        to_add.push(Math::Sub([i, j]));
                    }
                }
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact {
                continue;
            }

            if iter <= 2 && arithmetic(&synth.egraph[i].nodes) {
                if allowedp("sin") {
                    to_add.push(Math::Sin(i));
                }
                if allowedp("cos") {
                    to_add.push(Math::Cos(i));
                }
                if allowedp("tan") {
                    to_add.push(Math::Tan(i));
                }
            }

            if iter == 2 {
                if allowedp("~") && trig_node(&synth.egraph[i].nodes, &synth.egraph) {
                    to_add.push(Math::Neg(i));
                }

                if allowedp("sqr") && trig_node(&synth.egraph[i].nodes, &synth.egraph) {
                    to_add.push(Math::Sqr(i));
                }
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
                    ENodeOrVar::ENode(n) => !is_zero(n),
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
                ENodeOrVar::Var(v) => !egraph[subst[*v]].iter().any(is_zero),
                ENodeOrVar::ENode(n) => !is_zero(n),
            },
            _ => true,
        })
    }

    // custom constant folder
    fn constant_fold(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if egraph[id]
            .nodes
            .iter()
            .any(|x| matches!(x, Math::RealConst(_)))
        {
            return;
        }

        let mut to_add: Option<Math> = None;
        for n in &egraph[id].nodes {
            match n {
                Math::Neg(i) => {
                    if let Some(v) = extract_constant(&egraph[*i].nodes) {
                        if let Ok(x) = real_to_rational(&v) {
                            let r = Real::from((-x).to_string());
                            to_add = Some(Self::mk_constant(r, egraph));
                            break;
                        }
                    }
                }
                Math::Add([i, j]) => {
                    if let Some(v) = extract_constant(&egraph[*i].nodes) {
                        if let Some(w) = extract_constant(&egraph[*j].nodes) {
                            if let Ok(x) = real_to_rational(&v) {
                                if let Ok(y) = real_to_rational(&w) {
                                    let r = Real::from((x + y).to_string());
                                    to_add = Some(Self::mk_constant(r, egraph));
                                    break;
                                }
                            }
                        }
                    }
                }
                Math::Sub([i, j]) => {
                    if let Some(v) = extract_constant(&egraph[*i].nodes) {
                        if let Some(w) = extract_constant(&egraph[*j].nodes) {
                            if let Ok(x) = real_to_rational(&v) {
                                if let Ok(y) = real_to_rational(&w) {
                                    let r = Real::from((x - y).to_string());
                                    to_add = Some(Self::mk_constant(r, egraph));
                                    break;
                                }
                            }
                        }
                    }
                }
                Math::Mul([i, j]) => {
                    if let Some(v) = extract_constant(&egraph[*i].nodes) {
                        if let Some(w) = extract_constant(&egraph[*j].nodes) {
                            if let Ok(x) = real_to_rational(&v) {
                                if let Ok(y) = real_to_rational(&w) {
                                    let r = Real::from((x * y).to_string());
                                    to_add = Some(Self::mk_constant(r, egraph));
                                    break;
                                }
                            }
                        }
                    }
                }
                Math::Div([i, j]) => {
                    if let Some(v) = extract_constant(&egraph[*i].nodes) {
                        if let Some(w) = extract_constant(&egraph[*j].nodes) {
                            if let Ok(x) = real_to_rational(&v) {
                                if let Ok(y) = real_to_rational(&w) {
                                    if !y.is_zero() {
                                        let r = Real::from((x / y).to_string());
                                        to_add = Some(Self::mk_constant(r, egraph));
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

        if let Some(v) = to_add {
            // add (~ v) if v is negative
            if let Math::RealConst(n) = v {
                if let Ok(x) = real_to_rational(&n) {
                    if x.is_negative() {
                        let pos_id = egraph.add(Self::mk_constant(
                            Real::from((-x).to_string()),
                            &mut egraph.clone(),
                        ));
                        let neg_id = egraph.add(Math::Neg(pos_id));
                        egraph.union(neg_id, id);
                    }
                }
            }

            let cnst_id = egraph.add(v);
            egraph.union(cnst_id, id);
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
