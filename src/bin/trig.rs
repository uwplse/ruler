/*!
    Trigonometry from reals
!*/

use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::*;
use std::str::FromStr;

use num::{
    bigint::BigInt,
    rational::{ParseRatioError, Ratio},
    Zero,
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
        Real(Symbol::from(s))
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
        Variable(Symbol::from(s))
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

define_language! {
    pub enum Math {
        // trig operators
        "sin" = Sin(Id),
        "cos" = Cos(Id),
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
        RealConst(Real),
        Var(Variable),
    }
}

impl SynthLanguage for Math {
    type Constant = Real;

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

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::RealConst(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::RealConst(c)
    }

    // override default behavior
    fn is_constant(&self) -> bool {
        matches!(self, Math::RealConst(_))
    }

    fn is_extractable(&self) -> bool {
        matches!(
            self,
            Math::Sin(_)
                | Math::Cos(_)
                | Math::Neg(_)
                | Math::Add(_)
                | Math::Sub(_)
                | Math::Mul(_)
                | Math::Div(_)
                | Math::RealConst(_)
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

        // variables
        for i in 0..synth.params.variables {
            let var = Variable::from(letter(i));
            egraph.add(Math::Var(var));
        }

        // constants
        let zero_id = egraph.add(Math::RealConst(Real::from("0")));
        egraph.add(Math::Sin(zero_id));
        egraph.add(Math::Cos(zero_id));

        // rewrites
        synth.lifting_rewrites = vec![
            rewrite!("def-cos"; "(cos ?a)" <=> "(/ (+ (cis ?a) (cis (~ ?a))) 2)"),
            rewrite!("def-sin"; "(sin ?a)" <=> "(/ (- (cis ?a) (cis (~ ?a))) (* 2 I))"),
            rewrite!("def-sqr"; "(sqr ?a)" <=> "(* ?a ?a)"),
            rewrite!("sqr-distribute-mul"; "(sqr (* ?a ?b))" <=> "(* (sqr ?a) (sqr ?b))"),
            rewrite!("sqr-distribute-div"; "(sqr (/ ?a ?b))" <=> "(/ (sqr ?a) (sqr ?b))"),
            rewrite!("invert-cis"; "(cis (~ ?a))" <=> "(/ 1 (cis ?a))"),
            vec![
                rewrite!("square-i"; "(* I I)" => "-1"),
                rewrite!("add-zero-i"; "(+ I 0)" => "I"),
                rewrite!("mul-zero-i"; "(* I 0)" => "0"),
                rewrite!("mul-one-i"; "(* I 1)" => "I"),
                rewrite!("cis-0"; "(cis 0)" => "1"),
                rewrite!("add-cis"; "(cis (+ ?a ?b))" => "(* (cis ?a) (cis ?b))"),
            ],
        ]
        .concat();

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let extract = Extractor::new(&synth.egraph, NumberOfAllowedOps);
        let mut to_add = vec![];

        // disabled operators (TODO: validate input)
        let disabled_ops: Vec<&str> = if let Some(s) = &synth.params.disabled_ops {
            s.split(' ').collect()
        } else {
            vec![]
        };

        // predicate if disabled
        let allowedp = |s| !disabled_ops.iter().any(|x| x.eq(&s));

        // predicate for enumerable
        let enumerable = |n: &Math| {
            matches!(
                n,
                Math::Sin(_)
                    | Math::Cos(_)
                    | Math::Neg(_)
                    | Math::Add(_)
                    | Math::Sub(_)
                    | Math::Mul(_)
                    | Math::Div(_)
                    | Math::RealConst(_)
                    | Math::Var(_)
            )
        };

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
                    || !synth.egraph[i].nodes.iter().all(enumerable)
                    || !synth.egraph[j].nodes.iter().all(enumerable)
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

                if allowedp("+") {
                    to_add.push(Math::Add([i, j]));
                }

                // if allowedp("-") { to_add.push(Math::RSub([i, j])); }

                // if iter <= 1 {
                //     if allowedp("*") {
                //         to_add.push(Math::Mul([i, j]));
                //     }
                // }

                // if allowedp("/") && !synth.egraph[j].nodes.iter().any(|x| is_zero(x)) {
                //     to_add.push(Math::RDiv([i, j]));
                // }
            }

            if ids[&i] + 1 != iter
                || synth.egraph[i].data.exact
                || !synth.egraph[i].data.is_allowed
                || !synth.egraph[i].nodes.iter().all(enumerable)
            {
                continue;
            }

            if iter <= 3 {
                if allowedp("~") {
                    to_add.push(Math::Neg(i));
                }
                if allowedp("sqr") {
                    to_add.push(Math::Sqr(i));
                }
            }

            if allowedp("sin") {
                to_add.push(Math::Sin(i));
            }
            if allowedp("cos") {
                to_add.push(Math::Cos(i));
            }
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
                            to_add = Some(Self::mk_constant(r));
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
                                    to_add = Some(Self::mk_constant(r));
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
                                    to_add = Some(Self::mk_constant(r));
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
                                    to_add = Some(Self::mk_constant(r));
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
                                        to_add = Some(Self::mk_constant(r));
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
            let cnst_id = egraph.add(v);
            egraph.union(cnst_id, id);
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
