/*!
    Complex from Reals
!*/

use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::*;
use std::str::FromStr;

use num::bigint::BigInt;
use num::rational::{ParseRatioError, Ratio};
use num::Zero;

use egg::*;
use ruler::*;

pub type Rational = Ratio<BigInt>;

// custom implementation of a complex value
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Complex {
    val: Symbol,
}

impl Complex {
    pub fn as_str(self) -> &'static str {
        self.val.as_str()
    }
}

impl<S: AsRef<str>> From<S> for Complex {
    fn from(s: S) -> Self {
        let val = Symbol::from(s);
        Complex { val }
    }
}

impl From<Complex> for &'static str {
    fn from(s: Complex) -> Self {
        s.as_str()
    }
}

impl FromStr for Complex {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() > 1 && !s.starts_with('?') && s.ends_with('C') {
            Ok(s.into())
        } else {
            Err("not complex")
        }
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for Complex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

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
        let val = Symbol::from(s);
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
        if s.len() > 1 && !s.starts_with('?') && s.ends_with('R') {
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

define_language! {
    pub enum Math {
        // real domain
        "~R" = RNeg(Id),
        "+R" = RAdd([Id; 2]),
        "-R" = RSub([Id; 2]),
        "*R" = RMul([Id; 2]),
        "/R" = RDiv([Id; 2]),
        RealConst(Real),

        // real domain
        "~C" = CNeg(Id),
        "+C" = CAdd([Id; 2]),
        "-C" = CSub([Id; 2]),
        "*C" = CMul([Id; 2]),
        "/C" = CDiv([Id; 2]),
        "conj" = Conj(Id),
        ComplexConst(Complex),

        // conversions
        "Cart" = Cart([Id; 2]),
        "Polar" = Polar([Id; 2]),
        "Re" = Re(Id),
        "Im" = Im(Id),
        "abs" = Abs(Id),
        "Arg" = Arg(Id),

        // variables
        Var(Variable),
    }
}

// Match for folding real constants
macro_rules! fold_real {
    ($i:ident, $egraph:ident, $to_add:ident, $op:tt) => {{   // unary
        let n = $egraph[*$i].iter()
            .find_map(|x| match x {
                Math::RealConst(v) => Some(v),
                _ => None,
            });
        if let Some(v) = n {
            let r = $op(v);
            if let Some(v) = r {
                $to_add = Some(Math::RealConst(v));
            } else {
                $to_add = None;
            }
            break;
        }
    }};
    ($i:ident, $j:ident, $egraph:ident, $to_add:ident, $op:tt) => {{     // binary
        let ns = $egraph[*$i].iter()
            .zip($egraph[*$j].iter())
            .find_map(|x| match x {
                (Math::RealConst(v), Math::RealConst(w)) => Some((v, w)),
                _ => None,
            });
        if let Some((v, w)) = ns {
            let r = $op(v, w);
            if let Some(v) = r {
                $to_add = Some(Math::RealConst(v));
            } else {
                $to_add = None;
            }
            break;
        }
    }};
}

//
//  Helper functions
//

fn is_complex_str(
    s: &'static str,
) -> impl Fn(&mut EGraph<Math, SynthAnalysis>, Id, &Subst) -> bool {
    let var = s.parse().unwrap();
    move |egraph, _, subst| egraph[subst[var]].data.is_allowed
}

fn real_const_symbol(s: &str) -> Real {
    Real::from(s.to_owned() + "R")
}

fn complex_const_symbol(s: &str) -> Complex {
    Complex::from(s.to_owned() + "C")
}

fn is_complex_zero(n: &Math) -> bool {
    match n {
        Math::ComplexConst(v) => *v == complex_const_symbol("0"),
        _ => false,
    }
}

fn is_real_zero(n: &Math) -> bool {
    match n {
        Math::RealConst(v) => *v == real_const_symbol("0"),
        _ => false,
    }
}

//
//  Constant folding
//

fn symbol_to_rational(r: &Real) -> Result<Rational, ParseRatioError> {
    let s = r.as_str();
    (&s[..(s.len() - 1)]).parse()
}

fn fold_real_neg(v: &Real) -> Option<Real> {
    let n = symbol_to_rational(v);
    match n {
        Ok(x) => Some(real_const_symbol(&(-x).to_string())),
        _ => None,
    }
}

fn fold_real_add(v1: &Real, v2: &Real) -> Option<Real> {
    let n1 = symbol_to_rational(v1);
    let n2 = symbol_to_rational(v2);
    match (n1, n2) {
        (Ok(x), Ok(y)) => Some(real_const_symbol(&(x + y).to_string())),
        _ => None,
    }
}

fn fold_real_sub(v1: &Real, v2: &Real) -> Option<Real> {
    let n1 = symbol_to_rational(v1);
    let n2 = symbol_to_rational(v2);
    match (n1, n2) {
        (Ok(x), Ok(y)) => Some(real_const_symbol(&(x - y).to_string())),
        _ => None,
    }
}

fn fold_real_mul(v1: &Real, v2: &Real) -> Option<Real> {
    let n1 = symbol_to_rational(v1);
    let n2 = symbol_to_rational(v2);
    match (n1, n2) {
        (Ok(x), Ok(y)) => Some(real_const_symbol(&(x * y).to_string())),
        _ => None,
    }
}

fn fold_real_div(v1: &Real, v2: &Real) -> Option<Real> {
    let n1 = symbol_to_rational(v1);
    let n2 = symbol_to_rational(v2);
    match (n1, n2) {
        (Ok(x), Ok(y)) => {
            if y.is_zero() {
                None
            } else {
                Some(real_const_symbol(&(x / y).to_string()))
            }
        }
        _ => None,
    }
}

//
//  Ruler implementation
//

impl SynthLanguage for Math {
    type Constant = Complex; // not used

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
        if let Math::ComplexConst(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::ComplexConst(c)
    }

    // override default behavior
    fn is_constant(&self) -> bool {
        matches!(self, Math::ComplexConst(_))
    }

    // override default behavior
    fn is_allowed(&self) -> bool {
        matches!(
            self,
            Math::CNeg(_)
                | Math::CAdd([_, _])
                | Math::CSub([_, _])
                | Math::CMul([_, _])
                | Math::CDiv([_, _])
                | Math::Conj(_)
                | Math::Var(_)
                | Math::ComplexConst(_)
                | Math::Cart([_, _])
                | Math::Polar([_, _])
        )
    }

    fn is_extractable(&self) -> bool {
        matches!(
            self,
            Math::CNeg(_)
                | Math::CAdd([_, _])
                | Math::CSub([_, _])
                | Math::CMul([_, _])
                | Math::CDiv([_, _])
                | Math::Conj(_)
                | Math::Var(_)
                | Math::ComplexConst(_)
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
            egraph.add(Math::Var(Variable(var)));
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
        let re_zero = egraph.add(Math::RealConst(zero));
        let re_one = egraph.add(Math::RealConst(one));
        let re_pi_2 = egraph.add(Math::RealConst(pi_2));
        let re_pi = egraph.add(Math::RealConst(pi));

        let pi_2_pi_2 = egraph.add(Math::RAdd([re_pi_2, re_pi_2]));
        egraph.union(re_pi, pi_2_pi_2);

        // zero constant
        let cx_zero = egraph.add(Math::ComplexConst(complex_zero));
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
        let cx_one = egraph.add(Math::ComplexConst(complex_one));
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
        let cx_i = egraph.add(Math::ComplexConst(complex_i));
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

        synth.lifting_rewrites = vec![
            rewrite!("cartesian-form"; "?a" => "(Cart (Re ?a) (Im ?a))" if is_complex_str("?a")),
            rewrite!("polar-form"; "?a" => "(Polar (abs ?a) (Arg ?a))" if is_complex_str("?a")),
            rewrite!("def-neg-cart-re"; "(Re (~C ?a))" => "(~R (Re ?a))"),
            rewrite!("def-neg-cart-im"; "(Im (~C ?a))" => "(~R (Im ?a))"),
            // rewrite!("def-neg-polar-abs"; "(abs (~C ?a))" => "(abs ?a)",
            // rewrite!("def-neg-polar-arg"; "(Arg (~C ?a))" => "(+R (Arg ?a) pi)",
            rewrite!("def-conj-re"; "(Re (conj ?a))" => "(Re ?a)"),
            rewrite!("def-conj-im"; "(Im (conj ?a))" => "(~R (Im ?a))"),
            rewrite!("def-conj-abs"; "(abs (conj ?a))" => "(abs ?a)"),
            rewrite!("def-conj-arg"; "(Arg (conj ?a))" => "(~R (Arg ?a))"),
            rewrite!("def-add-re"; "(Re (+C ?a ?b))" => "(+R (Re ?a) (Re ?b))"),
            rewrite!("def-add-im"; "(Im (+C ?a ?b))" => "(+R (Im ?a) (Im ?b))"),
            rewrite!("def-sub-re"; "(Re (-C ?a ?b))" => "(-R (Re ?a) (Re ?b))"),
            rewrite!("def-sub-im"; "(Im (-C ?a ?b))" => "(-R (Im ?a) (Im ?b))"),
            rewrite!("def-mul-re"; "(Re (*C ?a ?b))" => "(-R (*R (Re ?a) (Re ?b)) (*R (Im ?a) (Im ?b)))"),
            rewrite!("def-mul-im"; "(Im (*C ?a ?b))" => "(+R (*R (Re ?a) (Im ?b)) (*R (Im ?a) (Re ?b)))"),
            rewrite!("def-mul-abs"; "(abs (*C ?a ?b))" => "(*R (abs ?a) (abs ?b))"),
            rewrite!("def-mul-arg"; "(Arg (*C ?a ?b))" => "(+R (Arg ?a) (Arg ?b))"),
            rewrite!("def-div-re"; "(Re (/C ?a ?b))" => "(/R (+R (*R (Re ?a) (Re ?b)) (*R (Im ?a) (Im ?b)))
                                                             (+R (*R (Re ?b) (Re ?b)) (*R (Im ?b) (Im ?b))))"),
            rewrite!("def-div-im"; "(Im (/C ?a ?b))" => "(/R (-R (*R (Im ?a) (Re ?b)) (*R (Re ?a) (Im ?b)))
                                                             (+R (*R (Re ?b) (Re ?b)) (*R (Im ?b) (Im ?b))))"),
            rewrite!("def-div-abs"; "(abs (/C ?a ?b))" => "(/R (abs ?a) (abs ?b))"),
            rewrite!("def-div-arg"; "(Arg (/C ?a ?b))" => "(-R (Arg ?a) (Arg ?b))"),
            // rewrite!("def-abssq"; "(+R (*R (Re ?a) (Re ?a)) (*R (Im ?a) (Im ?a))))" => "(*R (abs ?a) (abs ?a))"),
        ];

        synth.egraph = egraph;
        synth.egraph.rebuild();
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
                    || !synth.egraph[i].data.is_allowed
                    || !synth.egraph[j].data.is_allowed
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

                to_add.push(Math::CAdd([i, j]));
                to_add.push(Math::CSub([i, j]));
                to_add.push(Math::CMul([i, j]));

                if !synth.egraph[j].iter().any(is_complex_zero) {
                    to_add.push(Math::CDiv([i, j]));
                }
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.is_allowed
            {
                continue;
            }

            to_add.push(Math::Conj(i));
            to_add.push(Math::CNeg(i));
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
                ENodeOrVar::ENode(Math::RDiv([_, j])) => match pat.ast.index(*j) {
                    ENodeOrVar::Var(_) => true,
                    ENodeOrVar::ENode(n) => !is_real_zero(n),
                },
                ENodeOrVar::ENode(Math::CDiv([_, j])) => match pat.ast.index(*j) {
                    ENodeOrVar::Var(_) => true,
                    ENodeOrVar::ENode(n) => !is_complex_zero(n),
                },
                ENodeOrVar::ENode(Math::Polar([i, _])) => match rhs.ast.index(*i) {
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
            ENodeOrVar::ENode(Math::RDiv([_, j])) => match rhs.ast.index(*j) {
                ENodeOrVar::Var(v) => !egraph[subst[*v]].iter().any(is_real_zero),
                ENodeOrVar::ENode(n) => !is_real_zero(n),
            },
            ENodeOrVar::ENode(Math::CDiv([_, j])) => match rhs.ast.index(*j) {
                ENodeOrVar::Var(v) => !egraph[subst[*v]].iter().any(is_complex_zero),
                ENodeOrVar::ENode(n) => !is_complex_zero(n),
            },
            ENodeOrVar::ENode(Math::Polar([i, _])) => match rhs.ast.index(*i) {
                ENodeOrVar::Var(v) => !egraph[subst[*v]].iter().any(is_real_zero),
                ENodeOrVar::ENode(n) => !is_real_zero(n),
            },
            _ => true,
        })
    }

    // Constant folding for complex numbers
    fn constant_fold(egraph: &mut EGraph<Self, SynthAnalysis>, id: Id) {
        if !egraph[id].data.is_allowed {
            // lower domain
            if egraph[id].iter().any(|x| matches!(x, Math::RealConst(_))) {
                // early exit if constant exists
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
        }
    }
}

/// Entry point.
fn main() {
    Math::main()
}
