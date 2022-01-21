/*!
    Trigonometry from reals
!*/

use std::ops::*;
use std::str::FromStr;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use egg::*;
use ruler::*;

// custom implementation of a complex value
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Complex(Symbol);

impl Complex {
    fn as_str(self) -> &'static str {
        self.0.as_str()
    }

    fn is_real(self) -> bool {
        self.0.as_str().chars().position(|x| x == 'i').is_none()
    }
}

impl<S: AsRef<str>> From<S> for Complex {
    fn from(s: S) -> Self {
        Complex(Symbol::from(s))
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
        if s.len() > 1 && s.chars().next().unwrap() != '?' && s.chars().last().unwrap() == 'C' {
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
        if s.len() > 0 && s.chars().next().unwrap() != '?' {
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
        if s.len() == 1 && s.chars().nth(0).unwrap().is_alphabetic() {
            Ok(s.into())
        } else if s.len() == 2 && s.chars().nth(0).unwrap() == '?' &&
            s.chars().nth(1).unwrap().is_alphabetic() {
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
        // complex
        "~C" = CNeg(Id),
        "+C" = CAdd([Id; 2]),
        "-C" = CSub([Id; 2]),
        "*C" = CMul([Id; 2]),
        "/C" = CDiv([Id; 2]),
        "cis" = Cis(Id),

        // trig
        "sin" = Sin(Id),
        "cos" = Cos(Id),
        "~" = Neg(Id),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "Re" = Re(Id),
        "Im" = Im(Id),
        "sqr" = Sqr(Id),

        // constants
        ComplexConst(Complex),
        RealConst(Real),
        Var(Variable),
    }
}

// For any Complex(_) ?a, applier adds (Re ?a) to the egraph for any ?a
#[derive(Debug)]
struct ComplexToRealApplier(&'static str);
impl Applier<Math, SynthAnalysis> for ComplexToRealApplier {
    fn apply_one(&self, egraph: &mut EGraph<Math, SynthAnalysis>, _: Id, subst: &Subst, _searcher_pat: Option<&PatternAst<Math>>, _rule: Symbol) -> Vec<Id> {
        let id = subst[self.0.parse().unwrap()];
        if egraph[id].data.in_domain {
            return vec![];
        }
        
        let found = egraph[id]
            .iter()
            .find_map(|n| match n {
                Math::ComplexConst(c) => {
                    if c.is_real() {
                        let s = c.as_str();
                        Some(&s[..(s.len() - 1)])
                    } else {
                        None
                    }
                },
                _ => None,
            });

        if let Some(v) = found {
            let re_id = egraph.add(Math::Re(id));
            let cnst_id = egraph.add(Math::RealConst(Real::from(v)));
            egraph.union(re_id, cnst_id);
        }
        
        vec![]
    }
}

fn complex_const_symbol(s: &str) -> Complex {
    Complex::from(s.to_owned() + "C")
}

impl SynthLanguage for Math {
    type Constant = Real;  // not used

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
        match self {
            Math::RealConst(_) => true,
            _ => false,
        }
    }

    /// Returns true if the node is in the current domain.
    /// Useful for rule lifting.
    fn is_in_domain(&self) -> bool {
        match self {
            Math::Sin(_)        |
            Math::Cos(_)        |
            Math::Neg(_)        |
            Math::Add([_, _])   |
            Math::Sub([_, _])   |
            Math::Mul([_, _])   |
            Math::Div([_, _])   |
            Math::Re(_)         |
            Math::Im(_)         |
            Math::Sqr(_)        |
            Math::RealConst(_)  |
            Math::Var(_) => true,
            _ => false,
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
        let constants: Vec<Complex> = ["2", "1", "0", "-1", "-2"]
            .iter()
            .filter(|s| disabled_consts.iter().find(|x| x.eq(s)).is_none())
            .map(|s| complex_const_symbol(s))
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

        // variables
        for i in 0..synth.params.variables {
            let var = Variable::from(letter(i));
            egraph.add(Math::Var(var));
        }

        // constants
        for c in constants {
            egraph.add(Math::ComplexConst(c));
        }
        
        // i * i = -1;
        let i_id = egraph.add(Math::ComplexConst(complex_const_symbol("i")));
        let i2_id = egraph.add(Math::CMul([i_id, i_id]));
        let neg1_id = egraph.add(Math::ComplexConst(Complex::from("-1C")));
        egraph.union(i2_id, neg1_id);

        // 2i * 2i = -4;
        let twoi_id = egraph.add(Math::ComplexConst(complex_const_symbol("2i")));
        let twoi2_id = egraph.add(Math::CMul([twoi_id, twoi_id]));
        let neg4_id = egraph.add(Math::ComplexConst(Complex::from("-4C")));
        egraph.union(twoi2_id, neg4_id);

        // rewrites
        synth.lifting_rewrites = vec![
            rewrite!("def-cos"; "(cos ?a)" <=> "(Re (/C (+C (cis ?a) (cis (~ ?a))) 2C))"),
            rewrite!("def-sin"; "(sin ?a)" <=> "(Re (/C (-C (cis ?a) (cis (~ ?a))) 2iC))"),
            rewrite!("def-cis-add"; "(cis (+ ?a ?b))" <=> "(*C (cis ?a) (cis ?b))"),
            rewrite!("def-sqr"; "(sqr ?a)" <=> "(* ?a ?a)"),

            rewrite!("def-cos-sqr"; "(sqr (cos ?a))" <=>
                     "(Re (/C (*C (+C (cis ?a) (cis (~ ?a))) (+C (cis ?a) (cis (~ ?a)))) 2C))"),
            rewrite!("def-sin-sqr"; "(sqr (sin ?a))" <=>
                     "(Re (/C (*C (-C (cis ?a) (cis (~ ?a))) (-C (cis ?a) (cis (~ ?a)))) 2iC))"),

            // rewrite!("complex-foil"; "(*C (+C ?a ?b) (+C ?c ?d))" <=>
            //         "(+C (*C ?a ?c) (+C (*C ?a ?d) (+C (*C ?b ?c) (*C ?b ?d))))"),

            vec![
                rewrite!("def-add"; "(+ (Re ?a) (Re ?b))" => "(Re (+C ?a ?b))"),
                rewrite!("def-mul"; "(* (Re ?a) (Re ?b))" => "(Re (*C ?a ?b))"),
                rewrite!("cancel-cis-mul"; "(*C (cis ?a) (cis (~ ?a)))" => "1C"),
                rewrite!("pure-real"; "?a" => { ComplexToRealApplier("?a") }),
            ]
        ]
        .concat();

        // duplicate rules for HL
        for (_, eq) in &synth.old_eqs {
            let l = eq.lhs.to_string();
            let r = eq.rhs.to_string();
            let ls: String = l.chars().filter(|c| *c != 'C').collect();
            let rs: String = r.chars().filter(|c| *c != 'C').collect();
            let lhs: RecExpr<Self> = ls.parse().unwrap();
            let rhs: RecExpr<Self> = rs.parse().unwrap();
            if let Some(e) = Equality::new(&lhs, &rhs) {
                synth.all_eqs.insert(e.name.clone(), e);
            }
        }

        let extra_rewrites = vec![
            ("(*C (+C ?a ?b) (+C ?c ?d))",
             "(+C (*C ?a ?c) (+C (*C ?a ?d) (+C (*C ?b ?c) (*C ?b ?d))))"),
        ];
        
        extra_rewrites.iter().for_each(|(l, r)| {
            let lhs: RecExpr<Self> = l.parse().unwrap();
            let rhs: RecExpr<Self> = r.parse().unwrap();
            if let Some(e) = Equality::new(&lhs, &rhs) {
                synth.all_eqs.insert(e.name.clone(), e);
            }
        });

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let extract = Extractor::new(&synth.egraph, NumberOfDomainOps);
        let mut to_add = vec![];

        // disabled operators (TODO: validate input)
        let disabled_ops: Vec<&str> =
            if let Some(s) = &synth.params.disabled_ops {
                s.split(" ").collect()
            } else {
                vec![]
            };

        // predicate if disabled
        let allowedp = |s| disabled_ops.iter().find(|&x| x.eq(&s)).is_none();

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

                if allowedp("+") { to_add.push(Math::Add([i, j])); }
                // if allowedp("-") { to_add.push(Math::RSub([i, j])); }
                if allowedp("*") { to_add.push(Math::Mul([i, j])); }
                // if allowedp("/") && !synth.egraph[j].nodes.iter().any(|x| is_zero(x)) {
                //     to_add.push(Math::RDiv([i, j]));
                // }
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.in_domain {
                continue;
            }

            // if allowedp("~") { to_add.push(Math::RNeg(i)); }
            if iter <= 2 {
            if allowedp("sin") { to_add.push(Math::Sin(i)); }
            if allowedp("cos") { to_add.push(Math::Cos(i)); }
            if allowedp("sqr") { to_add.push(Math::Sqr(i)); }
            }
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

    // custom constant folder
    fn constant_fold(_egraph: &mut EGraph<Self, SynthAnalysis>, _id: Id) {
    }
}

/// Entry point.
fn main() {
    Math::main()
}
