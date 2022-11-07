use std::hash::Hash;

use egg::*;
use num::bigint::{BigInt, RandBigInt, Sign, ToBigInt};
use num::rational::Ratio;
use num::{ToPrimitive, Zero};

use rand_pcg::Pcg64;
use ruler::*;
use z3::ast::Ast;
use z3::SatResult;

type Constant = Ratio<BigInt>;

define_language! {
  pub enum Pred {
    Lit(Constant),
    "<" = Lt([Id;2]),
    "<=" = Leq([Id;2]),
    ">" = Gt([Id;2]),
    ">=" = Geq([Id;2]),
    "==" = Eq([Id;2]),
    "!=" = Neq([Id;2]),
    "->" = Implies([Id; 2]),
    "!" = Not(Id),
    "-" = Neg(Id),
    "&&" = And([Id;2]),
    "||" = Or([Id;2]),
    "^" = Xor([Id;2]),
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    "min" = Min([Id; 2]),
    "max" = Max([Id; 2]),
    "select" = Select([Id; 3]),
    Var(Symbol),
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

impl SynthLanguage for Pred {
    type Constant = Ratio<BigInt>;
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        Type::Top
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        let one = Ratio::new(1.to_bigint().unwrap(), 1.to_bigint().unwrap());
        let zero = Ratio::new(0.to_bigint().unwrap(), 1.to_bigint().unwrap());
        match self {
            Pred::Lit(c) => vec![Some(c.clone()); cvec_len],
            Pred::Lt([x, y]) => {
                map!(v, x, y => if x < y { Some(one.clone()) } else { Some(zero.clone()) })
            }
            Pred::Leq([x, y]) => {
                map!(v, x, y => if x <= y { Some(one.clone()) } else { Some(zero.clone()) })
            }
            Pred::Gt([x, y]) => {
                map!(v, x, y => if x > y { Some(one.clone()) } else { Some(zero.clone()) })
            }
            Pred::Geq([x, y]) => {
                map!(v, x, y => if x >= y { Some(one.clone()) } else { Some(zero.clone()) })
            }
            Pred::Eq([x, y]) => {
                map!(v, x, y => if x == y { Some(one.clone()) } else { Some(zero.clone()) })
            }
            Pred::Neq([x, y]) => {
                map!(v, x, y => if x != y { Some(one.clone()) } else { Some(zero.clone()) })
            }
            Pred::Not(x) => {
                map!(v, x => if x.clone() == zero { Some(one.clone()) } else { Some(zero.clone()) })
            }
            Pred::And([x, y]) => {
                map!(v, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool && ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Or([x, y]) => {
                map!(v, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool || ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Xor([x, y]) => {
                map!(v, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool ^ ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Implies([x, y]) => {
                map!(v, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if !xbool || ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Add([x, y]) => map!(v, x, y => Some(x + y)),
            Pred::Sub([x, y]) => map!(v, x, y => Some(x - y)),
            Pred::Mul([x, y]) => map!(v, x, y => Some(x * y)),
            Pred::Div([x, y]) => map!(v, x, y => {
                if y.is_zero() {
                    Some(zero.clone())
                } else {
                    Some(x / y)
                }
            }),
            Pred::Var(_) => vec![],
            Pred::Neg(x) => map!(v, x => Some(-x)),
            Pred::Min([x, y]) => map!(v,x,y => Some(x.min(y).clone())),
            Pred::Max([x, y]) => map!(v,x,y => Some(x.max(y).clone())),
            Pred::Select([x, y, z]) => {
                map!(v,x,y,z => {
                    let xbool = x.clone() != zero;
                    if xbool { Some(y.clone()) } else { Some(z.clone()) }
                })
            }
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Pred::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Pred::Var(sym)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Pred::Lit(_))
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Pred::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut consts: Vec<Option<Constant>> = vec![];

        for i in 0..synth.params.important_cvec_offsets {
            consts.push(Some(mk_constant(
                &i.to_bigint().unwrap(),
                &(1_u32.to_bigint().unwrap()),
            )));
            consts.push(Some(mk_constant(
                &(-i.to_bigint().unwrap()),
                &(1_u32.to_bigint().unwrap()),
            )));
        }

        consts.sort();
        consts.dedup();

        let cs = self_product(&consts, synth.params.variables);
        let cvec_len = cs[0].len();

        let mut egraph: EGraph<Pred, SynthAnalysis> = EGraph::new(SynthAnalysis {
            cvec_len,
            constant_fold: ConstantFoldMethod::NoFold,
            rule_lifting: false,
        });

        for (i, n_vals) in cs.iter().enumerate().take(synth.params.variables) {
            let n_id = egraph.add(Pred::Var(Symbol::from(letter(i))));
            egraph[n_id].data.cvec = n_vals.clone();
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let extract = Extractor::new(&synth.egraph, NumberOfOps);

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                for k in synth.ids() {
                    if ids[&i] + ids[&j] + ids[&k] + 1 != iter {
                        continue;
                    }
                    to_add.push(Pred::Select([i, j, k]));
                }
                if ids[&i] + ids[&j] + 1 != iter {
                    continue;
                }
                to_add.push(Pred::Lt([i, j]));
                to_add.push(Pred::Leq([i, j]));
                to_add.push(Pred::Gt([i, j]));
                to_add.push(Pred::Geq([i, j]));
                to_add.push(Pred::Eq([i, j]));
                to_add.push(Pred::Neq([i, j]));
                to_add.push(Pred::Implies([i, j]));
                to_add.push(Pred::And([i, j]));
                to_add.push(Pred::Or([i, j]));
                to_add.push(Pred::Xor([i, j]));
                to_add.push(Pred::Add([i, j]));
                to_add.push(Pred::Sub([i, j]));
                to_add.push(Pred::Mul([i, j]));
                to_add.push(Pred::Min([i, j]));
                to_add.push(Pred::Max([i, j]));
            }

            if ids[&i] + 1 != iter {
                continue;
            }
            to_add.push(Pred::Not(i));
            to_add.push(Pred::Neg(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        println!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        if synth.params.use_smt {
            let mut cfg = z3::Config::new();
            cfg.set_timeout_msec(1000);
            let ctx = z3::Context::new(&cfg);
            let solver = z3::Solver::new(&ctx);
            let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
            let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
            solver.assert(&lexpr._eq(&rexpr).not());
            match solver.check() {
                SatResult::Unsat => ValidationResult::Valid,
                SatResult::Sat => {
                    // println!("z3 validation: failed for {} => {}", lhs, rhs);
                    ValidationResult::Invalid
                }
                SatResult::Unknown => {
                    // println!("z3 validation: unknown for {} => {}", lhs, rhs)
                    synth.smt_unknown += 1;
                    ValidationResult::Unknown
                }
            }
        } else {
            ValidationResult::Invalid
        }
    }
}

pub fn gen_pos(rng: &mut Pcg64, bits: u64) -> BigInt {
    let mut res: BigInt;
    loop {
        res = BigInt::from_biguint(Sign::Plus, rng.gen_biguint(bits));
        if res != 0.to_bigint().unwrap() {
            break;
        }
    }
    res
}

pub fn sampler(rng: &mut Pcg64, num: u64, denom: u64, num_samples: usize) -> Vec<Constant> {
    let mut ret = vec![];
    for _ in 0..num_samples {
        let num = gen_pos(rng, num);
        let denom = gen_pos(rng, denom);
        ret.push(Ratio::new(num, denom));
    }
    ret
}

/// Convert expressions to Z3's syntax for using SMT based rule verification.
fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Pred]) -> z3::ast::Real<'a> {
    let mut buf: Vec<z3::ast::Real> = vec![];
    let zero = z3::ast::Real::from_real(ctx, 0, 1);
    let one = z3::ast::Real::from_real(ctx, 1, 1);
    for node in expr.as_ref().iter() {
        match node {
            Pred::Var(v) => buf.push(z3::ast::Real::new_const(ctx, v.to_string())),
            Pred::Lit(c) => buf.push(z3::ast::Real::from_real(
                ctx,
                (c.numer()).to_i32().unwrap(),
                (c.denom()).to_i32().unwrap(),
            )),
            Pred::Add([a, b]) => buf.push(z3::ast::Real::add(
                ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Pred::Sub([a, b]) => buf.push(z3::ast::Real::sub(
                ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Pred::Mul([a, b]) => buf.push(z3::ast::Real::mul(
                ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Pred::Div([a, b]) => {
                let l = &buf[usize::from(*a)].clone();
                let r = &buf[usize::from(*b)].clone();
                let r_zero = r._eq(&zero);
                buf.push(z3::ast::Bool::ite(
                    &r_zero,
                    &zero,
                    &z3::ast::Real::div(l, r),
                ))
            }
            Pred::Min([a, b]) => {
                let l = &buf[usize::from(*a)].clone();
                let r = &buf[usize::from(*b)].clone();
                buf.push(z3::ast::Bool::ite(&z3::ast::Real::le(l, r), l, r));
            }
            Pred::Max([a, b]) => {
                let l = &buf[usize::from(*a)].clone();
                let r = &buf[usize::from(*b)].clone();
                buf.push(z3::ast::Bool::ite(&z3::ast::Real::le(l, r), r, l));
            }
            Pred::Neg(a) => buf.push(z3::ast::Real::unary_minus(&buf[usize::from(*a)])),
            Pred::Lt([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Real::lt(l, r), &one, &zero))
            }
            Pred::Gt([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Real::gt(l, r), &one, &zero))
            }
            Pred::Leq([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Real::le(l, r), &one, &zero))
            }
            Pred::Geq([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Real::ge(l, r), &one, &zero))
            }
            Pred::Xor([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                let l_not_zero = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_zero = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::xor(&l_not_zero, &r_not_zero),
                    &one,
                    &zero,
                ))
            }
            Pred::And([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                let l_not_zero = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_zero = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::and(ctx, &[&l_not_zero, &r_not_zero]),
                    &one,
                    &zero,
                ))
            }
            Pred::Or([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                let l_not_zero = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_zero = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::or(ctx, &[&l_not_zero, &r_not_zero]),
                    &one,
                    &zero,
                ))
            }
            Pred::Not(a) => {
                let a_expr = &buf[usize::from(*a)];
                let a_is_zero = &a_expr._eq(&zero);
                buf.push(z3::ast::Bool::ite(a_is_zero, &one, &zero))
            }
            Pred::Implies([a, b]) => {
                let l = &buf[usize::from(*a)];
                let r = &buf[usize::from(*b)];
                let l_not_zero = z3::ast::Bool::not(&l._eq(&zero));
                let r_not_zero = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::implies(&l_not_zero, &r_not_zero),
                    &one,
                    &zero,
                ))
            }
            Pred::Eq([a, b]) => {
                let lexpr = &buf[usize::from(*a)];
                let rexpr = &buf[usize::from(*b)];
                buf.push(z3::ast::Bool::ite(&lexpr._eq(rexpr), &one, &zero))
            }
            Pred::Neq([a, b]) => {
                let lexpr = &buf[usize::from(*a)];
                let rexpr = &buf[usize::from(*b)];
                buf.push(z3::ast::Bool::ite(&lexpr._eq(rexpr), &zero, &one))
            }
            Pred::Select([a, b, c]) => {
                let a_expr = &buf[usize::from(*a)];
                let b_expr = &buf[usize::from(*b)];
                let c_expr = &buf[usize::from(*c)];
                let a_cond_bool = z3::ast::Bool::not(&a_expr._eq(&zero));
                buf.push(z3::ast::Bool::ite(&a_cond_bool, b_expr, c_expr))
            }
        }
    }
    buf.pop().unwrap()
}

fn main() {
    Pred::main()
}
