use egg::*;
use num::{BigInt, ToPrimitive, Zero};
use num_bigint::ToBigInt;
use ruler::*;
use z3::ast::Ast;

type Constant = BigInt;

define_language! {
  pub enum Pred {
    Lit(Constant),
    "<" = Lt([Id;2]),
    "<=" = Leq([Id;2]),
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

impl SynthLanguage for Pred {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        let one = 1.to_bigint().unwrap();
        let zero = 0.to_bigint().unwrap();
        match self {
            Pred::Lit(c) => vec![Some(c.clone()); cvec_len],
            Pred::Lt([x, y]) => {
                map!(get_cvec, x, y => if x < y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Leq([x, y]) => {
                map!(get_cvec, x, y => if x <= y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Eq([x, y]) => {
                map!(get_cvec, x, y => if x == y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Neq([x, y]) => {
                map!(get_cvec, x, y => if x != y {Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Implies([x, y]) => {
                map!(get_cvec, x, y => {
                  let xbool = x.clone() != zero;
                  let ybool = y.clone() != zero;
                  if !xbool || ybool {Some(one.clone())} else {Some(zero.clone())}
                })
            }
            Pred::Not(x) => {
                map!(get_cvec, x => if x.clone() == zero { Some(one.clone())} else {Some(zero.clone())})
            }
            Pred::Neg(x) => map!(get_cvec, x => Some(-x)),
            Pred::And([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool && ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Or([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool || ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Xor([x, y]) => {
                map!(get_cvec, x, y => {
                    let xbool = x.clone() != zero;
                    let ybool = y.clone() != zero;
                    if xbool ^ ybool { Some(one.clone()) } else { Some(zero.clone()) }
                })
            }
            Pred::Add([x, y]) => map!(get_cvec, x, y => Some(x + y)),
            Pred::Sub([x, y]) => map!(get_cvec, x, y => Some(x - y)),
            Pred::Mul([x, y]) => map!(get_cvec, x, y => Some(x * y)),
            Pred::Div([x, y]) => map!(get_cvec, x, y => {
              if y.is_zero() {
                Some(zero.clone())
              } else {
                Some(x / y)
              }
            }),
            Pred::Min([x, y]) => map!(get_cvec, x, y => Some(x.min(y).clone())),
            Pred::Max([x, y]) => map!(get_cvec, x, y => Some(x.max(y).clone())),
            Pred::Select([x, y, z]) => map!(get_cvec, x, y, z => {
              let xbool = x.clone() != zero;
              if xbool {Some(y.clone())} else {Some(z.clone())}
            }),
            Pred::Var(_) => vec![],
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

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Pred::Lit(c) = self {
            Some(c)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Pred::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let consts = vec![
            Some((-10).to_bigint().unwrap()),
            Some((-1).to_bigint().unwrap()),
            Some(0.to_bigint().unwrap()),
            Some(1.to_bigint().unwrap()),
            Some(2.to_bigint().unwrap()),
            Some(5.to_bigint().unwrap()),
            Some(100.to_bigint().unwrap()),
        ];

        let cvecs = self_product(&consts, synth.params.variables);

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: cvecs[0].len(),
        });
        for (i, cvec) in cvecs.iter().enumerate() {
            let var = egg::Symbol::from(letter(i));
            let id = egraph.add(Pred::Var(var));
            egraph[id].data.cvec = cvec.clone();
        }

        egraph.add(Pred::Lit((-1).to_bigint().unwrap()));
        egraph.add(Pred::Lit((0).to_bigint().unwrap()));
        egraph.add(Pred::Lit((1).to_bigint().unwrap()));

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, _iter: usize) -> Vec<Self> {
        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                for k in synth.ids() {
                    if synth.egraph[i].data.exact
                        && synth.egraph[j].data.exact
                        && synth.egraph[k].data.exact
                    {
                        continue;
                    }
                    to_add.push(Pred::Select([i, j, k]));
                }
                if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                }

                to_add.push(Pred::Lt([i, j]));
                to_add.push(Pred::Leq([i, j]));
                to_add.push(Pred::Eq([i, j]));
                to_add.push(Pred::Neq([i, j]));
                to_add.push(Pred::Implies([i, j]));
                to_add.push(Pred::And([i, j]));
                to_add.push(Pred::Or([i, j]));
                to_add.push(Pred::Xor([i, j]));
                to_add.push(Pred::Add([i, j]));
                to_add.push(Pred::Sub([i, j]));
                to_add.push(Pred::Mul([i, j]));
                to_add.push(Pred::Div([i, j]));
                to_add.push(Pred::Min([i, j]));
                to_add.push(Pred::Max([i, j]));
            }

            if synth.egraph[i].data.exact {
                continue;
            }
            to_add.push(Pred::Neg(i));
            to_add.push(Pred::Not(i));
        }

        println!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(_synth: &mut Synthesizer<Self>, lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool {
        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
        let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
        solver.assert(&lexpr._eq(&rexpr).not());
        match solver.check() {
            z3::SatResult::Unsat => true,
            z3::SatResult::Unknown => false,
            z3::SatResult::Sat => false,
        }
    }
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Pred]) -> z3::ast::Int<'a> {
    let mut buf: Vec<z3::ast::Int> = vec![];
    let zero = z3::ast::Int::from_i64(ctx, 0);
    let one = z3::ast::Int::from_i64(ctx, 1);
    for node in expr.as_ref().iter() {
        match node {
            Pred::Lit(c) => buf.push(z3::ast::Int::from_i64(ctx, c.to_i64().unwrap())),
            Pred::Lt([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::lt(x, y), &one, &zero))
            }
            Pred::Leq([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::le(x, y), &one, &zero))
            }
            Pred::Eq([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::_eq(x, y), &one, &zero));
            }
            Pred::Neq([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Int::_eq(x, y).not(),
                    &one,
                    &zero,
                ));
            }
            Pred::Implies([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                let x_not_z = x._eq(&zero).not();
                let y_not_z = y._eq(&zero).not();

                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::implies(&x_not_z, &y_not_z),
                    &one,
                    &zero,
                ));
            }
            Pred::Not(x) => {
                let x = &buf[usize::from(*x)];
                buf.push(z3::ast::Bool::ite(&x._eq(&zero), &one, &zero));
            }
            Pred::Neg(x) => {
                let x = &buf[usize::from(*x)];
                buf.push(z3::ast::Int::unary_minus(x));
            }
            Pred::And([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                let x_not_z = x._eq(&zero).not();
                let y_not_z = y._eq(&zero).not();
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::and(ctx, &[&x_not_z, &y_not_z]),
                    &one,
                    &zero,
                ));
            }
            Pred::Or([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                let x_not_z = x._eq(&zero).not();
                let y_not_z = y._eq(&zero).not();
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::or(ctx, &[&x_not_z, &y_not_z]),
                    &one,
                    &zero,
                ));
            }
            Pred::Xor([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                let x_not_z = x._eq(&zero).not();
                let y_not_z = y._eq(&zero).not();
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::xor(&x_not_z, &y_not_z),
                    &one,
                    &zero,
                ));
            }
            Pred::Add([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Int::add(ctx, &[x, y]));
            }
            Pred::Sub([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Int::sub(ctx, &[x, y]));
            }
            Pred::Mul([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Int::mul(ctx, &[x, y]));
            }
            Pred::Div([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(
                    &y._eq(&zero),
                    &zero,
                    &z3::ast::Int::div(x, y),
                ))
            }
            Pred::Min([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::le(x, y), x, y));
            }
            Pred::Max([x, y]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::le(x, y), y, x));
            }
            Pred::Select([x, y, z]) => {
                let x = &buf[usize::from(*x)];
                let y = &buf[usize::from(*y)];
                let z = &buf[usize::from(*z)];
                buf.push(z3::ast::Bool::ite(&x._eq(y).not(), y, z))
            }
            Pred::Var(v) => buf.push(z3::ast::Int::new_const(ctx, v.to_string())),
        }
    }

    buf.pop().unwrap()
}

/// Entry point
fn main() {
    Pred::main()
}
