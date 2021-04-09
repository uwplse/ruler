use egg::*;
use ruler::*;

use std::collections::HashSet;
use std::io::{self, Write};
use std::ops::*;
use std::process::{Command, Stdio};
use z3::ast::Ast;
use z3::*;

use rand_pcg::Pcg64;

use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;

macro_rules! impl_bits {
    ($inner:ty, $name:ident, $n:literal) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Hash, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $name($inner);

        impl $name {
            pub const ZERO: Self = Self(0);
            pub const MAX: Self = Self(<$inner>::MAX).mask();
            pub const WIDTH: std::primitive::u32 = $n;

            const fn mask(self) -> Self {
                $name(self.0 & (((1 as $inner) << $n).overflowing_sub(1).0))
            }

            pub fn wrapping_add(self, rhs: Self) -> Self {
                $name(self.0.wrapping_add(rhs.0)).mask()
            }

            pub fn wrapping_sub(self, rhs: Self) -> Self {
                $name(self.0.wrapping_sub(rhs.0)).mask()
            }

            pub fn wrapping_mul(self, rhs: Self) -> Self {
                $name(self.0.wrapping_mul(rhs.0)).mask()
            }

            pub fn wrapping_neg(self) -> Self {
                $name(self.0.wrapping_neg()).mask()
            }

            pub fn my_shl(self, rhs: Self) -> Self {
                if rhs.0 >= $n {
                    Self::ZERO
                } else {
                    $name(self.0 << rhs.0).mask()
                }
            }

            pub fn my_shr(self, rhs: Self) -> Self {
                if rhs.0 >= $n {
                    Self::ZERO
                } else {
                    $name(self.0 >> rhs.0).mask()
                }
            }
        }

        impl Not for $name {
            type Output = Self;
            fn not(self) -> Self {
                $name(self.0.not()).mask()
            }
        }

        impl BitAnd for $name {
            type Output = Self;
            fn bitand(self, rhs: Self) -> Self {
                $name(self.0.bitand(rhs.0)).mask()
            }
        }

        impl BitOr for $name {
            type Output = Self;
            fn bitor(self, rhs: Self) -> Self {
                $name(self.0.bitor(rhs.0)).mask()
            }
        }

        impl BitXor for $name {
            type Output = Self;
            fn bitxor(self, rhs: Self) -> Self {
                $name(self.0.bitxor(rhs.0)).mask()
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Debug::fmt(&self.0, f)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Display::fmt(&self.0, f)
            }
        }

        impl Distribution<$name> for rand::distributions::Standard {
            fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> $name {
                let inner: $inner = rng.gen();
                $name(inner).mask()
            }
        }

        impl std::str::FromStr for $name {
            type Err = std::num::ParseIntError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                s.parse().map(|inner| Self(inner).mask())
            }
        }

        impl From<$inner> for $name {
            fn from(t: $inner) -> Self {
                Self(t).mask()
            }
        }
    };
}

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "--" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "-" = Neg(Id),
        "~" = Not(Id),
        "<<" = Shl([Id; 2]),
        ">>" = Shr([Id; 2]),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "^" = Xor([Id; 2]),
        Num(u32),
        Var(egg::Symbol),
    }
}

impl SynthLanguage for Math {
    type Constant = u32;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Neg(a) => map!(v, a => Some(a.wrapping_neg())),
            Math::Not(a) => map!(v, a => Some(a.not())),

            Math::Add([a, b]) => map!(v, a, b => Some(a.wrapping_add(*b))),
            Math::Sub([a, b]) => map!(v, a, b => Some(a.wrapping_sub(*b))),
            Math::Mul([a, b]) => map!(v, a, b => Some(a.wrapping_mul(*b))),

            Math::Shl([a, b]) => map!(v, a, b => Some(a.my_shl(*b))),
            Math::Shr([a, b]) => map!(v, a, b => Some(a.my_shr(*b))),

            Math::And([a, b]) => map!(v, a, b => Some(*a & *b)),
            Math::Or([a, b]) => map!(v, a, b => Some(*a | *b)),
            Math::Xor([a, b]) => map!(v, a, b => Some(*a ^ *b)),

            Math::Num(n) => vec![Some(n.clone()); cvec_len],
            Math::Var(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<egg::Symbol> {
        if let Math::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::Num(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Num(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let params = &synth.params;
        let constants: Vec<u32> = vec![0.into(), 1.into(), 2.into()];

        // for eqsat soundness experiment
        let cvec_size = if params.cvec_none {
            0
        } else if params.only_consts {
            constants.len()
        } else if params.only_rand {
            params.n_samples
        } else if params.cross_prod {
            constants.len().pow(params.variables as std::primitive::u32)
        } else if params.rand_and_consts {
            params.n_samples + constants.len()
        } else {
            params.n_samples + constants.len().pow(params.variables as std::primitive::u32)
        };
        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: cvec_size,
        });

        let rng = &mut synth.rng;

        for i in 0..params.variables {
            let var = egg::Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));

            if params.cvec_none {
                egraph[id].data.cvec = vec![];
            } else if params.only_consts {
                for c in &constants {
                    egraph[id].data.cvec.push(Some(c.clone()));
                }
            } else if params.only_rand {
                egraph[id].data.cvec = (0..params.n_samples)
                    .map(|_| Some(rng.gen::<u32>()))
                    .collect();
            } else if params.cross_prod {
                egraph[id].data.cvec = chain_consts(
                    constants.clone(),
                    params.variables as std::primitive::u32,
                    i as std::primitive::u32,
                );
            } else if params.rand_and_consts {
                egraph[id].data.cvec = (0..params.n_samples)
                    .map(|_| Some(rng.gen::<u32>()))
                    .collect();
                for c in &constants {
                    egraph[id].data.cvec.push(Some(c.clone()));
                }
            } else {
                egraph[id].data.cvec = (0..params.n_samples)
                    .map(|_| Some(rng.gen::<u32>()))
                    .chain(chain_consts(
                        constants.clone(),
                        params.variables as std::primitive::u32,
                        i as std::primitive::u32,
                    ))
                    .collect();
            }
        }

        for n in &constants {
            egraph.add(Math::Num(n.clone()));
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, _iter: usize) -> Vec<Self> {
        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                }
                to_add.push(Math::Add([i, j]));
                to_add.push(Math::Sub([i, j]));
                to_add.push(Math::Mul([i, j]));

                // to_add.push(Math::Shl([i, j]));
                // to_add.push(Math::Shr([i, j]));

                to_add.push(Math::And([i, j]));
                to_add.push(Math::Or([i, j]));
                to_add.push(Math::Xor([i, j]));
            }
            if synth.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Not(i));
            to_add.push(Math::Neg(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(
        rng: &mut Pcg64,
        lhs: &egg::Pattern<Self>,
        rhs: &egg::Pattern<Self>,
        use_smt: &bool,
        smt_unknown: &mut usize,
        num_fuzz: &usize,
    ) -> bool {
        if *use_smt {
            let mut cfg = Config::new();
            cfg.set_timeout_msec(1000);
            let ctx = Context::new(&cfg);
            let solver = Solver::new(&ctx);
            let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
            let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
            solver.assert(&lexpr._eq(&rexpr).not());
            match solver.check() {
                SatResult::Unsat => true,
                SatResult::Sat => {
                    println!("z3 validation: failed for {} => {}", lhs, rhs);
                    false
                }
                SatResult::Unknown => {
                    *smt_unknown = *smt_unknown + 1;
                    println!("z3 validation: unknown for {} => {}", lhs, rhs);
                    false
                }
            }
        } else {
            let n = num_fuzz.clone();
            let mut env = HashMap::default();

            for var in lhs.vars() {
                env.insert(var, vec![]);
            }

            for var in rhs.vars() {
                env.insert(var, vec![]);
            }

            for cvec in env.values_mut() {
                cvec.reserve(n);
                for _ in 0..n {
                    let v = rng.gen::<u32>();
                    cvec.push(Some(v));
                }
            }

            let lvec = Self::eval_pattern(lhs, &env, n);
            let rvec = Self::eval_pattern(rhs, &env, n);

            lvec == rvec
        }
    }
}

fn chain_consts(
    constants: Vec<u32>,
    nvars: std::primitive::u32,
    i: std::primitive::u32,
) -> Vec<Option<u32>> {
    let mut res = vec![];
    let mut consts = vec![];
    for c in constants {
        consts.push(Some(c));
    }
    let nc = consts.len();
    let nrows = nc.pow(nvars);
    while res.len() < nrows {
        for c in &consts {
            for _ in 0..nc.pow(i) {
                res.push(c.clone())
            }
        }
    }
    res
}

pub fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[Math]) -> z3::ast::BV<'a> {
    let mut buf: Vec<z3::ast::BV> = vec![];
    for node in expr.as_ref().iter() {
        match node {
            Math::Var(v) => buf.push(ast::BV::new_const(&ctx, v.to_string(), 32)),
            Math::Num(c) => buf.push(ast::BV::from_u64(&ctx, c.0 as u64, 32)),
            Math::Add([a, b]) => buf.push(buf[usize::from(*a)].bvadd(&buf[usize::from(*b)])),
            Math::Sub([a, b]) => buf.push(buf[usize::from(*a)].bvsub(&buf[usize::from(*b)])),
            Math::Mul([a, b]) => buf.push(buf[usize::from(*a)].bvmul(&buf[usize::from(*b)])),
            Math::Shl([a, b]) => buf.push(buf[usize::from(*a)].bvshl(&buf[usize::from(*b)])),
            Math::Shr([a, b]) => buf.push(buf[usize::from(*a)].bvlshr(&buf[usize::from(*b)])),
            Math::And([a, b]) => buf.push(buf[usize::from(*a)].bvand(&buf[usize::from(*b)])),
            Math::Or([a, b]) => buf.push(buf[usize::from(*a)].bvor(&buf[usize::from(*b)])),
            Math::Xor([a, b]) => buf.push(buf[usize::from(*a)].bvxor(&buf[usize::from(*b)])),
            Math::Not(a) => buf.push(buf[usize::from(*a)].bvnot()),
            Math::Neg(a) => buf.push(buf[usize::from(*a)].bvneg()),
        }
    }
    buf.pop().unwrap()
}

fn main() {
    Math::main()
}

impl_bits!(std::primitive::u64, u32, 32);

// major h/t to Remy for this.
// fn validate(lhs: &egg::Pattern<Math>, rhs: &egg::Pattern<Math>) -> io::Result<bool> {
//     let expr = egg_to_smt(
//         Math::instantiate(lhs).as_ref(),
//         Math::instantiate(rhs).as_ref(),
//     );

//     let mut smt = Command::new("timeout")
//         .arg("1s")
//         .arg("bitwuzla")
//         .stdin(Stdio::piped())
//         .stdout(Stdio::piped())
//         .spawn()?;
//     let smt_stdin = smt.stdin.as_mut().unwrap();
//     smt_stdin.write_all(expr.as_bytes())?;
//     drop(smt_stdin);

//     let out = smt.wait_with_output()?;

//     Ok(String::from_utf8_lossy(&out.stdout) == "unsat\n")
// }

// pub fn egg_to_smt<'a>(lhs: &[Math], rhs: &[Math]) -> String {
//     let mut vars: HashSet<String> = HashSet::new();
//     let lhs_smt;
//     let rhs_smt;
//     {
//         let mut buf: Vec<String> = vec![];
//         for node in lhs.as_ref().iter() {
//             match node {
//                 Math::Var(v) => {
//                     buf.push(v.to_string());
//                     vars.insert(v.to_string());
//                 }
//                 Math::Num(c) => buf.push(format!("(_ bv{} 32)", c)),
//                 Math::Add([a, b]) => buf.push(format!(
//                     "(bvadd {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Sub([a, b]) => buf.push(format!(
//                     "(bvsub {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Mul([a, b]) => buf.push(format!(
//                     "(bvmul {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Shl([a, b]) => buf.push(format!(
//                     "(bvshl {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Shr([a, b]) => buf.push(format!(
//                     "(bvlshr {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::And([a, b]) => buf.push(format!(
//                     "(bvand {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Or([a, b]) => buf.push(format!(
//                     "(bvor {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Xor([a, b]) => buf.push(format!(
//                     "(bvxor {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Not(a) => buf.push(format!("(bvnot {})", buf[usize::from(*a)])),
//                 Math::Neg(a) => buf.push(format!("(bvneg {})", buf[usize::from(*a)])),
//             }
//         }
//         lhs_smt = buf.pop().unwrap();
//     }
//     {
//         let mut buf: Vec<String> = vec![];
//         for node in rhs.as_ref().iter() {
//             match node {
//                 Math::Var(v) => {
//                     buf.push(v.to_string());
//                     vars.insert(v.to_string());
//                 }
//                 Math::Num(c) => buf.push(format!("(_ bv{} 32)", c)),
//                 Math::Add([a, b]) => buf.push(format!(
//                     "(bvadd {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Sub([a, b]) => buf.push(format!(
//                     "(bvsub {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Mul([a, b]) => buf.push(format!(
//                     "(bvmul {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Shl([a, b]) => buf.push(format!(
//                     "(bvshl {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Shr([a, b]) => buf.push(format!(
//                     "(bvlshr {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::And([a, b]) => buf.push(format!(
//                     "(bvand {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Or([a, b]) => buf.push(format!(
//                     "(bvor {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Xor([a, b]) => buf.push(format!(
//                     "(bvxor {} {})",
//                     buf[usize::from(*a)],
//                     &buf[usize::from(*b)]
//                 )),
//                 Math::Not(a) => buf.push(format!("(bvnot {})", buf[usize::from(*a)])),
//                 Math::Neg(a) => buf.push(format!("(bvneg {})", buf[usize::from(*a)])),
//             }
//         }
//         rhs_smt = buf.pop().unwrap();
//     }
//     let assert = format!("(assert (not (= {} {})))", lhs_smt, rhs_smt);
//     let decl: Vec<_> = vars
//         .iter()
//         .map(|v| format!("(declare-const {} (_ BitVec 32))", v))
//         .collect();
//     format!("(set-logic QF_BV) {} {} (check-sat)", decl.concat(), assert)
// }