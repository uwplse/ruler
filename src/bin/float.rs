use std::collections::HashMap;

use egg::*;
use ruler::*;

use libm::{erf, erfc, fma, remainder};
use ordered_float::OrderedFloat;
use rand::{prelude::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;

pub type Constant = OrderedFloat<f64>;

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "~" = Neg(Id),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "fabs" = Fabs(Id),
        "exp" = Exp(Id),
        "cbrt" = Cbrt(Id),
        "sqrt" = Sqrt(Id),
        "pow" = Pow([Id; 2]),
        "remainder" = Remainder([Id; 2]),
        "log1p" = Log1p(Id),
        "expm1" = Expm1(Id),
        "erf" = Erf(Id),
        "erfc" = Erfc(Id),
        "fma" = Fma([Id; 3]),
        "log" = Log(Id),
        "sin" = Sin(Id),
        "cos" = Cos(Id),
        "tan" = Tan(Id),
        "atan" = Atan(Id),
        "acos" = Acos(Id),
        "asin" = Asin(Id),
        "tanh" = Tanh(Id),
        "cosh" = Cosh(Id),
        "sinh" = Sinh(Id),
        "atanh" = Atanh(Id),
        "acosh" = Acosh(Id),
        "asinh" = Asinh(Id),
        "atan2" = Atan2([Id; 2]),
        "hypot" = Hypot([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
    }
}

pub fn mk_constant(val: f64) -> Option<Constant> {
    // is_normal eliminates 0, but we want 0.
    if val.is_normal() || val == 0.0 || val == -0.0 {
        if val == -0.0 || val == 0.0 {
            // we want only 1 zero
            Some(OrderedFloat::from(0.0))
        } else {
            Some(OrderedFloat::from(val))
        }
    } else {
        None
    }
}
pub static TRICKY_FLOATS: [f64; 4] = [f64::NEG_INFINITY, f64::INFINITY, f64::MIN, f64::MAX];

impl SynthLanguage for Math {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Neg(a) => map!(v, a => Some(OrderedFloat::from(-a.into_inner()))),
            Math::Add([a, b]) => {
                map!(v, a, b => Some(OrderedFloat::from(a.into_inner() + b.into_inner())))
            }
            Math::Sub([a, b]) => {
                map!(v, a, b => Some(OrderedFloat::from(a.into_inner() - b.into_inner())))
            }
            Math::Mul([a, b]) => {
                map!(v, a, b => Some(OrderedFloat::from(a.into_inner() * b.into_inner())))
            }
            Math::Num(n) => vec![Some(OrderedFloat::from(n.clone())); cvec_len],
            Math::Var(_) => vec![],
            Math::Div([a, b]) => map!(v, a, b => {
                if b.into_inner() == 0.0 {
                    None
                } else{
                    Some(OrderedFloat::from(a.into_inner() / b.into_inner()))
                }
            }),
            Math::Fabs(a) => map!(v, a => Some(OrderedFloat::from(a.into_inner().abs()))),
            _ => todo!(),
        }
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

        let constants: Vec<Constant> = ["1", "0", "-1"]
            .iter()
            .map(|s| s.parse().unwrap())
            .collect();

        let mut egraph = EGraph::new(SynthAnalysis {
            // cvec_len: params.n_samples + params.constants.len(),
            cvec_len: params.n_samples
                + (constants.len() + TRICKY_FLOATS.len()).pow(params.variables as u32),
        });

        let rng = &mut synth.rng;
        let xyz_samples = gen_rand_xyz(rng, params.n_samples);
        for i in 0..params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));

            egraph[id].data.cvec = (0..params.n_samples)
                .map(|j| mk_constant(xyz_samples[j][i]))
                .chain(chain_consts(
                    constants.clone(),
                    params.variables as u32,
                    i as u32,
                ))
                .collect();
        }

        for n in &constants {
            egraph.add(Math::Num(n.clone()));
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>) -> Vec<Self> {
        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                    continue;
                }
                to_add.push(Math::Add([i, j]));
                to_add.push(Math::Sub([i, j]));
                to_add.push(Math::Mul([i, j]));
                to_add.push(Math::Div([i, j]));
            }
            if synth.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Fabs(i));
            to_add.push(Math::Neg(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(rng: &mut Pcg64, lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool {
        true
        // let n = 1000000;
        // let mut _failed_with: SampleStrat;
        // let ulp_rad_sm: u64 = 1000;
        // let ulp_rad_lg: u64 = 50000000000000;
        // let mut env = HashMap::new();

        // for var in lhs.vars() {
        //     env.insert(var, vec![]);
        // }

        // for var in rhs.vars() {
        //     env.insert(var, vec![]);
        // }

        // let mut samples = vec![];
        // for i in 0..n {
        //     let a = rand_float_repr(rng);
        //     let b;
        //     let c;
        //     match i % 5 {
        //         0 => {
        //             b = sample_float_range(rng, a, ulp_rad_sm);
        //             c = sample_float_range(rng, a, ulp_rad_sm);
        //             _failed_with = SampleStrat::RSmSm;
        //         }
        //         1 => {
        //             b = sample_float_range(rng, a, ulp_rad_lg);
        //             c = sample_float_range(rng, a, ulp_rad_sm);
        //             _failed_with = SampleStrat::RLgSm;
        //         }
        //         2 => {
        //             b = sample_float_range(rng, a, ulp_rad_sm);
        //             c = sample_float_range(rng, a, ulp_rad_lg);
        //             _failed_with = SampleStrat::RSmLg;
        //         }
        //         3 => {
        //             b = sample_float_range(rng, a, ulp_rad_lg);
        //             c = sample_float_range(rng, a, ulp_rad_lg);
        //             _failed_with = SampleStrat::RLgLg;
        //         }
        //         _ => {
        //             b = rand_float_repr(rng);
        //             c = rand_float_repr(rng);
        //             _failed_with = SampleStrat::RRR;
        //         }
        //     }
        //     if !a.is_normal() || !b.is_normal() || !c.is_normal() {
        //         continue;
        //     }
        //     samples.push(vec![
        //         Some(OrderedFloat::from(a)),
        //         Some(OrderedFloat::from(b)),
        //         Some(OrderedFloat::from(c)),
        //     ]);
        // }

        // for k in env.keys() {
        //     println!("{:?}", k);
        // }

        // for (v, cvec) in env.iter_mut() {
        //     if v.to_string() == "?a" {
        //         for i in 0..n {
        //             cvec.push(samples[i][0]);
        //         }
        //     }
        //     if v.to_string() == "?b" {
        //         for i in 0..n {
        //             cvec.push(samples[i][1]);
        //         }
        //     }
        //     if v.to_string() == "?c" {
        //         for i in 0..n {
        //             cvec.push(samples[i][2]);
        //         }
        //     }
        //     else {
        //         panic!("Validation failed: variable {} not found", v.to_string());
        //     }
        // }

        // let lvec = Self::eval_pattern(lhs, &env, n);
        // let rvec = Self::eval_pattern(rhs, &env, n);

        // lvec == rvec
    }
}

fn chain_consts(constants: Vec<Constant>, nvars: u32, i: u32) -> Vec<Option<Constant>> {
    let mut res = vec![];
    let mut consts = vec![];
    for c in constants {
        consts.push(mk_constant(c.into_inner()));
    }
    for c in &TRICKY_FLOATS {
        consts.push(Some(OrderedFloat::from(*c)));
    }

    let nc = consts.len();
    let nrows = nc.pow(nvars as u32);
    while res.len() < nrows {
        for c in &consts {
            for _ in 0..nc.pow(i) {
                res.push(c.clone())
            }
        }
    }
    res
}

#[derive(Debug)]
pub enum SampleStrat {
    RSmSm,
    RSmLg,
    RLgLg,
    RLgSm,
    RRR,
}

fn sample_float_range(rng: &mut Pcg64, x: f64, ulps_range: u64) -> f64 {
    let u = rng.gen_range(0, ulps_range);
    f64::from_bits(x.to_bits() + u)
}

fn rand_float_repr(rng: &mut Pcg64) -> f64 {
    let mut x = f64::NAN;
    while !x.is_normal() {
        x = f64::from_bits(rng.gen::<u64>())
    }
    x
}

// TODO: only works for 3 variables for now
fn gen_rand_xyz(rng: &mut Pcg64, n_samples: usize) -> Vec<Vec<f64>> {
    let ulp_rad_sm: u64 = 1000;
    let ulp_rad_lg: u64 = 50000000000000;
    let mut samples = vec![];
    for i in 0..n_samples {
        let mut a = rand_float_repr(rng);
        let b;
        let c;
        match i % 10 {
            0 => {
                b = sample_float_range(rng, a, ulp_rad_sm);
                c = sample_float_range(rng, a, ulp_rad_sm);
                samples.push(vec![a, b, c]);
            }
            1 => {
                b = sample_float_range(rng, a, ulp_rad_lg);
                c = sample_float_range(rng, a, ulp_rad_sm);
                samples.push(vec![a, b, c]);
            }
            2 => {
                b = sample_float_range(rng, a, ulp_rad_sm);
                c = sample_float_range(rng, a, ulp_rad_lg);
                samples.push(vec![a, b, c]);
            }
            3 => {
                b = sample_float_range(rng, a, ulp_rad_lg);
                c = sample_float_range(rng, a, ulp_rad_lg);
                samples.push(vec![a, b, c]);
            }
            4 => {
                match i % 3 {
                    0 => {
                        a = -1.0;
                    }
                    1 => {
                        a = 0.0;
                    }
                    _ => {
                        a = 1.0;
                    }
                }
                b = sample_float_range(rng, a, ulp_rad_lg);
                c = sample_float_range(rng, a, ulp_rad_lg);
                samples.push(vec![a, b, c]);
            }
            _ => {
                b = rand_float_repr(rng);
                c = rand_float_repr(rng);
                samples.push(vec![a, b, c]);
            }
        }
        if !a.is_normal() || !b.is_normal() || !c.is_normal() {
            continue;
        }
    }
    samples
}

fn main() {
    Math::main()
}
