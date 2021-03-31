use egg::*;
use ruler::*;

use num::bigint::{BigInt, RandBigInt, ToBigInt};
use num::{rational::Ratio, Signed, ToPrimitive, Zero};
use rand_pcg::Pcg64;

pub type Constant = Ratio<BigInt>;

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "~" = Neg(Id),
        "fabs" = Abs(Id),
        "pow" = Pow([Id; 2]),
        "recip" = Reciprocal(Id),
        Num(Constant),
        Var(egg::Symbol),
    }
}

fn mk_constant(n: &BigInt, d: &BigInt) -> Option<Constant> {
    if d.is_zero() {
        None
    } else {
        Some(Ratio::new(n.clone(), d.clone()))
    }
}

impl SynthLanguage for Math {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Neg(a) => map!(v, a => Some(-a)),
            Math::Add([a, b]) => map!(v, a, b => Some(a + b)),
            Math::Sub([a, b]) => map!(v, a, b => Some(a - b)),
            Math::Mul([a, b]) => map!(v, a, b => Some(a * b)),
            Math::Num(n) => vec![Some(n.clone()); cvec_len],
            Math::Var(_) => vec![],
            Math::Div([a, b]) => map!(v, a, b => {
                if b.is_zero() {
                    None
                } else{
                    Some(a / b)
                }
            }),
            Math::Abs(a) => map!(v, a => Some(a.abs())),
            Math::Pow([a, b]) => map!(v, a, b => {
                b.to_i32().map(|b| a.pow(b))
            }),
            Math::Reciprocal(a) => map!(v, a => {
                if a.is_zero() {
                    None
                } else {
                    Some(a.recip())
                }
            }),
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
            cvec_len: params.n_samples + constants.len().pow(params.variables as u32),
        });

        let rng = &mut synth.rng;
        for i in 0..params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));

            egraph[id].data.cvec = (0..params.n_samples)
                .map(|_| mk_constant(&rng.gen_bigint(32), &gen_denom(rng, 32)))
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
                to_add.push(Math::Div([i, j]));
                // to_add.push(Math::Pow([i, j]));
            }
            if synth.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Abs(i));
            to_add.push(Math::Neg(i));
            // to_add.push(Math::Reciprocal(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(rng: &mut Pcg64, lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> bool {
        let n = 1000;
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
                let numer = rng.gen_bigint(32);
                let denom = gen_denom(rng, 32);
                cvec.push(Some(Ratio::new(numer, denom)));
            }
        }

        let lvec = Self::eval_pattern(lhs, &env, n);
        let rvec = Self::eval_pattern(rhs, &env, n);

        lvec == rvec
    }
}

fn chain_consts(constants: Vec<Constant>, nvars: u32, i: u32) -> Vec<Option<Constant>> {
    let mut res = vec![];
    let mut consts = vec![];
    for c in constants {
        consts.push(mk_constant(c.numer(), c.denom()));
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

// randomly sample denoms so that they are not 0
// Ratio::new will panic if the denom is 0
pub fn gen_denom(rng: &mut Pcg64, bits: u64) -> BigInt {
    let mut res: BigInt;
    loop {
        res = rng.gen_bigint(bits);
        if res != 0.to_bigint().unwrap() {
            break;
        }
    }
    res
}

fn main() {
    Math::main()
}
