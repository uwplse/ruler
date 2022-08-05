/*!
Experimental Float domain.
This is a good example of how Ruler allows custom samplers for different domains.
Validation is done using random testing.
!*/

use egg::*;
use ordered_float::OrderedFloat;
use rand::Rng;
use rand_pcg::Pcg64;
use ruler::*;

/// Ordered Floats as constants.
pub type Constant = OrderedFloat<f64>;

/// A set of stricky floats to seed the cvecs with, and also
/// use for validation.
pub static TRICKY_FLOATS: [f64; 5] = [
    f64::NEG_INFINITY,
    f64::INFINITY,
    f64::MIN,
    f64::MAX,
    f64::NAN,
];

define_language! {
    /// Define the operators for the domain.
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "~" = Neg(Id),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "fabs" = Fabs(Id),
        "exp" = Exp(Id),
        "pow" = Pow([Id; 2]),
        "sin" = Sin(Id),
        "cos" = Cos(Id),
        "tan" = Tan(Id),
        Num(Constant),
        Var(egg::Symbol),
    }
}

/// Returns an ordered float from an f64.
/// It only returns 0.0 for both -0.0 and 0.0,
/// and the input for other values.
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
    type Constant = Constant;
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        Type::Top
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Neg(a) => map!(v, a => mk_constant(-a.into_inner())),
            Math::Add([a, b]) => map!(v, a, b => mk_constant((*a + *b).into_inner())),
            Math::Sub([a, b]) => map!(v, a, b => mk_constant((*a - *b).into_inner())),
            Math::Mul([a, b]) => map!(v, a, b => mk_constant((*a * *b).into_inner())),
            Math::Num(n) => vec![mk_constant((*n).into_inner()); cvec_len],
            Math::Var(_) => vec![],
            Math::Div([a, b]) => map!(v, a, b => mk_constant((*a / *b).into_inner())),
            Math::Pow([a, b]) => map!(v, a, b => mk_constant(a.into_inner().powf(b.into_inner()))),
            Math::Fabs(a) => map!(v, a => mk_constant(a.into_inner().abs())),
            Math::Exp(a) => map!(v, a => mk_constant(a.into_inner().exp())),
            Math::Sin(a) => map!(v, a => mk_constant(a.into_inner().sin())),
            Math::Cos(a) => map!(v, a => mk_constant(a.into_inner().cos())),
            Math::Tan(a) => map!(v, a => mk_constant(a.into_inner().tan())),
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

    fn is_constant(&self) -> bool {
        matches!(self, Math::Num(_))
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Num(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let params = &synth.params;

        // let constants: Vec<Constant> = ["1", "0", "-1"]
        //     .iter()
        //     .map(|s| s.parse().unwrap())
        //     .collect();

        let constants = vec![
            OrderedFloat::from(-1.0),
            OrderedFloat::from(0.0),
            OrderedFloat::from(1.0),
        ];

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: params.n_samples
                + (constants.len() + TRICKY_FLOATS.len()).pow(params.variables as u32),
            constant_fold: if synth.params.no_constant_fold {
                ConstantFoldMethod::NoFold
            } else {
                ConstantFoldMethod::CvecMatching
            },
            rule_lifting: false,
        });

        let rng = &mut synth.rng;
        // let xyz_samples = gen_rand_xyz(rng, params.n_samples);

        let xyz_samples = gen_samples(rng, params.n_samples, params.variables);
        for (i, sample) in xyz_samples.iter().enumerate().take(params.variables) {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));

            egraph[id].data.cvec = (0..params.n_samples)
                .map(|j| mk_constant(sample[j]))
                .chain(chain_consts(
                    constants.clone(),
                    params.variables as u32,
                    i as u32,
                ))
                .collect();
        }

        for n in &constants {
            egraph.add(Math::Num(*n));
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
            to_add.push(Math::Fabs(i));
            to_add.push(Math::Neg(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        let n = synth.params.num_fuzz;
        let mut env = HashMap::default();

        for var in lhs.vars() {
            env.insert(var, vec![]);
        }

        for var in rhs.vars() {
            env.insert(var, vec![]);
        }

        let mut cvecs = gen_samples(&mut synth.rng, n, env.keys().len());
        for (_, cvec) in env.iter_mut() {
            let c = cvecs.pop().unwrap();
            for v in c {
                cvec.push(Some(OrderedFloat::from(v)));
            }
        }

        let lvec = Self::eval_pattern(lhs, &env, n);
        let rvec = Self::eval_pattern(rhs, &env, n);
        ValidationResult::from(lvec == rvec)
    }
}

/// Helper for chaining a cross product of `TRICKY_FLOATS` to cvecs.
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
                res.push(*c)
            }
        }
    }
    res
}

/// Sample a float with some ULPS of the input `x`.
fn sample_float_range(rng: &mut Pcg64, x: f64, ulps_range: u64) -> f64 {
    let u = rng.gen_range(0, ulps_range);
    f64::from_bits(x.to_bits() + u)
}

/// Sample a normal float.
fn rand_float_repr(rng: &mut Pcg64) -> f64 {
    let mut x = f64::NAN;
    while !x.is_normal() {
        x = f64::from_bits(rng.gen::<u64>())
    }
    x
}

/// A float specific sampler that accounts for the distribution of floats,
/// and additionally generates
/// both independent and dependent samples.
fn gen_samples(rng: &mut Pcg64, n_samples: usize, n_vars: usize) -> Vec<Vec<f64>> {
    let ulp_rad_sm: u64 = 1000;
    let ulp_rad_lg: u64 = 50000000000000;
    let mut all_vecs = vec![];
    let mut first = vec![];
    for i in 0..n_samples {
        match i % 10 {
            9 => match i % 8 {
                0 => first.push(-1.0),
                1 => first.push(1.0),
                2 => first.push(0.0),
                3 => first.push(f64::INFINITY),
                4 => first.push(f64::NEG_INFINITY),
                5 => first.push(f64::NAN),
                6 => first.push(f64::MAX),
                _ => first.push(f64::MIN),
            },
            _ => first.push(rand_float_repr(rng)),
        }
    }
    for i in 0..(n_vars - 1) {
        let mut dep_samples = vec![];
        for (j, item) in first.iter().enumerate().take(n_samples) {
            match j % 10 {
                0 => {
                    if i % 2 == 0 {
                        dep_samples.push(sample_float_range(rng, *item, ulp_rad_lg));
                    } else {
                        dep_samples.push(sample_float_range(rng, *item, ulp_rad_sm));
                    }
                }
                1 => {
                    if i % 2 == 0 {
                        dep_samples.push(sample_float_range(rng, *item, ulp_rad_sm));
                    } else {
                        dep_samples.push(sample_float_range(rng, *item, ulp_rad_lg));
                    }
                }
                2 => dep_samples.push(sample_float_range(rng, *item, ulp_rad_sm)),
                3 => dep_samples.push(sample_float_range(rng, *item, ulp_rad_lg)),
                6 => {
                    if i % 2 == 0 {
                        match j % 5 {
                            0 => dep_samples.push(f64::INFINITY),
                            1 => dep_samples.push(f64::NEG_INFINITY),
                            2 => dep_samples.push(f64::NAN),
                            3 => dep_samples.push(f64::MAX),
                            _ => dep_samples.push(f64::MIN),
                        }
                    } else {
                        dep_samples.push(sample_float_range(rng, *item, ulp_rad_lg));
                    }
                }
                7 => {
                    if i % 2 == 0 {
                        dep_samples.push(sample_float_range(rng, *item, ulp_rad_sm));
                    } else {
                        match j % 5 {
                            0 => dep_samples.push(f64::INFINITY),
                            1 => dep_samples.push(f64::NEG_INFINITY),
                            2 => dep_samples.push(f64::NAN),
                            3 => dep_samples.push(f64::MAX),
                            _ => dep_samples.push(f64::MIN),
                        }
                    }
                }
                _ => dep_samples.push(rand_float_repr(rng)),
            }
        }
        all_vecs.push(dep_samples);
    }
    all_vecs.push(first);
    all_vecs
}

/// Entry point.
fn main() {
    Math::main()
}
