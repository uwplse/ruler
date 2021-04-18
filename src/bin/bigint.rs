use egg::*;
use ruler::*;

use num::bigint::{BigInt, RandBigInt, ToBigInt};
use rand_pcg::Pcg64;
use z3::ast::Ast;
use z3::*;

pub type Constant = BigInt;

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "~" = Neg(Id),
        Num(Constant),
        Var(egg::Symbol),
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
                if *b == 0.to_bigint().unwrap() {
                    None
                } else{
                    Some(a / b)
                }
            }),
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
        // this is for adding to the egraph, not used for cvec.
        // let constants: Vec<Constant> = vec![];
        // let constants: Vec<Constant> = ["0"]
        let constants: Vec<Constant> = ["0", "1", "-1", "2"]
            .iter()
            .map(|s| s.parse().unwrap())
            .collect();
        
        let mut consts: Vec<Option<Constant>> = vec![];

        for i in 0..synth.params.important_cvec_offsets {
            consts.push(i.to_bigint());
            let v = i.to_bigint();
            consts.push(Some(-(&v.unwrap())));
        }

        consts.sort();
        consts.dedup();

        // ensure 0 is not first!
        if consts[0] == 0.to_bigint() {
            consts.swap(0, 1);
        }

        println!("initial cvec");
        for c in &consts {
            println!("\t{:?}", c);
        }

        let mut consts = self_product(&consts, synth.params.variables);
        // add the necessary random values, if any
        for row in consts.iter_mut() {
            let n_samples = synth.params.n_samples;
            let svals: Vec<Constant> = sampler(&mut synth.rng, 32, n_samples);
            let mut vals: Vec<Option<Constant>> = vec![];
            for v in svals {
                vals.push(Some(v));
            }
            row.extend(vals);
        }

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: consts[0].len(),
        });
        for i in 0..synth.params.variables {
            let var = egg::Symbol::from(letter(i));
            let id = egraph.add(Math::Var(var));
            egraph[id].data.cvec = consts[i].clone();
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
            }
            if synth.egraph[i].data.exact {
                continue;
            }
            to_add.push(Math::Neg(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(
        synth: &mut Synthesizer<Self>,
        lhs: &egg::Pattern<Self>,
        rhs: &egg::Pattern<Self>,
    ) -> bool {
        if synth.params.use_smt {
            let mut cfg = z3::Config::new();
            cfg.set_timeout_msec(1000);
            let ctx = z3::Context::new(&cfg);
            let solver = z3::Solver::new(&ctx);
            let (lexpr, mut lasses) = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
            let (rexpr, mut rasses) = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
            lasses.append(&mut rasses);
            let all = &lasses[..];
            solver.assert(&lexpr._eq(&rexpr).not());
            match solver.check_assumptions(all) {
                // match solver.check() {
                SatResult::Unsat => true,
                SatResult::Sat => {
                    println!("z3 validation: failed for {} => {}", lhs, rhs);
                    false
                }
                SatResult::Unknown => {
                    synth.smt_unknown += 1;
                    println!("z3 validation: unknown for {} => {}", lhs, rhs);
                    false
                }
            }
        } else {
            let n = synth.params.num_fuzz;
            let mut env = HashMap::default();

            for var in lhs.vars() {
                env.insert(var, vec![]);
            }

            for var in rhs.vars() {
                env.insert(var, vec![]);
            }

            for cvec in env.values_mut() {
                cvec.reserve(n);
                for s in sampler(&mut synth.rng, 32, n) {
                    cvec.push(Some(s));
                }
            }

            let lvec = Self::eval_pattern(lhs, &env, n);
            let rvec = Self::eval_pattern(rhs, &env, n);

            lvec == rvec
        }
    }
}

pub fn sampler(rng: &mut Pcg64, nbits: u64, num_samples: usize) -> Vec<BigInt> {
    let mut ret = vec![];
    for _ in 0..num_samples {
        ret.push(rng.gen_bigint(nbits));
    }
    ret
}

fn egg_to_z3<'a>(
    ctx: &'a z3::Context,
    expr: &[Math],
) -> (z3::ast::Int<'a>, Vec<z3::ast::Bool<'a>>) {
    let mut buf: Vec<z3::ast::Int> = vec![];
    let mut assumes: Vec<z3::ast::Bool> = vec![];
    let zero = z3::ast::Int::from_i64(&ctx, 0);
    for node in expr.as_ref().iter() {
        match node {
            Math::Var(v) => buf.push(z3::ast::Int::new_const(&ctx, v.to_string())),
            Math::Num(c) => buf.push(z3::ast::Int::from_str(&ctx, &c.to_string()).unwrap()),
            Math::Add([a, b]) => buf.push(z3::ast::Int::add(
                &ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Math::Sub([a, b]) => buf.push(z3::ast::Int::sub(
                &ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Math::Mul([a, b]) => buf.push(z3::ast::Int::mul(
                &ctx,
                &[&buf[usize::from(*a)], &buf[usize::from(*b)]],
            )),
            Math::Div([a, b]) => {
                let denom = &buf[usize::from(*b)];
                let lez = z3::ast::Int::le(denom, &zero);
                let gez = z3::ast::Int::ge(denom, &zero);
                let assume = z3::ast::Bool::not(&z3::ast::Bool::and(&ctx, &[&lez, &gez]));
                assumes.push(assume);
                buf.push(z3::ast::Int::div(
                    &buf[usize::from(*a)],
                    &buf[usize::from(*b)],
                ))
            }
            Math::Neg(a) => buf.push(z3::ast::Int::unary_minus(&buf[usize::from(*a)])),
            // _ => unimplemented!(),
        }
    }
    (buf.pop().unwrap(), assumes)
}

fn main() {
    Math::main()
}
