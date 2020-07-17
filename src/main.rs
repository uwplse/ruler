use egg::{*};
use rand::{seq::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;
use std::cell::RefCell;

type Constant = i32;

define_language! {
    enum SimpleMath {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        // "/" = Div([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
    }
}

pub struct SynthParam {
    rng: RefCell<Pcg64>,
    n_samples: usize,
    iter : usize
}


#[derive(Default)]
pub struct RuleSynth;

// impl SimpleMath ...
fn get_const (node: &SimpleMath) -> Constant {
    match node {
        SimpleMath::Num(n) => *n,
        _ => unreachable!("get_const: not a constant")
    }
}

// fn eval_lang(node: &SimpleMath, get: impl Fn(Id) -> SimpleMath) -> Constant {
//     match node {
//         SimpleMath::Add([a, b]) => eval_lang(&get(*a), get) + eval_lang(&get(*b), get),
//         SimpleMath::Sub([a, b]) => eval_lang(&get(*a), get) - eval_lang(&get(*b), get),
//         SimpleMath::Mul([a, b]) => eval_lang(&get(*a), get) * eval_lang(&get(*b), get),
//         SimpleMath::Div([a, b]) => eval_lang(&get(*a), get) / eval_lang(&get(*b), get),
//         SimpleMath::Num(n) => *n,
//         SimpleMath::Var(v) => unreachable!("Shouldn't be asked to eval a var: {}", v),
//     }
// }

impl SynthParam {
    fn gen_random_vec(&self) -> Vec<Constant> {
        let n = self.n_samples;
        let mut vec = Vec::with_capacity(n);
        let rng = self.rng.borrow_mut();
        for i in 0..n {
            vec.push(rng.gen::<Constant>());
        }
        vec
    }

    // imple SynthParam
    fn gen_const_vec(&self, v: Constant) -> Vec<Constant> {
        let n = self.n_samples;
        let mut vec = Vec::with_capacity(n);
        for i in 0..n {
            vec.push(v);
        }
        vec
    }
}

fn add_vec(v1: Vec<i32>, v2: Vec<i32>) -> Vec<i32> {
    assert_eq! (v1.len(), v2.len());
    v1.iter().zip(v2.iter()).map(|(&x, &y)| x + y).collect()
}

fn sub_vec(v1: Vec<i32>, v2: Vec<i32>) -> Vec<i32> {
    assert_eq! (v1.len(), v2.len());
    v1.iter().zip(v2.iter()).map(|(&x, &y)| x - y).collect()
}

fn mul_vec(v1: Vec<i32>, v2: Vec<i32>) -> Vec<i32> {
    assert_eq! (v1.len(), v2.len());
    v1.iter().zip(v2.iter()).map(|(&x, &y)| x * y).collect()
}

fn div_vec(v1: Vec<i32>, v2: Vec<i32>) -> Vec<i32> {
    assert_eq! (v1.len(), v2.len());
    v1.iter().zip(v2.iter()).map(|(&x, &y)| x / y).collect()
}



impl Analysis<SimpleMath> for SynthParam {

    // doesnt need to be option
    type Data = Vec<Constant>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        assert_eq!(to, &from);
        false
    }

    fn make(egraph: &EGraph<SimpleMath, Self>, enode: &SimpleMath) -> Self::Data {
        let x = |i: &Id| egraph[*i].data.iter().copied();
        // let params : &mut SynthParam = &mut SynthParam {rng: Pcg64::seed_from_u64(0), n_samples: 50};
        let params = &egraph.analysis;
        match enode {
            SimpleMath::Num(n) => params.gen_const_vec(*n),
            SimpleMath::Var(_) => params.gen_random_vec(),
            SimpleMath::Add([a, b]) => x(a).zip(x(b)).map(|(x, y)| x + y).collect(),
            SimpleMath::Sub([a, b]) => x(a).zip(x(b)).map(|(x, y)| x - y).collect(),
            SimpleMath::Mul([a, b]) => x(a).zip(x(b)).map(|(x, y)| x * y).collect(),
            // SimpleMath::Div([a, b]) => x(a).zip(x(b)).map(|(x, y)| x / y).collect(),
        }
    }

    fn modify(egraph: &mut EGraph<SimpleMath, Self>, id: Id) {
        // shouldn't do anything I think?
        // actually it should add a new enoe is cv is constants
    }
}

// fn find_equiv (egraph: &mut EGraph<SimpleMath, RuleSynth>, i1: Id, i2: Id) -> () {
//     let vec1 = egraph[i1].data.as_ref();
//     let vec2 = egraph[i2].data.as_ref();
//     if vec1 == vec2 {
//         egraph.union(i1, i2);
//         print!("rule learned");
//     }
// }


fn find_equiv (egraph: EGraph<SimpleMath, SynthParam>) {
   for i in 0..egraph.analysis 
}

fn main() {
    let mut egraph = EGraph::<SimpleMath, RuleSynth>::default();
    let v1 = egraph.add_expr(&"x1".parse().unwrap());
    let v2 = egraph.add_expr(&"x2".parse().unwrap());
    let plus1 = egraph.add_expr(&"(+ x1 x2)".parse().unwrap());
    let plus2 = egraph.add_expr(&"(+ x2 x1)".parse().unwrap());
}



