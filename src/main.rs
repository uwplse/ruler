use egg::*;
use rand::{seq::SliceRandom, Rng, SeedableRng};

use std::cell::RefCell;
use std::collections::{HashMap, BTreeMap, BTreeSet};

type EGraph = egg::EGraph<Math, Sampler>;
type RecExpr = egg::RecExpr<Math>;
// type Rewrite = egg::Rewrite<Math, Sampler>;

type Constant = i32;

define_language! {
    enum Math {
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
    }
}

struct Sampler {
    n_samples: usize,
    vars: HashMap<egg::Symbol, Vec<Constant>>,
    eqs: RefCell<BTreeMap<RecExpr, BTreeSet<RecExpr>>>,
}

impl Sampler {
    fn submit_eq(&self, lhs: &RecExpr, rhs: &RecExpr) {
        if lhs == rhs {
            return
        }

        // check if seen
        if let Some(rights) = self.eqs.borrow().get(lhs) {
            if rights.contains(rhs) {
                return;
            }
        }

        println!("{} = {}", lhs, rhs);
        let mut eqs = self.eqs.borrow_mut();
        eqs.entry(lhs.clone()).or_default().insert(rhs.clone());
    }
}

#[derive(Debug)]
struct SampleData {
    depth: usize,
    expr: RecExpr,
    samples: Vec<Constant>,
}

fn mk_recexpr<'a>(node: &Math, mut mk: impl FnMut(Id) -> &'a RecExpr) -> RecExpr {
    fn build(to: &mut RecExpr, from: &[Math]) -> Id {
        let last = from.last().unwrap().clone();
        let new_node = last.map_children(|id| {
            let i = id as usize + 1;
            build(to, &from[0..i])
        });
        to.add(new_node)
    }

    let mut expr = RecExpr::default();
    let node = node
        .clone()
        .map_children(|id| build(&mut expr, mk(id).as_ref()));
    expr.add(node);
    expr
}

impl Analysis<Math> for Sampler {
    type Data = SampleData;

    fn make(egraph: &egg::EGraph<Math, Self>, enode: &Math) -> Self::Data {
        let samples = if let Math::Var(v) = enode {
            egraph.analysis.vars[v].clone()
        } else {
            let n = egraph.analysis.n_samples;
            (0..n)
                .map(|i| eval(enode, |&id| egraph[id].data.samples[i]))
                .collect()
        };

        SampleData {
            depth: 1 + enode.fold(0, |depth, id| depth.max(egraph[id].data.depth)),
            expr: mk_recexpr(enode, |id| &egraph[id].data.expr),
            samples,
        }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        assert_eq!(&to.samples, &from.samples);
        self.submit_eq(&to.expr, &from.expr);
        if from.depth < to.depth {
            *to = from;
            true
        } else {
            false
        }
    }

    fn modify(egraph: &mut egg::EGraph<Math, Self>, id: Id) {
        let mut to_union = vec![];

        let my_sample = &egraph[id].data.samples;
        let first = my_sample[0];
        if my_sample.iter().all(|&s| s == first) {
            to_union.push(egraph.add(Math::Num(first)))
        }

        let my_sample = &egraph[id].data.samples;
        for class in egraph.classes() {
            if class.id != id && &class.data.samples == my_sample {
                to_union.push(class.id)
            }
        }

        for id2 in to_union {
            egraph.union(id, id2);
        }
    }
}

fn eval(node: &Math, get: impl Fn(&Id) -> Constant) -> Constant {
    match node {
        Math::Add([a, b]) => get(a).wrapping_add(get(b)),
        Math::Mul([a, b]) => get(a).wrapping_mul(get(b)),
        Math::Num(n) => *n,
        Math::Var(v) => unreachable!("Shouldn't be asked to eval a var: {}", v),
    }
}

fn add_something(rng: &mut impl Rng, egraph: &mut EGraph) {
    let classes: Vec<_> = egraph.classes().collect();
    let max_depth = 1 + classes.iter().map(|c| c.data.depth).max().unwrap();
    macro_rules! mk {
        () => {
            classes.choose_weighted(rng, |c| max_depth - c.data.depth).unwrap().id
        };
    }
    let p: f32 = rng.gen();
    let node = match p {
        _ if p < 0.5 => Math::Add([mk!(), mk!()]),
        _ => Math::Mul([mk!(), mk!()]),
    };
    egraph.add(node);
}

fn main() {
    let n_samples = 50;
    let n_vars = 3;
    let interesting = vec![-1, 0, 1];

    assert!(n_samples > interesting.len());
    let n_to_sample = n_samples - interesting.len();

    let rng = &mut rand_pcg::Pcg64::seed_from_u64(0);

    let mut mk_samples = || -> Vec<Constant> {
        let mut samples = interesting.clone();
        samples.extend((0..n_to_sample).map(|_| rng.gen::<Constant>()));
        assert_eq!(samples.len(), n_samples);
        samples
    };

    let sampler = Sampler {
        n_samples,
        eqs: Default::default(),
        vars: (0..n_vars)
            .map(|i| (format!("x{}", i).into(), mk_samples()))
            .collect(),
    };

    let vars: Vec<_> = sampler.vars.keys().copied().collect();

    let mut egraph = EGraph::new(sampler);
    for c in &interesting {
        egraph.add(Math::Num(*c));
    }
    for var in vars {
        egraph.add(Math::Var(var));
    }

    for _ in 0..1000 {
        add_something(rng, &mut egraph);
        egraph.rebuild();
    }
}
