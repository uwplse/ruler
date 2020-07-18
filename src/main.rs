use egg::*;
use rand::{seq::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;
use std::{collections::HashMap, fmt::Display};

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

#[derive(Default, Clone)]
pub struct SynthAnalysis {
    n_samples: usize,
}

impl Analysis<SimpleMath> for SynthAnalysis {
    // doesnt need to be option
    type Data = Vec<Constant>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        assert_eq!(to, &from);
        false
    }

    fn make(egraph: &EGraph<SimpleMath, Self>, enode: &SimpleMath) -> Self::Data {
        let x = |i: &Id| egraph[*i].data.iter().copied();
        let params = &egraph.analysis;
        match enode {
            SimpleMath::Num(n) => (0..params.n_samples).map(|_| *n).collect(),
            SimpleMath::Var(_) => vec![],
            SimpleMath::Add([a, b]) => x(a).zip(x(b)).map(|(x, y)| x.wrapping_add(y)).collect(),
            SimpleMath::Sub([a, b]) => x(a).zip(x(b)).map(|(x, y)| x.wrapping_sub(y)).collect(),
            SimpleMath::Mul([a, b]) => x(a).zip(x(b)).map(|(x, y)| x.wrapping_mul(y)).collect(),
            // SimpleMath::Div([a, b]) => x(a).zip(x(b)).map(|(x, y)| x / y).collect(),
        }
    }

    fn modify(egraph: &mut EGraph<SimpleMath, Self>, id: Id) {
        let cv = &egraph[id].data;
        if cv.is_empty() {
            return;
        }
        if cv.iter().all(|x| *x == cv[0]) {
            let added = egraph.add(SimpleMath::Num(cv[0]));
            egraph.union(id, added);
        }
    }
}

fn generalize(expr: &RecExpr<SimpleMath>, map: &mut HashMap<Symbol, Var>) -> Pattern<SimpleMath> {
    let alpha = b"abcdefghijklmnopqrstuvwxyz";
    let nodes: Vec<_> = expr
        .as_ref()
        .iter()
        .map(|n| match n {
            SimpleMath::Var(sym) => {
                let var = if let Some(var) = map.get(&sym) {
                    *var
                } else {
                    let var = format!("?{}", alpha[map.len()] as char).parse().unwrap();
                    map.insert(*sym, var);
                    var
                };
                ENodeOrVar::Var(var)
            }
            n => ENodeOrVar::ENode(n.clone()),
        })
        .collect();

    Pattern::from(PatternAst::from(nodes))
}

pub struct SynthParam {
    rng: Pcg64,
    n_iter: usize,
    n_samples: usize,
    variables: Vec<egg::Symbol>,
    consts: Vec<Constant>,
}

impl SynthParam {
    fn mk_egraph(&mut self) -> EGraph<SimpleMath, SynthAnalysis> {
        let mut egraph = EGraph::new(SynthAnalysis {
            n_samples: self.n_samples,
        });
        let rng = &mut self.rng;
        for var in &self.variables {
            let id = egraph.add(SimpleMath::Var(*var));
            egraph[id].data = (0..self.n_samples).map(|_| rng.gen::<Constant>()).collect();
        }
        for c in &self.consts {
            let id = egraph.add(SimpleMath::Num(*c));
        }
        egraph
    }
    fn run(&mut self) {
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];
        let mut eg = self.mk_egraph();
        for iter in 0..self.n_iter {
            // part 1: add an operator to the egraph with all possible children
            let mut enodes_to_add = vec![];
            for i in eg.classes() {
                for j in eg.classes() {
                    enodes_to_add.push(SimpleMath::Add([i.id, j.id]));
                }
            }
            for enode in enodes_to_add {
                eg.add(enode);
            }

            // part 2: run the current rules.
            let rules = equalities.iter().flat_map(|eq| &eq.rewrites);
            let runner: Runner<SimpleMath, SynthAnalysis, ()> =
                Runner::new(eg.analysis.clone()).with_egraph(eg);
            eg = runner.run(rules).egraph;

            // part 3: discover rules
            let ids: Vec<Id> = eg.classes().map(|c| c.id).collect();
            let mut extract = Extractor::new(&eg, AstSize);
            let mut to_union = vec![];
            for &i in &ids {
                for &j in &ids {
                    if i < j && eg[i].data == eg[j].data {
                        to_union.push((i, j));
                        let (_cost1, expr1) = extract.find_best(i);
                        let (_cost2, expr2) = extract.find_best(j);

                        let names = &mut HashMap::default();
                        let pat1 = generalize(&expr1, names);
                        let pat2 = generalize(&expr2, names);

                        if let Some(eq) = Equality::new(pat1, pat2) {
                            println!("Learning {}", eq);
                            if equalities.iter().find(|eq2| eq.name == eq2.name).is_none() {
                                equalities.push(eq);
                            }
                        }
                    }
                }
            }

            println!("After iter {}, I know these equalities:", iter);
            for eq in &equalities {
                println!("  {}", eq);
            }
        }
    }
}

struct Equality<L, A> {
    lhs: Pattern<L>,
    rhs: Pattern<L>,
    name: String,
    rewrites: Vec<egg::Rewrite<L, A>>,
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<L: Language + 'static, A: Analysis<L>> Equality<L, A> {
    fn new(lhs: Pattern<L>, rhs: Pattern<L>) -> Option<Self> {
        let mut rewrites = vec![];

        let name = format!("{} => {}", lhs, rhs);
        if let Ok(rw) = egg::Rewrite::new(name.clone(), name, lhs.clone(), rhs.clone()) {
            rewrites.push(rw)
        }

        let name = format!("{} => {}", rhs, lhs);
        if let Ok(rw) = egg::Rewrite::new(name.clone(), name, rhs.clone(), lhs.clone()) {
            rewrites.push(rw)
        }

        let name = match rewrites.len() {
            1 => format!("{}", rewrites[0].long_name()),
            2 => format!("{} <=> {}", lhs, rhs),
            n => panic!("unexpected len {}", n),
        };

        if rewrites.is_empty() {
            None
        } else {
            Some(Self {
                rewrites,
                lhs,
                rhs,
                name,
            })
        }
    }
}

fn main() {
    let mut param = SynthParam {
        rng: SeedableRng::seed_from_u64(5),
        n_iter: 2,
        n_samples: 25,
        variables: vec!["x".into(), "y".into(), "z".into()],
        consts: vec![0, 1],
    };
    param.run()
}
