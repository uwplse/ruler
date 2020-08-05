use egg::*;
use rand::{prelude::SliceRandom, Rng};
use rand_pcg::Pcg64;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fmt::Formatter,
    time::Duration,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Constant {
    Number(i32),
    Boolean(bool),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", n),
            Constant::Boolean(b) => write!(f, "{}", b),
        }
    }
}

impl std::str::FromStr for Constant {
    type Err = String;
    fn from_str(s: &str) -> Result<Constant, Self::Err> {
        match s {
            "True" | "true" => Ok(Constant::Boolean(true)),
            "False" | "false" => Ok(Constant::Boolean(false)),
            _ => {
                if let Ok(n) = s.parse::<i32>() {
                    Ok(Constant::Number(n))
                } else {
                    Err(format!("'{}' is not a valid value for Constant", s))
                }
            }
        }
    }
}

define_language! {
    pub enum SimpleMath {
        "<>" = Neq([Id; 2]),
        "<=" = Leq([Id; 2]),
        ">=" = Geq([Id; 2]),
        "==" = Eq([Id; 2]),
        "<" = Le([Id; 2]),
        ">" = Ge([Id; 2]),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "~" = Neg(Id),
        "!" = Not(Id),
        Bool(Constant),
        Num(Constant),
        Var(egg::Symbol),
    }
}

#[derive(Default, Clone)]
pub struct SynthAnalysis {
    cvec_len: usize,
}

impl Analysis<SimpleMath> for SynthAnalysis {
    type Data = Vec<Option<Constant>>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        // only do assertions on non-empty vecs
        if !to.is_empty() && !from.is_empty() {
            assert_eq!(to, &from);
        }
        false
    }

    fn make(egraph: &EGraph<SimpleMath, Self>, enode: &SimpleMath) -> Self::Data {
        let x = |i: &Id| egraph[*i].data.iter().copied();
        let params = &egraph.analysis;
        match enode {
            SimpleMath::Num(n) => (0..params.cvec_len).map(|_| Some(*n)).collect(),
            SimpleMath::Bool(b) => (0..params.cvec_len).map(|_| Some(*b)).collect(),
            SimpleMath::Var(_) => vec![],
            SimpleMath::Not(a) => x(a)
                .map(|x| match x {
                    Some(Constant::Boolean(b)) => Some(Constant::Boolean(!b)),
                    _ => None,
                })
                .collect(),
            SimpleMath::Neg(a) => x(a)
                .map(|x| match x {
                    Some(Constant::Number(n)) => Some(Constant::Number(-n)),
                    _ => None,
                })
                .collect(),
            SimpleMath::And([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                        Some(Constant::Boolean(b1 && b2))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Or([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                        Some(Constant::Boolean(b1 || b2))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Eq([a, b]) => {
                if x(a).zip(x(b)).all(|(x, y)| x == y) {
                    vec![Some(Constant::Boolean(true)); x(a).len()]
                } else {
                    vec![Some(Constant::Boolean(false)); x(a).len()]
                }
            }
            SimpleMath::Neq([a, b]) => {
                if x(a).zip(x(b)).any(|(x, y)| x != y) {
                    vec![Some(Constant::Boolean(true)); x(a).len()]
                } else {
                    vec![Some(Constant::Boolean(false)); x(a).len()]
                }
            }
            SimpleMath::Leq([a, b]) => {
                if x(a).zip(x(b)).all(|(x, y)| x <= y) {
                    vec![Some(Constant::Boolean(true)); x(a).len()]
                } else {
                    vec![Some(Constant::Boolean(false)); x(a).len()]
                }
            }
            SimpleMath::Geq([a, b]) => {
                if x(a).zip(x(b)).all(|(x, y)| x >= y) {
                    vec![Some(Constant::Boolean(true)); x(a).len()]
                } else {
                    vec![Some(Constant::Boolean(false)); x(a).len()]
                }
            }
            SimpleMath::Le([a, b]) => {
                if x(a).zip(x(b)).all(|(x, y)| x < y) {
                    vec![Some(Constant::Boolean(true)); x(a).len()]
                } else {
                    vec![Some(Constant::Boolean(false)); x(a).len()]
                }
            }
            SimpleMath::Ge([a, b]) => {
                if x(a).zip(x(b)).all(|(x, y)| x > y) {
                    vec![Some(Constant::Boolean(true)); x(a).len()]
                } else {
                    vec![Some(Constant::Boolean(false)); x(a).len()]
                }
            }
            SimpleMath::Add([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        Some(Constant::Number(n1.wrapping_add(n2)))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Sub([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        Some(Constant::Number(n1.wrapping_sub(n2)))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Mul([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        Some(Constant::Number(n1.wrapping_mul(n2)))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Div([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        if n2 != 0 {
                            Some(Constant::Number(n1 / n2))
                        } else {
                            None
                        }
                    }
                    (_, _) => None,
                })
                .collect(),
        }
    }

    fn modify(egraph: &mut EGraph<SimpleMath, Self>, id: Id) {}
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
    pub rng: Pcg64,
    pub n_iter: usize,
    pub n_samples: usize,
    pub variables: Vec<egg::Symbol>,
    pub consts: Vec<Constant>,
}

impl SynthParam {
    fn mk_egraph(&mut self) -> EGraph<SimpleMath, SynthAnalysis> {
        let mut egraph = EGraph::new(SynthAnalysis {
            // for now just adding 0 forcefully to the cvecs for variables
            // to test conditional rules for division
            cvec_len: self.n_samples + 1,
        });
        let rng = &mut self.rng;
        for var in &self.variables {
            let id = egraph.add(SimpleMath::Var(*var));
            let mut cvec: Vec<Option<Constant>> = (0..self.n_samples)
                .map(|_| Some(Constant::Number(rng.gen::<i32>())))
                .collect();
            cvec.push(Some(Constant::Number(0)));
            cvec.shuffle(rng);
            egraph[id].data = cvec;
        }
        for n in &self.consts {
            egraph.add(SimpleMath::Num(*n));
        }
        egraph
    }

    fn learn_rules(
        &self,
        eg: &EGraph<SimpleMath, SynthAnalysis>,
        mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>>,
        to_union: &mut std::vec::Vec<(egg::Id, egg::Id)>,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let mut extract = Extractor::new(&eg, AstSize);
        let ids: Vec<Id> = eg.classes().map(|c| eg.find(c.id)).collect();
        for &i in &ids {
            for &j in &ids {
                if i < j && eg[i].data == eg[j].data {
                    to_union.push((i, j));
                    let (_cost1, expr1) = extract.find_best(i);
                    let (_cost2, expr2) = extract.find_best(j);

                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr1, names);
                    let pat2 = generalize(&expr2, names);
                    equalities.extend(Equality::new(pat1, pat2, None));
                    println!("new rule: {} => {}", &expr2, &expr1);
                } else if i < j && eg[i].data != eg[j].data {
                    let diff_idxs: Vec<usize> = eg[i]
                        .data
                        .iter()
                        .zip(eg[j].data.iter())
                        .enumerate()
                        .filter(|(_, (x, y))| x != y)
                        .map(|(i, _)| i)
                        .collect();
                    let mut same_idxs: Vec<usize> = vec![];
                    for i in 0..eg[j].data.len() {
                        if !diff_idxs.contains(&i) {
                            same_idxs.push(i)
                        }
                    }
                    let ec_datas: Vec<(Id, Vec<Option<Constant>>)> =
                        eg.classes().cloned().map(|c| (c.id, c.data)).collect();

                    let pred = |ec_data: &Vec<Option<Constant>>| {
                        diff_idxs
                            .iter()
                            .all(|i| ec_data[*i] == Some(Constant::Boolean(true)))
                            && same_idxs
                                .iter()
                                .all(|i| ec_data[*i] == Some(Constant::Boolean(false)))
                    };

                    let cond = match ec_datas.iter().find(|(_, ec_data)| pred(ec_data)) {
                        None => None,
                        Some((id, _)) => Some(extract.find_best(*id).1),
                    };

                    let (_cost1, expr1) = extract.find_best(i);
                    let (_cost2, expr2) = extract.find_best(j);

                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr1, names);
                    let pat2 = generalize(&expr2, names);

                    if cond.is_none() {
                        equalities.extend(Equality::new(pat1, pat2, None));
                        println!("new rule: {} => {}", &expr2, &expr1);
                    } else {
                        let c = generalize(&cond.unwrap(), names);
                        println!("new rule: {} => {} if {}", &expr2, &expr1, &c);
                        equalities.extend(Equality::new(pat1, pat2, Some(c)));
                    }
                }
            }
        }
        return equalities;
    }

    pub fn run(&mut self) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];
        let mut eg = self.mk_egraph();

        for iter in 0..self.n_iter {
            let cur_ids: Vec<Id> = eg.classes().map(|c| eg.find(c.id)).collect();

            let num_ops = 4;
            let mut op_ctr = 0;

            while op_ctr < (num_ops - 3) {
                println!(
                    "iter {} phase 1: Currently there are {} eclasses",
                    iter,
                    cur_ids.len()
                );
                for &i in &cur_ids {
                    for &j in &cur_ids {
                        if op_ctr == 0 {
                            let mut extract = Extractor::new(&eg, AstSize);
                            let (_cost1, expr1) = extract.find_best(i);
                            let (_cost2, expr2) = extract.find_best(j);
                            println!("Adding neq over enodes {} {}", expr1, expr2);
                            eg.add(SimpleMath::Neq([i, j]));
                            println!("Adding div over enodes {} {}", expr1, expr2);
                            eg.add(SimpleMath::Div([i, j]));
                        } else if op_ctr == 1 {
                            eg.add(SimpleMath::Add([i, j]));
                        } else if op_ctr == 2 {
                            eg.add(SimpleMath::Mul([i, j]));
                        } else {
                            eg.add(SimpleMath::Sub([i, j]));
                        }

                        println!(
                            "iter {} phase 2: before running rules, n={}, e={}",
                            iter,
                            eg.total_size(),
                            eg.number_of_classes()
                        );

                        let mut set = HashSet::new();
                        equalities.retain(|eq| set.insert(eq.name.clone()));
                        let rules = equalities.iter().map(|eq| &eq.no_add_rewrite);

                        eg.rebuild();
                        let runner: Runner<SimpleMath, SynthAnalysis, ()> =
                            Runner::new(eg.analysis.clone()).with_egraph(eg);

                        eg = runner
                            .with_time_limit(Duration::from_secs(20))
                            .with_node_limit(usize::MAX)
                            .with_iter_limit(100)
                            .with_scheduler(SimpleScheduler)
                            .run(rules)
                            .egraph;

                        eg.rebuild();

                        println!(
                            "       phase 2: after running {} rules, n={}, e={}",
                            &equalities.len(),
                            eg.total_size(),
                            eg.number_of_classes()
                        );

                        println!("iter {} phase 3: discover rules", iter);

                        let mut to_union = vec![];

                        equalities = self.learn_rules(&eg, equalities, &mut to_union);

                        println!("       phase 3: performing {} unions", to_union.len());
                        for (i, j) in to_union {
                            let mut extr = Extractor::new(&eg, AstSize);
                            let (c1, n1) = extr.find_best(i);
                            let (c2, n2) = extr.find_best(j);
                            println!("unioning {}, {}", n1, n2);

                            eg.union(i, j);
                        }

                        eg.rebuild();

                        println!(
                            "       phase 3: number of eclasses after union : {}",
                            eg.number_of_classes()
                        );
                    }
                }

                let mut set = HashSet::new();
                equalities.retain(|eq| set.insert(eq.name.clone()));
                op_ctr += 1;
            }

            let mut set = HashSet::new();
            equalities.retain(|eq| set.insert(eq.name.clone()));
            println!(
                "iter {} phase 3: found {} new rules",
                iter,
                equalities.len()
            );
        }
        println!("Overall found the following {} rules", equalities.len());
        for eq in &equalities {
            println!("{}", eq);
        }
        equalities
    }
}

pub struct Equality<L, A> {
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    pub cond: Option<Pattern<L>>,
    pub name: String,
    pub rewrite: egg::Rewrite<L, A>,
    pub no_add_rewrite: egg::Rewrite<L, A>,
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rewrite.name())
    }
}

impl<L: Language + 'static, A: Analysis<L>> Equality<L, A> {
    fn new(lhs: Pattern<L>, rhs: Pattern<L>, cond: Option<Pattern<L>>) -> Option<Self> {
        let name = if cond == None {
            format!("{} => {}", lhs, rhs)
        } else {
            format!("{} => {} if {}", lhs, rhs, &cond.as_ref().unwrap())
        };

        let rw = egg::Rewrite::new(name.clone(), name.clone(), lhs.clone(), rhs.clone()).ok()?;

        let no_add_rhs = NoAddPatternApplier(rhs.clone());
        let no_add =
            egg::Rewrite::new(name.clone(), name.clone(), lhs.clone(), no_add_rhs).unwrap();

        Some(Self {
            lhs,
            rhs,
            cond,
            name,
            rewrite: rw,
            no_add_rewrite: no_add,
        })
    }
}

struct NoAddPatternApplier<L>(Pattern<L>);

impl<L, A> egg::Applier<L, A> for NoAddPatternApplier<L>
where
    L: Language,
    A: Analysis<L>,
{
    fn apply_one(&self, egraph: &mut EGraph<L, A>, _eclass: Id, subst: &Subst) -> Vec<Id> {
        let mut so_far: Vec<Id> = vec![];
        for node in self.0.ast.as_ref() {
            let id = match node {
                ENodeOrVar::ENode(n) => {
                    match egraph.lookup(n.clone().map_children(|i| so_far[usize::from(i)])) {
                        Some(id) => id,
                        None => return vec![],
                    }
                }
                ENodeOrVar::Var(v) => subst[*v],
            };
            so_far.push(id);
        }

        vec![*so_far.last().unwrap()]
    }

    fn vars(&self) -> Vec<Var> {
        self.0.vars()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::SeedableRng;

    fn check_proves<L, A>(eqs: &[Equality<L, A>], a: &str, b: &str)
    where
        L: Language,
        A: Analysis<L> + Default,
    {
        let rules = eqs.iter().map(|eq| &eq.rewrite);
        let runner: Runner<L, A, ()> = Runner::default()
            .with_expr(&a.parse().unwrap())
            .with_expr(&b.parse().unwrap())
            .with_hook(|runner| {
                if runner.egraph.find(runner.roots[0]) == runner.egraph.find(runner.roots[1]) {
                    Err(format!("Done early"))
                } else {
                    Ok(())
                }
            })
            .run(rules);

        let id_a = runner.egraph.find(runner.roots[0]);
        let id_b = runner.egraph.find(runner.roots[1]);

        if id_a != id_b {
            panic!("Failed to simplify {} => {}", a, b)
        }
    }

    #[test]
    fn super_simple() {
        let mut param = SynthParam {
            rng: SeedableRng::seed_from_u64(5),
            n_iter: 1,
            n_samples: 25,
            variables: vec!["x".into(), "y".into(), "z".into()],
            consts: vec![Constant::Number(0), Constant::Number(1)],
        };

        let eqs = param.run();

        check_proves(&eqs, "(+ 0 a)", "a");
        check_proves(&eqs, "(+ a b)", "(+ b a)");
    }
}
