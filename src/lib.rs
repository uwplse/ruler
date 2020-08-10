use egg::*;
use indexmap::IndexMap;
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
        "<" = Lt([Id; 2]),
        ">" = Gt([Id; 2]),
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
            SimpleMath::Neq([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x != y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Leq([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x <= y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Geq([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x >= y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Lt([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x < y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Gt([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x > y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
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

    fn modify(_: &mut EGraph<SimpleMath, Self>, _: Id) {}
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

fn pattern_has_pred(pattern: &Pattern<SimpleMath>) -> bool {
    use SimpleMath::*;
    let mut nodes = pattern.ast.as_ref().iter();
    nodes.any(|n| match n {
        ENodeOrVar::Var(_) => false,
        ENodeOrVar::ENode(en) => matches! (en, Neq(..) | And(..) | Or(..) | Bool(_) | Gt(..) | Geq(..) | Lt(..) | Leq(..) | Not(_))
        }
    )
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
            // for now just adding 0 and 1 forcefully to the cvecs for variables
            // to test conditional rules for division
            cvec_len: self.n_samples + 2,
        });
        let rng = &mut self.rng;
        for var in &self.variables {
            let id = egraph.add(SimpleMath::Var(*var));
            let mut cvec: Vec<Option<Constant>> = (0..self.n_samples)
                .map(|_| Some(Constant::Number(rng.gen::<i32>())))
                .collect();
            cvec.push(Some(Constant::Number(0)));
            cvec.push(Some(Constant::Number(1)));
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

        let mut by_cvec_some: IndexMap<&Vec<Option<Constant>>, Vec<Id>> = IndexMap::new();

        for class in eg.classes() {
            if ids.contains(&class.id) {
                if !class.data.contains(&None) {
                    // the ids corresponding to a specific cvec key in this hash map are for normal rewrites and can be unioned.
                    by_cvec_some.entry(&class.data).or_default().push(class.id);
                }
            }
        }

        for ids in by_cvec_some.values() {
            let cross = ids.iter().flat_map(|id1| {
                ids.iter()
                    .filter_map(move |id2| if id1 > id2 { Some((id1, id2)) } else { None })
            });
            for (i, j) in cross {
                to_union.push((i.clone(), j.clone()));
                let (_cost1, expr1) = extract.find_best(i.clone());
                let (_cost2, expr2) = extract.find_best(j.clone());

                let names = &mut HashMap::default();
                let pat1 = generalize(&expr1, names);
                let pat2 = generalize(&expr2, names);
                if !pattern_has_pred(&pat1) && !pattern_has_pred(&pat2) {
                    equalities.extend(Equality::new(pat1, pat2, None));
                }
            }
        }

        let only_diff_at_none_poses =
            |(pos, (x, y)), none_poses: Vec<usize>| none_poses.contains(&pos) || x == y;

        let ec_datas: Vec<(Vec<Option<Constant>>, Id)> =
            eg.classes().cloned().map(|c| (c.data, c.id)).collect();

        let same_cvec = |v1: &Vec<Option<Constant>>, v2: &Vec<Option<Constant>>| {
            v1.iter().zip(v2.iter()).all(|(x, y)| match (x, y) {
                (Some(Constant::Boolean(true)), Some(Constant::Boolean(true))) => true,
                (Some(Constant::Boolean(false)), Some(Constant::Boolean(false))) => true,
                (_, _) => false,
            })
        };

        for &i in &ids {
            for &j in &ids {
                let mut agreement_vec: Vec<Option<Constant>> = Vec::new();
                if i != j
                    && eg[i].data != eg[j].data
                    && (eg[i].data.contains(&None) || eg[j].data.contains(&None))
                {
                    let i_nones: Vec<usize> = eg[i]
                        .data
                        .iter()
                        .zip(eg[j].data.iter())
                        .enumerate()
                        .filter(|(_, (x, y))| *x == &None || *y == &None)
                        .map(|(i, _)| i)
                        .collect();
                    if eg[i]
                        .data
                        .iter()
                        .zip(eg[j].data.iter())
                        .enumerate()
                        .all(|(i, (x, y))| only_diff_at_none_poses((i, (x, y)), i_nones.clone()))
                    {
                        for idx in 0..eg[j].data.len() {
                            if i_nones.contains(&idx) {
                                agreement_vec.push(Some(Constant::Boolean(false)));
                            } else {
                                agreement_vec.push(Some(Constant::Boolean(true)));
                            }
                        }

                        let cond = match ec_datas
                            .iter()
                            .find(|(ec_data, _)| same_cvec(&ec_data, &agreement_vec))
                        {
                            None => None,
                            Some((_, id)) => Some(extract.find_best(*id).1),
                        };
                        let (_cost1, expr1) = extract.find_best(i);
                        let (_cost2, expr2) = extract.find_best(j);

                        let names = &mut HashMap::default();
                        let pat1 = generalize(&expr1, names);
                        let pat2 = generalize(&expr2, names);
                        if cond.is_some() {
                            let c = generalize(&cond.unwrap(), names);
                            if !pattern_has_pred(&pat1)
                                && !pattern_has_pred(&pat2)
                                && pattern_has_pred(&c)
                            {
                                equalities.extend(Equality::new(pat1, pat2, Some(c)));
                            }
                        }
                    }
                }
            }
        }
        return equalities;
    }

    pub fn run(&mut self) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];
        let mut eg = self.mk_egraph();

        // number of ops in the language
        let num_ops = 13;

        for iter in 0..self.n_iter {
            let cur_ids: Vec<Id> = eg.classes().map(|c| eg.find(c.id)).collect();
            let mut op_ctr = 0;

            while op_ctr < num_ops - 0 {
                println!(
                    "iter {} phase 1: Currently there are {} eclasses",
                    iter,
                    cur_ids.len()
                );
                for &i in &cur_ids {
                    for &j in &cur_ids {
                        if op_ctr == 0 {
                            if i != j {
                                eg.add(SimpleMath::Neq([i, j]));
                            }
                        } else if op_ctr == 1 {
                            eg.add(SimpleMath::Div([i, j]));
                        } else if op_ctr == 2 {
                            eg.add(SimpleMath::Mul([i, j]));
                        } else if op_ctr == 3 {
                            eg.add(SimpleMath::Sub([i, j]));
                        } else if op_ctr == 4 {
                            eg.add(SimpleMath::Add([i, j]));
                        } else if op_ctr == 5 {
                            eg.add(SimpleMath::Not(i));
                        } else if op_ctr == 6 {
                            eg.add(SimpleMath::Leq([i, j]));
                        } else if op_ctr == 7 {
                            eg.add(SimpleMath::Geq([i, j]));
                        } else if op_ctr == 8 {
                            eg.add(SimpleMath::And([i, j]));
                        } else if op_ctr == 9 {
                            eg.add(SimpleMath::Or([i, j]));
                        } else if op_ctr == 10 {
                            eg.add(SimpleMath::Neg(i));
                        } else if op_ctr == 11 {
                            if i != j {
                                eg.add(SimpleMath::Lt([i, j]));
                            }
                        } else if op_ctr == 12 {
                            if i != j {
                                eg.add(SimpleMath::Gt([i, j]));
                            }
                        }
                        println!(
                            "iter {} phase 2: before running rules, n={}, e={}",
                            iter,
                            eg.total_size(),
                            eg.number_of_classes()
                        );

                        let mut set = HashSet::new();
                        equalities.retain(|eq| set.insert(eq.name.clone()));
                        let rules = equalities
                            .iter()
                            .filter(|eq| eq.cond == None)
                            .map(|eq| &eq.rewrite);

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
                            eg.union(i, j);
                        }

                        eg.rebuild();

                        println!(
                            "       phase 3: number of eclasses after union : {}",
                            eg.number_of_classes()
                        );
                    }
                }
                op_ctr += 1;
            }
            println!(
                "iter {} phase 3: found {} new rules",
                iter,
                equalities.len()
            );
        }
        let mut set = HashSet::new();
        equalities.retain(|eq| set.insert(eq.name.clone()));
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
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rewrite.name())
    }
}

impl<L: Language + 'static, A: Analysis<L>> Equality<L, A> {
    fn new(lhs: Pattern<L>, rhs: Pattern<L>, cond: Option<Pattern<L>>) -> Option<Self> {
        if let Some(cond) = cond {
            let name = format!("{} => {} if {}", lhs, rhs, cond);
            let applier: ConditionalApplier<ConditionEqual<Pattern<L>, Pattern<L>>, Pattern<L>> =
                ConditionalApplier {
                    applier: rhs.clone(),
                    condition: ConditionEqual(cond.clone(), "true".parse().unwrap()),
                };

            let rw = egg::Rewrite::new(name.clone(), name.clone(), lhs.clone(), applier).ok()?;

            Some(Self {
                lhs,
                rhs,
                cond: Some(cond),
                name,
                rewrite: rw,
            })
        } else {
            let name = format!("{} => {}", lhs, rhs);
            let applier = rhs.clone();
            let rw = egg::Rewrite::new(name.clone(), name.clone(), lhs.clone(), applier).ok()?;

            Some(Self {
                lhs,
                rhs,
                cond,
                name,
                rewrite: rw,
            })
        }
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
