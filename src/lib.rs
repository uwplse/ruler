use egg::*;
use indexmap::IndexMap;
use rand::Rng;
use rand_pcg::Pcg64;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    time::Duration,
};

type Constant = i32;

define_language! {
    pub enum SimpleMath {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "~" = Neg(Id),
        // "/" = Div([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
    }
}

#[derive(Default, Clone)]
pub struct SynthAnalysis {
    n_samples: usize,
    my_ids: BTreeSet<Id> 
}

impl Analysis<SimpleMath> for SynthAnalysis {
    // doesnt need to be option
    type Data = Vec<Constant>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        // only do assertions on non-empty vecs
        // there may be "bad" merges during minimization, that's ok
        if !to.is_empty() && !from.is_empty() {
            assert_eq!(to, &from);
        }
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
            SimpleMath::Neg(a) => x(a).map(|x| -x).collect(),
            // SimpleMath::Div([a, b]) => x(a).zip(x(b)).map(|(x, y)| x / y).collect(),
        }
    }

    fn modify(egraph: &mut EGraph<SimpleMath, Self>, id: Id) {
        let cv = &egraph[id].data;
        if cv.is_empty() {
            return;
        }
        let first = cv[0];
        if cv.iter().all(|x| *x == first) {
            let added = egraph.add(SimpleMath::Num(first));
            egraph.union(id, added);
        }

        // pruning seems to be pretty important for avoiding silly associativity cycles
        // if egraph[id].iter().any(|n| n.is_leaf()) {
        //     egraph[id].nodes.retain(|n| n.is_leaf())
        // }
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
    pub rng: Pcg64,
    pub n_iter: usize,
    pub n_samples: usize,
    pub variables: Vec<egg::Symbol>,
    pub consts: Vec<Constant>,
}

impl SynthParam {
    fn mk_egraph(&mut self) -> EGraph<SimpleMath, SynthAnalysis> {
        let mut egraph = EGraph::new(SynthAnalysis {
            n_samples: self.n_samples,
            my_ids: Default::default(),
        });
        let rng = &mut self.rng;
        for var in &self.variables {
            let id = egraph.add(SimpleMath::Var(*var));
            egraph[id].data = (0..self.n_samples).map(|_| rng.gen::<Constant>()).collect();
        }
        for c in &self.consts {
            egraph.add(SimpleMath::Num(*c));
        }
        egraph
    }

    pub fn run(&mut self) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        // self.run_with_eqs(vec![])
        let consts = std::mem::take(&mut self.consts);

        println!("Finding variable only equalities...");
        let var_only_eqs = self.run_with_eqs(vec![]);

        println!("Finding equalities with constants...");
        self.n_iter = 1;
        self.consts = consts;
        self.run_with_eqs(var_only_eqs)
    }

    pub fn run_with_eqs(
        &mut self,
        mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>>,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let mut eg = self.mk_egraph();

        // we will only operate on the ids that we added
        let mut my_ids: BTreeSet<Id> = eg.classes().map(|c| c.id).collect();

        for iter in 0..self.n_iter {
            my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();

            println!(
                "iter {} phase 1: adding ops over {} eclasses",
                iter,
                my_ids.len(),
            );
            let mut enodes_to_add = vec![];
            for &i in &my_ids {
                // enodes_to_add.push(SimpleMath::Neg(i));
                for &j in &my_ids {
                    enodes_to_add.push(SimpleMath::Add([i, j]));
                    // enodes_to_add.push(SimpleMath::Sub([i, j]));
                    enodes_to_add.push(SimpleMath::Mul([i, j]));
                }
            }
            for enode in enodes_to_add {
                my_ids.insert(eg.add(enode));
            }

            loop {
                eg.rebuild();
                println!(
                    "iter {} phase 2: running rules, n={}, e={}",
                    iter,
                    eg.total_size(),
                    eg.number_of_classes()
                );
                let rules = equalities.iter().map(|eq| &eq.no_add_rewrite);
                // let rules = equalities.iter().map(|eq| &eq.rewrite);
                let runner: Runner<SimpleMath, SynthAnalysis, ()> =
                    Runner::new(eg.analysis.clone()).with_egraph(eg);
                eg = runner
                    .with_time_limit(Duration::from_secs(20))
                    .with_node_limit(usize::MAX)
                    .with_iter_limit(100)
                    .with_scheduler(SimpleScheduler)
                    .run(rules)
                    .egraph;
                println!(
                    "       phase 2: running rules, n={}, e={}",
                    eg.total_size(),
                    eg.number_of_classes()
                );

                my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();
                eg.analysis.my_ids = my_ids.clone();

                println!("iter {} phase 3: discover rules", iter);
                println!("       phase 3: grouping");
                let mut by_cvec: IndexMap<&[Constant], Vec<Id>> = IndexMap::new();
                for class in eg.classes() {
                    if my_ids.contains(&class.id) {
                        by_cvec.entry(&class.data).or_default().push(class.id);
                    }
                }

                let pattern_cost = |pat: &RecExpr<SimpleMath>| {
                    let mut n_consts = 0;
                    let mut vars = HashSet::new();
                    for node in pat.as_ref() {
                        match node {
                            SimpleMath::Num(..) => n_consts += 1,
                            SimpleMath::Var(v) => {
                                vars.insert(v);
                            }
                            _ => (),
                        }
                    }
                    (-(vars.len() as isize), n_consts, pat.as_ref().len())
                };

                println!("       phase 3: scanning {} groups", by_cvec.len());
                let mut extract = Extractor::new(&eg, AstSize);

                let best = by_cvec
                    .values_mut()
                    .filter(|ids| ids.len() > 1)
                    .map(|ids| {
                        let mut extracted: Vec<_> = ids
                            .iter()
                            .map(|i| {
                                let expr = extract.find_best(*i).1;
                                (pattern_cost(&expr), *i, expr)
                            })
                            .collect();
                        extracted.sort_by(|a, b| a.0.cmp(&b.0).reverse());
                        // return the two cheapest things
                        (extracted.pop().unwrap(), extracted.pop().unwrap())
                    })
                    .min_by_key(|((cost1, _, _), (cost2, _, _))| {
                        // cost1.max(cost2).clone()
                        (cost1.0 + cost2.0, cost1.1 + cost2.1, cost1.2 + cost2.2)
                    });

                if let Some(((_, id1, expr1), (_, id2, expr2))) = best {
                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr1, names);
                    let pat2 = generalize(&expr2, names);
                    if let Some(eq) = Equality::new(pat1, pat2) {
                        if equalities.iter().all(|e| e.name != eq.name) {
                            equalities.push(eq)
                        }
                    }

                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr2, names);
                    let pat2 = generalize(&expr1, names);
                    if let Some(eq) = Equality::new(pat1, pat2) {
                        if equalities.iter().all(|e| e.name != eq.name) {
                            equalities.push(eq)
                        }
                    }

                    eg.union(id1, id2);
                } else {
                    break;
                }
            }

            println!("After iter {}, I know these equalities:", iter);
            for eq in &equalities {
                println!("  {}", eq);
            }
        }
        equalities
    }
}

struct FilterSearcher<F, S> {
    ecfilter: F,
    searcher: S,
}


impl<F, S, L, A> egg::Searcher<L, A> for FilterSearcher<F, S>
where
    L: Language,
    A: Analysis<L>,
    S: Searcher<L, A>,
    F: Fn(&EGraph<L, A>, Id) -> bool,
{
    fn search_eclass(&self, egraph: &EGraph<L, A>, eclass: Id) -> Option<SearchMatches> {
        if (self.ecfilter)(egraph, eclass) {
            self.searcher.search_eclass(egraph, eclass)
        } else {
            None
        }
    }

    fn vars(&self) -> Vec<Var> {
        self.searcher.vars()
    }
}


pub struct Equality<L, A> {
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    pub name: String,
    pub rewrite: egg::Rewrite<L, A>,
    pub no_add_rewrite: egg::Rewrite<L, A>,
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rewrite.name())
    }
}

impl Equality<SimpleMath, SynthAnalysis> {
    fn new(lhs: Pattern<SimpleMath>, rhs: Pattern<SimpleMath>) -> Option<Self> {
        let name = format!("{} => {}", lhs, rhs);

        let f = |eg: &EGraph<_, SynthAnalysis>, id| eg.analysis.my_ids.contains(&id);
            let searcher = FilterSearcher {
                ecfilter: f,
                searcher: lhs.clone(),
            };

        let rw = egg::Rewrite::new(name.clone(), name.clone(), lhs.clone(), rhs.clone()).ok()?;

        let no_add_rhs = NoAddPatternApplier(rhs.clone());
        let no_add =
            egg::Rewrite::new(name.clone(), name.clone(), lhs.clone(), no_add_rhs).unwrap();

        Some(Self {
            lhs,
            rhs,
            rewrite: rw,
            no_add_rewrite: no_add,
            name,
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
            consts: vec![-1, 0, 1],
        };

        let eqs = param.run();

        check_proves(&eqs, "(+ 0 a)", "a");
        check_proves(&eqs, "(+ a b)", "(+ b a)");
    }
}
