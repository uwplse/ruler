use egg::*;

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Sample<T, C> {
    fn get_random_vec(&mut self) -> Vec<C>;
}

pub trait Synthesizer<L, A>
where L: Language + 'static, A: Analysis<L> + Clone {
    type CharacteristicVector: Hash + PartialEq + Eq + Clone + Debug + PartialOrd + Ord;

    fn value_to_node(val: &Self::CharacteristicVector) -> Option<L>;
    fn symbol_to_node(sym: Symbol) -> L;
    fn node_to_symbol(node: &L) -> Option<Symbol>;

    fn make_node(&mut self, egraph: &EGraph<L, A>) -> L;
    fn initial_egraph(&mut self, params: &SynthesisParams<L, A>) -> EGraph<L, A>;
    fn eval(&mut self, enode: &L, egraph: &EGraph<L, A>, values: &HashMap<Id, Self::CharacteristicVector>) -> Self::CharacteristicVector;

    fn instantiate(pattern: &Pattern<L>) -> RecExpr<L> {
        let nodes: Vec<_> = pattern.ast.as_ref().iter()
            .map(|n| match n {
                ENodeOrVar::ENode(n) => n.clone(),
                ENodeOrVar::Var(v) => {
                    let s = v.to_string();
                    assert!(s.starts_with('?'));
                    Self::symbol_to_node(s[1..].into())
                }
            })
            .collect();

        RecExpr::from(nodes)
    }

    fn generalize(expr: &RecExpr<L>, map: &mut HashMap<Symbol, Var>) -> Pattern<L> {
        let alpha = b"abcdefghijklmnopqrstuvwxyz";
        let nodes: Vec<_> = expr.as_ref().iter()
            .map(|n| match Self::node_to_symbol(n) {
                Some(sym) => {
                    let var = if let Some(var) = map.get(&sym) {
                        *var
                    } else {
                        let var = format!("?{}", alpha[map.len()] as char).parse().unwrap();
                        map.insert(sym, var);
                        var
                    };
                    ENodeOrVar::Var(var)
                }
                None => ENodeOrVar::ENode(n.clone()),
            })
            .collect();

        Pattern::from(PatternAst::from(nodes))
    }

    fn minimize_equalities(&mut self, analysis: A, equalities: &mut Vec<Equality<L, A>>) -> Vec<Equality<L, A>> {
        let mut removed = vec![];
        'outer: while equalities.len() > 1 {
            for (i, eq) in equalities.iter().enumerate() {
                let other_eqs = equalities
                    .iter()
                    .enumerate()
                    .filter(|(j, _)| i != *j)
                    .flat_map(|(_, eq)| &eq.rewrites);
                println!("minimizing");
                let runner: Runner<L, A, ()> = Runner::new(analysis.clone())
                    .with_expr(&Self::instantiate(&eq.lhs))
                    .with_expr(&Self::instantiate(&eq.rhs))
                    .with_node_limit(3000)
                    .with_iter_limit(3)
                    .run(other_eqs);
                let are_same = |a, b| runner.egraph.find(a) == runner.egraph.find(b);
                if are_same(runner.roots[0], runner.roots[1]) {
                    println!("Removing {}", eq);
                    removed.push(equalities.remove(i));
                    continue 'outer;
                }
            }

            break;
        }

        return removed;
    }

    fn run(&mut self, mut params: SynthesisParams<L, A>) -> Vec<Equality<L, A>> {
        let mut egraph = self.initial_egraph(&params);
        let mut values: HashMap<Id, Self::CharacteristicVector> = Default::default();

        // initialize values map
        for class in egraph.classes() {
            if !values.contains_key(&class.id) {
                // TODO passing the egraph here is a hack
                let val = self.eval(&class.nodes[0], &egraph, &values);
                values.insert(class.id, val);
            }
        }

        for iter in 0..params.iterations {
            println!("Iteration {}, known rules", iter);
            for eq in &params.eqs {
                println!("  {}", eq);
            }

            // update id_to_val based on the egraph modifications
            values = values.into_iter().map(|(id, val)| (egraph.find(id), val)).collect();

            for class in egraph.classes() {
                if !values.contains_key(&class.id) {
                    let node = class
                        .iter()
                        .find(|n| n.children().iter().all(|id| values.contains_key(&egraph.find(*id))))
                        .unwrap_or_else(|| panic!("failed to find node for class {:?}", class));
                    let val = self.eval(node, &egraph, &values);
                    values.insert(class.id, val);
                }
            }

            let mut ctr = 0;
            // create the new nodes before adding to make sure they refer to existing nodes
            let to_add: Vec<(L, Self::CharacteristicVector)> = (0..params.additions_per_iteration)
                .map(|_| {
                    let n = self.make_node(&egraph);
                    println!("{}, evaling {:?}",ctr, n);
                    let val = self.eval(&n, &egraph, &values);
                    ctr = ctr + 1;
                    (n, val)
                })
                .collect();

            for (n, val) in to_add {
                let n2 = Self::value_to_node(&val);
                let id = egraph.add(n);
                if let Some(n2) = n2 {
                    let added = egraph.add(n2);
                    egraph.union(id, added);
                }
                values.insert(id, val);
            }
            egraph.rebuild();

            for class in egraph.classes() {
                if !values.contains_key(&class.id) {
                    let node = class
                        .iter()
                        .find(|n| n.children().iter().all(|id| values.contains_key(&egraph.find(*id))))
                        .unwrap_or_else(|| panic!("failed to find node for class {:?}", class));
                    let val = self.eval(node, &egraph, &values);
                    values.insert(class.id, val);
                }
            }

            // HACK with_egraph should create the runner
            egraph = Runner::<L, A, ()>::new(egraph.analysis.clone())
                .with_egraph(egraph)
                .with_iter_limit(3)
                .run(params.rewrites())
                .egraph;
            egraph.rebuild();

            // update id_to_val based on the egraph modifications
            values = values
                .into_iter()
                .map(|(id, sig)| (egraph.find(id), sig))
                .collect();

            // group things by value
            let mut groups: HashMap<Self::CharacteristicVector, Vec<Id>> = Default::default();
            for (id, val) in values.drain() {
                assert_eq!(id, egraph.find(id));
                groups.entry(val).or_default().push(egraph.find(id));
            }
            assert!(groups.len() <= egraph.number_of_classes());

            let mut ext = Extractor::new(&egraph, AstSize);
            for ids in groups.values_mut() {
                ids.sort();
                ids.dedup();
                let len = ids.len();

                for i in 0..len-1 {
                    let j = i + 1;
                    // for j in i + 1..len {
                        let var_map = &mut HashMap::default();
                        let pat1 = Self::generalize(&ext.find_best(ids[i]).1, var_map);
                        let pat2 = Self::generalize(&ext.find_best(ids[j]).1, var_map);
                        if let Some(eq) = Equality::new(pat1, pat2) {
                            println!("Learning {}", eq);
                            params.eqs.push(eq);
                        }
                    // }
                }
            }

            for (val, ids) in groups {
                for &id in &ids {
                    assert_eq!(id, egraph.find(id));
                }
                if ids.len() > 1 {
                    println!("Unioning {:?}", ids)
                }
                let id = fold1(ids, |x, y| egraph.union(x, y).0);
                let old = values.insert(egraph.find(id), val);
                assert!(old.is_none());
            }
            egraph.rebuild();
            // assert_eq!(values.len(), egraph.number_of_classes(), "here2");

            println!("Minimizing {} equalities...", params.eqs.len());
            self.minimize_equalities(egraph.analysis.clone(), &mut params.eqs);
            println!("Minimized to {} equalities", params.eqs.len());
        }

        println!("Final equalities");
        for eq in &params.eqs {
            println!("  {}", eq);
        }
        params.eqs
    }
}


fn fold1<T, F>(iter: impl IntoIterator<Item = T>, f: F) -> T
where F: FnMut(T, T) -> T,
{
    let mut iter = iter.into_iter();
    let first = iter.next().expect("can't be empty");
    iter.fold(first, f)
}

pub struct SynthesisParams<L, A> {
    pub iterations: usize,
    pub additions_per_iteration: usize,
    pub eqs: Vec<Equality<L, A>>,
}

impl<L, A> SynthesisParams<L, A> {
    fn rewrites(&self) -> impl Iterator<Item = &Rewrite<L, A>> {
        self.eqs.iter().flat_map(|eq| &eq.rewrites)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Signature<L> {
    samples: Vec<L>,
}

pub struct Equality<L, A> {
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    rewrites: Vec<egg::Rewrite<L, A>>,
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.rewrites.len() {
            1 => write!(f, "{}", self.rewrites[0].long_name()),
            2 => write!(f, "{} <=> {}", self.lhs, self.rhs),
            n => panic!("unexpected len {}", n),
        }
    }
}

impl<L: Language + 'static, A: Analysis<L>> Equality<L, A> {
    pub fn new(lhs: Pattern<L>, rhs: Pattern<L>) -> Option<Self> {
        let mut rewrites = vec![];

        let name = format!("{} => {}", lhs, rhs);
        if let Ok(rw) = egg::Rewrite::new(name.clone(), name, lhs.clone(), rhs.clone()) {
            rewrites.push(rw)
        }

        let name = format!("{} => {}", rhs, lhs);
        if let Ok(rw) = egg::Rewrite::new(name.clone(), name, rhs.clone(), lhs.clone()) {
            rewrites.push(rw)
        }

        if rewrites.is_empty() {
            None
        } else {
            Some(Self { rewrites, lhs, rhs })
        }
    }
}

