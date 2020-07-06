use egg::*;

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Synthesizer<L, A>
where
    L: Language + 'static,
    A: Analysis<L> + Clone,
{
    type Value: Hash + PartialEq + Eq + Clone + Debug;

    fn value_to_node(val: &Self::Value) -> Option<L>;
    fn symbol_to_node(sym: Symbol) -> L;
    fn node_to_symbol(node: &L) -> Option<Symbol>;

    fn make_node(&mut self, egraph: &EGraph<L, A>) -> L;
    fn initial_egraph(&mut self, params: &SynthesisParams<L, A>) -> EGraph<L, A>;
    fn eval(
        &mut self,
        enode: &L,
        egraph: &EGraph<L, A>,
        values: &HashMap<Id, Self::Value>,
    ) -> Self::Value;

    fn instantiate(pattern: &Pattern<L>) -> RecExpr<L> {
        let nodes: Vec<_> = pattern
            .ast
            .as_ref()
            .iter()
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
        let nodes: Vec<_> = expr
            .as_ref()
            .iter()
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

    fn minimize_equalities(
        &mut self,
        analysis: A,
        equalities: &mut Vec<Equality<L, A>>,
    ) -> Vec<Equality<L, A>> {
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

    // fn mk_signatures(&mut self, sigs: HashMap<Id, Signature<L>>, )

    fn run(&mut self, mut params: SynthesisParams<L, A>) -> Vec<Equality<L, A>> {
        let mut egraph = self.initial_egraph(&params);
        let mut values: HashMap<Id, Self::Value> = Default::default();

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
            values = values
                .into_iter()
                .map(|(id, val)| (egraph.find(id), val))
                .collect();

            // assert_eq!(values.len(), egraph.number_of_classes());

            // for class in egraph.classes() {
            //     assert!(
            //         values.contains_key(&class.id),
            //         "values doesn't contain {:?}",
            //         class
            //     );
            // }
            //
            
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


            // create the new nodes before adding to make sure they refer to existing nodes
            let to_add: Vec<(L, Self::Value)> = (0..params.additions_per_iteration)
                .map(|_| {
                    let n = self.make_node(&egraph);
                    println!("evaling {:?}", n);
                    let val = self.eval(&n, &egraph, &values);
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

            // let mut new_values = vec![];
            // // compute new values, since nodes have been added
            // for class in egraph.classes() {
            //     if !values.contains_key(&class.id) {
            //         // TODO passing the egraph here is a hack
            //         let node = class
            //             .iter()
            //             .find(|n| n.children().iter().all(|id| values.contains_key(&egraph.find(*id))))
            //             .unwrap_or_else(|| panic!("failed to find node for class {:?}", class));
            //         let val = self.eval(node, &egraph, &values);
            //         new_values.push((class.id, val));
            //     }
            // }

            // // add the new_values to values map, doing const addition on the way
            // for (mut id, val) in new_values {
            //     if let Some(node) = Self::value_to_node(&val) {
            //         let added = egraph.add(node);
            //         id = egraph.union(id, added).0;
            //     }
            //     values.insert(id, val);
            // }
            // egraph.rebuild();

            // group things by value
            let mut groups: HashMap<Self::Value, Vec<Id>> = Default::default();
            for (id, val) in values.drain() {
                assert_eq!(id, egraph.find(id));
                groups.entry(val).or_default().push(egraph.find(id));
            }
            assert!(groups.len() <= egraph.number_of_classes());
            // assert_eq!(
            //     egraph.number_of_classes(),
            //     groups.values().map(|ids| ids.len()).sum(),
            //     "sum failed"
            // );

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

// impl<A> SynthesisLanguage<A> for SymbolLang
// where
//     A: Analysis<SymbolLang>,
// {
//     fn make_from_symbol(sym: Symbol) -> Self {
//         SymbolLang::leaf(sym)
//     }
//     fn add_something<R: Rng>(_rng: &mut impl Rng, _egraph: &mut EGraph<Self, A>) {
//         unimplemented!()
//     }
// }

fn fold1<T, F>(iter: impl IntoIterator<Item = T>, f: F) -> T
where
    F: FnMut(T, T) -> T,
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

// impl<L: Language> Signature<L> {
//     fn constant(&self) -> Option<L> {

//     }
// }

// impl<L, A> SynthesisContext<L, A>
// where
//     L: SynthesisLanguage<A>,
//     A: Analysis<L> + Clone,
// {

//     fn run(&mut self, egraph: &mut EGraph<L, A>) {
//         for iter in 0..self.iterations {

//             // print!(
//             //     "\riter {}, r={}, n={}, e={}",
//             //     iter,
//             //     rewrites.len(),
//             //     egraph.total_number_of_nodes(),
//             //     egraph.number_of_classes()
//             // );
//             //
//             for _ in 0..i {
//                 egraph.add()
//             }

//             // egraph.rebuild();
//             // if !rewrites.is_empty() {
//             //     egraph = Runner::new(sampler.clone())
//             //         .with_egraph(egraph)
//             //         .with_iter_limit(2)
//             //         .with_scheduler(SimpleScheduler)
//             //         .run(&rewrites)
//             //         .egraph;
//             // }

//             // find new equalities
//             // let mut to_union = vec![];
//             // for c1 in egraph.classes() {
//             //     for c2 in egraph.classes() {
//             //         if c1.id < c2.id && c1.data.samples == c2.data.samples {
//             //             to_union.push((c1.id, c2.id))
//             //         }
//             //     }
//             // }

//             // let max_depth = 3;

//             // for (id1, id2) in to_union {
//             //     let data1 = &egraph[id1].data;
//             //     let data2 = &egraph[id2].data;
//             //     let mut ext = Extractor::new(&egraph, AstSize);
//             //     let lhs = ext.find_best(id1).1;
//             //     let rhs = ext.find_best(id2).1;
//             //     // let lhs = data1.expr.clone();
//             //     // let rhs = data2.expr.clone();
//             //     let depth1 = data1.depth;
//             //     let depth2 = data2.depth;
//             //     let (_, did_something) = egraph.union(id1, id2);
//             //     if did_something && depth1 <= max_depth && depth2 <= max_depth {
//             //         if let Some(rw) = generalize_to_rewrite(&lhs, &rhs) {
//             //             if rewrites.iter().find(|r| r.name() == rw.name()).is_none() {
//             //                 println!("Learned rewrite: {}", rw.name());
//             //                 rewrites.push(rw);
//             //                 iters_for_rewrites.push(i);
//             //                 egraph = Runner::new(sampler.clone())
//             //                     .with_egraph(egraph)
//             //                     .with_iter_limit(5)
//             //                     .run(&rewrites)
//             //                     .egraph;
//             //             }
//             //         }
//             //         if let Some(rw) = generalize_to_rewrite(&rhs, &lhs) {
//             //             if rewrites.iter().find(|r| r.name() == rw.name()).is_none() {
//             //                 println!("Learned rewrite: {}", rw.name());
//             //                 rewrites.push(rw);
//             //                 iters_for_rewrites.push(i);
//             //                 egraph = Runner::new(sampler.clone())
//             //                     .with_egraph(egraph)
//             //                     .with_iter_limit(5)
//             //                     .run(&rewrites)
//             //                     .egraph;
//             //             }
//             //         }
//             //     }
//             // }
//         }
//     }
// }

// fn generalize_to_rewrite(lhs: &RecExpr, rhs: &RecExpr) -> Option<Rewrite> {
//     let mut map = HashMap::default();
//     let lhs = generalize(lhs, &mut map);
//     let rhs = generalize(rhs, &mut map);
//     let name = format!("{} => {}", lhs, rhs);
//     if let Ok(rw) = Rewrite::new(name.clone(), name.clone(), lhs, rhs) {
//         Some(rw)
//     } else {
//         println!("Failed to create rewrite for {}", name);
//         None
//     }
// }

// fn add_something(rng: &mut impl Rng, egraph: &mut EGraph) {
//     let var_classes: Vec<_> = egraph
//         .classes()
//         .filter(|c| c.iter().any(|n| matches!(n, Math::Var(_))))
//         .collect();
//     let classes: Vec<_> = egraph
//         .classes()
//         .filter(|c| c.data.depth < 3 && c.data.constant().map_or(true, |n| -2 <= n && n <= 2))
//         .collect();
//     let max_depth = 1 + classes.iter().map(|c| c.data.depth).max().unwrap();
//     macro_rules! mk {
//         () => {
//             if rng.gen_bool(0.3) {
//                 var_classes.choose(rng).unwrap().id
//             } else {
//                 classes
//                     .choose(rng)
//                     // .choose_weighted(rng, |c| (max_depth - c.data.depth).pow(2))
//                     .unwrap()
//                     .id
//             }
//         };
//     }
//     let p: f32 = rng.gen();
//     let node = match p {
//         _ if p < 0.5 => Math::Add([mk!(), mk!()]),
//         _ => Math::Mul([mk!(), mk!()]),
//     };
//     egraph.add(node);
// }

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

// fn main() {
//     env_logger::init();

//     let mut eqs: Vec<Equality<SymbolLang, ()>> = vec![
//         Equality::new("(* ?a ?b)".parse().unwrap(), "(* ?b ?a)".parse().unwrap()),
//         Equality::new(
//             "(+ ?a ?b)".parse().unwrap(),
//             "(* 1 (+ ?a ?b))".parse().unwrap(),
//         ),
//         Equality::new("(* ?a 1)".parse().unwrap(), "?a".parse().unwrap()),
//     ];

//     // minimize_equalities(&mut eqs, ());
// }

// pub fn do_it() {
//     let n_samples = 50;
//     let n_vars = 3;
//     let interesting = vec![0, 1];

//     assert!(n_samples > interesting.len());
//     let n_to_sample = n_samples - interesting.len();

//     let rng = &mut rand_pcg::Pcg64::seed_from_u64(0xc0ffee);

//     let mut mk_samples = || -> Vec<Constant> {
//         let mut samples = interesting.clone();
//         samples.extend((0..n_to_sample).map(|_| rng.gen::<Constant>()));
//         assert_eq!(samples.len(), n_samples);
//         samples
//     };

//     let sampler = Sampler {
//         n_samples,
//         eqs: Default::default(),
//         vars: (0..n_vars)
//             .map(|i| (format!("x{}", i).into(), mk_samples()))
//             .collect(),
//     };

//     let vars: Vec<_> = sampler.vars.keys().copied().collect();

//     let mut iters_for_rewrites = vec![];
//     let mut rewrites: Vec<Rewrite> = vec![];

//     let mut egraph = EGraph::new(sampler.clone());
//     for c in &interesting {
//         egraph.add(Math::Num(*c));
//     }
//     for var in vars {
//         egraph.add(Math::Var(var));
//     }

//     for i in 0..100 {
//         print!(
//             "\riter {}, r={}, n={}, e={}",
//             i,
//             rewrites.len(),
//             egraph.total_number_of_nodes(),
//             egraph.number_of_classes()
//         );
//         for _ in 0..i {
//             add_something(rng, &mut egraph);
//         }

//         egraph.rebuild();
//         if !rewrites.is_empty() {
//             egraph = Runner::new(sampler.clone())
//                 .with_egraph(egraph)
//                 .with_iter_limit(2)
//                 .with_scheduler(SimpleScheduler)
//                 .run(&rewrites)
//                 .egraph;
//         }

//         // find new equalities
//         let mut to_union = vec![];
//         for c1 in egraph.classes() {
//             for c2 in egraph.classes() {
//                 if c1.id < c2.id && c1.data.samples == c2.data.samples {
//                     to_union.push((c1.id, c2.id))
//                 }
//             }
//         }

//         let max_depth = 3;

//         for (id1, id2) in to_union {
//             let data1 = &egraph[id1].data;
//             let data2 = &egraph[id2].data;
//             let mut ext = Extractor::new(&egraph, AstSize);
//             let lhs = ext.find_best(id1).1;
//             let rhs = ext.find_best(id2).1;
//             // let lhs = data1.expr.clone();
//             // let rhs = data2.expr.clone();
//             let depth1 = data1.depth;
//             let depth2 = data2.depth;
//             let (_, did_something) = egraph.union(id1, id2);
//             if did_something && depth1 <= max_depth && depth2 <= max_depth {
//                 if let Some(rw) = generalize_to_rewrite(&lhs, &rhs) {
//                     if rewrites.iter().find(|r| r.name() == rw.name()).is_none() {
//                         println!("Learned rewrite: {}", rw.name());
//                         rewrites.push(rw);
//                         iters_for_rewrites.push(i);
//                         egraph = Runner::new(sampler.clone())
//                             .with_egraph(egraph)
//                             .with_iter_limit(5)
//                             .run(&rewrites)
//                             .egraph;
//                     }
//                 }
//                 if let Some(rw) = generalize_to_rewrite(&rhs, &lhs) {
//                     if rewrites.iter().find(|r| r.name() == rw.name()).is_none() {
//                         println!("Learned rewrite: {}", rw.name());
//                         rewrites.push(rw);
//                         iters_for_rewrites.push(i);
//                         egraph = Runner::new(sampler.clone())
//                             .with_egraph(egraph)
//                             .with_iter_limit(5)
//                             .run(&rewrites)
//                             .egraph;
//                     }
//                 }
//             }
//         }
//     }

//     println!("Found {} rewrites:", rewrites.len());
//     for (rw, i) in rewrites.iter().zip(&iters_for_rewrites) {
//         println!("{:4}: {}", i, rw.long_name());
//     }
//     // for class in egraph.classes() {
//     //     println!("{}: {}", class.id, class.data.expr)
//     // }
// }
