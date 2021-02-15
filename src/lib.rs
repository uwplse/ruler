mod bv;

use std::collections::VecDeque;
use std::time::Duration;
use std::{collections::HashSet, fmt::Display, hash::Hash, rc::Rc};
use std::{ops::Not, time::Instant};

use serde::{Deserialize, Serialize};
use structopt::StructOpt;
use strum::EnumString;
use itertools::Itertools;

use egg::*;

use indexmap::IndexMap;
use rand::{prelude::SliceRandom, Rng, SeedableRng};
use rand_pcg::Pcg64;
use std::collections::HashMap;

pub type Runner = egg::Runner<Math, SynthAnalysis, ()>;
pub type Pattern<L = Math> = egg::Pattern<L>;
pub type RecExpr<L = Math> = egg::RecExpr<L>;
pub type Rewrite<L = Math, A = SynthAnalysis> = egg::Rewrite<L, A>;
pub type EGraph<L = Math, A = SynthAnalysis> = egg::EGraph<L, A>;

pub type Constant = bv::u4;
pub type CVec = Vec<Option<Constant>>;

struct NumberOfOps;

impl egg::CostFunction<Math> for NumberOfOps {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if enode.is_leaf() {
            0
        } else {
            enode.fold(1, |sum, id| sum + costs(id))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, EnumString, Serialize, Deserialize)]
#[strum(serialize_all = "kebab-case")]
#[serde(rename_all = "kebab-case")]
pub enum Domain {
    Bool,
    BV4,
    BV4NS,
}

#[derive(Debug, Clone)]
pub struct Signature {
    cvec: CVec,
    exact: bool,
}

define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "--" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "-" = Neg(Id),
        "~" = Not(Id),
        "<<" = Shl([Id; 2]),
        ">>" = Shr([Id; 2]),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "^" = Xor([Id; 2]),
        Num(Constant),
        Var(egg::Symbol),
    }
}

pub fn generalize(expr: &RecExpr<Math>, map: &mut HashMap<Symbol, Var>) -> Pattern<Math> {
    let alpha = b"abcdefghijklmnopqrstuvwxyz";
    let nodes: Vec<_> = expr
        .as_ref()
        .iter()
        .map(|n| match n {
            Math::Var(sym) => {
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

pub fn instantiate(pattern: &Pattern<Math>) -> RecExpr<Math> {
    let nodes: Vec<_> = pattern
        .ast
        .as_ref()
        .iter()
        .map(|n| match n {
            ENodeOrVar::ENode(n) => n.clone(),
            ENodeOrVar::Var(v) => {
                let s = v.to_string();
                assert!(s.starts_with('?'));
                Math::Var(s[1..].into())
            }
        })
        .collect();

    RecExpr::from(nodes)
}

impl Signature {
    fn fold1(&self, mut f: impl FnMut(Constant) -> Option<Constant>) -> Self {
        let cvec = self.cvec.iter().map(|x| x.and_then(&mut f)).collect();
        Self {
            cvec,
            exact: self.exact,
        }
    }
    fn fold2(
        &self,
        other: &Self,
        mut f: impl FnMut(Constant, Constant) -> Option<Constant>,
    ) -> Self {
        if !self.cvec.is_empty() && !other.cvec.is_empty() {
            assert_eq!(self.cvec.len(), other.cvec.len());
        }

        let compute = |(x, y): (&Option<Constant>, &Option<Constant>)| match (x, y) {
            (Some(n1), Some(n2)) => f(n1.clone(), n2.clone()),
            (_, _) => None,
        };
        let cvec = self.cvec.iter().zip(&other.cvec).map(compute).collect();
        Self {
            cvec,
            exact: self.exact && other.exact,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SynthAnalysis {
    cvec_len: usize,
}

impl Default for SynthAnalysis {
    fn default() -> Self {
        Self { cvec_len: 1 }
    }
}

impl Analysis<Math> for SynthAnalysis {
    type Data = Signature;

    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        let v = |i: &Id| &egraph[*i].data;
        let param = &egraph.analysis;
        let sig = match enode {
            Math::Neg(a) => v(a).fold1(|a| Some(a.wrapping_neg())),
            Math::Not(a) => v(a).fold1(|a| Some(a.not())),
            Math::Add([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_add(b))),
            Math::Sub([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_sub(b))),
            Math::Mul([a, b]) => v(a).fold2(v(b), |a, b| Some(a.wrapping_mul(b))),
            Math::Num(n) => Signature {
                cvec: (0..param.cvec_len).map(|_| Some(n.clone())).collect(),
                exact: true,
            },
            Math::Var(_) => Signature {
                cvec: vec![],
                exact: false,
            },
            Math::Shl([a, b]) => v(a).fold2(v(b), |a, b| Some(a.my_shl(b))),
            Math::Shr([a, b]) => v(a).fold2(v(b), |a, b| Some(a.my_shr(b))),
            Math::And([a, b]) => v(a).fold2(v(b), |a, b| Some(a & b)),
            Math::Or([a, b]) => v(a).fold2(v(b), |a, b| Some(a | b)),
            Math::Xor([a, b]) => v(a).fold2(v(b), |a, b| Some(a ^ b)),
        };
        sig
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        if !to.cvec.is_empty() && !from.cvec.is_empty() {
            assert_eq!(&to.cvec, &from.cvec);
            if !to.exact && from.exact {
                to.exact = true;
                return true;
            }
        }
        false
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        let sig = &egraph[id].data;
        let cv = &sig.cvec;
        let exact = sig.exact;
        if cv.is_empty() || cv.contains(&None) {
            return;
        }
        let first = cv[0].clone();
        if cv.iter().all(|x| *x == first) {
            match first {
                Some(n) => {
                    let added = egraph.add(Math::Num(n.clone()));
                    if exact {
                        egraph.union(id, added);
                    }
                }
                None => {}
            }
        }
    }
}

#[derive(Debug, Clone, StructOpt, Serialize, Deserialize)]
#[structopt(rename_all = "kebab-case")]
#[serde(rename_all = "kebab-case")]
pub struct SynthParams {
    #[structopt(long, default_value = "0")]
    pub seed: u64,
    #[structopt(long, default_value = "0")]
    pub n_samples: usize,
    #[structopt(long)]
    pub domain: Domain,
    #[structopt(long, use_delimiter = true)]
    pub constants: Vec<Constant>,
    #[structopt(long)]
    pub variables: usize,
    #[structopt(long)]
    pub outfile: Option<String>,

    // search params
    #[structopt(long)]
    pub iters: usize,
    #[structopt(long, default_value = "1")]
    pub rules_to_take: usize,
    #[structopt(long)]
    pub chunk_size: Option<usize>,
}

type EqualityMap<L = Math, A = SynthAnalysis> = IndexMap<Rc<str>, Equality<L, A>>;

#[allow(dead_code)]
pub struct Synthesizer {
    params: SynthParams,
    rng: Pcg64,
    egraph: EGraph,
    equalities: EqualityMap,
}

impl Synthesizer {
    pub fn new(mut params: SynthParams) -> Self {
        let mut rng = Pcg64::seed_from_u64(params.seed);

        if params.constants.is_empty() {
            params.constants = match params.domain {
                Domain::Bool => vec![Constant::ZERO, Constant::MAX],
                Domain::BV4 | Domain::BV4NS => vec![Constant::ZERO, 0x8.into(), 0x7.into()],
            }
        }

        let state_space_size = (1usize << 4).pow(params.variables as u32);

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: state_space_size,
        //     cvec_len: params.n_samples + params.constants.len(),
        });

        let vars = &["x", "y", "z", "a", "b", "c", "d"];
        let vars = &vars[..params.variables];
        let var_ids: Vec<Id> = vars.iter().map(|v| egraph.add(Math::Var(v.into()))).collect();

        for vals in itertools::repeat_n(0..=0b1111, params.variables).multi_cartesian_product() {
            for (&id, val) in var_ids.iter().zip(vals) {
                egraph[id].data.cvec.push(Some(val.into()));
            }
        }

        assert_eq!(egraph[var_ids[0]].data.cvec.len(), state_space_size);

        // initialize the variables
        // for var in 0..params.variables {
        //     let id = egraph.add(Math::Var(vars[var].into()));
        //     egraph[id].data.cvec = (0..params.n_samples)
        //         .map(|_| rng.gen::<Constant>())
        //         .chain(params.constants.iter().cloned())
        //         .map(Some)
        //         .collect();
        //     egraph[id].data.cvec.shuffle(&mut rng);
        // }

        for n in &params.constants {
            egraph.add(Math::Num(n.clone()));
        }

        Self {
            rng,
            egraph,
            params,
            equalities: Default::default(),
        }
    }

    fn ids(&self) -> impl '_ + Iterator<Item = Id> {
        self.egraph.classes().map(|c| c.id)
    }

    fn make_layer(&self, n_ops: usize) -> Vec<Math> {
        assert!(n_ops > 0);
        let mut extract = Extractor::new(&self.egraph, NumberOfOps);

        // maps ids to n_ops
        let ids: IndexMap<Id, usize> = self
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        let mut to_add = vec![];
        for i in self.ids() {
            for j in self.ids() {
                if ids[&i] + ids[&j] + 1 == n_ops {
                    to_add.push(Math::And([i, j]));
                    to_add.push(Math::Or([i, j]));

                    match self.params.domain {
                        Domain::Bool => {
                            to_add.push(Math::Xor([i, j]));
                        }
                        Domain::BV4 | Domain::BV4NS => {
                            to_add.push(Math::Add([i, j]));
                            to_add.push(Math::Sub([i, j]));
                            to_add.push(Math::Mul([i, j]));
                            if self.params.domain == Domain::BV4 {
                                to_add.push(Math::Shl([i, j]));
                                to_add.push(Math::Shr([i, j]));
                            }
                        }
                    }
                }
            }
            if ids[&i] + 1 == n_ops {
                to_add.push(Math::Not(i));

                if matches!(self.params.domain, Domain::BV4 | Domain::BV4NS) {
                    to_add.push(Math::Neg(i));
                }
            }
        }

        log::info!("Made a layer of {} enodes", to_add.len());

        to_add.retain(|n| !n.children().iter().all(|&id| self.egraph[id].data.exact));
        to_add
    }

    fn run_rewrites(&mut self) -> EGraph {
        let t = Instant::now();
        log::info!("Running {} rules...", self.equalities.len());

        // run the rewrites
        let rewrites = self.equalities.values().flat_map(|eq| &eq.rewrites);

        let mut egraph = self.egraph.clone();
        egraph.analysis.cvec_len = 1;
        for c in egraph.classes_mut() {
            c.data.cvec.truncate(1);
        }

        let mut runner = Runner::new(egraph.analysis.clone())
            .with_egraph(egraph.clone())
            .with_node_limit(usize::MAX)
            .with_iter_limit(2)
            .with_scheduler(SimpleScheduler)
            // .with_time_limit(Duration::from_secs(60))
            .with_time_limit(Duration::from_secs(10))
            .with_hook(|r| {
                for c in r.egraph.classes_mut() {
                    if c.nodes.iter().find(|n| matches!(n, Math::Num(_))).is_some() {
                        c.nodes.retain(|n| matches!(n, Math::Num(_)))
                    }
                }
                Ok(())
            })
            // .with_iter_limit(20)
            // .with_scheduler(BackoffScheduler::default().with_initial_match_limit(5_000))
            .run(rewrites);

        // update the clean egraph based on any unions that happened
        let mut found_unions = vec![];
        for id in self.ids() {
            let id2 = runner.egraph.find(id);
            if id != id2 {
                found_unions.push((id, id2))
            }
        }
        for (id, id2) in found_unions {
            self.egraph.union(id, id2);
        }

        runner.egraph.rebuild();

        log::info!(
            "Ran {} rules in {:?}, cvec matching...",
            self.equalities.len(),
            t.elapsed()
        );
        runner.egraph
    }

    fn cvec_match(&self) -> (EqualityMap, IndexMap<&CVec, Vec<Id>>) {
        // build the cvec matching data structure
        let mut by_cvec: IndexMap<&CVec, Vec<Id>> = IndexMap::new();
        for id in self.ids() {
            let class = &self.egraph[id];
            by_cvec.entry(&class.data.cvec).or_default().push(class.id);
        }

        log::info!("# unique cvecs: {}", by_cvec.len());

        let mut new_eqs = EqualityMap::default();
        let mut extract = Extractor::new(&self.egraph, AstSize);
        for ids in by_cvec.values() {
            if false {
                // limit id_iter so the cartesian product doesn't get too big
                let mut id_iter = ids.iter().take(50);
                while let Some(&id1) = id_iter.next() {
                    for &id2 in id_iter.clone() {
                        let (_, e1) = extract.find_best(id1);
                        let (_, e2) = extract.find_best(id2);
                        if let Some(mut eq) = Equality::new(&e1, &e2) {
                            log::debug!("  Candidate {}", eq);
                            eq.ids = Some((id1, id2));
                            new_eqs.insert(eq.name.clone(), eq);
                        }
                    }
                }
            } else {
                // try linear cvec matching
                let mut terms_ids: Vec<_> = ids.iter().map(|&id| (extract.find_best(id), id)).collect();
                terms_ids.sort_by_key(|x| x.0.0);
                for win in terms_ids.windows(2) {
                    let (((_c1, e1), id1), ((_c2, e2), id2)) = (&win[0], &win[1]);
                    if let Some(mut eq) = Equality::new(e1, e2) {
                        log::debug!("  Candidate {}", eq);
                        eq.ids = Some((*id1, *id2));
                        new_eqs.insert(eq.name.clone(), eq);
                    }
                }
            }
        }

        // TODO why is this needed
        new_eqs.retain(|k, _v| !self.equalities.contains_key(k));
        (new_eqs, by_cvec)
    }

    pub fn mrat(mut self) -> Report {
        let chunk_size = self.params.chunk_size.unwrap_or(usize::MAX);
        let rules_to_take = self.params.rules_to_take;

        let t = Instant::now();
        for n_ops in 1..=self.params.iters {
            log::info!("[[[[  Iteration {}   ]]]]", n_ops);
            let layer = self.make_layer(n_ops);
            for chunk in layer.chunks(chunk_size) {
                for node in chunk {
                    self.egraph.add(node.clone());
                }
                loop {
                    self.run_rewrites();
                    let (new_eqs, by_cvec) = self.cvec_match();
                    let n_cv = by_cvec.len();
                    log::info!(
                        "egraph n={}, e={}, cv={}",
                        self.egraph.total_size(),
                        self.egraph.number_of_classes(),
                        n_cv,
                    );

                    let eqs = choose_eqs_old(&self.equalities, new_eqs, rules_to_take);

                    if eqs.is_empty() {
                        break;
                    }
                    log::info!("Chose {} rules", eqs.len());
                    for eq in eqs.values() {
                        log::info!("  {}", eq);
                        if let Some((i, j)) = eq.ids {
                            self.egraph.union(i, j);
                        }
                    }

                    self.equalities.extend(eqs);
                    self.egraph.rebuild();
                }
            }
        }

        let time = t.elapsed().as_secs_f64();
        let eqs: Vec<_> = self.equalities.into_iter().map(|(_, eq)| eq).collect();

        for eq in &eqs {
            println!("{}", eq);
        }
        println!("Learned {} rules in {}", eqs.len(), time,);

        Report {
            params: self.params,
            time,
            eqs,
        }
    }
 
    pub fn run(mut self) -> Report {
        let chunk_size = self.params.chunk_size.unwrap_or(usize::MAX);
        let rules_to_take = self.params.rules_to_take;

        let t = Instant::now();
        for n_ops in 1..=self.params.iters {
            log::info!("[[[[  Iteration {}   ]]]]", n_ops);
            let layer = self.make_layer(n_ops);
            for chunk in layer.chunks(chunk_size) {
                for node in chunk {
                    self.egraph.add(node.clone());
                }
                for _inner in 0..3 {
                // loop {
                    self.run_rewrites();
                    let (new_eqs, by_cvec) = self.cvec_match();
                    let n_cv = by_cvec.len();
                    log::info!(
                        "egraph n={}, e={}, cv={}",
                        self.egraph.total_size(),
                        self.egraph.number_of_classes(),
                        n_cv,
                    );


                    let minimize = true;
                    let one_shot = true;

                    let eqs = if minimize {
                        choose_eqs(&self.equalities, new_eqs, rules_to_take)
                    } else {
                        choose_eqs_old(&self.equalities, new_eqs, 1)
                    };

                    if one_shot {
                        // this is ok because we are unioning all of the rules
                        let id_groups: Vec<Vec<Id>> = by_cvec.into_iter().map(|x| x.1).collect();
                        for ids in id_groups {
                            for win in ids.windows(2) {
                                self.egraph.union(win[0], win[1]);
                            }
                        }
                    }

                    if eqs.is_empty() {
                        break;
                    }
                    log::info!("Chose {} rules", eqs.len());
                    for eq in eqs.values() {
                        log::info!("  {}", eq);
                        // assert!(!self.equalities.contains_key(&eq.name));
                        // it's ok to union the commited rules, because they are
                        // guaranteed to get matched in the next go around.
                        // this can help us early exit
                        if let Some((i, j)) = eq.ids {
                            self.egraph.union(i, j);
                        }
                    }

                    self.equalities.extend(eqs);
                    self.egraph.rebuild();
                    if n_cv == self.egraph.number_of_classes() {
                        log::info!("Leaving inner loop early...");
                        break;
                    }

                    if one_shot {
                        break
                    }
                }
            }
        }

        let time = t.elapsed().as_secs_f64();
        let eqs: Vec<_> = self.equalities.into_iter().map(|(_, eq)| eq).collect();

        for eq in &eqs {
            println!("{}", eq);
        }
        println!("Learned {} rules in {}", eqs.len(), time,);

        Report {
            params: self.params,
            time,
            eqs,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Report {
    pub params: SynthParams,
    pub time: f64,
    pub eqs: Vec<Equality>,
}

// fn choose_eqs(old_eqs: &EqualityMap, mut new_eqs: EqualityMap, n: usize) -> EqualityMap {
//     new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)).reverse());
//     let n = n.min(new_eqs.len());
//     new_eqs.drain(n..);
//     assert_eq!(new_eqs.len(), n);
//     minimize(old_eqs, new_eqs)
// }
//

fn choose_eqs_old(old_eqs: &EqualityMap, mut new_eqs: EqualityMap, n: usize) -> EqualityMap {
    let t = Instant::now();
    let n_new_eqs = new_eqs.len();
    log::info!("Minimizing {} rules...", n_new_eqs);

    let mut keepers = EqualityMap::default();

    // make the best last
    // new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)));

    while !new_eqs.is_empty() {

        let name = new_eqs.values().max_by_key(|eq| eq.score).unwrap().name.clone();
        let eq = new_eqs.remove(&name).unwrap();

        keepers.insert(name, eq);

        if new_eqs.is_empty() || keepers.len() >= n {
            break;
        }

        let rewrites = old_eqs
            .values()
            .flat_map(|eq| &eq.rewrites)
            .chain(keepers.values().flat_map(|eq| &eq.rewrites));

        let mut runner = Runner::default()
            .with_iter_limit(2)
            .with_scheduler(SimpleScheduler)
            .with_node_limit(1_000_000);

        for candidate_eq in new_eqs.values() {
            runner = runner.with_expr(&instantiate(&candidate_eq.lhs));
            runner = runner.with_expr(&instantiate(&candidate_eq.rhs));
        }

        runner = runner.run(rewrites);

        let mut extract = Extractor::new(&runner.egraph, AstSize);
        new_eqs = runner.roots.chunks(2).zip(new_eqs.values()).filter_map(|(ids, eq)| {
            if runner.egraph.find(ids[0]) == runner.egraph.find(ids[1]) {
                None
            } else {
                let lhs = extract.find_best(ids[0]).1;
                let rhs = extract.find_best(ids[1]).1;
                // TODO get ids so they can be unioned by `run`
                if let Some(mut eq2) = Equality::new(&lhs, &rhs) {
                    if old_eqs.contains_key(&eq2.name) {
                        None
                    } else {
                        eq2.ids = eq.ids;
                        Some((eq2.name.clone(), eq2))
                    }
                } else {
                    None
                }
            }
        }).collect();
        // // make the best last
        // new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)));

        log::info!(
            "Minimizing... threw away {} rules, {} / {} remain",
            0,
            new_eqs.len(),
            n_new_eqs
        );
    }

    log::info!(
        "Minimized {}->{} rules in {:?}",
        n_new_eqs,
        keepers.len(),
        t.elapsed()
    );
    keepers
}

fn choose_eqs(old_eqs: &EqualityMap, new_eqs: EqualityMap, n: usize) -> EqualityMap {
    let t = Instant::now();
    let n_new_eqs = new_eqs.len();
    log::info!("Minimizing {} rules...", n_new_eqs);

    let mut flat: VecDeque<Equality> = new_eqs.into_iter().map(|(_, eq)| eq).collect();
    let mut test = vec![];

    for mut n_chunks in (2..).map(|i| 1 << i) {

        if n_chunks > flat.len() {
            if n_chunks >= flat.len() * 2 {
                break
            }
            n_chunks = flat.len();
        }

        println!("n chunks {}", n_chunks);

        let mut chunk_sizes = vec![flat.len() / n_chunks; n_chunks];
        for i in 0..(flat.len() % n_chunks) {
            chunk_sizes[i] += 1;
        }
        assert_eq!(flat.len(), chunk_sizes.iter().sum());

        for size in chunk_sizes {
            let n_new_eqs_this_loop = flat.len();
            test.clear();
            for _ in 0..size {
                test.push(flat.pop_front().unwrap());
            }
            assert_eq!(test.len(), size);

            println!("chunk size {}, flat {}", size, flat.len());

            let mut rewrites: Vec<_> = old_eqs
                .values()
                .flat_map(|eq| &eq.rewrites)
                .chain(flat.iter().flat_map(|eq| &eq.rewrites)).collect();

            rewrites.sort_by_key(|rw| rw.name());
            rewrites.dedup_by_key(|rw| rw.name());

            let mut runner = Runner::default()
                .with_iter_limit(2)
                .with_scheduler(SimpleScheduler)
                .with_time_limit(Duration::from_secs(60))
                .with_node_limit(10_000_000);

            for candidate_eq in &test {
                runner = runner.with_expr(&instantiate(&candidate_eq.lhs));
                runner = runner.with_expr(&instantiate(&candidate_eq.rhs));
            }

            runner = runner.run(rewrites);

            let mut extract = Extractor::new(&runner.egraph, AstSize);
            flat.extend(runner.roots.chunks(2).zip(&test).filter_map(|(ids, eq)| {
                if runner.egraph.find(ids[0]) == runner.egraph.find(ids[1]) {
                    None
                } else {
                    let lhs = extract.find_best(ids[0]).1;
                    let rhs = extract.find_best(ids[1]).1;
                    // TODO get ids so they can be unioned by `run`
                    if let Some(mut eq2) = Equality::new(&lhs, &rhs) {
                        eq2.ids = eq.ids;
                        Some(eq2)
                    } else {
                        None
                    }
                }
            }));

            log::info!(
                "Minimizing... threw away {} rules, {} / {} remain",
                n_new_eqs_this_loop - flat.len(),
                flat.len(),
                n_new_eqs
            );
        }
    }

    // // make the best last
    // new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)));
    //
    // for n_chunks in 2.. {
    //     if n_chunks > flat.len() {
    //         break
    //     }
    //     let chunk_size = flat.len() / n_chunks;

    //     let mut chunks = Vec::with_capacity(n_chunks);
    // }

    // while !new_eqs.is_empty() && keepers.len() < n {
    //     // keepers.insert(name, eq);

    //     // if new_eqs.is_empty() || keepers.len() >= n {
    //     //     break;
    //     // }

    //     let n_new_eqs_this_loop = new_eqs.len();
    //     let rewrites = old_eqs
    //         .values()
    //         .flat_map(|eq| &eq.rewrites)
    //         .chain(keepers.values().flat_map(|eq| &eq.rewrites));

    //     let mut runner = Runner::default()
    //         .with_iter_limit(2)
    //         .with_scheduler(SimpleScheduler)
    //         .with_node_limit(1_000_000);

    //     for candidate_eq in new_eqs.values() {
    //         runner = runner.with_expr(&instantiate(&candidate_eq.lhs));
    //         runner = runner.with_expr(&instantiate(&candidate_eq.rhs));
    //     }

    //     runner = runner.run(rewrites);

    //     let mut extract = Extractor::new(&runner.egraph, AstSize);
    //     new_eqs = runner.roots.chunks(2).filter_map(|ids| {
    //         if runner.egraph.find(ids[0]) == runner.egraph.find(ids[1]) {
    //             None
    //         } else {
    //             let lhs = extract.find_best(ids[0]).1;
    //             let rhs = extract.find_best(ids[1]).1;
    //             // TODO get ids so they can be unioned by `run`
    //             Equality::new(&lhs, &rhs).map(|eq| (eq.name.clone(), eq))
    //         }
    //     }).collect();

    //     // if !new_eqs.is_empty() {
    //     // }

    //     new_eqs.sort_by(|_, eq1, _, eq2| score(eq1).cmp(&score(eq2)));
    //     while !new_eqs.is_empty() {
    //         let (name, eq) = new_eqs.pop().unwrap();
    //         if !keepers.contains_key(&name) && !old_eqs.contains_key(&name) {
    //             keepers.insert(name, eq);
    //             break
    //         }
    //     }

    //     // let mut redundant = HashSet::new();
    //     // for (eq, ids) in new_eqs.values().zip(runner.roots.chunks(2)) {
    //     //     if runner.egraph.find(ids[0]) == runner.egraph.find(ids[1]) {
    //     //         redundant.insert(eq.name.clone());
    //     //     }
    //     // }

    //     // // Indexmap::retain preserves the score ordering
    //     // new_eqs.retain(|name, _eq| !redundant.contains(name));

    //     log::info!(
    //         "Minimizing... threw away {} rules, {} / {} remain",
    //         n_new_eqs_this_loop - new_eqs.len(),
    //         new_eqs.len(),
    //         n_new_eqs
    //     );
    // }

    log::info!(
        "Minimized {}->{} rules in {:?}",
        n_new_eqs,
        flat.len(),
        t.elapsed()
    );

    flat.into_iter().map(|eq| (eq.name.clone(), eq)).collect()
}

fn score(eq: &Equality) -> (isize, isize, isize) {
    let mut vars: HashSet<Var> = Default::default();
    vars.extend(eq.lhs.vars());
    vars.extend(eq.rhs.vars());
    // let size = usize::add(AstSize.cost_rec(&eq.lhs.ast), AstSize.cost_rec(&eq.rhs.ast));
    // let size = AstSize.cost_rec(&eq.lhs.ast) + AstSize.cost_rec(&eq.rhs.ast);
    let max = usize::max(AstSize.cost_rec(&eq.lhs.ast), AstSize.cost_rec(&eq.rhs.ast));
    let min = usize::min(AstSize.cost_rec(&eq.lhs.ast), AstSize.cost_rec(&eq.rhs.ast));
    // (vars.len() as isize, -(size as isize))
    (-(max as isize), vars.len() as isize, -(min as isize))
    // let x = (-(size as isize), vars.len() as isize);
    // // println!("{} {:?}", eq, x);
    // x
}

// fn score(eq: &Equality) -> (isize, isize) {
//     let mut vars: HashSet<Var> = Default::default();
//     vars.extend(eq.lhs.vars());
//     vars.extend(eq.rhs.vars());
//     let size = usize::max(AstSize.cost_rec(&eq.lhs.ast), AstSize.cost_rec(&eq.rhs.ast));
//     (-(size as isize), vars.len() as isize)
// }

// TODO should probably keep things together
#[derive(Clone, Serialize, Deserialize)]
#[serde(from = "SerializedEq")]
#[serde(into = "SerializedEq")]
#[serde(bound = "L: Clone + Language + 'static, A: Clone + Analysis<L>")]
pub struct Equality<L = Math, A = SynthAnalysis> {
    pub name: Rc<str>,
    pub lhs: Pattern<L>,
    pub ids: Option<(Id, Id)>,
    pub rhs: Pattern<L>,
    // pub cond: Option<Pattern<L>>,
    pub rewrites: Vec<Rewrite<L, A>>,
    pub score: (isize, isize, isize),
}

#[derive(Clone, Serialize, Deserialize)]
struct SerializedEq {
    lhs: String,
    rhs: String,
    bidirectional: bool,
}

impl<L: Language + 'static, A: Analysis<L>> From<SerializedEq> for Equality<L, A> {
    fn from(ser: SerializedEq) -> Self {
        let lhs: Pattern<L> = ser.lhs.parse().unwrap();
        let rhs: Pattern<L> = ser.rhs.parse().unwrap();
        let mut rewrites =
            vec![Rewrite::new(format!("{} => {}", lhs, rhs), lhs.clone(), rhs.clone()).unwrap()];
        let name = if ser.bidirectional {
            rewrites.push(
                Rewrite::new(format!("{} => {}", rhs, lhs), rhs.clone(), lhs.clone()).unwrap(),
            );
            format!("{} <=> {}", lhs, rhs)
        } else {
            format!("{} => {}", lhs, rhs)
        };
        Self {
            name: name.into(),
            lhs,
            rhs,
            rewrites,
            ids: None,
            score: Default::default(),
        }
    }
}

impl<L: Language, A: Analysis<L>> From<Equality<L, A>> for SerializedEq {
    fn from(eq: Equality<L, A>) -> Self {
        Self {
            lhs: eq.lhs.to_string(),
            rhs: eq.rhs.to_string(),
            bidirectional: eq.rewrites.len() > 1,
        }
    }
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Equality<Math, SynthAnalysis> {
    pub fn new(e1: &RecExpr, e2: &RecExpr) -> Option<Self> {
        let mut forward: (String, Pattern, Pattern, Option<Rewrite>) = {
            let map = &mut HashMap::default();
            let lhs = generalize(&e1, map);
            let rhs = generalize(&e2, map);
            let name = format!("{} => {}", lhs, rhs);
            (
                name.clone(),
                lhs.clone(),
                rhs.clone(),
                Rewrite::new(name, lhs.clone(), rhs.clone()).ok(),
            )
        };

        let mut back: (String, Pattern, Pattern, Option<Rewrite>) = {
            let map = &mut HashMap::default();
            let lhs = generalize(&e2, map);
            let rhs = generalize(&e1, map);
            let name = format!("{} => {}", lhs, rhs);
            (
                name.clone(),
                lhs.clone(),
                rhs.clone(),
                Rewrite::new(name, lhs.clone(), rhs.clone()).ok(),
            )
        };

        // make sure we always do things in the same order
        if back.0 > forward.0 {
            std::mem::swap(&mut forward, &mut back);
        }

        let mut eq = match (forward, back) {
            ((_, _, _, None), (_, _, _, None)) => return None,
            ((name, lhs, rhs, Some(rw)), (_, _, _, None))
            | ((_, _, _, None), (name, lhs, rhs, Some(rw))) => Self {
                name: name.into(),
                lhs,
                rhs,
                ids: None,
                score: Default::default(),
                rewrites: vec![rw],
            },
            ((_, lhs, rhs, Some(rw1)), (_, _, _, Some(rw2))) => Self {
                name: format!("{} <=> {}", lhs, rhs).into(),
                lhs,
                rhs,
                ids: None,
                score: Default::default(),
                rewrites: if rw1.name == rw2.name {
                    vec![rw1]
                } else {
                    vec![rw1, rw2]
                },
            },
        };
        eq.score = score(&eq);
        Some(eq)
    }
}

pub fn validate_one(n: usize, rng: &mut impl Rng, lhs: &RecExpr, rhs: &RecExpr) -> bool {
    let mut egraph = EGraph::new(SynthAnalysis { cvec_len: n });
    let mut vars = HashSet::new();
    for node in lhs.as_ref().iter().chain(rhs.as_ref()) {
        if let Math::Var(v) = node {
            vars.insert(v);
        }
    }

    for v in vars {
        let id = egraph.add(Math::Var(*v));
        egraph[id].data.cvec = (0..n).map(|_| rng.gen::<Constant>()).map(Some).collect();
    }

    let lhs_id = egraph.add_expr(lhs);
    let rhs_id = egraph.add_expr(rhs);

    let lhs_cvec = &egraph[lhs_id].data.cvec;
    let rhs_cvec = &egraph[rhs_id].data.cvec;

    lhs_cvec == rhs_cvec
}

pub fn validate(
    eqs: Vec<(RecExpr, RecExpr)>,
    n_samples: usize,
) -> (Vec<(RecExpr, RecExpr)>, Vec<(RecExpr, RecExpr)>) {
    let rng = &mut Pcg64::seed_from_u64(1);
    eqs.into_iter()
        .partition(|(l, r)| validate_one(n_samples, rng, l, r))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_proves(eqs: &[Equality], a: &str, b: &str) {
        let rules = eqs.iter().flat_map(|eq| &eq.rewrites);

        let runner = Runner::default()
            .with_expr(&a.parse().unwrap())
            .with_expr(&b.parse().unwrap())
            .with_hook(|runner| {
                if runner.egraph.find(runner.roots[0]) == runner.egraph.find(runner.roots[1]) {
                    Err(format!("Done"))
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
    fn orat1() {
        let _ = env_logger::try_init();
        let syn = Synthesizer::new(SynthParams {
            seed: 5,
            n_samples: 1000,
            constants: vec![0, 1].into_iter().map(Constant::from).collect(),
            variables: 3,
            domain: Domain::BV4,
            iters: 1,
            rules_to_take: 1,
            chunk_size: None,
            outfile: None,
        });
        let report = syn.run();

        check_proves(&report.eqs, "(+ a b)", "(+ b a)");
        check_proves(&report.eqs, "(* a b)", "(* b a)");
        check_proves(&report.eqs, "(+ 1 1)", "2");
        check_proves(&report.eqs, "a", "(* 1 a)");
        check_proves(&report.eqs, "a", "(+ a 0)");
    }

    #[test]
    fn mrat1() {
        let _ = env_logger::try_init();
        let syn = Synthesizer::new(SynthParams {
            seed: 5,
            n_samples: 1000,
            constants: vec![0, 1].into_iter().map(Constant::from).collect(),
            variables: 3,
            // search params
            domain: Domain::BV4,
            iters: 1,
            rules_to_take: usize::MAX,
            chunk_size: None,
            outfile: None,
        });

        let report = syn.run();
        check_proves(&report.eqs, "(+ a b)", "(+ b a)");
        check_proves(&report.eqs, "(* a b)", "(* b a)");
        check_proves(&report.eqs, "(+ 1 1)", "2");
        check_proves(&report.eqs, "a", "(* 1 a)");
        check_proves(&report.eqs, "a", "(+ a 0)");
    }
}
