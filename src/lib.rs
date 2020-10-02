mod metrics;
use egg::RecExpr;
use egg::*;
use indexmap::IndexMap;
use rand::{prelude::SliceRandom, Rng};
use rand_pcg::Pcg64;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    fmt::Formatter,
    time::Duration,
    time::Instant,
};

macro_rules! num {
    ($x:expr) => {
        Constant::Number($x)
    };
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Constant {
    Number(i32),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum ExprType {
    Number,
    Boolean,
    Invalid,
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
        "abs" = Abs(Id),
        "~" = Neg(Id),
        "!" = Not(Id),
        Bool(bool),
        Num(i32),
        Var(egg::Symbol),
    }
}

pub fn get_num(c: Constant) -> i32 {
    match c {
        Constant::Number(num) => num,
        _ => panic!("Not a num"),
    }
}

pub fn get_bool(c: Constant) -> bool {
    match c {
        Constant::Boolean(b) => b,
        _ => panic!("Not a bool"),
    }
}

type Ctx = HashMap<&'static str, Constant>;

pub fn eval(ctx: &Ctx, expr: &[SimpleMath]) -> Option<Constant> {
    match expr.last().expect("empty expr!") {
        SimpleMath::Num(n) => Some(Constant::Number(*n)),
        SimpleMath::Bool(b) => Some(Constant::Boolean(*b)),
        SimpleMath::Var(v) => {
            if v.as_str() == "x" {
                Some(*ctx.get("x").unwrap())
            } else if v.as_str() == "y" {
                Some(*ctx.get("y").unwrap())
            } else if v.as_str() == "z" {
                Some(*ctx.get("z").unwrap())
            } else {
                panic!("eval: currently only supports rules with 3 variables");
            }
        }
        SimpleMath::Add([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Number(
                get_num(e1.unwrap()).wrapping_add(get_num(e2.unwrap())),
            ))
        }
        SimpleMath::Sub([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Number(
                get_num(e1.unwrap()).wrapping_sub(get_num(e2.unwrap())),
            ))
        }
        SimpleMath::Mul([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Number(
                get_num(e1.unwrap()).wrapping_mul(get_num(e2.unwrap())),
            ))
        }
        SimpleMath::Div([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            if e2 != Some(Constant::Number(0)) {
                Some(Constant::Number(
                    get_num(e1.unwrap()).wrapping_div(get_num(e2.unwrap())),
                ))
            } else {
                None
            }
        }
        SimpleMath::Abs(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            Some(Constant::Number(get_num(e1.unwrap()).wrapping_abs()))
        }
        SimpleMath::Neg(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            Some(Constant::Number(-get_num(e1.unwrap())))
        }
        SimpleMath::Neq([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Boolean(
                get_num(e1.unwrap()) != get_num(e2.unwrap()),
            ))
        }
        SimpleMath::Leq([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Boolean(
                get_num(e1.unwrap()) <= get_num(e2.unwrap()),
            ))
        }
        SimpleMath::Geq([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Boolean(
                get_num(e1.unwrap()) >= get_num(e2.unwrap()),
            ))
        }
        SimpleMath::Lt([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Boolean(
                get_num(e1.unwrap()) < get_num(e2.unwrap()),
            ))
        }
        SimpleMath::Gt([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Boolean(
                get_num(e1.unwrap()) > get_num(e2.unwrap()),
            ))
        }
        SimpleMath::And([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Boolean(
                get_bool(e1.unwrap()) && get_bool(e2.unwrap()),
            ))
        }
        SimpleMath::Or([a, b]) => {
            let a = usize::from(*a);
            let b = usize::from(*b);
            let e1 = eval(ctx, &expr[..=a]);
            let e2 = eval(ctx, &expr[..=b]);
            Some(Constant::Boolean(
                get_bool(e1.unwrap()) || get_bool(e2.unwrap()),
            ))
        }
        SimpleMath::Not(a) => {
            let a = usize::from(*a);
            let e1 = eval(ctx, &expr[..=a]);
            Some(Constant::Boolean(!get_bool(e1.unwrap())))
        }
    }
}

#[derive(Default, Clone)]
pub struct SynthAnalysis {
    cvec_len: usize,
}

impl Analysis<SimpleMath> for SynthAnalysis {
    type Data = (Vec<Option<Constant>>, Vec<Option<Constant>>);

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        let mut to_changed = false;
        let pairs = to.0.iter().zip(from.0.iter());
        for (mut t, f) in pairs {
            match (t, f) {
                (None, Some(_)) => {
                    t = f;
                    to_changed = true;
                }
                (Some(a), Some(b)) => assert_eq!(a, b),
                (_, None) => continue,
            }
        }
        to_changed
        // // only do assertions on non-empty vecs
        // if !to.is_empty() && !from.is_empty() {
        //     assert_eq!(to, &from);
        // }
        // false
    }

    // fn pre_union(eg: &EGraph<SimpleMath, Self>, id1: Id, id2: Id) {
    //     let mut extractor = Extractor::new(eg,  AstSize);
    //     let (_, lhs) = extractor.find_best(id1);
    //     let (_, rhs) = extractor.find_best(id2);
    //     println!("preunion: lhs: {}, rhs: {}", lhs, rhs);
    // }

    fn make(egraph: &EGraph<SimpleMath, Self>, enode: &SimpleMath) -> Self::Data {
        // a closure to get the cvec for some eclass
        let x = |i: &Id| egraph[*i].data.1.iter().copied();
        let params = &egraph.analysis;
        match enode {
            SimpleMath::Num(n) => (
                (0..params.cvec_len)
                    .map(|_| Some(Constant::Number(*n)))
                    .collect(),
                (0..params.cvec_len)
                    .map(|_| Some(Constant::Number(*n)))
                    .collect(),
            ),
            SimpleMath::Bool(b) => (
                (0..params.cvec_len)
                    .map(|_| Some(Constant::Boolean(*b)))
                    .collect(),
                (0..params.cvec_len)
                    .map(|_| Some(Constant::Boolean(*b)))
                    .collect(),
            ),
            SimpleMath::Var(_) => (vec![], vec![]),
            SimpleMath::Not(a) => (
                x(a).map(|x| match x {
                    Some(Constant::Boolean(b)) => Some(Constant::Boolean(!b)),
                    _ => None,
                })
                .collect(),
                x(a).map(|x| match x {
                    Some(Constant::Boolean(b)) => Some(Constant::Boolean(!b)),
                    _ => None,
                })
                .collect(),
            ),
            SimpleMath::Neg(a) => (
                x(a).map(|x| match x {
                    Some(Constant::Number(n)) => Some(Constant::Number(-n)),
                    _ => None,
                })
                .collect(),
                x(a).map(|x| match x {
                    Some(Constant::Number(n)) => Some(Constant::Number(-n)),
                    _ => None,
                })
                .collect(),
            ),
            SimpleMath::And([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                            Some(Constant::Boolean(b1 && b2))
                        }
                        (_, _) => None,
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                            Some(Constant::Boolean(b1 && b2))
                        }
                        (_, _) => None,
                    })
                    .collect(),
            ),
            SimpleMath::Or([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                            Some(Constant::Boolean(b1 || b2))
                        }
                        (_, _) => None,
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                            Some(Constant::Boolean(b1 || b2))
                        }
                        (_, _) => None,
                    })
                    .collect(),
            ),
            SimpleMath::Neq([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x != y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x != y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
            ),
            SimpleMath::Leq([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x <= y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x <= y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
            ),
            SimpleMath::Geq([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x >= y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x >= y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
            ),
            SimpleMath::Lt([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x < y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x < y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
            ),
            SimpleMath::Gt([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x > y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| {
                        if x > y {
                            Some(Constant::Boolean(true))
                        } else {
                            Some(Constant::Boolean(false))
                        }
                    })
                    .collect(),
            ),
            SimpleMath::Add([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            Some(Constant::Number(n1.wrapping_add(n2)))
                        }
                        (_, _) => None,
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            Some(Constant::Number(n1.wrapping_add(n2)))
                        }
                        (_, _) => None,
                    })
                    .collect(),
            ),
            SimpleMath::Sub([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            Some(Constant::Number(n1.wrapping_sub(n2)))
                        }
                        (_, _) => None,
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            Some(Constant::Number(n1.wrapping_sub(n2)))
                        }
                        (_, _) => None,
                    })
                    .collect(),
            ),
            SimpleMath::Mul([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            Some(Constant::Number(n1.wrapping_mul(n2)))
                        }
                        (_, _) => None,
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            Some(Constant::Number(n1.wrapping_mul(n2)))
                        }
                        (_, _) => None,
                    })
                    .collect(),
            ),
            SimpleMath::Abs(a) => (
                x(a).map(|x| match x {
                    Some(Constant::Number(n1)) => Some(Constant::Number(n1.wrapping_abs())),
                    _ => None,
                })
                .collect(),
                x(a).map(|x| match x {
                    Some(Constant::Number(n1)) => Some(Constant::Number(n1.wrapping_abs())),
                    _ => None,
                })
                .collect(),
            ),
            SimpleMath::Div([a, b]) => (
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            if n2 != 0 {
                                Some(Constant::Number(n1.wrapping_div(n2)))
                            } else {
                                None
                            }
                        }
                        (_, _) => None,
                    })
                    .collect(),
                x(a).zip(x(b))
                    .map(|(x, y)| match (x, y) {
                        (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                            if n2 != 0 {
                                Some(Constant::Number(n1.wrapping_div(n2)))
                            } else {
                                None
                            }
                        }
                        (_, _) => None,
                    })
                    .collect(),
            ),
        }
    }

    fn modify(egraph: &mut EGraph<SimpleMath, Self>, id: Id) {
        let cv = &egraph[id].data.0;
        if cv.is_empty() || cv.contains(&None) {
            return;
        }
        let first = cv[0];
        if cv.iter().all(|x| *x == first) {
            match first {
                Some(Constant::Number(n)) => {
                    let added = egraph.add(SimpleMath::Num(n));
                    egraph.union(id, added);
                }
                Some(Constant::Boolean(b)) => {
                    let added = egraph.add(SimpleMath::Bool(b));
                    egraph.union(id, added);
                }
                None => panic!("Cvec contains None"),
            }
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
    pub cond_rule_iters: usize,
    pub cond_rule_rand_idx: usize,
    pub cond_diff_thresh: usize,
}

impl SynthParam {
    fn mk_egraph(&mut self) -> EGraph<SimpleMath, SynthAnalysis> {
        let mut egraph = EGraph::new(SynthAnalysis {
            // for now just adding 0 and 1 forcefully to the cvecs for variables
            // to test conditional rules for division
            cvec_len: self.n_samples + self.consts.len(),
        });
        let mut var_cvec_map: IndexMap<&Symbol, Vec<Option<Constant>>> = IndexMap::new();
        let rng = &mut self.rng;
        for var in &self.variables {
            let id = egraph.add(SimpleMath::Var(*var));
            let mut cvec: Vec<Option<Constant>> = (0..self.n_samples)
                .map(|_| Some(Constant::Number(rng.gen::<i32>())))
                .collect();
            cvec.push(Some(Constant::Number(0)));
            cvec.push(Some(Constant::Number(1)));
            cvec.push(Some(Constant::Number(-1)));
            // cvec.push(Some(Constant::Number(i32::MAX)));
            // cvec.push(Some(Constant::Number(i32::MIN)));
            // cvec.push(Some(Constant::Number(i32::MAX-1)));
            cvec.shuffle(rng);
            egraph[id].data.0 = cvec.clone();
            egraph[id].data.1 = cvec.clone();
            var_cvec_map.insert(var, cvec.clone());
            println!("var: {}, cvec: {:?}", var, cvec.clone());
        }
        // implicit sumbumption order here
        for n in &self.consts {
            if let Constant::Number(num) = n {
                egraph.add(SimpleMath::Num(*num));
            }
        }
        egraph
    }

    fn dfs_cycle_extract(
        edges: &IndexMap<usize, Vec<usize>>,
        path: &mut Vec<usize>,
        visited: &mut HashSet<usize>,
        results: &mut Vec<HashSet<usize>>,
        node: usize,
    ) {
        if path.contains(&node) {
            // Cycle!
            let mut set: HashSet<usize> = path
                .split(|i| *i == node)
                .last()
                .unwrap()
                .iter()
                .map(|i| *i)
                .collect();
            set.insert(node);
            // println!("CYCLE DETECTED: {:?}", set);
            results.push(set);
            return;
        }

        if visited.contains(&node) {
            return;
        } else {
            visited.insert(node);
        }

        if edges.contains_key(&node) {
            path.push(node);
            for child in edges.get(&node).unwrap() {
                Self::dfs_cycle_extract(edges, path, visited, results, *child);
            }
            path.pop();
        }
    }

    fn expr_subsumption_cost(pat: &RecExpr<SimpleMath>) -> (usize, isize) {
        let mut vars = HashSet::new();
        for node in pat.as_ref() {
            match node {
                SimpleMath::Var(v) => {
                    vars.insert(v);
                }
                _ => (),
            }
        }
        (pat.as_ref().len(), -(vars.len() as isize))
    }

    fn subsumption_find_best(
        &mut self,
        analysis: SynthAnalysis,
        mut potential_rules: Vec<((Id, RecExpr<SimpleMath>), (Id, RecExpr<SimpleMath>))>,
        poison_rules: &mut HashSet<(RecExpr<SimpleMath>, RecExpr<SimpleMath>)>,
    ) -> Option<((Id, RecExpr<SimpleMath>), (Id, RecExpr<SimpleMath>))> {
        let mut eg = EGraph::new(analysis);

        // Forget all known poison rules
        potential_rules = potential_rules
            .iter()
            .filter(|((_, expr1), (_, expr2))| {
                !poison_rules.contains(&(expr1.clone(), expr2.clone()))
            })
            .map(|r| r.clone())
            .collect();

        let mut ids = vec![];
        let mut eq_index = 0;
        for ((_, expr1), (_, expr2)) in potential_rules.iter() {
            ids.push(eg.add_expr(expr1));
            ids.push(eg.add_expr(expr2));

            // println!("EQ {}: {} => {}", eq_index, expr1, expr2);
            eq_index = eq_index + 1;
        }

        eg.rebuild();

        let mut lhs_to_eqs: HashMap<Id, Vec<(Id, usize)>> = HashMap::default();
        let mut eq_index = 0;
        for chunk in ids.chunks(2) {
            if let [lhs_id, rhs_id] = chunk {
                lhs_to_eqs
                    .entry(*lhs_id)
                    .or_default()
                    .push((*rhs_id, eq_index));
                eq_index = eq_index + 1;
            }
        }

        // f(x, y, ...) => g(x, y, ...) subsumes
        // f(x, h(x, y, ...), ...) => g(x, h(x, y, ...), ...)
        // This means subsumption is covariant across rules, but also
        // that the lhs and rhs substitutions must match each other
        let mut removed_eq_nums: HashSet<usize> = HashSet::default();
        let mut rule_sub_rels = vec![]; // This vec is for if we need a topo sort of the rules
        let mut eq_index = 0;
        let mut var_map = HashMap::default();

        for ((_, expr1), (_, expr2)) in potential_rules.iter() {
            let lhs_pat = generalize(expr1, &mut var_map);
            let rhs_pat = generalize(expr2, &mut var_map);

            for mat in lhs_pat.search(&eg) {
                let sub_lhs_id = mat.eclass;
                let sub_rhs_vec = lhs_to_eqs.get(&sub_lhs_id);
                if sub_rhs_vec.is_some() {
                    for (sub_rhs_id, sub_eq_num) in sub_rhs_vec.unwrap() {
                        let rhs_mat = rhs_pat.search_eclass(&eg, *sub_rhs_id);
                        if rhs_mat.is_some()
                            && *sub_eq_num != eq_index
                            && rhs_mat.unwrap().substs == mat.substs
                        {
                            // Rule subsumption!
                            removed_eq_nums.insert(*sub_eq_num);
                            rule_sub_rels.push((eq_index, sub_eq_num));
                            // println!("RULE SUB: {} > {}", eq_index, sub_eq_num);
                        }
                    }
                }
            }
            eq_index = eq_index + 1;
        }

        // Handle cycles: If n rules subsume each other, we need to pick exactly one to keep
        let mut cycle_groups: Vec<HashSet<usize>> = vec![];

        if !rule_sub_rels.is_empty() {
            let mut rule_sub_map: IndexMap<usize, Vec<usize>> = IndexMap::default();
            let mut rev_rule_sub_map: IndexMap<usize, Vec<usize>> = IndexMap::default();
            for (k, v) in rule_sub_rels.clone() {
                rule_sub_map.entry(k).or_default().push(*v);
                rev_rule_sub_map.entry(*v).or_default().push(k);
            }

            let mut visited: HashSet<usize> = HashSet::default();
            let mut path: Vec<usize> = vec![];

            for i in 0..(potential_rules.len() - 1) {
                if !visited.contains(&i) {
                    // println!("Cycle-checking {}, visited: {:?}", i, visited);
                    Self::dfs_cycle_extract(
                        &rule_sub_map,
                        &mut path,
                        &mut visited,
                        &mut cycle_groups,
                        i,
                    );
                }
            }

            // println!("Cycle groups before: {:?}", cycle_groups);
            let mut i = 0;
            while i < cycle_groups.len() {
                // Consolidate cycles together to find the maximal group of equivalent rules
                let mut cycle = cycle_groups[i].clone();
                let mut j = i + 1;
                while j < cycle_groups.len() {
                    let cycle2 = cycle_groups[j].clone();
                    if !cycle2.is_disjoint(&cycle) {
                        cycle.extend(cycle2.iter());
                        cycle_groups.remove(j);
                    } else {
                        j = j + 1;
                    }
                }
                cycle_groups[i] = cycle.clone();

                // If no rule from outside cycle subsumes any rule from within cycle, then one
                // rule can be chosen to continue and represent this cycle
                let mut parents: HashSet<usize> = HashSet::default();
                for node in cycle.iter() {
                    parents.extend(rev_rule_sub_map[node].clone());
                }
                // println!("CYCLE GROUP: {:?}, parents: {:?}", cycle, parents);
                if parents.is_subset(&cycle) {
                    let rep = cycle.iter().next().unwrap();
                    // println!("CYCLE GROUP: {:?}, REP: {}", cycle, rep);
                    removed_eq_nums.remove(rep);
                }

                i = i + 1;
            }
            // println!("Cycle groups after: {:?}", cycle_groups);
        }

        let mut rem_eq_nums_vec: Vec<&usize> = removed_eq_nums.iter().collect();
        rem_eq_nums_vec.sort();
        rem_eq_nums_vec.reverse();
        let mut candidate_rules = vec![];
        for i in 0..potential_rules.len() {
            candidate_rules.push((potential_rules[i].clone(), i));
        }

        for i in rem_eq_nums_vec {
            candidate_rules.remove(*i);
        }

        loop {
            candidate_rules.sort_by(|(((_, lhs1), (_, rhs1)), _), (((_, lhs2), (_, rhs2)), _)| {
                let cost1: (usize, isize) =
                    Self::expr_subsumption_cost(lhs1).max(Self::expr_subsumption_cost(rhs1));
                let cost2: (usize, isize) =
                    Self::expr_subsumption_cost(lhs2).max(Self::expr_subsumption_cost(rhs2));
                cost1.cmp(&cost2)
            });

            if candidate_rules.len() > 0 {
                let eq = candidate_rules.remove(0);

                // Run rule validator
                let valid_start = Instant::now();
                let is_valid = self.validate_rule(((eq.0).0).1.clone(), ((eq.0).1).1.clone());
                let validation_time = Instant::now().duration_since(valid_start);
                println!("validation time: {:?}", validation_time);
                if is_valid {
                    println!(
                        "VALIDATOR: {} for rule: {:?} => {:?}",
                        is_valid,
                        ((eq.0).0).1.to_string(),
                        ((eq.0).1).1.to_string()
                    );
                    return Some(eq.0);
                } else {
                    println!(
                        "VALIDATOR: {} for rule: {:?} => {:?}",
                        is_valid,
                        ((eq.0).0).1.to_string(),
                        ((eq.0).1).1.to_string()
                    );
                    // Invalidate this rule
                    let cycle_group = cycle_groups.iter().find(|&r| r.contains(&eq.1));
                    // println!("Cycle groups: {:?}", cycle_groups);
                    poison_rules.insert((((eq.0).0).1.clone(), ((eq.0).1).1.clone()));
                    if cycle_group.is_some() {
                        poison_rules.extend(cycle_group.unwrap().iter().map(|&r| {
                            (
                                (potential_rules[r].0).1.clone(),
                                (potential_rules[r].1).1.clone(),
                            )
                        }));
                    }

                    let subed_rules: HashSet<_> = rule_sub_rels
                        .iter()
                        .filter_map(|(r1, r2)| if *r1 == eq.1 { Some(**r2) } else { None })
                        .filter(|r| cycle_group.is_none() || !cycle_group.unwrap().contains(r))
                        .collect();

                    let still_subed: HashSet<_> = rule_sub_rels
                        .iter()
                        .filter(|(r1, r2)| {
                            subed_rules.contains(r2)
                                && *r1 != eq.1
                                && (cycle_group.is_none() || !cycle_group.unwrap().contains(r1))
                        })
                        .map(|(_, r2)| **r2)
                        .collect();

                    candidate_rules.extend(
                        subed_rules
                            .difference(&still_subed)
                            .map(|&r| (potential_rules[r].clone(), r)),
                    );
                }
            } else {
                // println!("NO EQUALITY FOUND");
                return None;
            }
        }
    }

    fn enumerate_preds(
        &mut self,
        eg: &mut EGraph<SimpleMath, SynthAnalysis>,
        phase_b: bool,
        rws: &Vec<Equality<SimpleMath, SynthAnalysis>>,
    ) {
        let mut ids: BTreeSet<Id> = eg.classes().map(|c| c.id).collect();
        let mut preds_to_add = vec![];
        // Phase A: "atoms"
        // Add subset of predicate ops to the new egraph
        for &i in &ids {
            for &j in &ids {
                if i != j {
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        preds_to_add.push(SimpleMath::Neq([i, j]));
                    }
                }
                if find_type(&eg, i) == ExprType::Number && find_type(&eg, j) == ExprType::Number {
                    preds_to_add.push(SimpleMath::Leq([i, j]));
                }
                if find_type(&eg, i) == ExprType::Number && find_type(&eg, j) == ExprType::Number {
                    preds_to_add.push(SimpleMath::Geq([i, j]));
                }
                if i != j {
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        preds_to_add.push(SimpleMath::Lt([i, j]));
                    }
                }
                if i != j {
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        preds_to_add.push(SimpleMath::Gt([i, j]));
                    }
                }
            }
        }
        println!("adding {} exprs Phase A:", preds_to_add.len());
        for pred in preds_to_add {
            let id = eg.add(pred);
            ids.insert(id);
        }
        println!("added phase A exprs");

        // let rules = rws
        //     .iter()
        //     .filter(|eq| eq.cond == None)
        //     .map(|eq| &eq.rewrite);

        // let runner: Runner<SimpleMath, SynthAnalysis, ()> =
        //     Runner::new(eg.analysis.clone()).with_egraph(eg.clone());

        // *eg = runner
        //     .with_time_limit(Duration::from_secs(20))
        //     .with_node_limit(usize::MAX)
        //     .with_iter_limit(5)
        //     .run(rules)
        //     .egraph;

        // eg.rebuild();
        ids = eg.classes().map(|c| eg.find(c.id)).collect();

        if phase_b {
            // Phase B: "combine atoms"
            // may need to do more rounds of these
            preds_to_add = vec![];
            // Add other subset of predicate ops to the egraph
            for &bid1 in &ids {
                for &bid2 in &ids {
                    if bid1 != bid2 {
                        if find_type(&eg, bid1) == ExprType::Boolean
                            && find_type(&eg, bid2) == ExprType::Boolean
                        {
                            preds_to_add.push(SimpleMath::And([bid1, bid2]));
                        }
                        if find_type(&eg, bid1) == ExprType::Boolean
                            && find_type(&eg, bid2) == ExprType::Boolean
                        {
                            preds_to_add.push(SimpleMath::Or([bid1, bid2]));
                        }
                        if find_type(&eg, bid2) == ExprType::Boolean {
                            preds_to_add.push(SimpleMath::Not(bid2));
                        }
                    }
                }
            }
            println!("adding {} exprs", preds_to_add.len());
            for pred in preds_to_add {
                eg.add(pred);
            }
            println!("added phase B exprs");
        }
    }

    fn mk_predicate_egraph(
        &mut self,
        old_eg: &EGraph<SimpleMath, SynthAnalysis>,
        rws: &Vec<Equality<SimpleMath, SynthAnalysis>>,
    ) -> EGraph<SimpleMath, SynthAnalysis> {
        // make a new egraph with ops from the main language
        let mut eg = EGraph::<SimpleMath, SynthAnalysis>::new(old_eg.analysis.clone());
        for var in &self.variables {
            let id = eg.add(SimpleMath::Var(*var));
            let old_id = old_eg.lookup(SimpleMath::Var(*var)).unwrap();
            eg[id].data.0 = old_eg[old_id].data.0.clone();
            eg[id].data.1 = old_eg[old_id].data.1.clone();
        }
        for n in &self.consts {
            if let Constant::Number(num) = n {
                eg.add(SimpleMath::Num(*num));
            }
        }
        println!("number of eclasses: {}", eg.number_of_classes());
        // let mut ids: BTreeSet<Id> = eg.classes().map(|c| c.id).collect();
        // let mut enodes_to_add = vec![];
        // for &i in &ids {
        //     for &j in &ids {
        //         if find_type(&eg, i) == ExprType::Number && find_type(&eg, j) == ExprType::Number {
        //             enodes_to_add.push(SimpleMath::Add([i, j]));
        //         }
        //         if find_type(&eg, i) == ExprType::Number && find_type(&eg, j) == ExprType::Number {
        //             enodes_to_add.push(SimpleMath::Sub([i, j]));
        //         }
        //         if find_type(&eg, i) == ExprType::Number && find_type(&eg, j) == ExprType::Number {
        //             enodes_to_add.push(SimpleMath::Mul([i, j]));
        //         }
        //         if find_type(&eg, i) == ExprType::Number {
        //             enodes_to_add.push(SimpleMath::Neg(i));
        //         }
        //         if find_type(&eg, i) == ExprType::Number && find_type(&eg, j) == ExprType::Number {
        //             enodes_to_add.push(SimpleMath::Div([i, j]));
        //         }
        //         if find_type(&eg, i) == ExprType::Number {
        //             enodes_to_add.push(SimpleMath::Abs(i));
        //         }
        //     }
        // }
        // for enode in enodes_to_add {
        //     ids.insert(eg.add(enode));
        // }
        self.enumerate_preds(&mut eg, false, rws);
        eg
    }

    fn learn_cond_rules(
        &mut self,
        eg: &mut EGraph<SimpleMath, SynthAnalysis>,
        num_cond_iters: usize,
        num_rand_idx: usize,
        rws: &Vec<Equality<SimpleMath, SynthAnalysis>>,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        // some helper closures
        let same_cvec = |v1: &Vec<Option<Constant>>, v2: &Vec<Option<Constant>>| {
            v1.iter().zip(v2.iter()).all(|(x, y)| match (x, y) {
                (Some(Constant::Boolean(true)), Some(Constant::Boolean(true))) => true,
                (Some(Constant::Boolean(false)), Some(Constant::Boolean(false))) => true,
                (_, _) => false,
            })
        };

        let no_free_vars =
            |c: RecExpr<SimpleMath>, expr1: RecExpr<SimpleMath>, expr2: RecExpr<SimpleMath>| {
                let mut c_vars: HashSet<&Symbol> = HashSet::new();
                let mut e_vars: HashSet<&Symbol> = HashSet::new();

                c.as_ref().iter().for_each(|en| {
                    if let SimpleMath::Var(v) = en {
                        c_vars.insert(v);
                    }
                });
                expr1.as_ref().iter().for_each(|en| {
                    if let SimpleMath::Var(v) = en {
                        e_vars.insert(v);
                    }
                });
                expr2.as_ref().iter().for_each(|en| {
                    if let SimpleMath::Var(v) = en {
                        e_vars.insert(v);
                    }
                });
                c_vars.iter().all(|cv| e_vars.contains(cv))
            };

        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];
        let ids: BTreeSet<Id> = eg.classes().map(|c| c.id).collect();
        let pred_eg = self.mk_predicate_egraph(eg, rws);
        let cvec_len = self.n_samples + self.consts.len();
        let mut rand_indices = vec![];
        // all predicate eclass cvecs and Ids
        let pred_datas: Vec<(Vec<Option<Constant>>, Id)> = pred_eg
            .classes()
            .cloned()
            .map(|c| (c.data.1, c.id))
            .collect();

        for _iter in 0..num_cond_iters {
            for _i in 0..num_rand_idx {
                rand_indices.push(self.rng.gen_range(0, cvec_len));
            }
            let mut by_rand_idx: IndexMap<Vec<(&usize, Option<Constant>)>, Vec<Id>> =
                IndexMap::new();

            for id in &ids {
                let mut values_at_rand_idxs = vec![];
                for idx in &rand_indices {
                    let data = eg[*id].data.1[*idx];
                    if data != None {
                        values_at_rand_idxs.push((idx, data)); // get a few non-None cvec values at random points
                    }
                }
                by_rand_idx
                    .entry(values_at_rand_idxs)
                    .or_default()
                    .push(*id);
            }

            by_rand_idx.retain(|_, vs| vs.len() > 1);
            println!("by_rand_idx len {}", by_rand_idx.len());

            for same_at_rands in by_rand_idx.keys() {
                let ecs: Vec<&Id> = by_rand_idx
                    .get(same_at_rands)
                    .unwrap()
                    .iter()
                    // remove ids that have all None values
                    .filter(|id| !&eg[**id].data.1.iter().all(|v| v == &None))
                    .collect();
                println!("ecs len: {}", ecs.len());
                let mut ctr = 0;
                for i in &ecs {
                    for j in &ecs {
                        // println!("i: {}", ctr);
                        ctr = ctr + 1;
                        let mut agreement_vec: Vec<Option<Constant>> = Vec::new();
                        if i != j && eg[**i].data.1 != eg[**j].data.1
                        // they must have _some_ None values
                        // && (eg[**i].data.contains(&None) || eg[**j].data.contains(&None))
                        {
                            // println!("some none value present");
                            let ds = eg[**i].data.1.iter().zip(eg[**j].data.1.iter()).enumerate();
                            // all indices where either cvecs have None
                            let i_nones: Vec<usize> = ds
                                .clone()
                                .filter(|(_, (x, y))| *x == &None || *y == &None)
                                .map(|(i, _)| i)
                                .collect();

                            // println!("inones found");
                            // all indices where neither cvec are None but they are different
                            let non_none_diff_poses: Vec<usize> = ds
                                .filter(|(_, (x, y))| *x != &None && *y != &None && x != y)
                                .map(|(i, _)| i)
                                .collect();

                            // the cvecs may be different or None at most at cond_diff_thresh positions
                            if non_none_diff_poses.len() <= self.cond_diff_thresh {
                                // println!("start making agreement vec");
                                for idx in 0..eg[**j].data.1.len() {
                                    if i_nones.contains(&idx) || non_none_diff_poses.contains(&idx)
                                    {
                                        // cvecs disagree at None positions
                                        agreement_vec.push(Some(Constant::Boolean(false)));
                                    } else {
                                        // cvecs agree elsewhere
                                        agreement_vec.push(Some(Constant::Boolean(true)));
                                    }
                                }
                                let mut extract = Extractor::new(&eg, AstSize);
                                let (_cost1, expr1) = extract.find_best(**i);
                                let (_cost2, expr2) = extract.find_best(**j);

                                let mut cond_extract = Extractor::new(&pred_eg, AstSize);
                                let cond = match pred_datas.iter().find(|(ec_data, cond_id)| {
                                    same_cvec(&ec_data, &agreement_vec)
                                        && no_free_vars(
                                            cond_extract.find_best(*cond_id).1,
                                            expr1.clone(),
                                            expr2.clone(),
                                        )
                                        && !(ec_data
                                            .iter()
                                            .all(|v| v == &Some(Constant::Boolean(false))))
                                        && !(ec_data
                                            .iter()
                                            .all(|v| v == &Some(Constant::Boolean(true))))
                                }) {
                                    None => None,
                                    Some((_, id)) => Some(cond_extract.find_best(*id).1),
                                };

                                //TODO: we will  likely need to do this, using inverse subsumption order
                                //let cond = most_specific(conds);

                                if cond.is_some() {
                                    let names = &mut HashMap::default();
                                    let c = generalize(&cond.clone().unwrap(), names);
                                    let pat1 = generalize(&expr1, names);
                                    let pat2 = generalize(&expr2, names);
                                    if pattern_has_pred(&c)
                                        && !pattern_has_pred(&pat1)
                                        && !pattern_has_pred(&pat2)
                                    {
                                        println!("cond: {} => {}, if {}", pat1, pat2, c);
                                        equalities.extend(Equality::new(pat1, pat2, Some(c)))
                                    }
                                    let names = &mut HashMap::default();
                                    let c = generalize(&cond.clone().unwrap(), names);
                                    let pat1 = generalize(&expr2, names);
                                    let pat2 = generalize(&expr1, names);
                                    if pattern_has_pred(&c.clone())
                                        && !pattern_has_pred(&pat1)
                                        && !pattern_has_pred(&pat2)
                                    {
                                        equalities.extend(Equality::new(pat1, pat2, Some(c)))
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut set = HashSet::new();
        equalities.retain(|eq| set.insert(eq.name.clone()));
        println!("{} conditional rules: ", equalities.len());
        for eq in &equalities {
            println!("  {}", eq);
        }
        return equalities;
    }

    pub fn update_clean_egraph(
        &mut self,
        tainted_eg: EGraph<SimpleMath, SynthAnalysis>,
        mut clean_eg: EGraph<SimpleMath, SynthAnalysis>,
    ) -> EGraph<SimpleMath, SynthAnalysis> {
        let clean_ids: Vec<Id> = clean_eg.classes().map(|ec| ec.id).collect();
        for id in clean_ids {
            clean_eg.union(id, tainted_eg.find(id));
        }
        return clean_eg;
    }

    pub fn run(
        &mut self,
        _num_ops: usize,
        _conditional: bool,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];
        let mut eg = self.mk_egraph();
        let mut my_ids: BTreeSet<Id> = eg.classes().map(|c| c.id).collect();
        let mut metrics = metrics::RulerProfile::new();
        let mut poison_rules: HashSet<(RecExpr<SimpleMath>, RecExpr<SimpleMath>)> =
            HashSet::default();

        for iter in 0..self.n_iter {
            my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();
            let mut enodes_to_add = vec![];

            println!(
                "iter {} phase 1: adding ops over {} eclasses",
                iter,
                my_ids.len(),
            );

            let before = Instant::now();
            for &i in &my_ids {
                for &j in &my_ids {
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Add([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Sub([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Mul([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number {
                        enodes_to_add.push(SimpleMath::Neg(i));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Div([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number {
                        enodes_to_add.push(SimpleMath::Abs(i));
                    }
                }
            }

            for enode in enodes_to_add {
                my_ids.insert(eg.add(enode));
            }
            let adding_exprs = Instant::now().duration_since(before);

            println!(
                "number of eclasses after enumeration in normal rules: {}",
                eg.number_of_classes()
            );

            let mut nloop = 0;
            loop {
                nloop += 1;
                let before = Instant::now();
                let mut tainted_eg = eg.clone();
                let cloning_pristine = Instant::now().duration_since(before);

                println!(
                    "iter {} phase 2: before running rules, tainted eg has: enodes = {}, eclasses = {}",
                    iter,
                    tainted_eg.total_size(),
                    tainted_eg.number_of_classes()
                );

                let before = Instant::now();
                let mut set = HashSet::new();
                equalities.retain(|eq| set.insert(eq.name.clone()));

                let rules = equalities
                    .iter()
                    .filter(|eq| eq.cond == None)
                    .map(|eq| &eq.rewrite);

                // for r in rules.clone() {
                //     println!("{:?}", r.name());
                // }
                let clean_rules = Instant::now().duration_since(before);

                let runner: Runner<SimpleMath, SynthAnalysis, ()> =
                    Runner::new(tainted_eg.analysis.clone()).with_egraph(tainted_eg);

                let before = Instant::now();
                let before_eqsat_eclasses = eg.number_of_classes();
                let before_eqsat_enodes = eg.total_number_of_nodes();
                tainted_eg = runner
                    .with_time_limit(Duration::from_secs(20))
                    .with_node_limit(usize::MAX)
                    .with_iter_limit(5)
                    //.with_scheduler(SimpleScheduler)
                    .run(rules)
                    .egraph;

                let tainted_eqsat = Instant::now().duration_since(before);

                let before = Instant::now();
                eg = self.update_clean_egraph(tainted_eg, eg);
                let update_pristine = Instant::now().duration_since(before);

                eg.rebuild();

                my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();

                println!(
                    "       phase 2: after running {} rules, tainted eg has enodes = {}, eclasses = {}",
                    &equalities.len(),
                    eg.total_size(),
                    eg.number_of_classes()
                );

                let before_cvec_eclasses = eg.number_of_classes();
                let before_cvec_enodes = eg.total_number_of_nodes();

                println!("iter {} phase 3: discover rules", iter);
                let before = Instant::now();
                let mut by_cvec_some: IndexMap<&Vec<Option<Constant>>, Vec<Id>> = IndexMap::new();

                for class in eg.classes() {
                    if my_ids.contains(&class.id) {
                        if !class.data.0.contains(&None) {
                            // the ids corresponding to a specific cvec key in this hash map are for normal rewrites and can be unioned.
                            by_cvec_some
                                .entry(&class.data.0)
                                .or_default()
                                .push(class.id);
                        }
                    }
                }
                let cvec_grouping = Instant::now().duration_since(before);
                let cvec_groups = by_cvec_some.len();
                let mut extract = Extractor::new(&eg, AstSize);

                // For subsumption:
                // Within each cvec class, we need the two "best" expressions
                // --> That's easy, we determine it first by ast size then by number of variables
                // Among possible equalities then run rule subsumption to filter out useless rules
                // From the remainder, either:
                //    1. Learn all rules at once (probably bad)
                //    2. Sort then by ast size/num of variables for all lhs and rhs
                //       and pick the rule with the lowest max (probably better)

                let by_cvec_recexps: Vec<Vec<(Id, RecExpr<SimpleMath>)>> = by_cvec_some
                    .iter()
                    .map(|(_, ids)| {
                        ids.iter()
                            .map(|id| (*id, extract.find_best(*id).1))
                            .collect()
                    })
                    .collect();

                let mut potential_rules = vec![];

                for exprs in by_cvec_recexps {
                    for (id1, expr1) in exprs.clone() {
                        for (id2, expr2) in exprs.clone() {
                            if id1 < id2 {
                                potential_rules.push(((id1, expr1.clone()), (id2, expr2)));
                            }
                        }
                    }
                }

                let before = Instant::now();
                let learn_a_rule: Duration;
                let after_cvec_eclasses: usize;
                let after_cvec_enodes: usize;
                let mut learned_rule = String::new();

                println!("subsumption potential rules: {}", potential_rules.len());
                let best = self.subsumption_find_best(
                    eg.analysis.clone(),
                    potential_rules,
                    &mut poison_rules,
                );

                if let Some(((id1, expr1), (id2, expr2))) = best {
                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr1, names);
                    let pat2 = generalize(&expr2, names);
                    if !pattern_has_pred(&pat1) && !pattern_has_pred(&pat2) {
                        if let Some(eq) = Equality::new(pat1, pat2, None) {
                            if equalities.iter().all(|e| e.name != eq.name) {
                                learned_rule = eq.name.clone();
                                equalities.push(eq)
                            }
                        }
                    }

                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr2, names);
                    let pat2 = generalize(&expr1, names);
                    if !pattern_has_pred(&pat1) && !pattern_has_pred(&pat2) {
                        if let Some(eq) = Equality::new(pat1, pat2, None) {
                            println!("Learned rule: {} => {}", expr2, expr1);
                            if equalities.iter().all(|e| e.name != eq.name) {
                                equalities.push(eq)
                            }
                        }
                    }
                    eg.union(id1, id2);
                    after_cvec_eclasses = eg.number_of_classes();
                    after_cvec_enodes = eg.total_number_of_nodes();
                    learn_a_rule = Instant::now().duration_since(before);
                } else {
                    break;
                }
                metrics.record(
                    iter,
                    adding_exprs,
                    nloop,
                    cloning_pristine,
                    clean_rules,
                    tainted_eqsat,
                    update_pristine,
                    cvec_grouping,
                    learn_a_rule,
                    before_eqsat_eclasses,
                    before_eqsat_enodes,
                    before_cvec_eclasses,
                    before_cvec_enodes,
                    after_cvec_eclasses,
                    after_cvec_enodes,
                    cvec_groups,
                    learned_rule,
                );
            }
            println!(
                "After iter {}, I know {} equalities:",
                iter,
                equalities.len()
            );
            for eq in &equalities {
                println!("  {}", eq);
            }
        }
        metrics.print_to_file();
        if _conditional {
            let cond_rws = self.learn_cond_rules(
                &mut eg,
                // var_cvecs,
                self.cond_rule_iters,
                self.cond_rule_rand_idx,
                &equalities,
            );
            equalities.extend(cond_rws);
        }
        let posion_map = &mut HashMap::default();
        let mut gen_poison_rules: Vec<Equality<SimpleMath, SynthAnalysis>> = Vec::new();
        let mut poison_set = HashSet::new();
        for (pl, pr) in poison_rules {
            let pl_gen = generalize(&pl, posion_map);
            let pr_gen = generalize(&pr, posion_map);
            // println!("{} => {}", pl_gen, pr_gen);
            if let Some(eq) = Equality::new(pl_gen, pr_gen, None) {
                if gen_poison_rules.iter().all(|e| e.name != eq.name) {
                    gen_poison_rules.push(eq);
                }
            }
        }
        gen_poison_rules.retain(|eq| poison_set.insert(eq.name.clone()));
        // println!("posion rule set size: {}", gen_poison_rules.len());
        equalities
    }

    // if there is an input that is bad, add it to the cvec and recompute them.
    // TODO: recompute all cvecs for egraph from there.
    // Altenative: add the rule to poison set. When you pick best, dont pick from poison rule
    fn validate_rule(&mut self, lhs: RecExpr<SimpleMath>, rhs: RecExpr<SimpleMath>) -> bool {
        // just setting some values manually
        let values: Vec<Constant> = vec![
            num!(-1),
            num!(0),
            num!(1),
            num!(-625956435),
            num!(1537958233),
            num!(2147483646),
            num!(i32::MAX),
            num!(i32::MIN),
        ];
        let mut env: Ctx = HashMap::new();
        let mut envs = vec![];
        // TODO: assuming only three variables for now
        // for valx in &values {
        //     env.insert("x", *valx);
        //     for valy in &values {
        //         env.insert("y", *valy);
        //         for valz in &values {
        //             env.insert("z", *valz);
        //             envs.push(env.clone());
        //         }
        //     }
        // }
        // let rng = &mut self.rng;
        for i in -20..21 {
            for j in -20..21 {
                for k in -20..21 {
                    env.insert("x", num!(i));
                    env.insert("y", num!(j));
                    env.insert("z", num!(k));
                    envs.push(env.clone());
                }
            }
        }
        // add more random inputs for testing
        // take the max values out of cvec and fuzz test [-10, 10]
        // for i in 0..100000 {
        //     let (x, y, z) = (rng.gen::<i32>(), rng.gen::<i32>(), rng.gen::<i32>());
        //     env.insert("x", num!(x));
        //     env.insert("y", num!(y));
        //     env.insert("z", num!(z));
        //     envs.push(env.clone());
        // }
        let c_eg = envs.iter().find(|env| {
            let l = eval(&env, lhs.as_ref());
            let r = eval(&env, rhs.as_ref());
            match (l, r) {
                (None, _) | (_, None) => true,
                (Some(l), Some(r)) => l != r,
            }
        });

        if let Some(c_eg) = c_eg {
            println!("lhs: {:?}", eval(c_eg, lhs.as_ref()));
            println!("rhs: {:?}", eval(c_eg, rhs.as_ref()));
            println!("false at input: {:?}", c_eg);
            false
        } else {
            true
        }
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

struct TypeBasedSearcher<F, S> {
    typefilter: F,
    searcher: S,
}

impl<F, S, L, A> egg::Searcher<L, A> for TypeBasedSearcher<F, S>
where
    L: Language,
    A: Analysis<L>,
    S: Searcher<L, A>,
    F: Fn(&EGraph<L, A>, Id) -> bool,
{
    fn search_eclass(&self, egraph: &EGraph<L, A>, eclass: Id) -> Option<SearchMatches> {
        if (self.typefilter)(egraph, eclass) {
            self.searcher.search_eclass(egraph, eclass)
        } else {
            None
        }
    }

    fn vars(&self) -> Vec<Var> {
        self.searcher.vars()
    }
}

fn find_type(eg: &EGraph<SimpleMath, SynthAnalysis>, id: Id) -> ExprType {
    match &eg[id].data.0[0] {
        Some(Constant::Number(_)) => ExprType::Number,
        Some(Constant::Boolean(_)) => ExprType::Boolean,
        // TODO: this is a bug. What if the first element is None due to div or other ops?
        None => ExprType::Invalid,
    }
}

impl Equality<SimpleMath, SynthAnalysis> {
    fn new(
        lhs: Pattern<SimpleMath>,
        rhs: Pattern<SimpleMath>,
        cond: Option<Pattern<SimpleMath>>,
    ) -> Option<Self> {
        if let Some(cond) = cond {
            let name = format!("{} => {} if {}", lhs, rhs, cond);
            // only run rules over non-predicate expressions since we already filter out predicate rules
            let f = |eg: &EGraph<_, SynthAnalysis>, id| find_type(eg, id) == ExprType::Number;
            let searcher = TypeBasedSearcher {
                typefilter: f,
                searcher: lhs.clone(),
            };

            let applier: ConditionalApplier<
                ConditionEqual<Pattern<SimpleMath>, Pattern<SimpleMath>>,
                Pattern<SimpleMath>,
            > = ConditionalApplier {
                applier: rhs.clone(),
                condition: ConditionEqual(cond.clone(), "true".parse().unwrap()),
            };

            let rw = egg::Rewrite::new(name.clone(), name.clone(), searcher, applier).ok()?;

            Some(Self {
                lhs,
                rhs,
                cond: Some(cond),
                name,
                rewrite: rw.clone(),
            })
        } else {
            let name = format!("{} => {}", lhs, rhs);
            // only run rules over non-predicate expressions since we already filter out predicate rules
            let f = |eg: &EGraph<_, SynthAnalysis>, id| find_type(eg, id) == ExprType::Number;
            let searcher = TypeBasedSearcher {
                typefilter: f,
                searcher: lhs.clone(),
            };

            let applier = rhs.clone();
            let rw = egg::Rewrite::new(name.clone(), name.clone(), searcher, applier).ok()?;

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
        L: Language + 'static,
        A: Analysis<L> + Default + 'static,
    {
        let mut rules: Vec<Rewrite<L, A>> = Vec::new();
        for eq in eqs {
            let l = eq.lhs.clone();
            let r = eq.rhs.clone();
            let rule = rewrite!(eq.name.clone(); l => r);
            rules.push(rule);
        }
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
            .run(&rules);
        let id_a = runner.egraph.find(runner.roots[0]);
        let id_b = runner.egraph.find(runner.roots[1]);

        if id_a != id_b {
            panic!("Failed to simplify {} => {}, {}, {}", a, b, id_a, id_b);
        }
    }

    #[test]
    fn super_simple() {
        let mut param = SynthParam {
            rng: SeedableRng::seed_from_u64(5),
            n_iter: 1,
            n_samples: 25,
            variables: vec!["x".into(), "y".into(), "z".into()],
            consts: vec![
                Constant::Number(0),
                Constant::Number(1),
                Constant::Number(-1),
            ],
            cond_rule_iters: 1,
            cond_rule_rand_idx: 1,
            cond_diff_thresh: 3,
        };

        let eqs = param.run(13, false);

        check_proves(&eqs, "(+ a b)", "(+ b a)");
        check_proves(&eqs, "(+ a 0)", "a");
        check_proves(&eqs, "(+ 0 a)", "a");
    }
}
