mod metrics;
use egg::*;
use indexmap::IndexMap;
use rand::{prelude::SliceRandom, Rng};
use rand_pcg::Pcg64;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    fmt::Formatter,
    time::Duration,
};

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
        "~" = Neg(Id),
        "!" = Not(Id),
        Bool(bool),
        Num(i32),
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
        // a closure to get the cvec for some eclass
        let x = |i: &Id| egraph[*i].data.iter().copied();
        let params = &egraph.analysis;
        match enode {
            SimpleMath::Num(n) => (0..params.cvec_len)
                .map(|_| Some(Constant::Number(*n)))
                .collect(),
            SimpleMath::Bool(b) => (0..params.cvec_len)
                .map(|_| Some(Constant::Boolean(*b)))
                .collect(),
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

fn instantiate(pattern: &Pattern<SimpleMath>) -> RecExpr<SimpleMath> {
    let nodes: Vec<_> = pattern
        .ast
        .as_ref()
        .iter()
        .map(|n| match n {
            ENodeOrVar::ENode(n) => n.clone(),
            ENodeOrVar::Var(v) => {
                let s = v.to_string();
                assert!(s.starts_with('?'));
                SimpleMath::Var(s[1..].into())
            }
        })
        .collect();

    RecExpr::from(nodes)
}

fn subsumption_filter(
    analysis: SynthAnalysis,
    equalities: &mut Vec<Equality<SimpleMath, SynthAnalysis>>,
) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
    let mut set = HashSet::new();
    equalities.retain(|eq| set.insert(eq.name.clone()));

    let mut eg = EGraph::new(analysis);

    let mut ids = vec![];
    let mut eq_index = 0;
    for eq in equalities.iter() {
        ids.push(eg.add_expr(&instantiate(&eq.lhs)));
        ids.push(eg.add_expr(&instantiate(&eq.rhs)));

        println!("EQ {}: {}", eq_index, eq);
        eq_index = eq_index + 1;
    }

    eg.rebuild();
    
    let mut lhs_to_eqs : HashMap<Id, Vec<(Id, usize)>> = HashMap::default();
    let mut eq_index = 0;
    for chunk in ids.chunks(2) {
        if let [lhs_id, rhs_id] = chunk {
            lhs_to_eqs.entry(*lhs_id).or_default().push((*rhs_id, eq_index));
            eq_index = eq_index + 1;
        }
    }

    // f(x, y, ...) => g(x, y, ...) subsumes
    // f(x, h(x, y, ...), ...) => g(x, h(x, y, ...), ...)
    // This means subsumption is covariant across rules, but also 
    // that the lhs and rhs substitutions must match each other
    let mut removed_eq_nums : HashSet<usize> = HashSet::default();
    let mut _rule_sub_rels = vec![]; // This vec is for if we need a topo sort of the rules
    let mut eq_index = 0;
    for eq in equalities.iter() {
        for mat in eq.lhs.search(&eg) {
            let sub_lhs_id = mat.eclass;
            let sub_rhs_vec = lhs_to_eqs.get(&sub_lhs_id);
            if sub_rhs_vec.is_some() {
                for (sub_rhs_id, sub_eq_num) in sub_rhs_vec.unwrap() {
                    let rhs_mat = eq.rhs.search_eclass(&eg, *sub_rhs_id);
                    if rhs_mat.is_some() && *sub_eq_num != eq_index && rhs_mat.unwrap().substs == mat.substs {
                        // Rule subsumption!
                        removed_eq_nums.insert(*sub_eq_num);
                        _rule_sub_rels.push((eq_index, sub_eq_num));
                        println!("RULE SUB: {} > {}", eq_index, sub_eq_num);
                    }
                }
            }
        }
        eq_index = eq_index + 1;
    }

    // TODO: Cycle removal. If n rules are equivalent and subsume each other,
    // then make sure to keep one instead of throwing them all away

    // TODO: One at a time. Use _rule_sub_rels to topo sort the rules in order of subsumption.
    // Then we can try adding them to the egraph in that order and skipping the superfluous ones.

    // TODO: Fix bug, subsumption not always found :(

    let mut removed_eqs = vec![];
    let mut rem_eq_nums_vec : Vec<&usize> = removed_eq_nums.iter().collect();
    rem_eq_nums_vec.sort();
    rem_eq_nums_vec.reverse();
    for i in rem_eq_nums_vec {
        removed_eqs.push(equalities.remove(*i));
    }

    println!("NUM REMOVED: {}", removed_eqs.len());
    removed_eqs
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
    pub diff_thresh: usize,
}

impl SynthParam {
    fn mk_egraph(&mut self) -> EGraph<SimpleMath, SynthAnalysis> {
        let mut egraph = EGraph::new(SynthAnalysis {
            // for now just adding 0 and 1 forcefully to the cvecs for variables
            // to test conditional rules for division
            cvec_len: self.n_samples + self.consts.len(),
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
        // implicit sumbumption order here
        for n in &self.consts {
            if let Constant::Number(num) = n {
                egraph.add(SimpleMath::Num(*num));
            }
        }
        egraph
    }

    fn expr_subsumption_cost(
        pat: &RecExpr<SimpleMath>
    ) -> (usize, isize) {
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
        analysis: SynthAnalysis,
        mut best_by_cvec: Vec<((Id, RecExpr<SimpleMath>), (Id, RecExpr<SimpleMath>))>,
    ) -> Option<((Id, RecExpr<SimpleMath>), (Id, RecExpr<SimpleMath>))> {
        let mut eg = EGraph::new(analysis);

        let mut ids = vec![];
        let mut eq_index = 0;
        for ((_, expr1), (_, expr2)) in best_by_cvec.iter() {
            ids.push(eg.add_expr(expr1));
            ids.push(eg.add_expr(expr2));

            println!("EQ {}: {} => {}", eq_index, expr1, expr2);
            eq_index = eq_index + 1;
        }

        eg.rebuild();
        
        let mut lhs_to_eqs : HashMap<Id, Vec<(Id, usize)>> = HashMap::default();
        let mut eq_index = 0;
        for chunk in ids.chunks(2) {
            if let [lhs_id, rhs_id] = chunk {
                lhs_to_eqs.entry(*lhs_id).or_default().push((*rhs_id, eq_index));
                eq_index = eq_index + 1;
            }
        }

        // f(x, y, ...) => g(x, y, ...) subsumes
        // f(x, h(x, y, ...), ...) => g(x, h(x, y, ...), ...)
        // This means subsumption is covariant across rules, but also 
        // that the lhs and rhs substitutions must match each other
        let mut removed_eq_nums : HashSet<usize> = HashSet::default();
        let mut _rule_sub_rels = vec![]; // This vec is for if we need a topo sort of the rules
        let mut eq_index = 0;
        let mut var_map = HashMap::default();

        for ((_, expr1), (_, expr2)) in best_by_cvec.iter() {
            let lhs_pat = generalize(expr1, &mut var_map);
            let rhs_pat = generalize(expr2, &mut var_map);

            for mat in lhs_pat.search(&eg) {
                let sub_lhs_id = mat.eclass;
                let sub_rhs_vec = lhs_to_eqs.get(&sub_lhs_id);
                if sub_rhs_vec.is_some() {
                    for (sub_rhs_id, sub_eq_num) in sub_rhs_vec.unwrap() {
                        let rhs_mat = rhs_pat.search_eclass(&eg, *sub_rhs_id);
                        if rhs_mat.is_some() && *sub_eq_num != eq_index && rhs_mat.unwrap().substs == mat.substs {
                            // Rule subsumption!
                            removed_eq_nums.insert(*sub_eq_num);
                            _rule_sub_rels.push((eq_index, sub_eq_num));
                            println!("RULE SUB: {} > {}", eq_index, sub_eq_num);
                        }
                    }
                }
            }
            eq_index = eq_index + 1;
        }

        let mut rem_eq_nums_vec : Vec<&usize> = removed_eq_nums.iter().collect();
        rem_eq_nums_vec.sort();
        rem_eq_nums_vec.reverse();
        for i in rem_eq_nums_vec {
            best_by_cvec.remove(*i);
        }
        best_by_cvec.sort_by(|((_, lhs1), (_, rhs1)), ((_, lhs2), (_, rhs2))| {
            let cost1 : (usize, isize) = Self::expr_subsumption_cost(lhs1).max(Self::expr_subsumption_cost(rhs1));
            let cost2 : (usize, isize) = Self::expr_subsumption_cost(lhs2).max(Self::expr_subsumption_cost(rhs2));
            cost1.cmp(&cost2)
        });

        if best_by_cvec.len() > 0 {
            let eq = best_by_cvec.remove(0);
            println!("PICKED: {} => {}", (eq.0).1, (eq.1).1);
            Some(eq)
        } else {
            println!("NO EQUALITY FOUND");
            None
        }
    }

    fn learn_cond_rules(
        &self,
        conditional: bool,
        eg: &EGraph<SimpleMath, SynthAnalysis>,
        added: Vec<Id>,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let ids: Vec<Id> = eg.classes().map(|c| c.id).collect();
        let mut extract = Extractor::new(&eg, AstSize);
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];

        let only_diff_at_none_poses =
            |(pos, (x, y)), none_poses: Vec<usize>| none_poses.contains(&pos) || x == y;

        let all_false = |cvec: &Vec<Option<Constant>>| {
            cvec.iter().all(|v| v == &Some(Constant::Boolean(false)))
        };

        // all eclass cvecs and Ids
        let ec_datas: Vec<(Vec<Option<Constant>>, Id)> =
            eg.classes().cloned().map(|c| (c.data, c.id)).collect();

        let same_cvec = |v1: &Vec<Option<Constant>>, v2: &Vec<Option<Constant>>| {
            v1.iter().zip(v2.iter()).all(|(x, y)| match (x, y) {
                (Some(Constant::Boolean(true)), Some(Constant::Boolean(true))) => true,
                (Some(Constant::Boolean(false)), Some(Constant::Boolean(false))) => true,
                (_, _) => false,
            })
        };

        let no_free_vars =
            |cond_ec: &Id, expr1: RecExpr<SimpleMath>, expr2: RecExpr<SimpleMath>| {
                let mut extract = Extractor::new(&eg, AstSize);
                let mut c_vars: HashSet<&Symbol> = HashSet::new();
                let mut e_vars: HashSet<&Symbol> = HashSet::new();
                let c = extract.find_best(*cond_ec).1;

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

        if conditional {
            for &i in &ids {
                for &j in &ids {
                    let mut agreement_vec: Vec<Option<Constant>> = Vec::new();

                    if i != j
                        && eg[i].data != eg[j].data
                        && (eg[i].data.contains(&None) || eg[j].data.contains(&None))
                    {
                        // all indices where either cvecs have None
                        let i_nones: Vec<usize> = eg[i]
                            .data
                            .iter()
                            .zip(eg[j].data.iter())
                            .enumerate()
                            .filter(|(_, (x, y))| *x == &None || *y == &None)
                            .map(|(i, _)| i)
                            .collect();

                        // if the cvecs are same everywhere else, then consider for potential conditional rule
                        if eg[i].data.iter().zip(eg[j].data.iter()).enumerate().all(
                            |(i, (x, y))| only_diff_at_none_poses((i, (x, y)), i_nones.clone()), //&& i_nones.clone().len() <= self.diff_thresh
                        ) {
                            for idx in 0..eg[j].data.len() {
                                if i_nones.contains(&idx) {
                                    // cvecs disagree at None positions
                                    agreement_vec.push(Some(Constant::Boolean(false)));
                                } else {
                                    // cvecs agree elsewhere
                                    agreement_vec.push(Some(Constant::Boolean(true)));
                                }
                            }

                            let (_cost1, expr1) = extract.find_best(i);
                            let (_cost2, expr2) = extract.find_best(j);

                            let cond = match ec_datas.iter().find(|(ec_data, cond_id)| {
                                same_cvec(&ec_data, &agreement_vec)
                                    && !all_false(&ec_data)
                                    && no_free_vars(cond_id, expr1.clone(), expr2.clone())
                            }) {
                                None => None,
                                Some((_, id)) => Some(extract.find_best(*id).1),
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
                            }
                        }
                    }
                }
            }
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
        num_ops: usize,
        _conditional: bool,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];
        let mut eg = self.mk_egraph();
        let mut my_ids: BTreeSet<Id> = eg.classes().map(|c| c.id).collect();

        let mut eqsat_iter = 0;
        let mut metrics = metrics::EgraphStats::new();

        for iter in 0..self.n_iter {
            my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();

            println!(
                "iter {} phase 1: adding ops over {} eclasses",
                iter,
                my_ids.len(),
            );
            let mut enodes_to_add = vec![];

            for &i in &my_ids {
                for &j in &my_ids {
                    eqsat_iter = eqsat_iter + 1;

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
                    /*
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Div([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Boolean {
                        enodes_to_add.push(SimpleMath::Not(i));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Leq([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Geq([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Boolean
                        && find_type(&eg, j) == ExprType::Boolean
                    {
                        enodes_to_add.push(SimpleMath::And([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Boolean
                        && find_type(&eg, j) == ExprType::Boolean
                    {
                        enodes_to_add.push(SimpleMath::Or([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number {
                        enodes_to_add.push(SimpleMath::Neg(i));
                    }
                    if i != j {
                        if find_type(&eg, i) == ExprType::Number
                            && find_type(&eg, j) == ExprType::Number
                        {
                            enodes_to_add.push(SimpleMath::Neq([i, j]));
                        } else if find_type(&eg, i) == ExprType::Boolean
                            && find_type(&eg, j) == ExprType::Boolean
                        {
                            enodes_to_add.push(SimpleMath::Neq([i, j]));
                        }
                    }
                    if i != j {
                        if find_type(&eg, i) == ExprType::Number
                            && find_type(&eg, j) == ExprType::Number
                        {
                            enodes_to_add.push(SimpleMath::Lt([i, j]));
                        }
                    }
                    if i != j {
                        if find_type(&eg, i) == ExprType::Number
                            && find_type(&eg, j) == ExprType::Number
                        {
                            enodes_to_add.push(SimpleMath::Gt([i, j]));
                        }
                    } */
                }
            }

            for enode in enodes_to_add {
                my_ids.insert(eg.add(enode));
            }

            loop {
                eg.rebuild();

                let mut tainted_eg = eg.clone();

                println!(
                    "iter {} phase 2: before running rules, n={}, e={}",
                    iter,
                    tainted_eg.total_size(),
                    tainted_eg.number_of_classes()
                );

                metrics.record(
                    eqsat_iter,
                    tainted_eg.total_size(),
                    tainted_eg.number_of_classes(),
                );

                let mut set = HashSet::new();
                equalities.retain(|eq| set.insert(eq.name.clone()));

                let rules = equalities
                    .iter()
                    .filter(|eq| eq.cond == None)
                    .map(|eq| &eq.rewrite);

                eg.rebuild();

                let runner: Runner<SimpleMath, SynthAnalysis, ()> =
                    Runner::new(tainted_eg.analysis.clone()).with_egraph(tainted_eg);

                tainted_eg = runner
                    .with_time_limit(Duration::from_secs(20))
                    .with_node_limit(usize::MAX)
                    .with_iter_limit(5)
                    .with_scheduler(SimpleScheduler)
                    .run(rules)
                    .egraph;

                eg = self.update_clean_egraph(tainted_eg, eg);

                my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();

                println!(
                    "       phase 2: after running {} rules, n={}, e={}",
                    &equalities.len(),
                    eg.total_size(),
                    eg.number_of_classes()
                );

                metrics.record(eqsat_iter, eg.total_size(), eg.number_of_classes());

                println!("iter {} phase 3: discover rules", iter);
                let mut by_cvec_some: IndexMap<&Vec<Option<Constant>>, Vec<Id>> = IndexMap::new();
                for class in eg.classes() {
                    if !eg[class.id].data.contains(&None) && my_ids.contains(&class.id) {
                        // the ids corresponding to a specific cvec key in this hash map are for normal rewrites and can be unioned.
                        by_cvec_some
                            .entry(&eg[class.id].data)
                            .or_default()
                            .push(class.id);
                    }
                }

                let mut extract = Extractor::new(&eg, AstSize);

                // For subsumption:
                // Within each cvec class, we need the two "best" expressions
                // --> That's easy, we determine it first by ast size then by number of variables
                // Among possible equalities then run rule subsumption to filter out useless rules
                // From the remainder, either:
                //    1. Learn all rules at once (probably bad)
                //    2. Sort then by ast size/num of variables for all lhs and rhs
                //       and pick the rule with the lowest max (probably better)

                let best_by_cvec : Vec<_> = by_cvec_some
                    .values_mut()
                    .filter(|ids| ids.len() > 1)
                    .map(|ids| {
                        let mut extracted: Vec<_> = ids
                            .iter()
                            .map(|i| {
                                let expr = extract.find_best(*i).1;
                                (Self::expr_subsumption_cost(&expr), *i, expr)
                            })
                            .collect();
                        extracted.sort_by(|a, b| a.0.cmp(&b.0).reverse());
                        // return the two cheapest things
                        (extracted.pop().unwrap(), extracted.pop().unwrap())
                    })
                    .map(|((_, id1, expr1), (_, id2, expr2))| ((id1, expr1), (id2, expr2)))
                    .collect();

                let best = Self::subsumption_find_best(eg.analysis.clone(), best_by_cvec);

                // let best = 
                //     .min_by_key(|((cost1, _, _), (cost2, _, _))| {
                //         // cost1.max(cost2).clone()
                //         (cost1.0 + cost2.0, cost1.1 + cost2.1, cost1.2 + cost2.2)
                //     });

                if let Some(((id1, expr1), (id2, expr2))) = best {
                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr1, names);
                    let pat2 = generalize(&expr2, names);
                    if !pattern_has_pred(&pat1) && !pattern_has_pred(&pat2) {
                        if let Some(eq) = Equality::new(pat1, pat2, None) {
                            if equalities.iter().all(|e| e.name != eq.name) {
                                equalities.push(eq)
                            }
                        }
                    }

                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr2, names);
                    let pat2 = generalize(&expr1, names);
                    if !pattern_has_pred(&pat1) && !pattern_has_pred(&pat2) {
                        if let Some(eq) = Equality::new(pat1, pat2, None) {
                            if equalities.iter().all(|e| e.name != eq.name) {
                                equalities.push(eq)
                            }
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
        // println!("eclasses: {}, hashcons: {}, enodes: {}, myids: {}", eg.number_of_classes(), eg.total_number_of_nodes(), eg.total_size(), my_ids.len());
        // let cond_rws = self.learn_cond_rules(_conditional, &eg, added);
        // equalities.extend(cond_rws); 
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
    match &eg[id].data[0] {
        Some(Constant::Number(_)) => ExprType::Number,
        Some(Constant::Boolean(_)) => ExprType::Boolean,
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
            consts: vec![Constant::Number(0), Constant::Number(1)],
            diff_thresh: 5,
        };

        let eqs = param.run(13, false);

        check_proves(&eqs, "(+ a b)", "(+ b a)");
        check_proves(&eqs, "(+ a 0)", "a");
        check_proves(&eqs, "(+ 0 a)", "a");
    }
}
