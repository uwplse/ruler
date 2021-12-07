/*!
    Trigonometry from reals
!*/

use std::ops::*;

use egg::*;
use ruler::*;

define_language! {
    pub enum Math {
        // complex
        "~C" = CNeg(Id),
        "+C" = CAdd([Id; 2]),
        "-C" = CSub([Id; 2]),
        "*C" = CMul([Id; 2]),
        "/C" = CDiv([Id; 2]),
        "cis" = Cis(Id),

        // trig
        "sin" = Sin(Id),
        "cos" = Cos(Id),
        "~" = Neg(Id),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "Re" = Re(Id),
        "Im" = Im(Id),

        // constants
        Complex(egg::Symbol),
        Real(egg::Symbol),
        Var(egg::Symbol),
    }
}

// For any Complex(_) ?a, applier adds (Re ?a) to the egraph for any ?a
#[derive(Debug)]
struct ComplexToRealApplier(&'static str);
impl Applier<Math, SynthAnalysis> for ComplexToRealApplier {
    fn apply_one(&self, egraph: &mut EGraph<Math, SynthAnalysis>, _: Id, subst: &Subst) -> Vec<Id> {
        let id = subst[self.0.parse().unwrap()];
        if egraph[id].data.in_domain {
            return vec![];
        }
        
        let found = egraph[id]
            .iter()
            .find_map(|n| match n {
                Math::Complex(sym) => {
                    let s = sym.as_str();
                    if s.chars().position(|c| c == 'i').is_none() {
                        Some(&s[..(s.len() - 1)])
                    } else {
                        None
                    }
                },
                _ => None,
            });

        if let Some(v) = found {
            let re_id = egraph.add(Math::Re(id));
            let cnst_id = egraph.add(Math::Real(Symbol::from(v)));
            egraph.union(re_id, cnst_id);
        }
        
        vec![]
    }
}

fn complex_const_symbol(s: &str) -> egg::Symbol {
    egg::Symbol::from(s.to_owned() + "C")
}

impl SynthLanguage for Math {
    type Constant = egg::Symbol;  // not used

    // no evaluation needed
    fn eval<'a, F>(&'a self, _cvec_len: usize, mut _v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        vec![]
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Math::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Math::Real(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Math::Real(c)
    }

    // override default behavior
    fn is_constant(&self) -> bool {
        match self {
            Math::Real(_) => true,
            _ => false,
        }
    }

    /// Returns true if the node is in the current domain.
    /// Useful for rule lifting.
    fn is_in_domain(&self) -> bool {
        match self {
            Math::Sin(_)        |
            Math::Cos(_)        |
            Math::Add([_, _])   |
            Math::Mul([_, _])   |
            Math::Re(_)         |
            Math::Im(_)         |
            Math::Real(_)       |
            Math::Var(_) => true,
            _ => false,
        }
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        // disabled constants (TODO: validate input)
        let disabled_consts: Vec<&str> =
            if let Some(s) = &synth.params.disabled_consts {
                s.split(" ").collect()
            } else {
                vec![]
            };

        // this is for adding to the egraph, not used for cvec.
        let constants: Vec<egg::Symbol> = ["2", "1", "0", "-1", "-2"]
            .iter()
            .filter(|s| disabled_consts.iter().find(|x| x.eq(s)).is_none())
            .map(|s| complex_const_symbol(s))
            .collect();

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: 0,
            constant_fold: if synth.params.no_constant_fold {
                ConstantFoldMethod::NoFold
            } else {
                ConstantFoldMethod::Lang
            },
            rule_lifting: true,
        });

        // variables
        for i in 0..synth.params.variables {
            let var = egg::Symbol::from(letter(i));
            egraph.add(Math::Var(var));
        }

        // constants
        for c in constants {
            egraph.add(Math::Complex(c));
        }
        
        let i_id = egraph.add(Math::Complex(complex_const_symbol("i")));
        let i2_id = egraph.add(Math::CMul([i_id, i_id]));
        let neg1_id = egraph.add(Math::Complex(Symbol::from("-1C")));
        egraph.union(i2_id, neg1_id);

        // rewrites
        synth.lifting_rewrites = vec![
            rewrite!("def-cos"; "(cos ?a)" <=> "(Re (/C (+C (cis ?a) (cis (~C ?a))) 2C))"),
            rewrite!("def-sin"; "(sin ?a)" <=> "(Re (/C (-C (cis ?a) (cis (~C ?a))) (*C 2C iC)))"),
            rewrite!("def-cis-re"; "(cos ?a)" <=> "(Re (cis ?a))"),
            rewrite!("def-cis-im"; "(sin ?a)" <=> "(Im (cis ?a))"),
            rewrite!("def-cis-add"; "(cis (+ ?a ?b))" <=> "(*C (cis ?a) (cis ?b))"),
            vec![
                rewrite!("cancel-cis-mul"; "(*C (cis ?a) (~C (cis ?a)))" => "1C"),
                rewrite!("pure-real"; "?a" => { ComplexToRealApplier("?a") })
            ]
        ]
        .concat();

        // duplicate rules for HL
        if let Some(s) = synth.params.prior_rules {
            
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let mut extract = Extractor::new(&synth.egraph, NumberOfDomainOps);
        let mut to_add = vec![];

        // disabled operators (TODO: validate input)
        let disabled_ops: Vec<&str> =
            if let Some(s) = &synth.params.disabled_ops {
                s.split(" ").collect()
            } else {
                vec![]
            };

        // predicate if disabled
        let allowedp = |s| disabled_ops.iter().find(|&x| x.eq(&s)).is_none();

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        for i in synth.ids() {
            for j in synth.ids() {
                if (ids[&i] + ids[&j] + 1 != iter) ||
                    !synth.egraph[i].data.in_domain ||
                    !synth.egraph[j].data.in_domain {
                    continue;
                }

                if iter > synth.params.no_constants_above_iter {
                    if synth.egraph[i].data.exact || synth.egraph[j].data.exact {
                        continue;
                    }
                } else {
                    if synth.egraph[i].data.exact && synth.egraph[j].data.exact {
                        continue;
                    }
                };

                if allowedp("+") { to_add.push(Math::Add([i, j])); }
                // if allowedp("-") { to_add.push(Math::RSub([i, j])); }
                if allowedp("*") { to_add.push(Math::Mul([i, j])); }
                // if allowedp("/") && !synth.egraph[j].nodes.iter().any(|x| is_zero(x)) {
                //     to_add.push(Math::RDiv([i, j]));
                // }
            }

            if ids[&i] + 1 != iter || synth.egraph[i].data.exact || !synth.egraph[i].data.in_domain {
                continue;
            }

            // if allowedp("~") { to_add.push(Math::RNeg(i)); }
            if allowedp("sin") { to_add.push(Math::Sin(i)); }
            if allowedp("cos") { to_add.push(Math::Cos(i)); }
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> bool {
        true
    }

    // custom constant folder
    fn constant_fold(_egraph: &mut EGraph<Self, SynthAnalysis>, _id: Id) {
        // if !egraph[id].data.in_domain { // lower domain
        //     if egraph[id].nodes.iter().any(|x| {
        //         match x {
        //             Math::Rat(_) => true,
        //             _ => false,
        //         }
        //     }) {        // early exit if constant exists
        //         return;
        //     }

        //     let mut to_add: Option<(Self, String)> = None;
        //     for x in &egraph[id].nodes {
        //         match x {
        //             Math::Neg(i) => constant_fold!(i, egraph, to_add, -),
        //             Math::Add([i, j]) => constant_fold!(i, j, egraph, to_add, +),
        //             Math::Sub([i, j]) => constant_fold!(i, j, egraph, to_add, -),
        //             Math::Mul([i, j]) => constant_fold!(i, j, egraph, to_add, *),
        //             Math::Div([i, j]) => {  // explicit because of zero check
        //                 for n in &egraph[*i].nodes {
        //                     match n {
        //                         Math::Rat(v) => {
        //                             for n in &egraph[*j].nodes {
        //                                 match n {
        //                                     Math::Rat(w) => {
        //                                         if !w.is_zero() {
        //                                             let r = v / w;
        //                                             let s = r.to_string();
        //                                             to_add = Some((Math::Rat(r), s));
        //                                             break;
        //                                         }
        //                                     },
        //                                     _ => (),
        //                                 }
        //                             }
        //                         },
        //                         _ => (),
        //                     }
        //                 }
        //             }
        //             _ => (),
        //         }
        //     }

        //     if let Some((n, s)) = to_add {
        //         // log::info!("constant fold lower: {}", s);
        //         let mut to_update = vec![];
        //         for id in egraph.classes().map(|c| c.id) {
        //             for n in &egraph[id].nodes {
        //                 match n {
        //                     Math::Lim(i) => {
        //                         if *i == id {
        //                             to_update.push(*i);
        //                         }
        //                     },
        //                     _ => (),
        //                 }
        //             }
        //         }

        //         // C = lim c
        //         // c = id = seq C
        //         let c_id = egraph.add(n);
        //         let r_id = egraph.add(Math::Real(real_const_symbol(&s)));
        //         let seq_id = egraph.add(Math::Seq(r_id));
        //         let lim_id = egraph.add(Math::Lim(c_id));

        //         egraph.union(r_id, lim_id);
        //         egraph.union(c_id, id);
        //         egraph.union(c_id, seq_id);
        //         for i in to_update {
        //             egraph.union(lim_id, i);
        //         }

        //         let r_id = egraph.find(r_id);
        //         egraph[r_id].data.exact = true;  
        //     }
        // } else {
        //     let lim_ids: Vec<Id> = egraph[id].nodes.iter()
        //         .filter_map(|n| match n {
        //             Math::Lim(v) => Some(*v),
        //             _ => None,
        //         })
        //         .collect();

        //     // if !lim_ids.is_empty() {
        //     //     log::info!("constant fold higher");
        //     // }

        //     for id in lim_ids {
        //         Self::constant_fold(egraph, id);
        //     }
        // }
    }

    /// Heuristics for ranking rewrites based on number of variables,
    /// constants, size of the `lhs` and `rhs`, total size of `lhs` and `rhs`,
    /// and number of ops.
    fn score(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> [i32; 5] {
        let lhs_recpat = Self::recpat_instantiate(&lhs.ast);
        let rhs_recpat = Self::recpat_instantiate(&rhs.ast);
        let sz_lhs = DomainAstSize.cost_rec(&lhs_recpat) as i32;
        let sz_rhs = DomainAstSize.cost_rec(&rhs_recpat) as i32;
        // let sz_max_pattern = sz_lhs.max(sz_rhs);

        // lhs.vars() and rhs.vars() is deduping
        // examples
        //   (- x x) => 0 --- 1 b/c x only var
        //   (- x 0) => x --- 1 b/c x only var
        //   (+ x y) => (+ y x) --- 2 b/c x, y only vars
        let mut var_set: HashSet<Var> = Default::default();
        var_set.extend(lhs.vars());
        var_set.extend(rhs.vars());
        let n_vars_rule = var_set.len() as i32;

        let mut op_set: HashSet<String> = Default::default();
        for node in lhs.ast.as_ref().iter().chain(rhs.ast.as_ref()) {
            if !node.is_leaf() {
                op_set.insert(node.display_op().to_string());
            }
        }
        let n_ops = op_set.len() as i32;

        let n_consts = lhs
            .ast
            .as_ref()
            .iter()
            .chain(rhs.ast.as_ref())
            .filter(|n| match n {
                ENodeOrVar::ENode(n) => n.is_constant(),
                ENodeOrVar::Var(_) => false,
            })
            .count() as i32;

        // (-sz_max_pattern, n_vars_rule)
        [
            n_vars_rule,
            -n_consts,
            -i32::max(sz_lhs, sz_rhs),
            // -i32::min(sz_lhs, sz_rhs),
            -(sz_lhs + sz_rhs),
            -n_ops,
            // 0
        ]
    }
}

/// Entry point.
fn main() {
    Math::main()
}
