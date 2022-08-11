/*!
Experimental String domain.
!*/

use egg::*;
use ruler::*;

use std::fmt;

use rand::{seq::SliceRandom, Rng};
use rand_pcg::Pcg64;
use smallstr::SmallString;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SVar(egg::Symbol);
impl fmt::Display for SVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s")
    }
}
impl std::str::FromStr for SVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('s') {
            Ok(SVar(Symbol::from(s)))
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NVar(egg::Symbol);
impl fmt::Display for NVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n")
    }
}
impl std::str::FromStr for NVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('n') {
            Ok(NVar(Symbol::from(s)))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    Num(i64),
    Str(SmallString<[u8; 24]>),
}
impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Num(n) => n.fmt(f),
            Constant::Str(s) => {
                let s = s.as_str();
                write!(f, "\"")?;
                s.fmt(f)?;
                write!(f, "\"")
            }
        }
    }
}

impl std::str::FromStr for Constant {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(i) = i64::from_str(s) {
            Ok(Self::Num(i))
        } else {
            Ok(Self::Str(SmallString::from_str(s.trim_matches('"'))))
        }
    }
}

impl Constant {
    fn to_str(&self) -> Option<&str> {
        match self {
            Constant::Str(s) => Some(s.as_str()),
            Constant::Num(_) => None,
        }
    }

    fn to_num(&self) -> Option<i64> {
        match self {
            Constant::Num(n) => Some(*n),
            Constant::Str(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    String,
    Number,
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "s"),
            Type::Number => write!(f, "n"),
        }
    }
}

define_language! {
    /// Defines operators for Strings.
    pub enum Lang {
        SVar(SVar),
        NVar(NVar),
        "str.++" = Concat([Id; 2]),
        "str.replace" = Replace([Id; 3]),
        "str.at" = At([Id; 2]),
        "str.substr" = SubStr([Id; 3]),
        "str.from_int" = FromInt(Id),
        "str.to_int" = ToInt(Id),
        "str.len" = Len(Id),
        "str.indexof" = IndexOf([Id; 3]),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        Lit(Constant),
    }
}

impl SynthLanguage for Lang {
    type Constant = Constant;
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        match self {
            Lang::Lit(c) => match c {
                Constant::Num(_) => Type::Number,
                Constant::Str(_) => Type::String,
            },
            Lang::SVar(_)
            | Lang::Concat(_)
            | Lang::Replace(_)
            | Lang::At(_)
            | Lang::SubStr(_)
            | Lang::FromInt(_) => Type::String,
            Lang::NVar(_)
            | Lang::Len(_)
            | Lang::ToInt(_)
            | Lang::IndexOf(_)
            | Lang::Add(_)
            | Lang::Sub(_) => Type::Number,
        }
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Lang::SVar(_) => vec![],
            Lang::NVar(_) => vec![],
            Lang::Lit(c) => vec![Some(c.clone()); cvec_len],
            Lang::Concat([a, b]) => {
                map!(v, a, b => Some(Constant::Str(SmallString::from_str(&format!("{}{}", a.to_str().unwrap(), b.to_str().unwrap())))))
            }
            Lang::Replace([s, t, t2]) => map!(v, s, t, t2 => {
                let s = s.to_str().unwrap();
                let t = t.to_str().unwrap();
                let t2 = t2.to_str().unwrap();
                let out = s.replacen(t, t2, 1);
                Some(Constant::Str(SmallString::from_str(&out)))
            }),
            Lang::At([a, i]) => map!(v, a, i => {
                let a = a.to_str().unwrap();
                let i = i.to_num().unwrap();
                if i < 0 || i > (a.len() as _) {
                    Some(Constant::Str(SmallString::default()))
                } else {
                    let i = i as usize;
                    Some(Constant::Str(SmallString::from(a.get(i..i+1).unwrap_or(""))))
                }
            }),
            Lang::SubStr([a, i, len]) => map!(v, a, i, len => {
                let a = a.to_str().unwrap();
                let n = a.len() as usize;
                let i = i.to_num().unwrap() as usize;
                let len = len.to_num().unwrap() as usize;
                let j = (i + len).min(n) as usize;
                match a.get(i..j) {
                    Some(s) => Some(Constant::Str(SmallString::from(s))),
                    None => Some(Constant::Str(SmallString::default()))
                }
            }),
            Lang::FromInt(i) => map!(v, i => {
                let i = i.to_num().unwrap();
                Some(Constant::Str(SmallString::from(i.to_string())))
            }),
            Lang::ToInt(s) => map!(v, s => {
                let s = s.to_str().unwrap();
                let i = s.parse::<i64>().unwrap_or(-1);
                Some(Constant::Num(i))
            }),
            Lang::Len(s) => {
                map!(v, s => Some(Constant::Num(s.to_str().unwrap().len() as i64)))
            }
            Lang::IndexOf([s, t, i]) => map!(v, s, t, i => {
                let s = s.to_str().unwrap();
                let t = t.to_str().unwrap();
                let i = i.to_num().unwrap() as usize;
                match s.get(i..s.len()).unwrap_or("").find(t) {
                    Some(j) => Some(Constant::Num(j as _)),
                    None => Some(Constant::Num(-1))
                }
            }),
            Lang::Add([i, j]) => map!(v, i, j => {
                let i = i.to_num().unwrap();
                let j = j.to_num().unwrap();
                Some(Constant::Num(i.checked_add(j).unwrap()))
            }),
            Lang::Sub([i, j]) => map!(v, i, j => {
                let i = i.to_num().unwrap();
                let j = j.to_num().unwrap();
                Some(Constant::Num(i.checked_sub(j).unwrap()))
            }),
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Lang::SVar(SVar(sym)) => Some(*sym),
            Lang::NVar(NVar(sym)) => Some(*sym),
            _ => None,
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        if sym.as_str().starts_with('s') {
            Lang::SVar(SVar(sym))
        } else if sym.as_str().starts_with('n') {
            Lang::NVar(NVar(sym))
        } else {
            panic!("invalid variable: {}", sym)
        }
    }

    fn is_constant(&self) -> bool {
        matches!(self, Lang::Lit(_))
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Lang::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let strs = vec![
            "A", "B", "AA", "AB", "BA", "BB", "AAA", "AAB", "ABA", "ABB", "BAA", "BAB", "BBA",
            "BBA",
        ];
        let str_consts: Vec<Option<Constant>> = strs
            .into_iter()
            .map(|s| Some(Constant::Str(SmallString::from_str(s))))
            .collect();

        let int_consts = vec![
            Some(Constant::Num(-1)),
            Some(Constant::Num(0)),
            Some(Constant::Num(1)),
            Some(Constant::Num(1000)),
        ];

        let str_consts = self_product(&str_consts, synth.params.variables);
        let int_consts = self_product(&int_consts, synth.params.variables);

        let cvec_len = str_consts[0].len();
        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len,
            constant_fold: if synth.params.no_constant_fold {
                ConstantFoldMethod::NoFold
            } else {
                ConstantFoldMethod::CvecMatching
            },
            rule_lifting: false,
        });

        egraph.add(Lang::Lit(Constant::Num(-1)));
        egraph.add(Lang::Lit(Constant::Num(0)));
        egraph.add(Lang::Lit(Constant::Num(1)));

        for i in 0..synth.params.variables {
            let s_id = egraph.add(Lang::SVar(SVar(Symbol::from("s".to_owned() + letter(i)))));
            let n_id = egraph.add(Lang::NVar(NVar(Symbol::from("n".to_owned() + letter(i)))));
            egraph[s_id].data.cvec = str_consts[i].clone();
            egraph[n_id].data.cvec = int_consts[i]
                .iter()
                .cycle()
                .take(cvec_len)
                .cloned()
                .collect();
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let extract = Extractor::new(&synth.egraph, NumberOfOps);

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        let is_str = |&id: &Id| -> bool { synth.egraph[id].data.class_type == Type::String };
        let is_num = |&id: &Id| -> bool { synth.egraph[id].data.class_type == Type::Number };

        let mut to_add = vec![];
        for a in synth.ids().filter(is_str) {
            if ids[&a] + 1 == iter {
                to_add.push(Lang::ToInt(a));
                to_add.push(Lang::Len(a));
            }
            for b in synth.ids().filter(is_str) {
                for c in synth.ids().filter(is_str) {
                    if ids[&a] + ids[&b] + ids[&c] + 1 == iter {
                        to_add.push(Lang::Replace([a, b, c]));
                    }
                }

                if ids[&a] + ids[&b] + 1 == iter {
                    to_add.push(Lang::Concat([a, b]));
                }

                for i in synth.ids().filter(is_num) {
                    if ids[&a] + ids[&b] + ids[&i] + 1 == iter {
                        to_add.push(Lang::IndexOf([a, b, i]));
                    }
                }
            }
            for i in synth.ids().filter(is_num) {
                for j in synth.ids().filter(is_num) {
                    if ids[&a] + ids[&i] + ids[&j] + 1 == iter {
                        to_add.push(Lang::SubStr([a, i, j]));
                    }
                }
                if ids[&a] + ids[&i] + 1 == iter {
                    to_add.push(Lang::At([a, i]));
                }
            }
        }
        for i in synth.ids().filter(is_num) {
            if ids[&i] + 1 == iter {
                to_add.push(Lang::FromInt(i));
            }
            for j in synth.ids().filter(is_num) {
                if ids[&i] + ids[&j] + 1 == iter {
                    to_add.push(Lang::Add([i, j]));
                    to_add.push(Lang::Sub([i, j]));
                }
            }
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn validate(
        synth: &mut Synthesizer<Self>,
        lhs: &Pattern<Self>,
        rhs: &Pattern<Self>,
    ) -> ValidationResult {
        let n = synth.params.num_fuzz;
        let mut env: HashMap<Var, Vec<Option<Constant>>> = HashMap::default();

        for var in lhs.vars() {
            env.insert(var, vec![]);
        }

        for var in rhs.vars() {
            env.insert(var, vec![]);
        }

        for (var, cvec) in env.iter_mut() {
            cvec.reserve(n);
            let additional = if var.to_string().starts_with("?s") {
                str_sampler(&mut synth.rng, n)
            } else if var.to_string().starts_with("?n") {
                int_sampler(&mut synth.rng, n)
            } else {
                vec![]
            };

            for s in additional {
                cvec.push(Some(s));
            }
        }

        let lvec = Self::eval_pattern(lhs, &env, n);
        let rvec = Self::eval_pattern(rhs, &env, n);
        ValidationResult::from(lvec == rvec)
    }
}

pub fn str_sampler(rng: &mut Pcg64, num_samples: usize) -> Vec<Constant> {
    let mut ret = vec![];
    let letters = vec!["a", "b", "c", "d", "e"];
    for _ in 0..num_samples {
        let len = rng.gen::<usize>() % 10;
        let mut s = SmallString::default();
        for _ in 0..len {
            s.push_str(letters.choose(rng).unwrap());
        }
        ret.push(Constant::Str(s));
    }
    ret
}

pub fn int_sampler(rng: &mut Pcg64, num_samples: usize) -> Vec<Constant> {
    let mut ret = vec![];
    for _ in 0..num_samples {
        let flip = rng.gen::<bool>();
        if flip {
            ret.push(Constant::Num(rng.gen::<i64>() % 10));
        } else {
            // generate i32s so they can be added or subtracted without overflow
            ret.push(Constant::Num(rng.gen::<i32>().into()));
        }
    }
    ret
}

/// Entry point.
fn main() {
    Lang::main()
}
