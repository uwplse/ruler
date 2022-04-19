/*!
Experimental String domain.
!*/

use egg::*;
use ruler::*;

use std::ops::*;

use smallvec::SmallVec;
use std::fmt;

pub type SmallStr = SmallVec<[u8; 24]>;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Two types of Constants: Num and Str.
pub enum Constant {
    Num(i64),
    Str(SmallStr),
}

impl Constant {
    fn to_num(&self) -> Option<i64> {
        match self {
            Constant::Num(n) => Some(*n),
            Constant::Str(_) => None,
        }
    }
    fn to_slice(&self) -> Option<&[u8]> {
        match self {
            Constant::Num(_) => None,
            Constant::Str(s) => Some(s.as_slice()),
        }
    }
    fn to_str(&self) -> Option<&str> {
        self.to_slice()
            .map(|bytes| unsafe { std::str::from_utf8_unchecked(bytes) })
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Num(n) => n.fmt(f),
            Constant::Str(s) => {
                let s = std::str::from_utf8(s).unwrap();
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
        match i64::from_str(s) {
            Ok(i) => Ok(Self::Num(i)),
            Err(_) => {
                let s = s.trim_matches('"');
                Ok(Self::Str(s.as_bytes().into()))
            }
        }
    }
}

define_language! {
    /// Defines operators for Strings.
    pub enum Lang {
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
        Var(egg::Symbol),
    }
}

/// Concatenate strings.
fn concat(a: &[u8], b: &[u8]) -> SmallStr {
    a.iter().chain(b).copied().collect()
}

impl SynthLanguage for Lang {
    type Constant = Constant;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Lang::Concat([a, b]) => map!(v, a, b => {
                let a = a.to_slice().unwrap();
                let b = b.to_slice().unwrap();
                Some(Constant::Str(concat(a, b)))
            }),
            Lang::Replace([s, t, t2]) => map!(v, s, t, t2 => {
                let s = s.to_str().unwrap();
                let t = t.to_str().unwrap();
                let t2 = t2.to_str().unwrap();
                let out = s.replacen(t, t2, 1);
                Some(Constant::Str(out.into_bytes().into()))
            }),
            Lang::At([a, i]) => map!(v, a, i => {
                let a = a.to_slice().unwrap();
                let i = i.to_num().unwrap();
                if 0 <= i && i < (a.len() as _) {
                    let i = i as usize;
                    Some(Constant::Str(a[i..i+1].into()))
                } else {
                    Some(Constant::Str(SmallStr::default()))
                }
            }),
            Lang::SubStr([a, i, len]) => map!(v, a, i, len => {
                let a = a.to_slice().unwrap();
                let n = a.len() as i64;
                let i = i.to_num().unwrap();
                let len = len.to_num().unwrap();
                if 0 <= i && i < n && len > 0 {
                    let j = (i + len).min(n) as usize;
                    let i = i as usize;
                    Some(Constant::Str(a[i..j].into()))
                } else {
                    Some(Constant::Str(SmallStr::default()))
                }
            }),
            Lang::FromInt(i) => map!(v, i => {
                let i = i.to_num().unwrap();
                Some(Constant::Str(i.to_string().into_bytes().into()))
            }),
            Lang::ToInt(s) => map!(v, s => {
                let s = s.to_str().unwrap();
                let i = s.parse::<i64>().unwrap_or(-1);
                Some(Constant::Num(i))
            }),
            Lang::Len(s) => map!(v, s => {
                let s = s.to_str().unwrap();
                Some(Constant::Num(s.len() as _))
            }),
            Lang::IndexOf([s, t, i]) => map!(v, s, t, i => {
                let s = s.to_str().unwrap();
                let t = t.to_str().unwrap();
                let i = i.to_num().unwrap();
                if 0 <= i && i <= (s.len() as _) {
                    let i = i as usize;
                    match s[i..].find(t) {
                        None => Some(Constant::Num(-1)),
                        Some(j) => Some(Constant::Num(j as _))
                    }
                } else {
                    Some(Constant::Num(-1))
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
            Lang::Lit(n) => vec![Some(n.clone()); cvec_len],
            Lang::Var(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Lang::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Lang::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Lang::Lit(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Lang::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        let mut str_consts: Vec<SmallStr> = vec![SmallStr::new()];
        for _ in 0..3 {
            let mut new = vec![SmallStr::new()];
            for s in &str_consts {
                let mut s = s.clone();
                s.push(b'A');
                new.push(s.clone());
                s.pop();
                s.push(b'B');
                new.push(s)
            }
            str_consts = new
        }
        str_consts.push(b"1".iter().copied().collect());
        str_consts.push(b"-1000".iter().copied().collect());

        let str_consts: Vec<Option<Constant>> = str_consts
            .into_iter()
            .map(|s| Some(Constant::Str(s)))
            .collect();

        for s in &str_consts {
            println!("const: {}", s.as_ref().unwrap());
        }

        let int_consts = vec![
            Some(Constant::Num(-1)),
            Some(Constant::Num(0)),
            Some(Constant::Num(1)),
            Some(Constant::Num(1000)),
        ];
        let int_consts = self_product(&int_consts, synth.params.str_int_variables);

        let str_consts = self_product(&str_consts, synth.params.variables);

        let cvec_len = str_consts[0].len();
        println!("cvec_len: {}", cvec_len);
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
        // egraph.add(Lang::Lit("\"A\"".parse().unwrap()));
        // egraph.add(Lang::Lit("\"B\"".parse().unwrap()));
        egraph.add(Lang::Lit(Constant::Str(Default::default())));

        for (i, item) in str_consts.iter().enumerate().take(synth.params.variables) {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Lang::Var(var));
            egraph[id].data.cvec = item.clone();
        }
        for (i, item) in int_consts
            .iter()
            .enumerate()
            .take(synth.params.str_int_variables)
        {
            let int_var = Symbol::from(letter(i + synth.params.variables));
            let id = egraph.add(Lang::Var(int_var));
            egraph[id].data.cvec = item.iter().cycle().take(cvec_len).cloned().collect();
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

        let is_str = |&id: &Id| -> bool {
            match synth.egraph[id].data.cvec[0] {
                Some(Constant::Num(_)) => false,
                Some(Constant::Str(_)) => true,
                None => panic!(),
            }
        };
        let is_num = |id: &Id| !is_str(id);

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
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> ValidationResult {
        ValidationResult::Valid
    }
}

/// Entry point.
fn main() {
    Lang::main()
}
