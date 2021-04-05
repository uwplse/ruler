use egg::*;
use ruler::*;

use std::ops::*;

use rand_pcg::Pcg64;

use smallvec::SmallVec;
use std::fmt;

pub type SmallStr = SmallVec<[u8; 24]>;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub enum Lang {
        "str.++" = Concat([Id; 2]),
        "str.replace" = Replace([Id; 3]),
        Lit(Constant),
        Var(egg::Symbol),
    }
}

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
            // Math::Add([a, b]) => map!(v, a, b => Some(a.wrapping_add(*b))),
            // Math::Sub([a, b]) => map!(v, a, b => Some(a.wrapping_sub(*b))),
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
                s.push('A' as u8);
                new.push(s.clone());
                s.pop();
                s.push('B' as u8);
                new.push(s)
            }
            str_consts = new
        }

        let str_consts: Vec<Option<Constant>> = str_consts
            .into_iter()
            .map(|s| Some(Constant::Str(s)))
            .collect();

        for s in &str_consts {
            println!("const: {}", s.as_ref().unwrap());
        }

        let int_consts: Vec<Option<Constant>> =
            vec![Some(Constant::Num(0)), Some(Constant::Num(1))];

        let str_consts = self_product(&str_consts, synth.params.variables - 1);

        let cvec_len = str_consts[0].len();
        println!("cvec_len: {}", cvec_len);
        let mut egraph = EGraph::<Self, _>::new(SynthAnalysis { cvec_len });

        egraph.add(Lang::Lit("0".parse().unwrap()));
        egraph.add(Lang::Lit("1".parse().unwrap()));
        // egraph.add(Lang::Lit("\"A\"".parse().unwrap()));
        // egraph.add(Lang::Lit("\"B\"".parse().unwrap()));
        egraph.add(Lang::Lit("\"\"".parse().unwrap()));

        for i in 0..synth.params.variables {
            if i < synth.params.variables - 1 {
                let var = Symbol::from(letter(i));
                let id = egraph.add(Lang::Var(var));
                egraph[id].data.cvec = str_consts[i].clone();
            } else {
                let int_var = Symbol::from(letter(i));
                let id = egraph.add(Lang::Var(int_var));
                egraph[id].data.cvec = int_consts.iter().cycle().cloned().take(cvec_len).collect();
            }
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let mut extract = Extractor::new(&synth.egraph, NumberOfOps);

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        let is_str = |id: Id| -> bool {
            match synth.egraph[id].data.cvec[0] {
                Some(Constant::Num(_)) => false,
                Some(Constant::Str(_)) => true,
                None => panic!(),
            }
        };

        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                for k in synth.ids() {
                    if ids[&i] + ids[&j] + ids[&k] + 1 == iter {
                        if is_str(i) && is_str(j) && is_str(k) {
                            to_add.push(Lang::Replace([i, j, k]));
                        }
                    }
                }

                if ids[&i] + ids[&j] + 1 != iter {
                    continue;
                }

                if is_str(i) && is_str(j) {
                    to_add.push(Lang::Concat([i, j]));
                }
            }
            if ids[&i] + 1 != iter {
                continue;
            }
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(_rng: &mut Pcg64, _lhs: &Pattern<Self>, _rhs: &Pattern<Self>) -> bool {
        true
    }
}

fn main() {
    Lang::main()
}
