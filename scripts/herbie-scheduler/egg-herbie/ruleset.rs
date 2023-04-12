use indexmap::map::{IntoIter, Iter, IterMut, Values, ValuesMut};
use indexmap::IndexMap;
use std::str::FromStr;
use std::sync::Arc;

use crate::math::*;
use crate::rule::Rule;

pub type RecExpr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
pub type EGraph = egg::EGraph<Math, ConstantFold>;
pub type Rewrite = egg::Rewrite<Math, ConstantFold>;

#[derive(Clone, Debug)]
pub struct Ruleset(pub IndexMap<Arc<str>, Rule>);

pub fn mk_ruleset(tuples: &[(&str, &str, &str)]) -> Ruleset {
    let mut rs = Ruleset::default();
    for (name, left, right) in tuples {
        let left = Pattern::from_str(left).unwrap();
        let right = Pattern::from_str(right).unwrap();
        rs.add(Rule::new(String::from(*name), left, right).unwrap());
    }

    rs
}

impl PartialEq for Ruleset {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for ((name1, _), (name2, _)) in self.0.iter().zip(other.0.iter()) {
            if name1 != name2 {
                return false;
            }
        }
        true
    }
}

impl Default for Ruleset {
    fn default() -> Self {
        Self(IndexMap::default())
    }
}

impl IntoIterator for Ruleset {
    type Item = (Arc<str>, Rule);
    type IntoIter = IntoIter<Arc<str>, Rule>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Ruleset {
    type Item = (&'a Arc<str>, &'a Rule);
    type IntoIter = Iter<'a, Arc<str>, Rule>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a mut Ruleset {
    type Item = (&'a Arc<str>, &'a mut Rule);
    type IntoIter = IterMut<'a, Arc<str>, Rule>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl Ruleset {
    // pub fn new<I>(vals: I) -> Self
    // where
    //     I: IntoIterator,
    //     I::Item: AsRef<str>,
    // {
    //     let mut map = IndexMap::default();
    //     for v in vals {
    //         if let Ok((forwards, backwards)) = Rule::from_string(v.as_ref()) {
    //             map.insert(forwards.name.clone(), forwards);
    //             if let Some(backwards) = backwards {
    //                 map.insert(backwards.name.clone(), backwards);
    //             }
    //         }
    //     }
    //     Ruleset(map)
    // }

    pub fn iter(&self) -> Values<'_, Arc<str>, Rule> {
        self.0.values()
    }

    pub fn iter_mut(&mut self) -> ValuesMut<'_, Arc<str>, Rule> {
        self.0.values_mut()
    }

    pub fn to_str_vec(&self) -> Vec<String> {
        match self {
            Ruleset(m) => m.iter().map(|(name, _val)| name.to_string()).collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn bidir_len(&self) -> usize {
        let mut bidir = 0;
        let mut unidir = 0;
        for (_, rule) in &self.0 {
            let name = format!("{} ==> {}", rule.rhs, rule.lhs);
            let reverse = Rule::new(name, rule.rhs.clone(), rule.lhs.clone());
            if reverse.is_some() && self.contains(&reverse.unwrap()) {
                bidir += 1;
            } else {
                unidir += 1;
            }
        }
        unidir + (bidir / 2)
    }

    pub fn contains(&self, rule: &Rule) -> bool {
        self.0.contains_key(&rule.name)
    }

    pub fn add(&mut self, rule: Rule) {
        self.0.insert(rule.name.clone(), rule);
    }

    pub fn remove_all(&mut self, other: Self) {
        for (name, _) in other.0 {
            self.0.remove(&name);
        }
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn partition<F>(&self, f: F) -> (Self, Self)
    where
        F: Fn(&Rule) -> bool,
        // F: Fn(&Rule) -> bool + std::marker::Sync,
    {
        // let results = Mutex::new((Ruleset::default(), Ruleset::default()));
        // let rules: Vec<&Rule> = self.0.values().collect();
        // rules.into_par_iter().for_each(|rule| {
        //     let f_rule = f(rule);
        //     let mut results = results.lock().unwrap();
        //     if f_rule {
        //         results.0.add(rule.clone());
        //     } else {
        //         results.1.add(rule.clone());
        //     }
        // });
        // results.into_inner().unwrap()

        let mut results = (Ruleset::default(), Ruleset::default());
        let rules: Vec<&Rule> = self.0.values().collect();
        for rule in rules {
            let f_rule = f(rule);
            if f_rule {
                results.0.add(rule.clone());
            } else {
                results.1.add(rule.clone());
            }
        }

        results
    }
}
