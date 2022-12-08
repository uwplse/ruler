use std::io::Write;

use crate::{Equality, SynthLanguage};

#[derive(Clone, Debug)]
pub struct Ruleset<L: SynthLanguage>(pub Vec<Equality<L>>);

impl<L: SynthLanguage> PartialEq for Ruleset<L> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for (x, y) in self.0.iter().zip(other.0.iter()) {
            if x.name != y.name {
                return false;
            }
        }
        true
    }
}

impl<L: SynthLanguage> Default for Ruleset<L> {
    fn default() -> Self {
        Self(vec![])
    }
}

impl<L: SynthLanguage> Ruleset<L> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn to_file(&self, filename: &str) {
        let mut file = std::fs::File::create(&filename)
            .unwrap_or_else(|_| panic!("Failed to open '{}'", filename));
        for eq in &self.0 {
            write!(file, "{}\n", eq.name).expect("Unable to write");
        }
    }

    pub fn from_file(filename: &str) -> Self {
        let infile = std::fs::File::open(filename).expect("can't open file");
        let reader = std::io::BufReader::new(infile);
        let mut eqs = vec![];
        for line in std::io::BufRead::lines(reader) {
            let line = line.unwrap();
            let l = line.parse::<Equality<L>>().unwrap();
            eqs.push(l);
        }
        Self(eqs)
    }
}
