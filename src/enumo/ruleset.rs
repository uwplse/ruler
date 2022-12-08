use crate::{Equality, SynthLanguage};

#[derive(Clone)]
pub struct Ruleset<L: SynthLanguage>(pub Vec<Equality<L>>);

impl<L: SynthLanguage> Default for Ruleset<L> {
    fn default() -> Self {
        Self(vec![])
    }
}

impl<L: SynthLanguage> Iterator for Ruleset<L> {
    type Item = Equality<L>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            None
        } else {
            let first = self.0[0].clone();
            self.0 = self.0[1..].to_vec();
            Some(first)
        }
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
}
