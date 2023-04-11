/*!
8 bit implementation of Bitvectors.
!*/

ruler::impl_bv!(8);

#[cfg(test)]
pub mod test {
    use std::time::Instant;

    use ruler::enumo::Ruleset;

    use crate::Bv;

    #[test]
    fn read_bv4() {
        let bv4_rules: Ruleset<Bv> = Ruleset::from_file("bv4.rules");
        // bv4_rules.pretty_print();
        let start = Instant::now();
        let (sound, unsound) = bv4_rules.partition(|rule| rule.is_valid());
        println!("Validating BV4 rules in BV8");
        println!("{} sound, {} unsound", sound.len(), unsound.len());
        println!("Elapsed time: {}", start.elapsed().as_secs());
    }
}
