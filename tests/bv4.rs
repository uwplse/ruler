/*!
4 bit implementation of Bitvectors.
!*/

#[path = "./recipes/bv4.rs"]
pub mod bv4;

ruler::impl_bv!(4);

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::bv4::bv4_rules;
    use ruler::enumo::Ruleset;
    use std::time::Instant;

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bv4_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<_>::from_file("baseline/bv4.rules");

        logger::write_output(&rules, &baseline, "bv4", "oopsla", duration);
    }
}
