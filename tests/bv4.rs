/*!
4 bit implementation of Bitvectors.
!*/

#[path = "./recipes/bv4_fancy.rs"]
pub mod bv4_fancy;

#[path = "./recipes/bv4_base.rs"]
pub mod bv4_base;

ruler::impl_bv!(4);

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::bv4_base::bv4_rules;
    use crate::bv4_fancy::bv4_fancy_rules;
    use ruler::enumo::Ruleset;
    use std::time::Instant;

    #[test]
    fn run() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }

        let start = Instant::now();
        let rules = bv4_fancy_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<bv4_fancy::Bv>::from_file("baseline/bv4.rules");

        logger::write_baseline(&rules, "bv4_fancy", &baseline, "oopsla", duration);

        let start = Instant::now();
        let rules = bv4_rules();
        let duration = start.elapsed();
        let baseline = Ruleset::<bv4_base::Bv>::from_file("baseline/bv4.rules");

        logger::write_baseline(&rules, "bv4_base", &baseline, "oopsla", duration);
    }
}
