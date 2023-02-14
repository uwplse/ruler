# <img src="ruler.svg" alt="ruler logo" height="40" align="left"> Ruler

__This project is under active development and not yet stable.
 If you are looking for the version of Ruler from Nandi et al's
 [*Rewrite Rule Inference Using Equality Saturation*](https://dl.acm.org/doi/abs/10.1145/3485496), please use [this branch](https://github.com/uwplse/ruler/tree/oopsla21-aec)__

---
Ruler is a framework that uses equality saturation
 to automatically infer small, expressive
 rulesets for a domain.

### Installation
Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
To build Ruler, type `cargo build --release`.
This should take a few minutes.

### Dependencies
To install Ruler, the following dependencies must be installed:

  * Rust
  * libz3

If `libz3` is not offered on your system, you can edit `Cargo.toml` in this directory
  by changing the dependency `z3 = xxx` to `z3 = {version=xxx, features = ["static-link-z3"]}`.
This will statically link to a built copy of z3 instead of dynamically linking, but the build
  process will take considerably longer.
It is recommended that you install `libz3` if possible.

### Usage
`cargo test` will run all the tests, including tests that
 find rules for a handful of example domains. See the
 `tests` directory for examples of how to set up a domain,
 construct workloads, and find rules.

### Project Layout 
- The source code resides in the `src` directory.
   * `lib.rs` is the main entrypoint and defines some auxiliary data types. 
   * `language.rs` defines the `SynthLanguage` trait, which must be implemented in order to use the Ruler DSL to find rules for a domain. There are two main things that must be implemented: an interpreter and a rule validator. (In the case of rule lifting, the interpreter is not needed.)
   * `workload.rs` contains the `Workload` data type. Workloads evaluate to a list of terms (s-expressions). Workloads can be constructed directly from a list of s-expressions (`Workload::Set`), combined (`Workload::Append`), refined with a filter (`Workload::Filter`), or composed via plugs (`Workload::Plug`). Plug is a novel operation for generating workloads from smaller workloads. It takes two workloads, $W_1$ and $W_2$ and a string, $s$; for each term in $W_2$ that contains $s$, it “plugs” in the values in $W_1$ and generates a new workload which is a cross product of all the new plugged terms.
   * `sexp.rs` contains an implementation of s-expressions.
   *  `equality.rs` defines a rewrite rule.
   * `ruleset.rs` contains the `Ruleset` data type. Rulesets are implemented as an `IndexMap` of `Equality`. There are several operations over Rulesets that can be used to combine, compose, and refine rulesets. For example, `Ruleset::cvec_match` extracts a set of equalities from an egraph via cvec-matching; `Ruleset::minimize` can be used to eliminate redundant rules from a ruleset; `Ruleset::derive` tests the proving power of one ruleset compared to another.
   * `filter.rs` defines the `Filter` data type which can be used to filter workloads. `pattern.rs` and `metric.rs` define data types that are used in `Filter`.
   * `util.rs` has some small helper functions.

### Publications

* C. Nandi, M. Willsey, A. Zhu, Y. Wang, B. Saiki, A. Anderson, A. Schulz, D. Grossman, Z. Tatlock,
[*Rewrite Rule Inference Using Equality Saturation*](https://dl.acm.org/doi/abs/10.1145/3485496).
(OOPSLA 2021, Distinguished Paper Award)

See [here](OOPSLA21.md) for documentation on OOPSLA artifacts.

### Extending Ruler to Support New Domains
Ruler's goal is to support rewrite inference for new domains,
 given a grammar, an interpreter, and a validation technique.
To understand how to add support for a new domain,
  you can look at the domains in the `tests` directory.
Note that some domains are experimental and not reported in the paper,
but they all provide examples of how you can add support for new domains.
