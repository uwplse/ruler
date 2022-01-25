# <img src="ruler.svg" alt="ruler logo" height="40" align="left"> Ruler

### Installation of Ruler
Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
You can then get the code from Zenodo and
run the tool as described below.
To build Ruler, type `cargo build --release`.
This should take ~40 min.

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
You can generate rules for a `domain` as follows:

```
cargo run --bin domain --release -- synth --iters i --variables v
```

Type `cargo domain --help` to see all available flags and parameters.

### Project Layout
- The source code resides in the `src` directory.
   * The main algorithm of Ruler is implemented in `lib.rs`.
   *  `equality.rs` defines a rewrite rule.
   * `derive.rs` has the code for running the derivability checks (e.g., see `Sections 4, 5`).
   * `util.rs` has some small helper functions.
   * `convert_sexp.rs` has code that converts the rewrites from CVC4's format to Ruler's format.
   * `bv.rs` has a generic implemenation of bitvectors
      which are specialized to various sizes in `src/bin/`
   * `src/bin/` also contains the implementation of other domains including rationals and bools.
     There are some prototype implementations (floats, strings, bigints)
     that are not evaluated in the paper --- these are work in progress,
     but should give an idea of how to add support for other domains.
     See below for more information on supporting other domains.
- `scripts` has all the scripts used for evaluating Ruler --- each is in a
    designated subdirectory.

### Publications

* C. Nandi, M. Willsey, A. Zhu, Y. Wang, B. Saiki, A. Anderson, A. Schulz, D. Grossman, Z. Tatlock
[Rewrite Rule Inference Using Equality Saturation](https://dl.acm.org/doi/abs/10.1145/3485496).
(OOPSLA 2021)

See [here](./OOPSLA.md) for documentation on OOPSLA artifacts.

### Extending Ruler to Support New Domains
Ruler's goal is to support rewrite inference for new domains,
  given a grammar, an interpreter, and a validation technique.
We have already generated documentation for you.
Open `target/doc/ruler/index.html` in your preferred browser to navigate the documentation.

You can generate documentation on your own in a new machine by running:

```
cargo doc --no-deps
```

To run Ruler with different flags (documentation at `SynthParams.html`)
see the various example usages in `.cargo/config` and try replacing them with other values and look at the results!
For example, you can try

```
cargo run --release --bin rational -- synth --num-fuzz 10 --iters 2
```
to synthesize
rewrite rules for rationals till depth 2
using fuzzing (with 10 values) for rule validation.

To understand how to add support for a new domain,
  you can look at the documentation of the various supported domains like
   `rational` (`target/doc/rational/index.html`),
   `rational_new_div` (`target/doc/rational_new_div/index.html`, relevant for `Section 6.3` in the paper),
   `bool` (`target/doc/bool/index.html`), etc.
Note that some domains (e.g., floats, strings, bigints)
are experimental and not reported in the paper,
but they all provide examples of how you can add support for new domains.
