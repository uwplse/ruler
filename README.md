# Ruler

Ruler is a framework that uses equality saturation
 to automatically infer rewrite rules for a domain, given an interpreter.
We have used Ruler for a variety of domains including
 booleans,
 bitvectors,
 rationals,
 floats,
 integers, and
 strings.

### Installation
Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
You can then clone the repo and run the tool as described below.

### Usage:
You can generate rules for a `domain` as follows:

```cargo run --bin domain --release -- synth --iters 2 --variables 3```

Type `cargo domain --help` to see all available flags and parameters.
