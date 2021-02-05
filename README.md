# Ruler

Ruler is a framework that uses equality saturation
 to automatically infer rewrite rules for a domain, given an interpreter.
We have used Ruler for a variety of domains.
Right now, each domain is in it's own branch.
The master branch has two simple domains
as examples: booleans and bitvectors.

### Installation
Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
You can then clone the repo and run the tool as described below.

### Usage:
For booleans (bool) and 4-bit bitvectors (bv4) you can generate rules as follows:

```cargo run --release --bin ruler -- --domain bool --variables 2 --iters 3```

These domains are simple enough that the rules are correct by construction.
For other domains like floats and reals, we validate the rules by fuzzing.
For synthesizing rules for floats for example,
you can checkout the `floats` branch and run

```cargo run --release --bin ruler```
