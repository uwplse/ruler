# Ruler

Ruler is a framework that uses equality saturation
 to automatically infer rewrite rules for a domain, given an interpreter.
We have used Ruler for a variety of domains.
Right now, each domain is in it's own branch.

### Example usage:
1. for synthesizing rules for booleans with 2 variables:

```cargo run --release --bin ruler -- --domain bool --variables 2 --iters 3```
