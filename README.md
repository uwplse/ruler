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

### Writing a Program to Infer Rules
To show how users can write a Ruler program for rule inference, let's take a look at `bv4_fancy.rs`, located in the `tests/recipes` directory. This program showcases several of Ruler's provided features for "guided search"-style rule synthesis using one of the example domains, `bv.rs`, located in `src/`.

```
// create a new set of rules for bitvectors, initially empty
let mut rules: Ruleset<Bv> = Ruleset::default();

// define a new language we want to enumerate terms for
let lang = Lang::new(
    &["0", "1"],
    &["a", "b", "c"],
    &[&["~", "-"], &["&", "|", "*", "--", "+", "<<", ">>"]],
);

```
After initializing an empty ruleset, `rules`, we define a language we want to enumerate terms over. In this case, our language contains some constants (`0` and `1`), some variables (`a`, `b`, `c`), the unary operators `~` and `-`, and some binary operators, including `&`, `|`, `*`, and others. Note that `lang` is actually a subset of the operators supported by the `bv.rs` implementation---the operator `^`, for example, is *not* included. This is one way Ruler allows users to easily omit information that is not important for their purposes, enabling for faster, more scalable synthesis.

```
// find rules using terms over the provided language up to 5 atoms in size
// and add them to our ruleset
rules.extend(recursive_rules(
    enumo::Metric::Atoms,
    5,
    lang.clone(),
    Ruleset::default(),
));
```
The `recursive_rules` function, included in `src/recipe_utils.rs`, is one of several convenience features included in Ruler. It recursively builds up a ruleset by enumerating all terms over a passed-in `Language` up to a specified size---in our case, the the language is `lang`, and the size is 5 `Atoms` (referring to the total size of the leaves of the subexpressions in the term). Other size metrics are supported, including `Depth` (depth of subexpressions) and `Lists` (number of operators). 

In `recursive_rules`, terms with a single atom are enumerated, rule synthesis occurs, and the newly-synthesized rules are used---along with any prior rules, in our case the empty starting ruleset---as known axioms to support rulefinding for 2-atom terms, etc. 

Once we've found all the rules up to 5 atoms in size, we append them to the starting ruleset.

```
let a6_canon = iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 6)
        .plug("VAR", &Workload::new(lang.vars))
        .plug("VAL", &Workload::empty())
        .plug("OP1", &Workload::new(lang.ops[0].clone()))
        .plug("OP2", &Workload::new(lang.ops[1].clone()))
        .filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));

```
Ruler allows its users to decouple workload generation from rule synthesis. In the above code snippet, we are not finding rules at all: we are *just* building up a workload! `a6_canon` is pretty complicated, so let's break it up a bit to make it easier:

```
iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 6)
```
Here, we specify that we want to enumerate all terms over *binary* operators by passing the argument 2 to `base_lang` (also included in `recipe_utils.rs`). We then enumerate all terms over binary operators up to 6 atoms in size.

```
.plug("VAR", &Workload::new(lang.vars))
.plug("VAL", &Workload::empty())
```
The `plug` operator is one of Ruler's most powerful features. An `EXPR` contains `VARS` (variables) and `VALS` (values) as its leaves, and `plug` specifies what can be "plugged in" as variables and values--in this case, a workload containing `lang`'s variables and an empty workload, respectively.
```
.filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
```
Ruler also supports filtering terms out of generated workloads that do not interest the user. In this case, after the workload is generated, terms that are not *canonicalized* are removed. Canonicalization here means that `a` must be the first variable introduced. `a` can be followed by another `a` any number of times, but the next variable introduced must be `b`, and so on. Canonicalization drastically expediates rule inference by eliminating duplicate terms, often representing the difference between a workload that is too large to perform rule inference over and one that finishes near-instantaneously.
```
let consts = Workload::new(["0", "1"]);
let wkld = Workload::Append(vec![a6_canon, consts]);
```
We can also compose workloads: here, we combine a workload consisting of the constants `0` and `1` with `a6_canon`.
```
let wkld = Workload::Append(vec![a6_canon, consts]);
    rules.extend(run_workload(
        wkld,
        rules.clone(),
        Limits::synthesis(),
        Limits::minimize(),
        true,
    ));

    rules
```
Finally, we are ready to run rule synthesis for the final time, which we do using the workload we just created, the rules we got from `recursive_rules`, and some resource limits. We return this final ruleset. This is a complete Ruler program!
