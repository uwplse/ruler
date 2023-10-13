# <img src="ruler.svg" alt="ruler logo" height="40" align="left"> Enumo

Enumo is a domain-specific language for programmable theory exploration.
It uses equality saturation to infer small, expressive rulesets for a domain.

### Publications

- A. Pal, B. Saiki, R. Tjoa, C. Richey, A. Zhu, O. Flatt, M. Willsey, Z. Tatlock, C. Nandi,
  [_Equality Saturation Theory Exploration à la Carte_](https://ajpal.github.io/assets/files/enumo.pdf)

- (OOPSLA 2021, Distinguished Paper Award) C. Nandi, M. Willsey, A. Zhu, Y. Wang, B. Saiki, A. Anderson, A. Schulz, D. Grossman, Z. Tatlock,
  [_Rewrite Rule Inference Using Equality Saturation_](https://dl.acm.org/doi/abs/10.1145/3485496). \*

\* This paper is based on an older version of this repository. If you are looking for the code
associated with this paper, please use [this branch](https://github.com/uwplse/ruler/tree/oopsla21-aec).

### Installation

Enumo is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
To build Ruler, type `cargo build --release`.
This should take a few minutes.

### Dependencies

To install Enumo, the following dependencies must be installed:

- Rust
- libz3

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
  - `lib.rs` is the main entrypoint and defines some auxiliary data types.
  - `language.rs` defines the `SynthLanguage` trait, which must be implemented in order to use the Ruler DSL to find rules for a domain. There are two main things that must be implemented: an interpreter and a rule validator. (In the case of rule lifting, the interpreter is not needed.)
  - `workload.rs` contains the `Workload` data type. Workloads evaluate to a list of terms (s-expressions). Workloads can be constructed directly from a list of s-expressions (`Workload::Set`), combined (`Workload::Append`), refined with a filter (`Workload::Filter`), or composed via plugs (`Workload::Plug`). Plug is a novel operation for generating workloads from smaller workloads. It takes two workloads, $W_1$ and $W_2$ and a string, $s$; for each term in $W_2$ that contains $s$, it “plugs” in the values in $W_1$ and generates a new workload which is a cross product of all the new plugged terms.
  - `sexp.rs` contains an implementation of s-expressions.
  - `equality.rs` defines a rewrite rule.
  - `ruleset.rs` contains the `Ruleset` data type. Rulesets are implemented as an `IndexMap` of `Equality`. There are several operations over Rulesets that can be used to combine, compose, and refine rulesets. For example, `Ruleset::cvec_match` extracts a set of equalities from an egraph via cvec-matching; `Ruleset::minimize` can be used to eliminate redundant rules from a ruleset; `Ruleset::derive` tests the proving power of one ruleset compared to another.
  - `filter.rs` defines the `Filter` data type which can be used to filter workloads. `pattern.rs` and `metric.rs` define data types that are used in `Filter`.
  - `util.rs` has some small helper functions.

## Further Use / Extending Enumo

Our goal is that Enumo is extensible and can support rule inference in any domain.
There are some examples of Enumo programs in [tests/recipes](tests/recipes) that
can be used as a model. In addition, we provide some brief instructions for
constructing a basic Enumo program below:

### Writing a Program to Infer Rules

The Enumo DSL enables rewrite rule inference for any domain
given a grammar, an interpreter, and a validation technique.
To understand how to add support for a new domain,
you can look at the domains in the `tests` directory.
Note that some domains are experimental and not reported in the paper,
but they all provide examples of how you can add support for new domains.

To use Enumo for a new domain, you must first implement the `SynthLanguage` trait for your domain. This requires implementing a rule validator and either an interpreter (for non-fast-forwarding domains) or a set of lifting rules (for fast-forwarding domains).

To show how users can write an Enumo program for rule inference, let's take a look at `bv4_fancy.rs`, located in the `tests/recipes` directory. This program showcases several of Enumo's provided features for "guided search"-style rule synthesis using one of the example domains, `bv.rs`, located in `src/`.

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

After initializing an empty ruleset, `rules`, we define a language we want to enumerate terms over. In this case, our language contains some constants (`0` and `1`), some variables (`a`, `b`, `c`), the unary operators (`~` and `-`), and some binary operators, including (`&`, `|`, `*`), and others. Note that `lang` is actually a subset of the operators supported by the `bv.rs` implementation—the operator `^`, for example, is _not_ included. This is one way Enumo allows users to easily omit information that is not important for their purposes, enabling for faster, more scalable synthesis.

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

The `recursive_rules` function, included in `src/recipe_utils.rs`, is one of several convenience features included in Enumo. It recursively builds up a ruleset by enumerating all terms over a passed-in `Language` up to a specified size. Enumo supports three
measures of term size: `Atoms` (number of literals in the term), `Depth`
(depth of s-expression), and `Lists` (number of operators in the term).

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

Enumo allows its users to decouple workload generation from rule synthesis. In the above code snippet, we are not finding rules at all: we are _just_ building up a workload. `a6_canon` is pretty complicated, so let's break it up a bit to make it easier:

```
iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 6)
```

Here, we specify that we want to enumerate all terms up to 6 atoms. `base_lang(n)`
is a convenience function that constructs a base workload for any language
consisting of variables, constants, and operators with up to `n` arguments.

```
.plug("VAR", &Workload::new(lang.vars))
.plug("VAL", &Workload::empty())
```

`plug` is the core Enumo operator for constructing and composing workloads.
An `EXPR` contains `VARS` (variables) and `VALS` (values) as its leaves, and `plug` specifies what can be "plugged in" as variables and values—in this case, a workload containing `lang`'s variables and an empty workload, respectively.

```
.filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
```

Enumo also supports filtering terms out of generated workloads that do not interest the user. In this case, after the workload is generated, terms that are not _canonicalized_ are removed. Canonicalization here means that `a` must be the first variable introduced. `a` can be followed by another `a` any number of times, but the next new variable introduced must be `b`, and so on. Canonicalization drastically expedites rule inference by eliminating duplicate terms, often representing the difference between a workload that is too large to perform rule inference over and one that finishes near-instantaneously.

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

Finally, we are ready to run rule synthesis for the final time, which we do using the workload we just created, the rules we got from `recursive_rules`, and some resource limits. We return this final ruleset. This is a complete Enumo program!
