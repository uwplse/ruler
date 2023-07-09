# <img src="ruler.svg" alt="ruler logo" height="40" align="left"> Equality Saturation Theory Exploration à la Carte

This is the artifact for our paper "Equality Saturation Theory Exploration à la Carte".
In our paper, we present a new domain-specific language for programmable theory exploration.

- Available
- Functional
- Reusable

If you use this work, please cite it using the following BibTeX.

## <span style="color:red">TODO</span>

---

## Getting Started

<span style="color:red">TODO: instructions for setting up the virtual machine</span>

Ruler is a framework that uses equality saturation
to automatically infer small, expressive
rulesets for a domain.

## Kick the Tires

To check that you are able to run the tool, run `cargo bool` from the `ruler` directory. This should take less than a second. It will generate and print
14 boolean rewrite rules.

## Overview

Our paper has five sections in the evaluation, each supported by a separate experiment.

1. **How does guided enumeration in Renumo compare to prior work on rule synthesis?**  
   We show that Renumo outperforms Ruler on `bool`, `bv4`, `bv32`, and `rational` domains.
   The results are in `Table 2`.
2. **Can Renumo scale to larger grammars than existing tools can handle?**  
   We present a case-study showing that Renumo is better at scaling to large grammars
   than Ruler. The results of this experiment are described in section 6.1.2 of the paper.
3. **Can Renumo's fast-forwarding algorithm enable rule inference for new domains that**
   **prior work could not support?**  
   We used Renumo's fast-forwarding algorithm to infer rules for `trig` and `exponential`,
   domains which are not supported by other rule synthesis tools. We compare these rules
   against Herbie's handcrafted rulesets. The results are shown in `Table 3`.
4. **How does fast-forwarding impact client applications in terms of performance and results?**  
    We perform three case studies evaluating Renumo's synthesized rules in `Herbie`,
   `Megalibm`, and `Szalinski`. `Figure 8`, `Figure 9`, `Figure 10`, and `Table 4` show the results
   of these case studies.
5. **Do the abstrctions in Renumo enable cross-domain rule synthesis techniques?**  
   We show an example of the kind of ruleset manipulation made possible by Renumo by porting
   rules from small bitvectors to large bitvectors. We compare the quality of the rules and
   time to generate rules against rules synthesized directly for the large bitvectors.
   `Table 5` shows the results of this experiment.

Many of these experiments take a long time to run and some require significant memory
(see paper for the exact machine specifications we used to run thes experiments).

For each experiment, we include three ways to replicate the results:

1. Reproduce the table or figure from precomputed data.
2. Recompute the data for a small subset of the experiment and reproduce the table or figure.
3. Recompute the data for the entire experiment and reproduce the table or figure.

Please note that we have continued to work on Renumo since submission, so there may
be small variation between the numbers reported in the submitted paper and the results
produced by this artifact.

## Use Pre-Computed Data

The goal is to reproduce the tables quickly using precomputed data.  
From the `ruler/` directory, run

```
./scripts/oopsla23/kick-tires.sh
```

Navigate to `ruler/scripts/oopsla23/out` to review the results.

- `table2.pdf` - Comparison against Ruler
- `table3.pdf` - Rule inference using fast-forwarding
- `table5.pdf` - Cross-domain ruleset manipulation

## Use a Subset of Data

The goal is to regenerate the data for the experiments that finish quickly.
Some of the experiments take a long time to run and require significant memory,
so the goal in this stage is to run a representative subset of the experiments
that have lower resource requirements.  
From the `ruler/` directory, run

```
./scripts/oopsla23/lite.sh
```

Navigate to `ruler/scripts/oopsla23/out` to review the results.

- `table2.pdf` - Comparison against Ruler for `bool` domain only
- `table3.pdf` - Rule inference using fast-forwarding for `exponential` domain only
- `table5.pdf` - Cross-domain ruleset manipulation for `BV8` only

## Regenerate All Data

The goal is to run all of the experiments from scratch to fully replicate the data.
This will take a long time to run and may require 32GB of memory.  
From the `ruler/` directory, run

```
./scripts/oopsla23/full.sh
```

Navigate to `ruler/scripts/oopsla23/out` to review the results.

- `table2.pdf` - Comparison against Ruler for `bool`, `bv4`, `bv32`, and
  `rational` domains
- `table3.pdf` - Rule inference using fast-forwarding for `exponential`,
  `rational`, and `trig` domains
- `table5.pdf` - Cross-domain ruleset manipulation for `BV8`, `BV16`, `BV32`,
  `BV64`, and `BV128`.

## Additional information about each experiment

### Experiment 1: Comparison against Ruler

- This experiment uses Renumo to infer rules for a variety of domains and compares the results against Ruler's rules for the same domains.
- `kick-tires.sh` does not invoke Renumo, it just uses the precomputed data from
  `ruler/scripts/oopsla23/precomputed.json`.
- `lite.sh` runs Ruler for the `bool` domain using the Renumo program located at
  `ruler/tests/recipes/bool.rs`.
- `full.sh` runs Ruler for the `bool`, `bv4`, `bv32`, and `rational` domains using Renumo programs
  in the `ruler/tests/recipes` directory.
- The data will be located at `out/output.json`
- `generateLatex.js` converts `output.json` to LaTeX, which will be written to `out/table.tex`
- `out/table.pdf` is generated from the `.tex` file using `pdflatex`.

### Experiment 2: Scaling to Large Domains

### Experiment 3: Rule Inference Using Fast-Forwarding

- This experiment uses Renumo to infer rules for numeric domains (`exponential`, `rational`, and `trig`) and compares them against Herbie's handcrafted rules.
- `kick-tires.sh` does not invoke Renumo, it just uses the precomputed data from
  `ruler/scripts/oopsla23/precomputed.json`.
- `lite.sh` runs Ruler for the `exponential` domain using the Renumo program located at
  `ruler/tests/recipes/exponential.rs`.
- `full.sh` runs Ruler for the `exponential`, `rational`, and `trig` domains using Renumo programs in the `ruler/tests/recipes` directory.
- The data will be located at `out/output.json`
- `generateLatex.js` converts `output.json` to LaTeX, which will be written to `out/table.tex`
- `out/table.pdf` is generated from the `.tex` file using `pdflatex`.

### Experiment 4: Fast-Forwarding Case Studies

#### Case Study 1: Herbie

#### Case Study 2: Megalibm

#### Case Study 3: Szalinski

### Experiment 5: Cross-Domain Ruleset Manipulation

- This experiment uses Renumo-inferred rules for `BV4` to construct rulesets for larger bitvectors (`BV8`, `BV16`, `BV32`, `BV64`, and `BV128`) and compares them against rulesets directly inferred for the larger bitvector domains.
- `kick-tires.sh` does not invoke Renumo, it just uses the precomputed data from
  `ruler/scripts/oopsla23/precomputed.json`.
- `lite.sh` runs Ruler for the `BV8` domain using the Renumo program located at
  `ruler/tests/recipes/bv8.rs`.
- `full.sh` runs Ruler for the `BV8`, `BV16`, `BV32`,
  `BV64`, and `BV128` domains using Renumo programs in the `ruler/tests/recipes` directory.
- The data will be located at `out/output.json`
- `generateLatex.js` converts `output.json` to LaTeX, which will be written to `out/table.tex`
- `out/table.pdf` is generated from the `.tex` file using `pdflatex`.

<!-- ## Running from Previous Runs

Since many of our experiments take a long time to run from scratch, we have
included precomputed data on the VM. The data is located at `ruler/scripts/oopsla23/precomputed.json`.
Fo

### Installation

Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
To build Ruler, type `cargo build --release`.
This should take a few minutes.

### Dependencies

To install Ruler, the following dependencies must be installed:

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

### Publications

- C. Nandi, M. Willsey, A. Zhu, Y. Wang, B. Saiki, A. Anderson, A. Schulz, D. Grossman, Z. Tatlock,
  [_Rewrite Rule Inference Using Equality Saturation_](https://dl.acm.org/doi/abs/10.1145/3485496).
  (OOPSLA 2021, Distinguished Paper Award)

See [here](OOPSLA21.md) for documentation on OOPSLA artifacts. -->

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

After initializing an empty ruleset, `rules`, we define a language we want to enumerate terms over. In this case, our language contains some constants (`0` and `1`), some variables (`a`, `b`, `c`), the unary operators `~` and `-`, and some binary operators, including `&`, `|`, `*`, and others. Note that `lang` is actually a subset of the operators supported by the `bv.rs` implementation—the operator `^`, for example, is _not_ included. This is one way Ruler allows users to easily omit information that is not important for their purposes, enabling for faster, more scalable synthesis.

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

The `recursive_rules` function, included in `src/recipe_utils.rs`, is one of several convenience features included in Ruler. It recursively builds up a ruleset by enumerating all terms over a passed-in `Language` up to a specified size—in our case, the the language is `lang`, and the size is 5 `Atoms` (referring to the total size of the leaves of the subexpressions in the term). Other size metrics are supported, including `Depth` (depth of subexpressions) and `Lists` (number of operators).

In `recursive_rules`, terms with a single atom are enumerated, rule synthesis occurs, and the newly-synthesized rules are used—along with any prior rules, in our case the empty starting ruleset—as known axioms to support rulefinding for 2-atom terms, etc.

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

Ruler allows its users to decouple workload generation from rule synthesis. In the above code snippet, we are not finding rules at all: we are _just_ building up a workload! `a6_canon` is pretty complicated, so let's break it up a bit to make it easier:

```
iter_metric(base_lang(2), "EXPR", enumo::Metric::Atoms, 6)
```

Here, we specify that we want to enumerate all terms up to 6 atoms in size over unary _or_ binary operators. If we had passed 3 instead of 2 to `base_lang`, we would also enumerate over ternary operators.

```
.plug("VAR", &Workload::new(lang.vars))
.plug("VAL", &Workload::empty())
```

The `plug` operator is one of Ruler's most powerful features. An `EXPR` contains `VARS` (variables) and `VALS` (values) as its leaves, and `plug` specifies what can be "plugged in" as variables and values—in this case, a workload containing `lang`'s variables and an empty workload, respectively.

```
.filter(Filter::Canon(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]));
```

Ruler also supports filtering terms out of generated workloads that do not interest the user. In this case, after the workload is generated, terms that are not _canonicalized_ are removed. Canonicalization here means that `a` must be the first variable introduced. `a` can be followed by another `a` any number of times, but the next new variable introduced must be `b`, and so on. Canonicalization drastically expediates rule inference by eliminating duplicate terms, often representing the difference between a workload that is too large to perform rule inference over and one that finishes near-instantaneously.

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
