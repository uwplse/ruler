# OOPSLA 2021 Paper #94 Artifact Overview

This is the artifact for our paper
"Rewrite Rule Inference Using Equality Saturation".
In our paper, we presented a framework, Ruler, 
that uses equality saturation
 to automatically infer small, expressive
 rulesets for a domain,
 given an interpreter.

Below we first provide instructions for "Getting Started"
  which should take [XXX] minutes.
The next part is "Step-by-Step" which first lists the claims we
  made in the paper and provides instructions on how to validate them.
Finally, we also provide instructions on "Extending Ruler"
  which describes how to install Ruler
  on a different machine,
  modify the code, and
  perform further experiments.

## Getting started
Please download the `.ova` file [here]
  and open it with Virtual Box by
  going to `File -> import appliance` and giving the path to the `.ova` file
  and clicking on `continue`. In the next window that pops up, click on
  `Import`. It should take a few minutes to import.

* Next, please open the virtual machine image in virtual box by clicking on the
  green `Start` button.

* Login is automatic, but in case needed, the password is: `oopsla21`.

* The terminal should be open at startup. The project repository is already
  cloned.  Navigate to the `ruler` directory.  All the required packages
  are already installed and Ruler is already compiled for you, ready to be run.

* To allow a quick verification of our artifact,
  we provided pre-generated data and
  results in the VM.
  You can therefore directly view the results (see below on how to do that).

* You can also generate only the plots from the
pre-generated data (see below on how to do that).

* To run the tool yourself entirely from scratch,
first delete all the results directories (as shown below)
and follow the instructions.

### Kick the tires

To check that you are able to run Ruler, type the following in the command line:
```
cargo bool
```
This should take less than a second (when ruler is pre-built)
  and should generate and print
  5 rewrite rules on the console and the time it took to infer them.

TODO 1: supress the warning for cvc4 when some rules are invalid.
TODO 2: install cvc4

## Step-by-step

Our paper has 4 quantitative evaluations:
- Comparing with CVC4 (`Section 4`): We show that Ruler can infer smaller,
  powerful rulesets faster by comparing the rules inferred for `bool`, `bv4`, and
  `bv32` with varying expression sizes (2, 3). The results are in `Table 1`.

- Integrating with Herbie (`Section 5`): We show that Ruler's rules can 
  be used to replace human-written rules by
  comparing the [Herbie](https://github.com/uwplse/herbie)
  tool's results in fours different configurations: `None`, `Herbie`, `Ruler`, `Both`.
  The results are in `Figure 7`.

- Search Parameter Analyis (`Section 6.1`): We profiled Ruler's search algorithm
  to measure how much time is spent  in each phase. `Figure 8` shows the results for
  `bv4`, `bv32`, and `rationals` domains.
We also compared different variations of `choose_eqs`
  by varying `n` in `Figure 5, Line 3`,
  whose default value is infinity.
The results are shown in `Figure 9a` for `bv4`, `bv32`, and `rationals`.
Importantly, we measure both running time,
  and the number of rules learned.
We also measured running time,
  number of rules learned,
  and number of e-classes in the egraph with and without
  invoking `run_rewrites` (`Figure 4, Line 9`) to study it's effect.
The results are shown in `Figure 9b` for `bv4`, `bv32`, and `rationals`.

- Validation Analysis (`Section 6.2`): We compared different
  rule validation methods for `bv4`, `bv32`, and `rationals`.
The results are shown in `Table 2`.

Below we describe how to run our artifact and reproduce all of them.


### Comparing with CVC4

### Integrating with Herbie

### Search Parameter Analysis
Search ablation aims to examine how different configurations
affect the search. 
In particular, we are interested in the effect of
run-rewrites, particularly when selecting only one rule at a time,
and the effect of changing how many rules are selected.

- How to run
ablation.sh script (choose between pre-gen and your own - long)
- What happens
We run each configuration and collect statistics.
Results availble as pdfs.
- What you might exepct to see
- How to change parameters to get different results
Modify script run.sh

### Validation Analysis




## Further Use / Extending Ruler
This section describes how to install Ruler in a different machine,
  the required dependencies and installation guidelines to reproduce the
  results in the paper on a different machine,
  and how to extend our tool for other domains.

### Dependencies
To install and run the evaluation on a different machine,
  the following dependencies must be installed.
- xsv
- python
- rosette
- cvc4
- herbie
- racket

### Installation
Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
You can then clone the repo and run the tool as described below.

We tested our setup on macOS (Big Sur) and on [LINUXTODO].
To build Ruler, type `cargo build`. This should take ~7 min.

### Usage:
You can generate rules for a `domain` as follows:

```cargo run --bin domain --release -- synth --iters i --variables v```

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
- `Makefile` is for the CVC4 evaluation.
- `cvc4/` has some of the grammars from CVC4's rule inference tool that we used
   for our evaluation in `Section 4`.
- `results/cvc4/` has the pre-run results from CVC4's rule inference tool.

### Extending Ruler to Support New Domains
Ruler's goal is to support rewrite inference for new domains,
  given a grammar, an interpreter, and a validation technique.
You can generate some basic documentation for the core implementation by typing
```
cargo doc --lib --no-deps
```

To understand how to add support for a new domain,
  you can look at the documentation for `rational.rs` which
  provides a detailed guideline.

