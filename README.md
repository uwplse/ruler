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

# Getting started
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

## Kick the tires

To check that you are able to run Ruler, type the following in the command line:
```
cargo bool
```
This should take less than a second (when ruler is pre-built)
  and should generate and print
  5 rewrite rules on the console.

TODO 1: supress the printing of costs
TODO 2: supress the warning for cvc4 when some rules are invalid.

# Step-by-step


## cvc4

## Herbie experiment

## Search ablation
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

## Validation ablation

# Extending Ruler

### Dependencies
xsv
python
rosette


### Installation
Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
You can then clone the repo and run the tool as described below.

We tested our setup on macOS (Big Sur),  on [Windows10], and on [LINUXTODO].
To build Ruler, type `cargo build`. This should take ~7 min.

### Usage:
You can generate rules for a `domain` as follows:

```cargo run --bin domain --release -- synth --iters 2 --variables 3```

Type `cargo domain --help` to see all available flags and parameters.
