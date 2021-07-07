# OOPSLA 2021 Paper #94 Artifact Overview

This is the artifact for our paper
"Rewrite Rule Inference Using Equality Saturation".
In our paper, we presented a framework, Ruler, 
that uses equality saturation
 to automatically infer small, expressive
 rulesets for a domain,
 given an interpreter.

- **Available**: The artifact is available on Zenodo at: [link]
- **Functional**: Below we first provide instructions for "Getting Started"
  which should take less than 10 minutes.
The next part is "Step-by-Step" which first lists the claims we
  made in the paper and provides instructions on how to validate them.
- **Reusable**: Finally, we also provide instructions on "Further Use / Extending Ruler"
  which describes how to install Ruler
  on a different machine,
  modify the code, and
  perform further experiments.

## Getting started
We recommend running the VM on a machine with at least 32GB RAM.
Please download the `.ova` file [here]
  and open it with Virtual Box by
  going to `File -> import appliance` and giving the path to the `.ova` file
  and clicking on `continue`. In the next window that pops up, click on
  `Import`. It should take a few minutes to import.
  Please change the RAM to 24 GB and allocate 4 processors by going to
  `Settings` under `System -> Motherboard` and `System -> Processor` respectively.

* Next, please open the virtual machine image in virtual box by clicking on the
  green `Start` button.

* Login is automatic, but in case needed, the password is: `ruler`.

* Open a terminal window. The project repository is already
  cloned.  Navigate to the `ruler` directory.  All the required packages
  are already installed and Ruler is already compiled for you, ready to be run.

* To allow a quick verification of our artifact,
  we provided pre-generated data and
  results in the VM.
  You can therefore directly view the results (see below on how to do that).

* You can also run the tool yourself entirely from scratch,
as shown below for each section of the evaluation.

Note that in the paper,
  the evaluations were often done is larger scale
  which is not possible on the VM.
We have provided recommendations below on how
to run it on a smaller scale (e.g., fewer
iterations, fewer seeds) when possible.

### Kick the tires

To check that you are able to run Ruler, type the following in the command line:
```
cargo bool
```
This should take less than a second (when ruler is pre-built, and it is)
  and should generate and print
  5 rewrite rules on the console and the time it took to infer them.

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
For each experiment, we provide three step:
- navigating to the right directory
- viewing plots/tables from pre-generated data
- generated all results from scratch.
We recommend saving this last step for last (you may skip too) and have also
provided instructions for running them in smaller inputs on the VM.

### Comparing with CVC4
The goal is to reproduce `Table 1`.
- Type `cd scripts/cvc4-eval/` to go to the correct directory.

- To generate the table from the pre-run results,
type `make` and it will print it to the terminal instantly.

- To regenerate the data, type `make clean` to remove all
 pre-generated results and run `make` again.
 This will take approximately 1.5 hours.

**Note that  in some cases the numbers
  will not match exactly with what
  we reported in the paper because
  the VM is less powerful than the machine
  we had for running the eval in the paper (see paper for details),
  and the heuristics also may have slightly different effects.**

#### Additional information about the scripts.
The `Makefile` is the main script for this part.
`cvc4/` has the grammars from CVC4's rule inference tool that we used.
The script runs both Ruler and CVC4 for `bool`, `bv4`, and `bv32` with
2 variables, 3 variables, 2 iterations, and 3 iterations.
It then generates reports in json and uses the
derivability test (`derive.rs`) to compare the proving power of the rulesets
from Ruler and CVC4.
The table is generated by the `compare.py` script and using pandoc and xsv.
`results/cvc4/` has the pre-run results from CVC4's rule inference tool.

### Integrating with Herbie
The goal is to reproduce `Figure 7`.
Herbie is an external tool which we used for this evaluation.
Therefore to avoid any issues that may come up due to Herbie,
  we have uploaded our pre-generated data and the plots that we have in the paper.
Herbie is already installed in the VM.

- Type `cd scripts/herbie-rational` to go to the correct directory from the `ruler` directory.

- To view the provided plots, go to `output/ruler-herbie-eval/results/pre-gen-2021-04-13-1331`
and open the following three PDFs
    * for `Figure 7a`: `by-config-all-tests-avg_bits_err_improve-boxplot.pdf`
    * for `Figure 7b`: `by-config-all-tests-output_parens-boxplot.pdf`
    * for `Figure 7c`: `by-config-all-tests-time-boxplot.pdf`

We also provide various other plots that you are welcome to look at! These are
however not presented in the paper and not relevant to this artifact.

- To reproduce all the data
type: `./herbie-eval.sh`.
This requires racket 7.9 which is already
pre-installed in the directory.
This runs the script with 1 seed by default.
You can run it for fewer or more seeds by typing `./herbie-eval.sh NSEEDS`.
We recommend trying with `5` seeds
(type `./herbie-eval.sh 5`)
to check the results (look for plots with same names as mentioned above in the timestamped directory under
`output/ruler-herbie-eval/results/`) -- they should be similar.
In the VM this should take approximately 5 hours.
You may notice some timeouts / errors on some benchmarks but those is due to Herbie, not Ruler.
In the paper we ran the experiment with `30` seeds but that will take over 15 hours,
and is better run on a real machine as opposed to a VM because it will
generate more data and will be much slower on the VM.
Other Herbie specific
arguments to the `herbie-eval.sh` script are set to their defaults but
the script has documentation for how to change them.
The script will print the configuration being used.
To map them to the figure, use the following guide:
  * `herbie-no-simpl` is `None`
  * `herbie-only` is `Herbie`
  * `ruler-only` is `Ruler`
  * `herbie-ruler` is `Both`

#### Additional information about the scripts.
`herbie-eval.sh` is the main script and it has comments to indidate what it does.
`seed-variance.sh` is the script that runs Herbie with different seeds and generates the reports.
All results are saved in `output/ruler-herbie-eval/results/` under timestamped directories.
The rules used are also saved in a txt file.
Since we are still actively working on Ruler, there
are some scripts that may not be relevant for this part of the evaluation.
Below are the scripts relevant for this eval and a brief description of what they do:

`filter.rkt` filters benchmarks from Herbie that contain only rational operators.
`preprocess.py` preprocesses Ruler's rewrites to make them match with Herbie's syntax,
and also removes expansive directions of rules.
Plotting scripts are in `plots/` directory.
`plots/plot-results.sh` calls these scripts to generate the plots.
`plots/config-all-tests-box-plot.py` is the script that generates the plots in the paper.

### Search Parameter Analysis
The goal is to reproduce `Figure 8` and `Figure 9`.

- Type `cd scripts/ablation` to go to the correct directory.

- To only view the plots, go to `submitted-plots/`.
`Figure 8` in the paper corresponds to the `10-run/by-domain-phase-times.pdf` plot.
`Figure 9a` plots are the pdfs under `10-run/bv4`, `10-run/bv32`, and `10-run/rat`.
`Figure 9b` plots are the pdfs under `orat-rr/bv4`, `orat-rr/bv32`, and `orat-rr/rat`
(`orat` means "One Rule At A Time" which corresponds to `n = 1` in the caption in the paper).
To make plots from the pre-generated data,
type `./ablation.sh -r use-existing`.
This will make plots using the data provided in the folder `submitted-data`
and put them into the `output` folder.
It will also print some of the data in the terminal, which we used
for debugging.
Feel free to ingore that.
The .tar file in the `submitted-data` folder contains the log of each run.
This is not used, and is provided for interest only.

- To run your own evaluation and make new plots from scratch,
type  `./ablation.sh -r generate-new`.
This runs Ruler with different configurations,
saving each run to its own timestamped folder
under `output/`,
and then parses the statistics from the log outputs.
These statistics are collected into json files
and then plotted in matplotlib.
Resultant pdf plots are available inside the 
timestamped folder for that experiment.
With the default settings provided
(but only 1 run instead of the 10 run aggregate reported in the paper
--- see below on how to evaluate over more runs),\
this will take approximately 1.5 hours for all runs that terminate.
However, it is almost certain that `orat` rationals
in the `no-rr` setting will not terminate after 24 hours.
If you do not wish to wait 24 hours (per run), 
you can change the timeout setting (see the list of 
arguments available below).

# TODO for Amy: fix the parsing script so that the non-termination doesn't break it (skip it)

In the published evaluation,
we ran Ruler with 3 variables,
5 iterations, over 10 runs, but
we recommend only running for 1 run on the VM.

Note that timing results should only be compared between 
themselves and not as absolute values, since logging is enabled
during the ablation runs.

#### Additional information about the scripts.
`run.sh` controls the entire experiment.
It calls `run-ruler.sh` and `run-ruler-rr.sh` for each domain,
then calls the parsing and visualizing scripts.
`run-ruler.sh` and `run-ruler-rr.sh` will call Ruler 
for a particular domain
with the parameters provided, 
for as many runs as required.
To change the parameters, simply modify the arguments 
passed to `run-ruler.sh` and `run-ruler-rr.sh` 
from inside `run.sh`.
- `-v` is the number of variables,
- `-i` is the iterations,
- `-r` is number of runs (i.e. providing how many independent data points we average over),
- `-d` is the domain
- `-o` is the output folder.
Additionally, there is one parameter that only affects
the the `run-ruler-rr.sh`, since 
there is a possibility of timeout in 
`orat` rational `no-rr`:
- `-t` is the amount of time each run will execute before timing out.

Lastly, any succeeding parameters will be
passed directly to the Ruler invocation.

If for some reason a run of Ruler fails to find rules,
the entire pipeline will fail and no plots will be generated,
so please keep an eye out for failures. 
You can temporarily generate the plots by removing
the log file of the aborted run and rerunning
the parsing and plotting scripts as they are invoked in `run.sh`, i.e.

```
node parse.js "output/$TIMESTAMP/compare/"
node parse.js "output/$TIMESTAMP/no-rr/" yes

python visualize.py "output/$TIMESTAMP/compare/" "$MYDIR/output/$TIMESTAMP/no-rr/"
```

### Validation Analysis
The goal is to reproduce `Table 2`.
This part of the eval requires rosette 4.0 and racket 8.0 which
are already pre-installed in this directory.

- Type `cd scripts/eqsat-sound` to go to the correct directory.

- To view `Table 2` directly from pre-generated data,
  go to `output/pre-gen-2021-07-06-2242` and type: `python3 ../../tabulate.py all.json`.
  Note that the first table is the one for `bv32`,
  then second for `bv4`, and
  third for `rational`.
  Compare these printed latex tables with the ones in the paper.
  Other than a few timing numbers which may slightly vary due to machine differences,
  the tables should be similar.
  The cells that are empty correspond to
  the ones with only a `-` in the paper's `Table 2`.

- To reproduce all the data,
type `./eqsat-soundness.sh`.
This will take [XXX] hours.
The data will be generated and put in a timestamped directory under `output`.
Each domain and configuration will have a directory and `all.json` will contain
all of them.
In each directory, you will find a `failed-validation.txt` file that has the rules
that failed post-pass validation, a `post_pass.json` which contains the result of postpass
validation with SMT (rosette in this case),
and a `rkt` script that uses rosette to validate the rules one at a time.
You will notice that Rosette times out on some of the post-pass validations.
For these, we had manually checked the
soundness of the rules using other solvers but that is not
in scope for this artifact.
The script will also print the number of unsound (and unknown) rules,
the domain (`rational`, `4`, `32`),
the number of variables, cvec length,
and sample size used for fuzzing (or `smt` for SMT based verification).

#### Additional information about the scripts.
`eqsat-soundness.sh` is the main script which runs
all three domains (`bv4`, bv32, `rational`)
for various cvec lengths, ways of generating cvecs, and
validation approaches.
It calls `postpass.sh` to verify the rules as a postpass, after
Ruler's execution is complete.
`postpass.sh` uses rosette to verify the rules.
It calls the `aggregate.sh` script to gather data into a
json file from which `tabulate.py` generates a latex table.

## Further Use / Extending Ruler
This section describes how to install Ruler on a different machine,
  the required dependencies,
  and how to extend our tool for other domains.

### Dependencies
We tested our artifact setup on a Ubuntu 20.04 VM.
To install and run the entire evaluation on a
  fresh machine with the same OS,
  the following dependencies must be installed:

- Basic tools
  * git
  * python3
  * moreutils
  * cmake
  * curl
  * rust

- More specialized tools
  * racket 7.9 (for Herbie)
  * racket 8.0 (for rosette)
  * rosette 4.0
  * cvc4 version 1.8
  * herbie (see the `herbie-eval.sh` script for version info)

- Tools for generating tables and plots
  * pandoc
  * xsv
  * python3-distutils
  * jq
  * matplotlib

The `scripts/eval.sh` can be used to run all the experiments with a single
script but since they all take varying amounts of time,
we recommend doing them separately.

### Installation of Ruler
Ruler is implemented in [Rust](rust-lang.org/).
You can install Rust [here](https://www.rust-lang.org/tools/install).
You can then get the code from Zenodo and
run the tool as described below.
To build Ruler, type `cargo build --release`.
This should take ~40 min.

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

### Extending Ruler to Support New Domains
Ruler's goal is to support rewrite inference for new domains,
  given a grammar, an interpreter, and a validation technique.
We have already generated documentation for you.
Open `target/doc/ruler/index.html` in your preferred browser to navigate the documentation.

You can generate documentation on your own in a new machine by typing:

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
rewrite rules for rationals till depth 2,
and by using fuzzing (with 10 values) for rule validation.

To understand how to add support for a new domain,
  you can look at the documentation of the various supported domains like
   `rational` (`target/doc/rational/index.html`),
   `rational_new_div` (`target/doc/rational_new_div/index.html`, relevant for `Section 6.3` in the paper),
   `bool` (`target/doc/bool/index.html`), etc.
Note that some domains (e.g., floats, strings, bigints)
are experimental and not reported in the paper,
but they all provide examples of how you can add support for new domains.