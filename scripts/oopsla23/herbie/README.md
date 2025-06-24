## Herbie evaluation

Herbie evaluation for OOPSLA 2023

### Dependencies

To run this evaluation, the following dependencies must be installed:
 - Bash
 - Python3
 - Racket (>= 8.2)
 - Rust (>= 1.60)

Optionally, install:
 - GNU Parallel (needed for `env_parallel`)

### Components

Scripts:
- `eval.sh`: top-level script for running the evaluation
- `install.sh`: installs Herbie under `herbie/`
- `plot.sh`: top-level plotting script
- `seed-variance.sh`: script for running Herbie, possibly in parallel

Plotting:
- `plot/config-all-tests-box-plot.py`: generates plots
- `plot/plot-results.sh`: preprocesses evaluation data for plotting

Directories:
- `bench`: directory where benchmarks are collated for evaluation
- `herbie`: directory where Herbie is installed
- `plot`: plotting code stored under here
- `reports`: Herbie reports generated here
- `saved`: A previous run for use in the artifact
- `rules`: Ruler rules need to be stored here to run the evaluation

### Results from Previous Runs

Generating the results of
  this experiment takes exceptionally long.
For the evaluation of this paper,
  we ran this experiment on a machine with 512 GB of RAM
  utilizing 60 cores at a time, and it took 6 hours
  to complete.
We recommend running: `scripts/oopsla23/plot.sh`:

```
cd scripts/oopsla23/herbie
./plot.sh saved/
```

to generate the results
  from a previous run that has be
  included with this VM.
It should take no more than 20 seconds
  and should emit .png and .pdf files under `saved/`.

### Usage

This WILL take a long time to run.
Running a single seed takes approximately 45 minutes
  and requires 4 cores and at least 8 GB of RAM.

First, run Ruler to generate a
  unified `*.json` file containing the rules
  from all the domains it knows about.
Copy or move that file to `rules/output.json`.

To generate Herbie data from scratch:
```
bash eval.sh <num seeds>
```
  where the evaluation runs Herbie over `<num seeds>` seeds
  over the same benchmark set.
For the evaluation we used 30 seeds.
Optionally,
  set the environment variable `PARALLEL_SEEDS`
  to run Herbie in parallel (This requires `env_parallel`.)
When the evaluation is finished,
  the plots will be stored in a timestamped folder
  under `reports/`.
