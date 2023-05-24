## Herbie evaluation

Herbie evaluation for OOPSLA '23

### Dependencies

To run this evaluation, the following dependencies must be installed:
 - Bash
 - Python3
 - Racket (>= 8.0)
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
- `rules`: Ruler rules need to be stored here to run the evaluation

### Usage

First,
  run Ruler to generate a nightly output `*.json`
  file as `rules/output.json`.

To generate Herbie data from scratch:
```
bash eval.sh <num seeds>
```
  where the evaluation runs Herbie over `<num seeds>` seeds
  over the same benchmark set.
Optionally,
  set the environment variable `PARALLEL_SEEDS`
  to run Herbie in parallel (This requires `env_parallel`.)
When the evaluation is finished,
  the plots will be stored in a timestamped
  report under `reports/`.
