# JFP 2026 LLM case studies

This branch implements the LLM case studies from section 6 of the JFP 2026
paper: extending Enumo with `Ruleset::from_llm` and `Workload::from_llm` and
using them for end-to-end ruleset synthesis (case study 1: Exponential,
Trigonometric, Halide) and workload synthesis (case study 2: Halide). This
directory holds all of the evidence those runs produce; artifact names match
the paper's table names (`LLM-1`, `LLM-A5-2`, `LLM-W-ENUMO`, `ENUMO`,
`HALIDE`, `HERBIE`, ...), so every table cell can be traced to a file.

## Running

Requires `OPENROUTER_API_KEY` in a top-level `.env` (gitignored); the models
queried are listed in `src/llm.rs` (three models, two queries each per
prompt). A complete run costs about 4.5 hours, dominated by case study 2's
derivability (~3 h) and case study 1 halide's LLM queries (~50 min).

```sh
bash scripts/replicate_jfp.sh
```

runs everything from scratch in dependency order (stages can be commented
out to run a subset). The individual stages, which must respect this order:

```sh
cargo test --test exponential establish_baseline -- --nocapture
cargo test --test trig        establish_baseline -- --nocapture
cargo test --test halide      establish_baseline -- --nocapture

cargo test --test exponential case_study1 -- --nocapture
cargo test --test trig        case_study1 -- --nocapture
cargo test --test halide      case_study1 -- --nocapture

cargo test --test halide      case_study2 -- --nocapture   # needs cs1 halide
```

Re-running a stage truncates its `log.txt` and overwrites its artifacts, so
each directory always describes exactly one run (older runs live in git
history). LLM output is nondeterministic: rule counts and derivability
percentages shift between runs, though the trends are stable.

## Layout

| Path | Contents |
|---|---|
| `baseline/{exp,trig,halide}/` | ENUMO (and, for halide, A5) baseline rulesets plus baseline-vs-baseline derivability |
| `cs1/{exp,trig,halide}/` | case study 1: LLM ruleset synthesis, two rounds (initial prompt + reprompt) |
| `cs2/halide/` | case study 2: LLM workload synthesis, rules inferred per prior |
| `cs1/trig/complex.rules` | **input**, not generated: the trusted arithmetic rules for the trig pipeline |
| `../baseline/` (repo root) | committed expert baselines (`herbie-*.rules`, `halide.rules`) |

Every run directory contains:

- `log.txt` — the run narrative: a provenance header (date, git commit,
  `-dirty` if the tree had uncommitted changes), one line per LLM query
  (`model (query n) | lines | new (invalid) | time`), the pipeline funnel
  per ruleset (candidates, then `sound/unverified` for exp/trig or
  `selected (invalid) of N candidates` for halide, then minimized counts),
  one line per derivability comparison, and a `=== ... complete ===`
  footer. A log without the footer is from an interrupted run.
- `results.json` — provenance plus `{count, time}` per synthesized artifact:
  the paper tables' "# Rules" and "Synthesis Time" columns.
- `<ROW>-<COL>-derive.json` — one derivability cell: `percent_derivable`
  and `time` (the table cell), plus the full `can`/`cannot` rule lists.
- `raw/` — the exact prompt sent for each query batch (`<name>-prompt.txt`;
  reprompts embed rules synthesized earlier in the same run) and every
  model's raw response (`<name>-<model>-q<n>.txt`).

## Mapping to the paper tables

- **Table llm1** (derivability matrices): LLM-row cells come from
  `cs1/<domain>/<ROW>-<COL>-derive.json`; baseline-row cells (`A5`,
  `ENUMO` vs the expert baselines and each other) come from
  `baseline/<domain>/`; baseline rows vs LLM columns come from
  `cs1/<domain>/ENUMO-LLM-1-derive.json` etc. Synthesis times come from
  each directory's `results.json`. For exp/trig the `ENUMO -> ENUMO` 100%
  cells are definitional and not computed (halide's self-cells are, by its
  baseline run).
- **Table llm2**: rows `LLM-W*` come from `cs2/halide/`; the `LLM-2` row is
  cumulative over case study 1 halide (`LLM-1.rules` union `LLM-2.rules`;
  its synthesis time is the sum of both rounds' `results.json` entries).
  The workload itself is recorded as `llm-wkld` (terms in `llm-wkld.terms`,
  generation time in `results.json`) and is excluded from the `LLM-W*`
  synthesis times, as in the paper.

## Validation, per domain

- **Halide**: SMT (z3, 1s timeout per rule), fused into `minimize` — the
  logged `invalid` counts are rules flagged during minimization.
- **Exponential / Trigonometric**: SMT does not apply; candidates count as
  sound only if derivable from the trusted rules in each test's
  `start_rules()` (arithmetic rules, the exploratory definitions, and a
  small set of starting identities for the domain's operators).

## Code map

- `src/llm.rs` — OpenRouter client: `models()`, `query` (one query; records
  the raw response), `query_each` (the runner: every model twice, per-query
  stats).
- `src/enumo/ruleset.rs` / `src/enumo/workload.rs` — `from_llm`.
- `src/logger.rs` — `RunLog`: the single interface all evidence goes
  through (log lines, derivability json, raw prompts/responses,
  `results.json`).
- `tests/{exponential,trig,halide}.rs` — the baselines and case studies.
