# JFP 2026 LLM case-study data

Inputs and outputs for the LLM case studies (paper Tables 7 and 8,
`table:llm1` / `table:llm2` in 06-extensibility.tex).

## Layout

- `baseline/` — Enumo and A5 baseline rulesets (written by the
  `establish_baseline` tests) and baseline-vs-baseline derivability.
- `cs1/{halide,trig,exp}/` — case study 1: LLM-generated *rules*.
  Candidate/validated/minimized rulesets and derivability matrices
  against the baselines, for each prior and reprompt round.
- `cs2/halide/` — case study 2: LLM-generated *workload*, then normal
  Ruler synthesis over it per prior. Depends on cs1 halide outputs
  (`cs1/halide/LLM-None-{1,2}.rules`), so run cs1 first.

## Conventions

- `.rules`, `.terms`, and `*-derive.json` files are committed;
  `*.txt` logs are gitignored (top-level `.gitignore`).
- `cs1/trig/complex.rules` is an *input*: the trusted start rules for
  validating LLM trig candidates. It is a pruned copy of
  `scripts/oopsla21/trig/complex.rules` (division rules removed — some
  are unsound at 0, which matters when the ruleset is used to *verify*
  candidates); the original OOPSLA'21 artifact stays untouched.
- Run the case-study tests by name with `--release --nocapture`; never
  bare `cargo test` (LLM spend + hour-long recipes). Requires
  `OPENROUTER_API_KEY` in `.env` at the repo root.

## Row-name mapping to the paper

`LLM-None-N` = paper `LLM-N`; `w-None` = `LLM-W`; `w-X` = `LLM-W-X`;
`-C`/`-RAT` suffixes (unions with the complex/rational rules) are
dropped in the paper.
