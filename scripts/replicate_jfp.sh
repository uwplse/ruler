#!/usr/bin/env bash
# Replicate the JFP 2026 LLM case studies (paper section 6) from scratch:
# clean out previously generated evidence, re-establish the baselines, and
# run both case studies, in dependency order.
#
# Requirements:
#   - OPENROUTER_API_KEY in .env at the repo root (or exported); the case
#     studies issue real LLM queries (54 total: 9 prompts x 3 models x 2).
#   - Network access on first build (the z3 crate downloads libz3).
#
# Based on the previous complete run, expect ~4.5 hours end to end:
# case_study2's derivability alone is ~3 h, and case_study1 halide ~50 min.
# Progress streams to the terminal and to jfp/**/log.txt; every finished
# stage's log ends with a "=== ... complete ===" footer (checked at the
# end of this script).
#
# Note: the run writes new artifacts under jfp/, so later stages' logs
# are stamped "git <sha>-dirty" unless the cleanup below is committed
# before running.

set -euo pipefail
cd "$(dirname "$0")/.."

# --- preflight ----------------------------------------------------------
if ! { [ -f .env ] && grep -q "OPENROUTER_API_KEY" .env; } \
    && [ -z "${OPENROUTER_API_KEY:-}" ]; then
    echo "error: OPENROUTER_API_KEY not found in .env or the environment." >&2
    echo "The case studies would silently skip without it." >&2
    exit 1
fi
if [ -n "$(git status --porcelain)" ]; then
    echo "warning: working tree is already dirty; provenance headers will say -dirty." >&2
fi
# complex.rules is a hand-curated input the clean step must preserve and
# nothing regenerates. Refuse to start (before deleting anything) if it is
# missing, rather than wiping the other results and failing mid-run.
if [ ! -f jfp/cs1/trig/complex.rules ]; then
    echo "error: jfp/cs1/trig/complex.rules is missing." >&2
    echo "It is a required input, not a generated file; restore it from git" >&2
    echo "(git checkout -- jfp/cs1/trig/complex.rules) before replicating." >&2
    exit 1
fi

# Compile everything up front so build time isn't attributed to a stage.
cargo test --no-run

# --- clean generated evidence (inputs are preserved) ---------------------
# jfp/cs1/trig/complex.rules is an input (the trusted arithmetic rules);
# everything else under jfp/ is a generated artifact of a previous run.
# The baseline/ directory (Herbie/Halide rulesets) is untouched.
echo ">>> removing previously generated results under jfp/"
rm -rf jfp/baseline jfp/cs1/exp jfp/cs1/halide jfp/cs2
if [ -d jfp/cs1/trig ]; then
    find jfp/cs1/trig -type f ! -name complex.rules -delete
    rm -rf jfp/cs1/trig/raw
fi

stage() {
    local desc="$1"
    shift
    echo
    echo ">>> ${desc} | started $(date '+%Y-%m-%d %H:%M:%S')"
    local t0=$SECONDS
    "$@"
    local dt=$((SECONDS - t0))
    echo ">>> ${desc} | finished in $((dt / 60))m$((dt % 60))s"
}

# Input sanity checks (fast; also catches an over-eager clean).
stage "preflight: exp inputs parse" \
    cargo test --test exponential herbie_baseline_parses -- --nocapture
stage "preflight: trig inputs parse" \
    cargo test --test trig trusted_rule_files_parse -- --nocapture

# --- baselines (independent of each other; ~13 min total) ----------------
stage "baseline: exponential (~5s)" \
    cargo test --test exponential establish_baseline -- --nocapture
stage "baseline: trig (~11 min)" \
    cargo test --test trig establish_baseline -- --nocapture
stage "baseline: halide (~2 min)" \
    cargo test --test halide establish_baseline -- --nocapture

# --- case study 1 (needs the baselines; ~65 min total) -------------------
stage "case_study1: exponential (~10 min)" \
    cargo test --test exponential case_study1 -- --nocapture
stage "case_study1: trig (~6 min)" \
    cargo test --test trig case_study1 -- --nocapture
stage "case_study1: halide (~50 min)" \
    cargo test --test halide case_study1 -- --nocapture

# --- case study 2 (needs case_study1 halide; ~3 h) -----------------------
stage "case_study2: halide (~3 h, derivability-dominated)" \
    cargo test --test halide case_study2 -- --nocapture

# --- verify every run finished -------------------------------------------
echo
echo ">>> log footers (a log without a 'complete' footer was interrupted):"
for log in jfp/baseline/*/log.txt jfp/cs1/*/log.txt jfp/cs2/*/log.txt; do
    printf '%-28s %s\n' "${log}:" "$(tail -1 "${log}")"
done
echo ">>> replication done $(date '+%Y-%m-%d %H:%M:%S')"
