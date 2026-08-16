#!/usr/bin/env bash
# Run the full LLM case-study pipeline in order:
#   1. baselines      (exp, trig, halide)
#   2. case study 1   (exp, trig, halide)
#   3. case study 2   (halide; reads cs1 halide outputs)
# printing the time each step takes and a summary at the end.
#
# Outputs land in jfp/{baseline,cs1,cs2}/ (see jfp/README.md).
# Requires OPENROUTER_API_KEY in .env at the repo root (or exported);
# the case-study tests silently skip without it, so refuse to start.

set -e

cd "$(dirname "$0")/.."

# Keep the machine awake for the whole run (macOS). Instant-based timings
# in the logs pause during system sleep, and in-flight LLM queries can
# time out across sleep cycles, so sleeping mid-run both distorts and
# breaks results. Note: -s only prevents sleep on AC power, so plug in.
if [ -z "$RULER_CAFFEINATED" ] && command -v caffeinate >/dev/null 2>&1; then
    RULER_CAFFEINATED=1 exec caffeinate -is "$0" "$@"
fi
if command -v pmset >/dev/null 2>&1 && ! pmset -g batt | grep -q "AC Power"; then
    echo "warning: on battery power — macOS may still sleep mid-run; plug in" >&2
fi

# --- preflight ---------------------------------------------------------
if [ -z "$OPENROUTER_API_KEY" ] && ! grep -q "OPENROUTER_API_KEY" .env 2>/dev/null; then
    echo "error: OPENROUTER_API_KEY is not exported and not in .env" >&2
    echo "The case-study tests would silently skip without it." >&2
    exit 1
fi

STEPS=()
TIMES=()

run_step() {
    local name="$1"
    shift
    echo
    echo "=== $name ==="
    local start=$SECONDS
    if "$@"; then
        local dur=$((SECONDS - start))
        printf '=== %s done in %dm%02ds ===\n' "$name" $((dur / 60)) $((dur % 60))
        STEPS+=("$name")
        TIMES+=("$dur")
    else
        local dur=$((SECONDS - start))
        printf '=== %s FAILED after %dm%02ds ===\n' "$name" $((dur / 60)) $((dur % 60)) >&2
        exit 1
    fi
}

# Build first so the first step's time is not compile time
run_step "build" cargo build --release --tests

run_step "baseline: exp" \
    cargo test --release --test exponential establish_baseline -- --nocapture
run_step "baseline: trig" \
    cargo test --release --test trig establish_baseline -- --nocapture
run_step "baseline: halide" \
    cargo test --release --test halide establish_baseline -- --nocapture

run_step "case study 1: exp" \
    cargo test --release --test exponential case_study1 -- --nocapture
run_step "case study 1: trig" \
    cargo test --release --test trig case_study1 -- --nocapture
run_step "case study 1: halide" \
    cargo test --release --test halide case_study1 -- --nocapture

run_step "case study 2: halide" \
    cargo test --release --test halide case_study2 -- --nocapture

# --- summary -----------------------------------------------------------
echo
echo "=== summary ==="
total=0
for i in "${!STEPS[@]}"; do
    printf '%-24s %dm%02ds\n' "${STEPS[$i]}" $((TIMES[i] / 60)) $((TIMES[i] % 60))
    total=$((total + TIMES[i]))
done
printf '%-24s %dm%02ds\n' "total" $((total / 60)) $((total % 60))
