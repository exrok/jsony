#!/usr/bin/env bash
# Quantify `jsony_macros_source` coverage by driving the macro source in-process
# (the `derive_tester expand` mode), then scoping an llvm-cov report to the
# macro-source package.
#
# Usage:  crates/derive_tester/coverage.sh [COUNT] [summary|lines] [MODE]
#   COUNT          number of generated types to expand (default 1000)
#   summary|lines  report form: `summary` (table only, default) or `lines`
#                  (table + uncovered line numbers, i.e. --show-missing-lines)
#   MODE           what to drive (default `all`):
#                    all      expand-only corpora (valid + errors + templates)
#                             plus the generated sweep — the full instrument
#                    sweep    only the generated sweep (the original baseline)
#                    corpus   only the valid expand-only corpus
#                    errors   only the malformed-input corpus
#                    templates only the object!/array! corpus
#
# DT_SEED pins the id band (default 0) so successive counts are nested and the
# coverage-vs-count curve is monotonic. Override to explore a different band.
#
# Two steps are required: `run` cannot scope its report to a non-running package,
# so the run records profile data with `--no-report` and a separate `report -p
# jsony_macros_source` renders only the macro-source files.
set -euo pipefail

count="${1:-1000}"
form="${2:-summary}"
mode="${3:-all}"
export DT_SEED="${DT_SEED:-0}"

root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$root"

case "$form" in
    summary) report_flag="--summary-only" ;;
    lines) report_flag="--show-missing-lines" ;;
    *) echo "unknown report form: $form (expected summary|lines)" >&2; exit 2 ;;
esac

# Map the mode onto the `expand` sub-command argument vector.
case "$mode" in
    all)       run_args=(expand all "$count") ;;
    sweep)     run_args=(expand "$count") ;;
    corpus)    run_args=(expand corpus) ;;
    errors)    run_args=(expand errors) ;;
    templates) run_args=(expand templates) ;;
    *) echo "unknown mode: $mode (expected all|sweep|corpus|errors|templates)" >&2; exit 2 ;;
esac

cargo +nightly llvm-cov clean --workspace --profraw-only
cargo +nightly llvm-cov run --no-report --branch -p derive_tester -- "${run_args[@]}"
cargo +nightly llvm-cov report --branch "$report_flag" -p jsony_macros_source
