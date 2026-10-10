#!/usr/bin/env bash
# delta_validate_wasm.sh — WASM parse-back for the Delta(G)=0 law.
#
# Witnessed design (see delta-validation-spec.md Section 3.3):
#   .wat: `wat2wasm mod.wat -o /dev/null` is the syntax gate (refuses
#   malformed text). Binary .wasm: `wasm2wat mod.wasm` first. The surface
#   is then the `(export "name")` clauses, parsed line-oriented from the
#   wat text — the text format is the toolchain's own grammar.
#
# Usage: delta_validate_wasm.sh <artifact.wat|artifact.wasm> <expected-surface.txt> [--report <path>]
#   expected-surface.txt: one export name per line, # comments.
#   Exit 0 = ALIVE (Delta empty), exit 1 = ABORT (Delta nonempty or tool failure).
set -euo pipefail

art=$1; exp=$2; report=""
[[ "${3:-}" == "--report" ]] && report=$4

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

case "$art" in
  *.wat)
    if ! wat2wasm "$art" -o /dev/null 2>"$work/wat2wasm.err"; then
      echo "ABORT: wat2wasm refused $art" >&2; cat "$work/wat2wasm.err" >&2; exit 1
    fi
    cp "$art" "$work/mod.wat"
    ;;
  *.wasm)
    if ! wasm2wat "$art" -o "$work/mod.wat" 2>"$work/wasm2wat.err"; then
      echo "ABORT: wasm2wat refused $art" >&2; cat "$work/wasm2wat.err" >> /dev/stderr; exit 1
    fi
    ;;
  *) echo "usage: <artifact.wat|.wasm>" >&2; exit 1;;
esac

grep -o '(export "[^"]*"' "$work/mod.wat" | sed 's/(export "//; s/"$//' | sort -u > "$work/recovered.txt"

expected=$(grep -v '^\s*#' "$exp" | grep -v '^\s*$')
recovered_n=$(wc -l < "$work/recovered.txt" | tr -d ' ')
exp_n=$(printf '%s\n' "$expected" | grep -c . || true)

delta=$(comm -23 "$work/recovered.txt" <(printf '%s\n' "$expected" | sort -u))
dl_n=$(printf '%s\n' "$delta" | grep -c . || true)

{
  echo "delta-validation report (wasm)"
  echo "subject: $art"
  echo "G_original (expected surface): $exp_n exports"
  echo "G_recovered (wat export surface): $recovered_n exports"
  echo "Delta(G) = G_recovered \\ G_original ="
  if [[ $dl_n -eq 0 ]]; then
    if [[ "$recovered_n" -eq "$exp_n" ]]; then
      echo "  (none)"
      echo "VERDICT: ALIVE (Delta(G) = 0)"
    else
      echo "  (none — soundness holds; completeness gap: recovered < expected)"
      echo "VERDICT: ALIVE (soundness only; completeness gap of $((exp_n - recovered_n)))"
    fi
  else
    printf '  %s\n' "$delta"
    echo "VERDICT: ABORT (Delta(G) nonempty)"
    echo 1 > "$work/verdict"
  fi
} | tee ${report:+"$report"}

exit $(cat "$work/verdict" 2>/dev/null || echo 0)
