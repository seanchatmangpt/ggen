#!/usr/bin/env bash
# delta_validate_rust.sh — Rust parse-back for the Delta(G)=0 law.
#
# Witnessed design (delta-validation-spec.md Section 3.2, receipt 2026-10-09):
#   1. `rustc --emit=metadata` output (.rmeta) is NOT readable by `nm` —
#      "The file was not recognized as a valid object file". Spec design
#      corrected: metadata-only emission has no symbol surface.
#   2. `nm -g` on a full rlib works but is unreliable on this host: Apple nm
#      (Xcode CLT) and llvm-nm 21 both refuse some LLVM-22 object members
#      with "Unknown attribute kind (102/105)" — the rlib for a crate using
#      std alloc glue was unreadable while a flat crate read fine. Degraded
#      fallback only, never primary.
#   3. Primary anchor: `rustdoc --output-format json -Z unstable-options`
#      (nightly). Public items are machine-extractable from the JSON index
#      (`visibility == "public"`), no object parsing at all.
#
# Requires a NIGHTLY rustdoc. If the ambient toolchain is not nightly, the
# script retries with the rustup-active nightly (ggen pins one in
# rust-toolchain.toml).
#
# Usage: delta_validate_rust.sh <artifact.rs> <expected-surface.txt> [--report <path>]
#   expected-surface.txt: one "crate::item" path per line, # comments.
#   Exit 0 = ALIVE (Delta empty), exit 1 = ABORT (Delta nonempty or tool failure).
set -euo pipefail

rs=$1; exp=$2; report=""
[[ "${3:-}" == "--report" ]] && report=$4

# --- resolve a nightly rustdoc ---
RD=(rustdoc)
if ! rustdoc --version 2>/dev/null | grep -q nightly; then
  tc=$(rustup show active-toolchain 2>/dev/null || true)
  if [[ -z "$tc" || "$tc" != *nightly* ]]; then
    tc=$(rustup toolchain list 2>/dev/null | grep -i nightly | head -1 | awk '{print $1}')
  fi
  if [[ -n "$tc" && "$tc" == *nightly* ]]; then
    RD=(rustup run "$tc" rustdoc)
  else
    echo "ABORT: nightly rustdoc required (rustdoc JSON backend is nightly-only)" >&2
    exit 1
  fi
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
outdir="$work/doc"
mkdir -p "$outdir"

if ! "${RD[@]}" --output-format json -Z unstable-options "$rs" -o "$outdir" 2>"$work/rustdoc.err"; then
  echo "ABORT: rustdoc failed on $rs" >&2; cat "$work/rustdoc.err" >&2; exit 1
fi

crate=$(basename "$rs" .rs)
json=$(find "$outdir" -name "${crate}.json" | head -1)
if [[ -z "$json" ]]; then
  echo "ABORT: rustdoc JSON for $crate not found under $outdir" >&2; exit 1
fi

# Surface = public items, path crate::name (top-level; nested-module paths
# are future work — the demo ontologies declare top-level surfaces).
python3 - "$json" "$crate" > "$work/recovered.txt" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
crate = sys.argv[2]
for i in d["index"].values():
    if i.get("visibility") == "public" and i["name"] != crate:
        print(f"{crate}::{i['name']}")
PY

expected=$(grep -v '^\s*#' "$exp" | grep -v '^\s*$' | sort -u)
# Delta = recovered \ expected: items recovered that the expected surface does not declare
delta=$(comm -23 <(sort -u "$work/recovered.txt") <(printf '%s\n' "$expected" | sort -u))

rec_n=$(wc -l < "$work/recovered.txt" | tr -d ' ')
exp_n=$(printf '%s\n' "$expected" | grep -c . || true)
dl_n=$(printf '%s\n' "$delta" | grep -c . || true)
v=0

{
  echo "delta-validation report (rust/rustdoc-json)"
  echo "subject: $rs"
  echo "G_original (expected surface): $exp_n paths"
  echo "G_recovered (public-item surface): $rec_n paths"
  echo "Delta(G) = G_recovered \\ G_original ="
  if [[ $dl_n -eq 0 ]]; then
    echo "  (none)"
    echo "VERDICT: ALIVE (Delta(G) = 0)"
  else
    printf '  %s\n' "$delta"
    echo "RECOVERED SET:"
    sed 's/^/  /' "$work/recovered.txt"
    echo "VERDICT: ABORT (Delta(G) nonempty)"
    echo 1 > "$work/verdict"
  fi
} | tee ${report:+"$report"}

exit $(cat "$work/verdict" 2>/dev/null || echo 0)
