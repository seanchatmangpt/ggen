#!/usr/bin/env bash
# Target-API fidelity check for wasm4pm-facts-pack.
#
# ontology.ttl's compat:CognitionBreed and pi:ProcessIntelligenceAlgorithm
# individuals are a one-time hand copy of wasm4pm's own
# ggen/ontology/{breeds,algorithms}.ttl. This script re-diffs the copy
# against a live upstream checkout, per-individual (subject-block level, not
# whole-file), and fails loudly (non-zero exit) on any drift instead of
# leaving it to silently accumulate.
#
# Usage: scripts/check_upstream_drift.sh [path-to-wasm4pm-checkout]
# Default upstream path: $HOME/wasm4pm (the checkout this pack was last
# verified against; commit b6fedcbef8d5fbdab4dbb9827226e802fe961a71,
# 2026-07-18 -- see DRIFT_LOG.md in this directory for the dated
# verification history and prior findings).
set -euo pipefail

UPSTREAM_ROOT="${1:-$HOME/wasm4pm}"
UPSTREAM_BREEDS="$UPSTREAM_ROOT/ggen/ontology/breeds.ttl"
UPSTREAM_ALGOS="$UPSTREAM_ROOT/ggen/ontology/algorithms.ttl"
PACK_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PACK_ONTOLOGY="$PACK_ROOT/ontology.ttl"

if [[ ! -f "$UPSTREAM_BREEDS" || ! -f "$UPSTREAM_ALGOS" ]]; then
  echo "FAIL: upstream wasm4pm ontology files not found under $UPSTREAM_ROOT" >&2
  echo "  expected: $UPSTREAM_BREEDS" >&2
  echo "  expected: $UPSTREAM_ALGOS" >&2
  exit 2
fi

python3 - "$UPSTREAM_BREEDS" "$UPSTREAM_ALGOS" "$PACK_ONTOLOGY" <<'PYEOF'
import re
import sys

upstream_breeds_path, upstream_algos_path, pack_path = sys.argv[1:4]


def extract(path, marker):
    txt = open(path, encoding="utf-8").read()
    return re.findall(marker + r".*?\.\n", txt, re.S)


def norm(block):
    return re.sub(r"\s+", " ", block).strip()


breed_marker = r"compat:Breed_\w+ a compat:CognitionBreed ;"
algo_marker = r"pi:Algo_\w+ a pi:ProcessIntelligenceAlgorithm ;"

upstream_breeds = {norm(b) for b in extract(upstream_breeds_path, breed_marker)}
upstream_algos = {norm(b) for b in extract(upstream_algos_path, algo_marker)}
pack_breeds = {norm(b) for b in extract(pack_path, breed_marker)}
pack_algos = {norm(b) for b in extract(pack_path, algo_marker)}

failed = False

for label, upstream, pack in (
    ("breed", upstream_breeds, pack_breeds),
    ("algorithm", upstream_algos, pack_algos),
):
    missing_from_pack = upstream - pack
    extra_in_pack = pack - upstream
    if missing_from_pack or extra_in_pack:
        failed = True
        print(f"DRIFT in {label} individuals:")
        for b in sorted(missing_from_pack):
            subj = b.split()[0]
            print(f"  upstream-only (pack is stale) : {subj}")
        for b in sorted(extra_in_pack):
            subj = b.split()[0]
            print(f"  pack-only (diverged from upstream) : {subj}")
    else:
        print(f"OK: {label} individuals match upstream exactly ({len(upstream)} rows)")

sys.exit(1 if failed else 0)
PYEOF
