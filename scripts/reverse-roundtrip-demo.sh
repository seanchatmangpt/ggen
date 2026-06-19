#!/usr/bin/env bash
# reverse-roundtrip-demo.sh — Phase 4: the full O → O* → μ → A → replay loop.
#
# Composes the two shipped pipelines end-to-end to prove that `ggen reverse`
# output is forward-consumable by `ggen sync`:
#
#   code (O)
#     → ggen reverse scan      → .specify/discovered/<name>.ttl  (O*, + receipt)
#     → ggen sync --ontology … → generated Rust + .ggen/receipts/latest.json (A)
#
# Receipts at both ends are the replay/provenance record.
#
# HONEST FINDING (evidence-first): this round-trip is also a process-mining probe
# — feeding REAL discovered RDF through the forward line surfaced a PRE-EXISTING
# defect in the low-level forward sync. `ggen_core::graph::Graph::query_cached`
# returns SPARQL solution values via `term.to_string()`, which QUOTES string
# literals; the low-level `generate_rust` then uses those values as struct
# names, so generated files are named `"telemetry".rs` (with literal quotes)
# rather than `telemetry.rs`. That is a forward-pipeline issue in a SHARED graph
# method (broad blast radius across every query_cached consumer), NOT a defect of
# the reverse pipeline — the reverse pipeline's own `clean_term` already strips
# this. It is documented here as a follow-up, not patched, because changing the
# shared method's output format needs its own blast-radius analysis.
#
# Usage:  bash scripts/reverse-roundtrip-demo.sh
# Requires:  a built `ggen` binary (cargo build -p ggen-cli --bin ggen).

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
GGEN="${GGEN:-$REPO_ROOT/target/debug/ggen}"

if [[ ! -x "$GGEN" ]]; then
  echo "REFUSED: ggen binary not found at $GGEN (run: cargo build -p ggen-cli --bin ggen)"
  exit 64
fi

WORK="$(mktemp -d "${TMPDIR:-/tmp}/ggen-roundtrip.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/src" "$WORK/q"

# A small real source file to discover.
cat > "$WORK/src/lib.rs" <<'RS'
pub struct Telemetry { id: u64, sector: String, speed_kph: f64 }
pub struct PitStop { lap: u32, duration_ms: u64 }
RS

# The forward query: the low-level pipeline groups by ?service, so we project the
# discovered service names back out of the authority graph.
cat > "$WORK/q/extract-services.rq" <<'RQ'
SELECT ?service WHERE {
  ?svc <https://ggen.io/discovered#serviceName> ?service .
}
ORDER BY ?service
RQ

cd "$WORK"

echo "== [O→O*] ggen reverse scan =="
"$GGEN" reverse scan --paths src --name roundtrip

echo "== [μ→A] ggen sync over the discovered graph =="
"$GGEN" sync --ontology .specify/discovered/roundtrip.ttl --queries q --language rust --output-dir gen

echo "== generated artifacts (A) =="
find gen -type f

echo "== provenance (replay) =="
ls .ggen/receipts/

# The loop is sound iff: a discovered graph was produced, the forward pipeline
# consumed it to generate code, and receipts exist at both ends.
test -f .specify/discovered/roundtrip.ttl
test -n "$(find gen -name '*.rs' -print -quit)"
test -n "$(find .ggen/receipts -name 'reverse-scan-*.json' -print -quit)"
test -f .ggen/receipts/latest.json

echo "ROUND-TRIP OK: reverse output is forward-consumable (see HONEST FINDING in header re: generated-name quoting)."
