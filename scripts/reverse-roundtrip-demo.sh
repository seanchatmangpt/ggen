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
# PROCESS-MINING NOTE: this round-trip first exposed a forward-sync defect —
# `generate_rust` named files `"telemetry".rs` (literal quotes) because the
# low-level sync did not strip quotes from string-literal SPARQL bindings. That
# defect is now FIXED in `crates/ggen-core/src/sync/mod.rs` (`group_by_key` →
# `strip_rdf_term_artifacts`), so generated filenames are clean (`telemetry.rs`).
# The shared `Graph::query_cached` is intentionally left quoted (its behaviour is
# test-documented in `tests/rdf_literal_type_test.rs`); the fix is sync-side only.
#
# Usage:  bash scripts/reverse-roundtrip-demo.sh
# Requires:  a built `ggen` binary (cargo build -p ggen-cli-lib --bin ggen).

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
GGEN="${GGEN:-$REPO_ROOT/target/debug/ggen}"

if [[ ! -x "$GGEN" ]]; then
  echo "REFUSED: ggen binary not found at $GGEN (run: cargo build -p ggen-cli-lib --bin ggen)"
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
