#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MANIFEST="$ROOT/tools/ggen-architecture/Cargo.toml"
STATE="$ROOT/architecture/ggen-enterprise.json"
STIMULI="$ROOT/architecture/stimuli/sample-cycle.json"
OUT="${GGEN_ARCHITECTURE_RECEIPTS_DIR:-$ROOT/target/architecture-receipts}"
OBSERVED_AT="${GGEN_ARCHITECTURE_OBSERVED_AT:-synthetic-proof-v1}"

mkdir -p "$OUT"

cargo run --quiet --manifest-path "$MANIFEST" -- \
  validate --state "$STATE" --json > "$OUT/registry-validation.json"

cargo run --quiet --manifest-path "$MANIFEST" -- \
  doctor --state "$STATE" --json > "$OUT/doctor.json"

cargo run --quiet --manifest-path "$MANIFEST" -- \
  capacity --state "$STATE" --json > "$OUT/capacity-envelope.json"

cargo run --quiet --manifest-path "$MANIFEST" -- \
  impact --state "$STATE" --asset enterprise-architecture-ontology --json \
  > "$OUT/ontology-impact.json"

cargo run --quiet --manifest-path "$MANIFEST" -- \
  cycle --state "$STATE" --stimuli "$STIMULI" \
  --observed-at "$OBSERVED_AT" --json > "$OUT/autonomic-cycle.json"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$OUT" && sha256sum *.json > SHA256SUMS)
elif command -v shasum >/dev/null 2>&1; then
  (cd "$OUT" && shasum -a 256 *.json > SHA256SUMS)
else
  printf '%s\n' "SHA-256 utility unavailable; JSON receipts remain individually BLAKE3-bound." \
    > "$OUT/SHA256SUMS.unavailable"
fi

printf 'ggen architecture autonomics: ALIVE\nreceipts: %s\n' "$OUT"
