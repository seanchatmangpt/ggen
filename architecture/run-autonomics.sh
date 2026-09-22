#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MANIFEST="$ROOT/tools/ggen-architecture/Cargo.toml"
# tools/ggen-architecture/src/bin/ has two binaries (ggen-architecture, ggen-ea)
# with no `default-run` set, so a bare `cargo run` is ambiguous ("could not
# determine which binary to run") -- pin explicitly to the one whose CLI
# surface (validate/doctor/capacity/impact/cycle/fortune5) this script drives.
BIN="ggen-architecture"
STATE="$ROOT/architecture/ggen-enterprise.json"
STIMULI="$ROOT/architecture/stimuli/sample-cycle.json"
FORTUNE5="$ROOT/architecture/fortune5/synthetic-level5.json"
CROWN="$ROOT/architecture/fortune5-crown-synthetic.json"
OUT="${GGEN_ARCHITECTURE_RECEIPTS_DIR:-$ROOT/target/architecture-receipts}"
OBSERVED_AT="${GGEN_ARCHITECTURE_OBSERVED_AT:-synthetic-proof-v1}"

mkdir -p "$OUT"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  validate --state "$STATE" --json > "$OUT/registry-validation.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  doctor --state "$STATE" --json > "$OUT/doctor.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  capacity --state "$STATE" --json > "$OUT/capacity-envelope.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  impact --state "$STATE" --asset enterprise-architecture-ontology --json \
  > "$OUT/ontology-impact.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  cycle --state "$STATE" --stimuli "$STIMULI" \
  --observed-at "$OBSERVED_AT" --json > "$OUT/autonomic-cycle.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  fortune5 catalog --json > "$OUT/fortune5-catalog.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  fortune5 assess --program "$FORTUNE5" --json > "$OUT/fortune5-assessment.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  fortune5 plan --program "$FORTUNE5" --json > "$OUT/fortune5-autonomic-plan.json"

cargo run --quiet --manifest-path "$MANIFEST" --bin "$BIN" -- \
  fortune5 crown --program "$FORTUNE5" --crown "$CROWN" --json \
  > "$OUT/fortune5-level5-crown.json"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$OUT" && sha256sum *.json > SHA256SUMS)
elif command -v shasum >/dev/null 2>&1; then
  (cd "$OUT" && shasum -a 256 *.json > SHA256SUMS)
else
  printf '%s\n' "SHA-256 utility unavailable; JSON receipts remain individually BLAKE3-bound." \
    > "$OUT/SHA256SUMS.unavailable"
fi

printf 'ggen architecture autonomics: ALIVE\nreceipts: %s\n' "$OUT"
