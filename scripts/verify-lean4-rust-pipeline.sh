#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
EXAMPLE="$ROOT/examples/ggen-first-lean4-rust"
GGEN="$ROOT/target/debug/ggen"
EVIDENCE="$EXAMPLE/generated/evidence"

python3 "$ROOT/scripts/validate_lean4_rust_pipeline.py"

mapfile -t authored < <(find "$EXAMPLE" -maxdepth 1 -type f -printf '%f\n' | sort)
printf '%s\n' "${authored[@]}"
test "${#authored[@]}" -eq 2
test "${authored[0]}" = "ggen.toml"
test "${authored[1]}" = "ontology.ttl"
test ! -e "$EXAMPLE/Cargo.toml"
test ! -e "$EXAMPLE/Main.lean"

cargo build --manifest-path "$ROOT/Cargo.toml" -p ggen-cli-lib --bin ggen

rm -rf "$EXAMPLE/generated" "$EXAMPLE/.ggen-v2"
mkdir -p "$EXAMPLE/generated"

(
  cd "$EXAMPLE"
  "$GGEN" sync run
)

test -f "$EXAMPLE/generated/lean/lean-toolchain"
test -f "$EXAMPLE/generated/lean/lake-manifest.json"
test -f "$EXAMPLE/generated/lean/lakefile.lean"
test -f "$EXAMPLE/generated/lean/Lean4RustPipeline.lean"
test -f "$EXAMPLE/generated/lean/Main.lean"
test -f "$EXAMPLE/generated/PIPELINE.md"
test ! -e "$EXAMPLE/generated/rust"

mkdir -p "$EVIDENCE"
(
  cd "$EXAMPLE"
  "$GGEN" receipt verify
) | tee "$EVIDENCE/ggen-receipt-verification.json"
jq -e '.valid == true and .signed == true and .signature_valid == true' \
  "$EVIDENCE/ggen-receipt-verification.json"

sha256sum \
  "$EXAMPLE/generated/lean/lean-toolchain" \
  "$EXAMPLE/generated/lean/lake-manifest.json" \
  "$EXAMPLE/generated/lean/lakefile.lean" \
  "$EXAMPLE/generated/lean/Lean4RustPipeline.lean" \
  "$EXAMPLE/generated/lean/Main.lean" \
  "$EXAMPLE/generated/PIPELINE.md" \
  > /tmp/lean4-rust-ggen-first.sha256

if rg -n '\b(sorry|axiom|admit|unsafe|partial_fixpoint)\b' \
  "$EXAMPLE/generated/lean/Lean4RustPipeline.lean" \
  "$EXAMPLE/generated/lean/Main.lean"; then
  echo '::error::generated Lean source expands trust or contains an incomplete proof'
  exit 1
fi

(
  cd "$EXAMPLE/generated/lean"
  lake build 2>&1 | tee "$EVIDENCE/lean-build.log"
  lake exe emitRust
)

jq -e '
  .schema == "ggen.lean4-rust/proof/v1" and
  .standing == "PROVEN" and
  .bound == 10 and
  .witness_input == 9 and
  .witness_output == 10 and
  .theorems == ["step_le", "step_witness", "step_fixed_point"]
' "$EXAMPLE/generated/lean/proof-receipt.json"

test -f "$EXAMPLE/generated/rust/Cargo.toml"
test -f "$EXAMPLE/generated/rust/src/lib.rs"
test -f "$EXAMPLE/generated/rust/src/main.rs"
grep -q '^\[workspace\]$' "$EXAMPLE/generated/rust/Cargo.toml"

(
  cd "$EXAMPLE/generated/rust"
  cargo fmt --all
  cargo fmt --all -- --check
  cargo clippy --all-targets --all-features -- -D warnings
  cargo test --all-targets --all-features -- --nocapture
  cargo run --quiet -- 9 > "$EVIDENCE/execution-receipt.json"
  cargo run --quiet -- 9 > "$EVIDENCE/execution-receipt-replay.json"
)

diff -u "$EVIDENCE/execution-receipt.json" "$EVIDENCE/execution-receipt-replay.json"
jq -e '
  .schema == "ggen.lean4-rust/execution/v1" and
  .standing == "EXECUTED" and
  .input == 9 and
  .output == 10 and
  (.lean_proof_blake3 | length) == 64
' "$EVIDENCE/execution-receipt.json"

sha256sum \
  "$EXAMPLE/generated/lean/proof-receipt.json" \
  "$EXAMPLE/generated/rust/Cargo.toml" \
  "$EXAMPLE/generated/rust/src/lib.rs" \
  "$EXAMPLE/generated/rust/src/main.rs" \
  > /tmp/lean4-rust-product-first.sha256

original_ontology=$(mktemp)
cp "$EXAMPLE/ontology.ttl" "$original_ontology"
restore_ontology() {
  cp "$original_ontology" "$EXAMPLE/ontology.ttl"
}
trap restore_ontology EXIT
python3 - "$EXAMPLE/ontology.ttl" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
text = path.read_text(encoding="utf-8")
updated = text.replace('lr:proofTheorem "step_le"', 'lr:proofTheorem "unproved_step"')
if updated == text:
    raise SystemExit("proof theorem sabotage did not apply")
path.write_text(updated, encoding="utf-8")
PY

if (
  cd "$EXAMPLE"
  "$GGEN" sync run
) > "$EVIDENCE/unproved-theorem-refusal.log" 2>&1; then
  echo '::error::unsupported proof theorem unexpectedly generated Rust authority'
  exit 1
fi

test -f "$EXAMPLE/generated/lean/proof-receipt.json"
test -f "$EXAMPLE/generated/rust/src/lib.rs"
restore_ontology
trap - EXIT
rm -f "$original_ontology"

(
  cd "$EXAMPLE"
  "$GGEN" sync run
)
sha256sum \
  "$EXAMPLE/generated/lean/lean-toolchain" \
  "$EXAMPLE/generated/lean/lake-manifest.json" \
  "$EXAMPLE/generated/lean/lakefile.lean" \
  "$EXAMPLE/generated/lean/Lean4RustPipeline.lean" \
  "$EXAMPLE/generated/lean/Main.lean" \
  "$EXAMPLE/generated/PIPELINE.md" \
  > /tmp/lean4-rust-ggen-second.sha256
diff -u /tmp/lean4-rust-ggen-first.sha256 /tmp/lean4-rust-ggen-second.sha256

(
  cd "$EXAMPLE/generated/lean"
  lake build
  lake exe emitRust
)
(
  cd "$EXAMPLE/generated/rust"
  cargo fmt --all
)
sha256sum \
  "$EXAMPLE/generated/lean/proof-receipt.json" \
  "$EXAMPLE/generated/rust/Cargo.toml" \
  "$EXAMPLE/generated/rust/src/lib.rs" \
  "$EXAMPLE/generated/rust/src/main.rs" \
  > /tmp/lean4-rust-product-second.sha256
diff -u /tmp/lean4-rust-product-first.sha256 /tmp/lean4-rust-product-second.sha256

printf '%s\n' 'ggen-first-lean4-rust-pipeline: ALIVE'
