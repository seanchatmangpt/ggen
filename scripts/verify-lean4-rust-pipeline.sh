#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
EXAMPLE="$ROOT/examples/ggen-first-lean4-rust"
GGEN="$ROOT/target/debug/ggen"
EVIDENCE="$EXAMPLE/generated/evidence"
PROOF_RECEIPT="$EXAMPLE/generated/lean/proof-receipt.json"
RUST="$EXAMPLE/generated/rust"

python3 "$ROOT/scripts/validate_lean4_rust_pipeline.py"

mapfile -t root_files < <(find "$EXAMPLE" -maxdepth 1 -type f -printf '%f\n' | sort)
for root_file in "${root_files[@]}"; do
  case "$root_file" in
    ggen.lock | ggen.toml | ontology.ttl) ;;
    *)
      echo "::error::unexpected consumer root file: $root_file"
      exit 1
      ;;
  esac
done
mapfile -t authored < <(
  find "$EXAMPLE" -maxdepth 1 -type f \
    \( -name 'ggen.toml' -o -name 'ontology.ttl' \) \
    -printf '%f\n' | sort
)
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
test -f "$EXAMPLE/generated/lean/Fortune5Policy.lean"
test -f "$EXAMPLE/generated/lean/RustLib.lean"
test -f "$EXAMPLE/generated/lean/RustMain.lean"
test -f "$EXAMPLE/generated/lean/RustEvidence.lean"
test -f "$EXAMPLE/generated/lean/Lean4RustPipeline.lean"
test -f "$EXAMPLE/generated/lean/Main.lean"
test -f "$EXAMPLE/generated/PIPELINE.md"
test ! -e "$RUST"

mkdir -p "$EVIDENCE"
(
  cd "$EXAMPLE"
  "$GGEN" receipt verify
) | tee "$EVIDENCE/ggen-receipt-verification.json"
jq -e '.valid == true and .signed == true and .signature_valid == true and .outputs == 10' \
  "$EVIDENCE/ggen-receipt-verification.json"

sha256sum \
  "$EXAMPLE/generated/lean/lean-toolchain" \
  "$EXAMPLE/generated/lean/lake-manifest.json" \
  "$EXAMPLE/generated/lean/lakefile.lean" \
  "$EXAMPLE/generated/lean/Fortune5Policy.lean" \
  "$EXAMPLE/generated/lean/RustLib.lean" \
  "$EXAMPLE/generated/lean/RustMain.lean" \
  "$EXAMPLE/generated/lean/RustEvidence.lean" \
  "$EXAMPLE/generated/lean/Lean4RustPipeline.lean" \
  "$EXAMPLE/generated/lean/Main.lean" \
  "$EXAMPLE/generated/PIPELINE.md" \
  > /tmp/lean4-rust-ggen-first.sha256

if grep -Enw '(sorry|axiom|admit|partial_fixpoint)' \
  "$EXAMPLE/generated/lean/Fortune5Policy.lean" \
  "$EXAMPLE/generated/lean/RustLib.lean" \
  "$EXAMPLE/generated/lean/RustMain.lean" \
  "$EXAMPLE/generated/lean/RustEvidence.lean" \
  "$EXAMPLE/generated/lean/Lean4RustPipeline.lean" \
  "$EXAMPLE/generated/lean/Main.lean" || \
  grep -En '^[[:space:]]*unsafe([[:space:]]|$)' \
  "$EXAMPLE/generated/lean/Fortune5Policy.lean" \
  "$EXAMPLE/generated/lean/RustLib.lean" \
  "$EXAMPLE/generated/lean/RustMain.lean" \
  "$EXAMPLE/generated/lean/RustEvidence.lean" \
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
  .schema == "ggen.lean4-rust/proof/v2" and
  .standing == "PROVEN" and
  .pipeline == "bounded-successor+fortune5-promotion" and
  .bound == 10 and
  .witness_input == 9 and
  .witness_output == 10 and
  .theorems == ["step_le", "step_witness", "step_fixed_point"] and
  .fortune5.slo_p99_ns == {"r1": 2, "w1": 1000000, "c1": 500000000} and
  .fortune5.r1_tick_budget == 8 and
  .fortune5.measurements == {"r1": "rdtsc", "w1": "otel-span", "c1": "otel-span"} and
  .fortune5.promotion.canary_required == true and
  .fortune5.promotion.staging_validation_required == true and
  .fortune5.promotion.auto_rollback_required == true and
  .fortune5.promotion.promotion_receipt_required == true and
  .fortune5.multi_region.region_count == 3 and
  .fortune5.multi_region.quorum == 2 and
  .fortune5.multi_region.cross_region_replication_required == true and
  .fortune5.multi_region.receipt_synchronization_required == true and
  .fortune5.multi_region.failover_required == true and
  .fortune5.multi_region.legal_hold_required == true and
  .fortune5.security.spiffe_id == "spiffe://ggen.chatmangpt.com/ns/ggen/sa/lean4-rust" and
  .fortune5.security.certificate_refresh_seconds == 3600 and
  .fortune5.security.key_rotation_seconds == 86400 and
  .fortune5.security.mtls_required == true and
  .fortune5.security.kms == ["aws-kms", "azure-key-vault", "hashicorp-vault"] and
  .fortune5.security.network_policy_required == true and
  .fortune5.security.firewall_policy_required == true and
  .fortune5.observability.otel_required == true and
  (.fortune5.theorems | length) == 17
' "$PROOF_RECEIPT"

test -f "$RUST/Cargo.toml"
test -f "$RUST/src/lib.rs"
test -f "$RUST/src/main.rs"
test -f "$RUST/src/bin/verify_receipt.rs"
test -f "$RUST/src/bin/slo_probe.rs"
grep -q '^\[workspace\]$' "$RUST/Cargo.toml"
test "$(grep -c 'unsafe {' "$RUST/src/bin/slo_probe.rs")" -eq 2
test "$(grep -c 'SAFETY: LFENCE/RDTSC' "$RUST/src/bin/slo_probe.rs")" -eq 2
if grep -RIn 'unsafe {' "$RUST/src/lib.rs" "$RUST/src/main.rs" "$RUST/src/bin/verify_receipt.rs"; then
  echo '::error::unsafe Rust escaped the isolated RDTSC probe'
  exit 1
fi

(
  cd "$RUST"
  cargo fmt --all
  cargo fmt --all -- --check
  cargo clippy --all-targets --all-features -- -D warnings
  cargo test --all-targets --all-features -- --nocapture
  cargo run --quiet --bin lean-proof-cell -- canonical 9 > "$EVIDENCE/execution-canonical.json"
  cargo run --quiet --bin lean-proof-cell -- canonical 9 > "$EVIDENCE/execution-canonical-replay.json"
  cargo run --quiet --bin lean-proof-cell -- canary 9 > "$EVIDENCE/execution-canary.json"
  cargo run --quiet --bin lean-proof-cell -- production 9 > "$EVIDENCE/execution-production.json"
  cargo run --quiet --bin lean-proof-cell -- slo-violation 9 > "$EVIDENCE/execution-slo-violation.json"
  cargo run --quiet --bin lean-proof-cell -- quorum-loss 9 > "$EVIDENCE/execution-quorum-loss.json"
  cargo run --quiet --bin lean-proof-cell -- security-expired 9 > "$EVIDENCE/execution-security-expired.json"
  cargo run --quiet --bin lean-proof-cell -- receipt-mismatch 9 > "$EVIDENCE/execution-receipt-mismatch.json"
  cargo run --quiet --bin lean-proof-cell -- failover-unready 9 > "$EVIDENCE/execution-failover-unready.json"
  cargo run --quiet --bin lean-proof-cell -- identity-mismatch 9 > "$EVIDENCE/execution-identity-mismatch.json"
  cargo run --quiet --bin lean-proof-cell -- kms-unready 9 > "$EVIDENCE/execution-kms-unready.json"
  cargo run --quiet --bin lean-proof-cell -- replication-unready 9 > "$EVIDENCE/execution-replication-unready.json"
  cargo run --quiet --bin lean-proof-cell -- promotion-receipt-missing 9 > "$EVIDENCE/execution-promotion-receipt-missing.json"
  cargo run --quiet --bin lean-proof-cell -- alerts-unready 9 > "$EVIDENCE/execution-alerts-unready.json"
  cargo run --release --quiet --bin slo_probe > "$EVIDENCE/slo-probe-r1.json"
)

diff -u "$EVIDENCE/execution-canonical.json" "$EVIDENCE/execution-canonical-replay.json"

jq -e '
  .schema == "ggen.lean4-rust/execution/v2" and
  .standing == "EXECUTED" and
  .scenario == "canonical" and
  .decision == "PROMOTE_PRODUCTION" and
  .stage == "STAGING" and
  .input == 9 and .output == 10 and
  .slo_class == "C1" and .slo_target_ns == 500000000 and
  .observed_p99_ns == 400000000 and .observed_p99_ticks == 0 and .r1_tick_budget == 8 and .slo_compliant == true and
  .region_count == 3 and .region_quorum == 2 and .promotion_receipt_ready == true and .quorum_acks == 3 and
  .cross_region_replication_ready == true and .receipt_synchronized == true and .failover_ready == true and .legal_hold_ready == true and
  .observed_spiffe_id == .spiffe_id and .spiffe_authenticated == true and .certificate_age_seconds <= .certificate_refresh_seconds and
  .key_age_seconds <= .key_rotation_seconds and .mtls_established == true and
  .aws_kms_ready == true and .azure_key_vault_ready == true and .hashicorp_vault_ready == true and
  .network_policy_enforced == true and .firewall_policy_enforced == true and .otel_correlated == true and .alerts_ready == true and
  .kms == ["aws-kms", "azure-key-vault", "hashicorp-vault"] and
  (.lean_proof_blake3 | length) == 64 and (.region_receipt_blake3 | length) == 64
' "$EVIDENCE/execution-canonical.json"

jq -e '.scenario == "canary" and .decision == "PROMOTE_STAGING"' "$EVIDENCE/execution-canary.json"
jq -e '.scenario == "production" and .decision == "HOLD"' "$EVIDENCE/execution-production.json"
jq -e '.scenario == "slo-violation" and .slo_compliant == false and .decision == "ROLLBACK"' "$EVIDENCE/execution-slo-violation.json"
jq -e '.scenario == "quorum-loss" and .quorum_acks == 1 and .decision == "REFUSE"' "$EVIDENCE/execution-quorum-loss.json"
jq -e '.scenario == "security-expired" and .certificate_age_seconds > .certificate_refresh_seconds and .key_age_seconds > .key_rotation_seconds and .decision == "REFUSE"' "$EVIDENCE/execution-security-expired.json"
jq -e '.scenario == "receipt-mismatch" and .receipt_synchronized == false and .decision == "REFUSE"' "$EVIDENCE/execution-receipt-mismatch.json"
jq -e '.scenario == "failover-unready" and .failover_ready == false and .decision == "REFUSE"' "$EVIDENCE/execution-failover-unready.json"
jq -e '.scenario == "identity-mismatch" and .observed_spiffe_id != .spiffe_id and .decision == "REFUSE"' "$EVIDENCE/execution-identity-mismatch.json"
jq -e '.scenario == "kms-unready" and .azure_key_vault_ready == false and .decision == "REFUSE"' "$EVIDENCE/execution-kms-unready.json"
jq -e '.scenario == "replication-unready" and .cross_region_replication_ready == false and .decision == "REFUSE"' "$EVIDENCE/execution-replication-unready.json"
jq -e '.scenario == "promotion-receipt-missing" and .promotion_receipt_ready == false and .decision == "REFUSE"' "$EVIDENCE/execution-promotion-receipt-missing.json"
jq -e '.scenario == "alerts-unready" and .alerts_ready == false and .decision == "REFUSE"' "$EVIDENCE/execution-alerts-unready.json"

(
  cd "$RUST"
  cargo run --quiet --bin verify_receipt -- \
    "$EVIDENCE/execution-canonical.json" "$PROOF_RECEIPT" \
    > "$EVIDENCE/execution-canonical-verification.json"
)
jq -e '.valid == true and (.lean_proof_blake3 | length) == 64 and (.region_receipt_blake3 | length) == 64' \
  "$EVIDENCE/execution-canonical-verification.json"

python3 - "$EVIDENCE/execution-canonical.json" "$EVIDENCE/execution-canonical-tampered.json" <<'PY'
import json
import sys

source, target = sys.argv[1:]
data = json.loads(open(source, encoding="utf-8").read())
data["lean_proof_blake3"] = "0" * 64
open(target, "w", encoding="utf-8").write(json.dumps(data, separators=(",", ":")) + "\n")
PY
if (
  cd "$RUST"
  cargo run --quiet --bin verify_receipt -- \
    "$EVIDENCE/execution-canonical-tampered.json" "$PROOF_RECEIPT"
) > "$EVIDENCE/tampered-receipt-refusal.log" 2>&1; then
  echo '::error::tampered execution receipt unexpectedly verified'
  exit 1
fi
grep -q 'LEAN_PROOF_DIGEST_MISMATCH' "$EVIDENCE/tampered-receipt-refusal.log"

python3 - "$EVIDENCE/execution-canonical.json" "$EVIDENCE/execution-canonical-region-tampered.json" <<'PY'
import json
import sys

source, target = sys.argv[1:]
data = json.loads(open(source, encoding="utf-8").read())
data["region_receipt_blake3"] = "0" * 64
open(target, "w", encoding="utf-8").write(json.dumps(data, separators=(",", ":")) + "\n")
PY
if (
  cd "$RUST"
  cargo run --quiet --bin verify_receipt -- \
    "$EVIDENCE/execution-canonical-region-tampered.json" "$PROOF_RECEIPT"
) > "$EVIDENCE/tampered-region-receipt-refusal.log" 2>&1; then
  echo '::error::tampered regional receipt unexpectedly verified'
  exit 1
fi
grep -q 'REGION_RECEIPT_DIGEST_MISMATCH' "$EVIDENCE/tampered-region-receipt-refusal.log"

python3 - "$EVIDENCE/slo-probe-r1.json" <<'PY'
import json
import sys

data = json.loads(open(sys.argv[1], encoding="utf-8").read())
assert data["schema"] == "ggen.lean4-rust/slo-probe/v2"
assert data["class"] == "R1"
assert data["measurement"] == "rdtsc+steady-clock-batched"
assert data["samples"] >= 1024
assert data["iterations_per_sample"] >= 4096
assert data["p50_ns"] <= data["p95_ns"] <= data["p99_ns"]
assert data["p50_ticks"] <= data["p95_ticks"] <= data["p99_ticks"]
assert data["target_ns"] == 2
assert data["target_ticks"] == 8
assert data["compliant"] == (data["p99_ns"] <= data["target_ns"] and data["p99_ticks"] <= data["target_ticks"])
expected = "PROMOTE_PRODUCTION" if data["compliant"] else "ROLLBACK"
assert data["promotion_decision"] == expected
PY

sha256sum \
  "$PROOF_RECEIPT" \
  "$RUST/Cargo.toml" \
  "$RUST/src/lib.rs" \
  "$RUST/src/main.rs" \
  "$RUST/src/bin/verify_receipt.rs" \
  "$RUST/src/bin/slo_probe.rs" \
  > /tmp/lean4-rust-product-first.sha256

original_ontology=$(mktemp)
cp "$EXAMPLE/ontology.ttl" "$original_ontology"
restore_ontology() {
  cp "$original_ontology" "$EXAMPLE/ontology.ttl"
}
trap restore_ontology EXIT

assert_graph_refusal() {
  local label=$1
  local old=$2
  local new=$3
  restore_ontology
  python3 - "$EXAMPLE/ontology.ttl" "$old" "$new" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
old, new = sys.argv[2:]
text = path.read_text(encoding="utf-8")
updated = text.replace(old, new, 1)
if updated == text:
    raise SystemExit(f"sabotage did not apply: {old}")
path.write_text(updated, encoding="utf-8")
PY
  if (
    cd "$EXAMPLE"
    "$GGEN" sync run
  ) > "$EVIDENCE/${label}-refusal.log" 2>&1; then
    echo "::error::$label sabotage unexpectedly passed graph admission"
    exit 1
  fi
}

assert_graph_refusal unproved-theorem 'lr:proofTheorem "step_le"' 'lr:proofTheorem "unproved_step"'
assert_graph_refusal slo-target 'lr:c1P99Nanos 500000000' 'lr:c1P99Nanos 500000001'
assert_graph_refusal r1-ticks 'lr:r1TickBudget 8' 'lr:r1TickBudget 9'
assert_graph_refusal quorum 'lr:regionQuorum 2' 'lr:regionQuorum 1'
assert_graph_refusal mtls 'lr:mtlsRequired true' 'lr:mtlsRequired false'
assert_graph_refusal observability 'lr:otelRequired true' 'lr:otelRequired false'

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
  "$EXAMPLE/generated/lean/Fortune5Policy.lean" \
  "$EXAMPLE/generated/lean/RustLib.lean" \
  "$EXAMPLE/generated/lean/RustMain.lean" \
  "$EXAMPLE/generated/lean/RustEvidence.lean" \
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
  cd "$RUST"
  cargo fmt --all
)
sha256sum \
  "$PROOF_RECEIPT" \
  "$RUST/Cargo.toml" \
  "$RUST/src/lib.rs" \
  "$RUST/src/main.rs" \
  "$RUST/src/bin/verify_receipt.rs" \
  "$RUST/src/bin/slo_probe.rs" \
  > /tmp/lean4-rust-product-second.sha256
diff -u /tmp/lean4-rust-product-first.sha256 /tmp/lean4-rust-product-second.sha256

printf '%s\n' 'ggen-first-lean4-rust-fortune5-pipeline: ALIVE'
