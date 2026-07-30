#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
CONSUMER="$ROOT/examples/combinatorial-maximalism"
GGEN="$ROOT/target/debug/ggen"
MANIFEST="$CONSUMER/generated/cmd-cell/Cargo.toml"
export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-/tmp/ggen-cmd-target}"

generated_digest_manifest() {
  sha256sum \
    "$CONSUMER/generated/cmd-cell/Cargo.toml" \
    "$CONSUMER/generated/cmd-cell/src/lib.rs" \
    "$CONSUMER/generated/cmd-cell/src/main.rs" \
    "$CONSUMER/generated/cmd-cell/tests/cmd_e2e.rs" \
    "$CONSUMER/generated/cmd-plan.json" \
    "$CONSUMER/generated/CMD_REPORT.md"
}

python3 "$ROOT/scripts/validate_combinatorial_maximalism_pack.py"

mapfile -t authored < <(find "$CONSUMER" -maxdepth 1 -type f -printf '%f\n' | sort)
printf '%s\n' "${authored[@]}"
test "${#authored[@]}" -eq 2
test "${authored[0]}" = "ggen.toml"
test "${authored[1]}" = "ontology.ttl"
test ! -e "$CONSUMER/generated"

cargo build -p ggen-cli-lib --bin ggen
(
  cd "$CONSUMER"
  "$GGEN" sync run
  "$GGEN" receipt verify
)

test -f "$CONSUMER/generated/cmd-cell/src/lib.rs"
test -f "$CONSUMER/generated/cmd-cell/src/main.rs"
test -f "$CONSUMER/generated/cmd-cell/tests/cmd_e2e.rs"
test -f "$CONSUMER/generated/cmd-plan.json"
test -f "$CONSUMER/generated/CMD_REPORT.md"

generated_digest_manifest > /tmp/cmd-first-sync.sha256

cargo fmt --manifest-path "$MANIFEST" --all -- --check
cargo clippy --manifest-path "$MANIFEST" --all-targets --all-features -- -D warnings
cargo test --manifest-path "$MANIFEST" --all-targets --all-features -- --nocapture

TARGET_DIR=$(cargo metadata --manifest-path "$MANIFEST" --format-version 1 --no-deps \
  | python3 -c 'import json,sys; print(json.load(sys.stdin)["target_directory"])')
BINARY="$TARGET_DIR/debug/cmdctl"
cargo build --manifest-path "$MANIFEST"

WORKSPACE=$(mktemp -d)
cleanup() {
  rm -rf "$WORKSPACE"
  rm -rf "$ROOT/examples/.cmd-negative-"*
}
trap cleanup EXIT

CMD_WORKSPACE="$WORKSPACE" "$BINARY" verify | python3 -c 'import json,sys; assert json.load(sys.stdin)["status"] == "ALIVE"'
ACTUATION=$(CMD_WORKSPACE="$WORKSPACE" "$BINARY" actuate native-json '{"source":"workflow","law":"A=mu(O*)"}')
RECEIPT=$(printf '%s' "$ACTUATION" | python3 -c 'import json,sys; print(json.load(sys.stdin)["receipt"])')
test -f "$RECEIPT"
CMD_WORKSPACE="$WORKSPACE" "$BINARY" replay "$RECEIPT" \
  | python3 -c 'import json,sys; assert json.load(sys.stdin)["status"] == "REPLAY_VERIFIED"'

mkdir -p "$CONSUMER/generated/evidence"
cp -R "$(dirname "$RECEIPT")" "$CONSUMER/generated/evidence/transaction"
cp "$CONSUMER/.ggen-v2/receipt.json" "$CONSUMER/generated/evidence/ggen-sync-receipt.json"

before=$(find "$WORKSPACE/.cmd/runtime/transactions" -mindepth 1 -maxdepth 1 -type d | wc -l)
if CMD_WORKSPACE="$WORKSPACE" "$BINARY" actuate wasm-turtle '{}' > /tmp/cmd-refused.out 2> /tmp/cmd-refused.err; then
  echo "::error::unverified candidate unexpectedly actuated"
  exit 1
fi
grep -q 'CANDIDATE_NOT_VERIFIED' /tmp/cmd-refused.err
after=$(find "$WORKSPACE/.cmd/runtime/transactions" -mindepth 1 -maxdepth 1 -type d | wc -l)
test "$before" -eq "$after"

OUTPUT=$(python3 - "$RECEIPT" <<'PY'
import json, pathlib, sys
receipt = pathlib.Path(sys.argv[1])
envelope = json.loads(receipt.read_text())
print(receipt.parent / envelope["receipt"]["output_relative_path"])
PY
)
printf 'tampered\n' > "$OUTPUT"
if CMD_WORKSPACE="$WORKSPACE" "$BINARY" replay "$RECEIPT" > /tmp/cmd-tamper.out 2> /tmp/cmd-tamper.err; then
  echo "::error::tampered output unexpectedly replayed"
  exit 1
fi
grep -q 'OUTPUT_DIGEST_MISMATCH' /tmp/cmd-tamper.err

run_gate_refusal() {
  local name=$1
  local expected=$2
  local mutation=$3
  local directory="$ROOT/examples/.cmd-negative-$name"
  mkdir -p "$directory"
  cp "$CONSUMER/ggen.toml" "$directory/ggen.toml"
  python3 - "$CONSUMER/ontology.ttl" "$directory/ontology.ttl" "$mutation" <<'PY'
from pathlib import Path
import sys
source, target, mutation = Path(sys.argv[1]), Path(sys.argv[2]), sys.argv[3]
text = source.read_text()
if mutation == "unreceipted":
    text = text.replace("cmd:receiptRequired true", "cmd:receiptRequired false", 1)
elif mutation == "premature-authority":
    text = text.replace('cmd:standing "VERIFIED"', 'cmd:standing "CANDIDATE"', 1)
elif mutation == "candidate-count":
    text = text.replace("cmd:expectedCandidateCount 4", "cmd:expectedCandidateCount 3", 1)
elif mutation == "hook-actuation":
    text += "\n<urn:cmd:hook:design-selection> cmd:directlyActuates <urn:cmd:actuation:native-json> .\n"
else:
    raise SystemExit(f"unknown mutation: {mutation}")
target.write_text(text)
PY
  if (cd "$directory" && "$GGEN" sync run > /tmp/cmd-gate.out 2> /tmp/cmd-gate.err); then
    echo "::error::$name mutation unexpectedly passed"
    exit 1
  fi
  cat /tmp/cmd-gate.out /tmp/cmd-gate.err | grep -q "$expected"
  rm -rf "$directory"
}

run_gate_refusal unreceipted actuation-must-require-receipt unreceipted
run_gate_refusal premature-authority candidate-prematurely-authorized premature-authority
run_gate_refusal candidate-count candidate-count-does-not-match-admitted-expectation candidate-count
run_gate_refusal hook-actuation hook-direct-actuation-forbidden hook-actuation

(
  cd "$CONSUMER"
  "$GGEN" sync run
  "$GGEN" receipt verify
)
generated_digest_manifest > /tmp/cmd-second-sync.sha256
diff -u /tmp/cmd-first-sync.sha256 /tmp/cmd-second-sync.sha256

if rg -n 'TODO|FIXME|todo!|unimplemented!|mockall|#\[automock\]' \
  "$CONSUMER/generated/cmd-cell" "$CONSUMER/generated/cmd-plan.json" "$CONSUMER/generated/CMD_REPORT.md"; then
  echo "::error::manufactured product contains a forbidden surface"
  exit 1
fi

printf '%s\n' 'combinatorial-maximalism-pack: ALIVE'
