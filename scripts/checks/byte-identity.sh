#!/usr/bin/env bash
# ver:req-byte-identity's checkCommand hook (packs/ggen-verify-pack's
# consumer-specific delegation point -- see that pack's ontology.ttl
# comment on ver:req-byte-identity).
#
# ggen's own repo has no external "reference" tree to diff generated files
# against (unlike examples/tcps-generated, which sweeps generated files
# against a vendored reference project). The real, ggen-native analog of
# byte-identity for a self-hosting consumer is RE-SYNC IDEMPOTENCY: running
# `ggen sync run` twice from the same graph state must produce byte-identical
# outputs the second time (no drift, no nondeterminism in template
# rendering). This script builds the real `ggen` binary (if not already
# built at the pinned target dir) and asserts idempotency by hashing every
# generated output listed in the most recent .ggen-v2/receipt.json before
# and after a second real sync.
#
# Exits 0 iff every hash matches. Exits nonzero (real, not swallowed) on any
# mismatch or on a missing receipt (nothing to compare against yet).
set -uo pipefail

cd "$(dirname "$0")/../.."

RECEIPT=".ggen-v2/receipt.json"
if [ ! -f "$RECEIPT" ]; then
  echo "byte-identity.sh: no $RECEIPT yet -- run ggen sync run once before this check" >&2
  exit 3
fi

TARGET_DIR="${GGEN_BYTE_IDENTITY_TARGET_DIR:-/tmp/tgt-selfretro}"
BIN="$TARGET_DIR/debug/ggen"
if [ ! -x "$BIN" ]; then
  echo "byte-identity.sh: building ggen at $TARGET_DIR" >&2
  cargo build -p ggen-cli-lib --bin ggen --target-dir "$TARGET_DIR" >/tmp/byte-identity-build.log 2>&1 || {
    echo "byte-identity.sh: build failed, see /tmp/byte-identity-build.log" >&2
    exit 4
  }
fi

# Snapshot output hashes from the receipt (root-relative path -> blake3 hex
# already computed by ggen-engine at write time).
OUTPUTS_BEFORE="$(python3 -c '
import json
d = json.load(open("'"$RECEIPT"'"))
for k, v in sorted(d["payload"]["outputs"].items()):
    print(f"{k}\t{v}")
')"

# Real second sync.
"$BIN" sync run >/tmp/byte-identity-resync.log 2>&1
RC=$?
if [ "$RC" -ne 0 ]; then
  echo "byte-identity.sh: second sync run exited $RC, see /tmp/byte-identity-resync.log" >&2
  exit "$RC"
fi

OUTPUTS_AFTER="$(python3 -c '
import json
d = json.load(open("'"$RECEIPT"'"))
for k, v in sorted(d["payload"]["outputs"].items()):
    print(f"{k}\t{v}")
')"

if [ "$OUTPUTS_BEFORE" = "$OUTPUTS_AFTER" ]; then
  echo "byte-identity.sh: idempotent -- $(echo "$OUTPUTS_AFTER" | wc -l | tr -d ' ') outputs, all hashes unchanged across re-sync"
  exit 0
else
  echo "byte-identity.sh: DRIFT -- outputs changed across re-sync (non-idempotent generation)" >&2
  diff <(echo "$OUTPUTS_BEFORE") <(echo "$OUTPUTS_AFTER") >&2 || true
  exit 1
fi
