#!/usr/bin/env bash
# guard-pack-proofs.sh — the pack-proof gate (L5 push, track A3).
#
# Makes "the generated proof suites pass" a checkable fact from repo state,
# not a claim about a session that once ran them: for each committed pack
# consumer below, re-syncs it, verifies the re-sync was idempotent
# (byte-identical generated output), and runs its full test suite (the
# generated proofs plus its own).
#
# Consumers covered (same checks, same failure semantics, for each):
#   - examples/receiptctl         (wires 6 packs)
#   - examples/praxis-core-verify (wires packs/praxis-core-pack,
#     praxis-core-live feature ON by default). This gate runs its
#     default-features `cargo test -q`, the same plain invocation used for
#     every consumer; that consumer also has a --no-default-features path,
#     which this gate does not separately re-run.
#
# Uses the release ggen binary if present (fast path), else builds the debug
# one. Any sync refusal (including the pack-shapes gate FM-PACK-013), any
# test failure, or any regeneration diff fails this script.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# Consumer directories to guard. Add new committed pack consumers here
# rather than duplicating this script.
CONSUMERS=(
    "examples/receiptctl"
    "examples/praxis-core-verify"
)

GGEN_BIN="target/release/ggen"
if [[ ! -x "$GGEN_BIN" ]]; then
    GGEN_BIN="target/debug/ggen"
fi
if [[ ! -x "$GGEN_BIN" ]]; then
    echo "guard-pack-proofs: building ggen binary (debug)..."
    cargo build -q -p ggen-cli-lib --bin ggen
    GGEN_BIN="target/debug/ggen"
fi
GGEN_BIN="$(pwd)/$GGEN_BIN"

for CONSUMER in "${CONSUMERS[@]}"; do
    echo "guard-pack-proofs: === ${CONSUMER} ==="

    echo "guard-pack-proofs: sync ${CONSUMER} (binary: ${GGEN_BIN})"
    (cd "$CONSUMER" && "$GGEN_BIN" sync run >/dev/null)

    echo "guard-pack-proofs: verifying idempotent regeneration for ${CONSUMER}"
    snapshot="$(mktemp -d)"
    trap 'rm -rf "$snapshot"' EXIT
    cp -R "$CONSUMER/src" "$snapshot/src"
    cp -R "$CONSUMER/tests" "$snapshot/tests"
    cp -R "$CONSUMER/docs" "$snapshot/docs"
    (cd "$CONSUMER" && "$GGEN_BIN" sync run >/dev/null)
    diff -rq "$snapshot/src" "$CONSUMER/src"
    diff -rq "$snapshot/tests" "$CONSUMER/tests"
    diff -rq "$snapshot/docs" "$CONSUMER/docs"
    rm -rf "$snapshot"
    trap - EXIT

    echo "guard-pack-proofs: running ${CONSUMER} test suite"
    (cd "$CONSUMER" && cargo test -q)

    echo "guard-pack-proofs: ${CONSUMER} OK (sync clean, idempotent, all generated proofs pass)"
done

echo "guard-pack-proofs: OK (${#CONSUMERS[@]} consumers: sync clean, idempotent, all generated proofs pass)"
