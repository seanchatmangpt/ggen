#!/usr/bin/env bash
# ==============================================================================
# 02_verify_package_constraints.sh
# Verifies that ggen-graph is a standalone crate with no workspace dependencies.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [02] Verifying Package Constraints ==="
CARGO_TOML="crates/ggen-graph/Cargo.toml"
if [ ! -f "$CARGO_TOML" ]; then
    echo "ERROR: $CARGO_TOML does not exist." >&2
    exit 1
fi
# Ensure no local/sibling ggen crates are listed in dependencies
# Excluding dependencies from standard registry like oxigraph, blake3, etc.
SIBLING_DEPS=$(grep -E '^\s*ggen-[a-z0-9\-]+\s*=' "$CARGO_TOML" || true)
if [ -n "$SIBLING_DEPS" ]; then
    echo "FAIL: ggen-graph depends on other workspace crates: $SIBLING_DEPS" >&2
    exit 1
fi
echo "PASS: ggen-graph is standalone and has no sibling ggen dependency constraints."
exit 0
