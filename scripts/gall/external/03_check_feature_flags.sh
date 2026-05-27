#!/usr/bin/env bash
# ==============================================================================
# 03_check_feature_flags.sh
# Verifies that no feature flags are enabled in ggen-graph.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [03] Verifying Feature Flags ==="
CARGO_TOML="crates/ggen-graph/Cargo.toml"
if [ ! -f "$CARGO_TOML" ]; then
    echo "ERROR: $CARGO_TOML does not exist." >&2
    exit 1
fi
if grep -q "^\[features\]" "$CARGO_TOML"; then
    echo "FAIL: [features] section found in $CARGO_TOML" >&2
    exit 1
fi
echo "PASS: No feature flags defined in $CARGO_TOML"
exit 0
