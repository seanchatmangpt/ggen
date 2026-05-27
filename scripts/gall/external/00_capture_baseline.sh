#!/usr/bin/env bash
# ==============================================================================
# 00_capture_baseline.sh
# Records workspace state before verification.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [00] Capturing Workspace Baseline ==="
if command -v git &>/dev/null && [ -d .git ]; then
    echo "Git HEAD: $(git rev-parse HEAD || echo 'Not committed yet')"
    git status --short
else
    echo "Not a git repository, listing project directory structure:"
    find crates/ggen-graph/src -type f
fi
echo "Baseline captured successfully."
exit 0
