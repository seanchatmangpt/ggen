#!/usr/bin/env bash
# ==============================================================================
# 24_run_clean_room_rebuild.sh
# Path: scripts/gall/external/24_run_clean_room_rebuild.sh
# ==============================================================================
set -euo pipefail

if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
TEMP_DIR=$(mktemp -d)

cleanup() {
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT INT TERM

# Sync workspace files into the clean room (avoid target build dir & git files)
rsync -a --exclude="target" --exclude=".git" --exclude=".agents" --exclude=".gemini" --exclude=".antigravitycli" "$WORKSPACE_ROOT/" "$TEMP_DIR/"

start_time=$(date +%s)
set +e
(
    cd "$TEMP_DIR"
    export CARGO_TARGET_DIR="$WORKSPACE_ROOT/target/clean_room_cache"
    cargo build --offline -p ggen-graph --all-targets >/dev/null 2>&1 && \
    cargo test --offline -p ggen-graph >/dev/null 2>&1
)
STATUS_CODE=$?
set -e
end_time=$(date +%s)
duration=$((end_time - start_time))

BUILD_STATUS="PASS"
if [ "$STATUS_CODE" -ne 0 ]; then
    BUILD_STATUS="FAIL"
fi

TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
OUTPUT_JSON="crates/ggen-graph/audit/clean_room_rebuild.json"

cat <<EOF > "$WORKSPACE_ROOT/$OUTPUT_JSON"
{
  "timestamp": "$TIMESTAMP",
  "clean_room_directory": "$TEMP_DIR",
  "rebuild_status": "$BUILD_STATUS",
  "exit_code": $STATUS_CODE,
  "duration_seconds": $duration
}
EOF

if [ "$STATUS_CODE" -eq 0 ]; then
    echo "W4: Clean-room build verification passed."
    exit 0
else
    echo "W4: Clean-room build verification failed with exit code $STATUS_CODE."
    exit 1
fi
