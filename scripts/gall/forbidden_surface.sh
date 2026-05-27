#!/usr/bin/env bash
# Compliance script to ensure no forbidden surfaces exist in ggen-graph.
set -euo pipefail

TARGET_DIR="crates/ggen-graph/src"
VIOLATIONS=0

FORBIDDEN_PATTERNS=(
    "std::process::Command"
    "Command::new"
    "std::net"
    "tokio::net"
    "reqwest"
    "hyper"
)

echo "=== Running Forbidden Surface Check ==="
if [ ! -d "$TARGET_DIR" ]; then
    echo "Target directory $TARGET_DIR does not exist."
    exit 1
fi

for pattern in "${FORBIDDEN_PATTERNS[@]}"; do
    # Search recursively for pattern in target directory, excluding src/bin
    if grep -rnF --exclude-dir="bin" "$pattern" "$TARGET_DIR" > /dev/null; then
        echo "FAIL: Forbidden surface pattern '$pattern' found in $TARGET_DIR (excluding bin/):"
        grep -rnF --exclude-dir="bin" "$pattern" "$TARGET_DIR"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
done

if [ "$VIOLATIONS" -eq 0 ]; then
    echo "PASS: Zero forbidden surfaces detected."
    exit 0
else
    echo "FAIL: $VIOLATIONS forbidden surface violations found."
    exit 1
fi
