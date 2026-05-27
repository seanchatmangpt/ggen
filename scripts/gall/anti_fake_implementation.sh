#!/usr/bin/env bash
# Compliance script to ensure no mocks, stubs, or placeholders exist in ggen-graph.
set -euo pipefail

TARGET_SRC="crates/ggen-graph/src"
TARGET_TESTS="crates/ggen-graph/tests"
VIOLATIONS=0

FORBIDDEN_PATTERNS=(
    "mockall"
    "mock!"
    "#[automock]"
    "TODO"
    "FIXME"
    "unimplemented!"
    "hash_placeholder"
    "uuid_placeholder"
    "fake_signature"
)

echo "=== Running Anti-Fake Implementation Check ==="

# 1. Scan src
if [ -d "$TARGET_SRC" ]; then
    for pattern in "${FORBIDDEN_PATTERNS[@]}"; do
        if grep -rnF "$pattern" "$TARGET_SRC" > /dev/null; then
            echo "FAIL: Forbidden fake/stub pattern '$pattern' found in source:"
            grep -rnF "$pattern" "$TARGET_SRC"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    done
fi

# 2. Scan tests (excluding anti_fake_implementation.rs)
if [ -d "$TARGET_TESTS" ]; then
    for pattern in "${FORBIDDEN_PATTERNS[@]}"; do
        # Search while filtering out anti_fake_implementation.rs matches
        MATCHES=$(grep -rnF "$pattern" "$TARGET_TESTS" | grep -v "anti_fake_implementation.rs" || true)
        if [ -n "$MATCHES" ]; then
            echo "FAIL: Forbidden fake/stub pattern '$pattern' found in tests:"
            echo "$MATCHES"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    done
fi

if [ "$VIOLATIONS" -eq 0 ]; then
    echo "PASS: Zero fake implementation markers detected."
    exit 0
else
    echo "FAIL: $VIOLATIONS fake implementation violations found."
    exit 1
fi
