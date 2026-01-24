#!/usr/bin/env bash
# verify-cargo-lock.sh - Verify Cargo.lock integrity with SHA-256 checksum
#
# Week 8: Supply chain security - Lock file verification
# Ensures Cargo.lock hasn't been tampered with and dependencies are pinned correctly

set -euo pipefail

# Configuration
LOCK_FILE="${1:-Cargo.lock}"
CHECKSUM_FILE=".cargo-lock-checksum"
REQUIRE_CHECKSUM_FILE="${2:-false}"

echo "🔒 Cargo.lock Verification"
echo "=========================="
echo ""

# Check if Cargo.lock exists
if [ ! -f "$LOCK_FILE" ]; then
    echo "::error::Cargo.lock not found at $LOCK_FILE"
    exit 1
fi

# Calculate current checksum
CURRENT_CHECKSUM=$(sha256sum "$LOCK_FILE" | awk '{print $1}')
echo "Current SHA-256: $CURRENT_CHECKSUM"

# Verify Cargo.lock is up to date with Cargo.toml
echo ""
echo "Verifying Cargo.lock is up to date..."
if cargo update --workspace --locked --dry-run 2>&1 | grep -q "no package updates"; then
    echo "✅ Cargo.lock is up to date with Cargo.toml"
else
    echo "::error::Cargo.lock is out of sync with Cargo.toml"
    echo "::error::Run 'cargo update' to synchronize"
    exit 1
fi

# Check for uncommitted changes
echo ""
echo "Checking for uncommitted changes..."
if git diff --exit-code "$LOCK_FILE" > /dev/null 2>&1; then
    echo "✅ No uncommitted changes in Cargo.lock"
else
    echo "::warning::Cargo.lock has uncommitted changes"
fi

# Verify stored checksum (if checksum file exists)
if [ -f "$CHECKSUM_FILE" ]; then
    STORED_CHECKSUM=$(cat "$CHECKSUM_FILE")
    echo ""
    echo "Stored SHA-256:  $STORED_CHECKSUM"

    if [ "$CURRENT_CHECKSUM" == "$STORED_CHECKSUM" ]; then
        echo "✅ Checksum verification passed"
    else
        echo "::error::Checksum mismatch! Cargo.lock may have been tampered with"
        echo "::error::Expected: $STORED_CHECKSUM"
        echo "::error::Got:      $CURRENT_CHECKSUM"
        exit 1
    fi
elif [ "$REQUIRE_CHECKSUM_FILE" == "true" ]; then
    echo "::error::Checksum file not found: $CHECKSUM_FILE"
    echo "::error::Run './scripts/verify-cargo-lock.sh --update-checksum' to create it"
    exit 1
else
    echo ""
    echo "ℹ️  No stored checksum found (optional)"
fi

# Verify dependency checksums
echo ""
echo "Verifying dependency checksums..."
if cargo fetch --locked > /dev/null 2>&1; then
    echo "✅ All dependency checksums verified"
else
    echo "::error::Dependency checksum verification failed"
    echo "::error::This indicates potential tampering with downloaded crates"
    exit 1
fi

# Check for duplicate dependencies (waste detection)
echo ""
echo "Checking for duplicate dependencies..."
DUPLICATES=$(cargo tree --duplicates --depth 1 2>/dev/null || echo "")
if [ -z "$DUPLICATES" ]; then
    echo "✅ No duplicate dependencies found"
else
    echo "⚠️  Duplicate dependencies detected:"
    echo "$DUPLICATES"
    echo ""
    echo "::warning::Consider consolidating dependency versions to reduce waste"
fi

# Generate detailed report
echo ""
echo "📊 Dependency Statistics"
echo "----------------------"

TOTAL_DEPS=$(cargo metadata --format-version 1 --locked | jq '.packages | length')
DIRECT_DEPS=$(cargo metadata --format-version 1 --locked --no-deps | jq '.packages | length')
TRANSITIVE_DEPS=$((TOTAL_DEPS - DIRECT_DEPS))

echo "Total dependencies:      $TOTAL_DEPS"
echo "Direct dependencies:     $DIRECT_DEPS"
echo "Transitive dependencies: $TRANSITIVE_DEPS"

echo ""
echo "✅ Cargo.lock verification complete"
