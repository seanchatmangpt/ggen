#!/bin/bash
# Release Breaking Changes Detection
# FMEA Fix: Detects breaking changes that require documentation (RPN 240)

set -euo pipefail

echo "🔍 Checking for breaking changes..."

# Get version from root Cargo.toml
root_version=$(grep '^version = ' Cargo.toml | head -1 | sed 's/.*version = "\(.*\)"/\1/' | tr -d '"')
if [ -z "$root_version" ]; then
    echo "❌ ERROR: Could not determine version from Cargo.toml"
    exit 1
fi

# Extract major version
major_version=$(echo "$root_version" | cut -d. -f1)

# Check if this is a major version bump (would indicate breaking changes)
prev_version=$(git describe --tags --abbrev=0 2>/dev/null || echo "")
if [ -n "$prev_version" ]; then
    prev_major=$(echo "$prev_version" | sed 's/v//' | cut -d. -f1)
    if [ "$major_version" -gt "$prev_major" ]; then
        echo "⚠️  WARNING: Major version bump detected ($prev_version -> $root_version)"
        echo "   This indicates breaking changes - ensure they are documented in CHANGELOG.md"
    fi
fi

# Check for deprecation warnings in code
deprecations=$(grep -r "#\[deprecated" crates/ --include="*.rs" 2>/dev/null | wc -l || echo "0")
if [ "$deprecations" -gt 0 ]; then
    echo "⚠️  WARNING: Found $deprecations deprecation(s) - ensure they are documented"
fi

# Check CHANGELOG for breaking changes section
if grep -q "### Breaking" CHANGELOG.md || grep -q "### Breaking Changes" CHANGELOG.md; then
    echo "✅ Breaking changes documented in CHANGELOG.md"
else
    echo "ℹ️  INFO: No breaking changes section found (OK if no breaking changes)"
fi

echo "✅ Breaking changes check complete"
exit 0


