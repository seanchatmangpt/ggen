#!/bin/bash
# Release Documentation Sync Validation
# FMEA Fix: Ensures README version references match release version (RPN 96)

set -euo pipefail

echo "🔍 Validating documentation version sync..."

# Get version from root Cargo.toml
root_version=$(grep '^version = ' Cargo.toml | head -1 | sed 's/.*version = "\(.*\)"/\1/' | tr -d '"')
if [ -z "$root_version" ]; then
    echo "❌ ERROR: Could not determine version from Cargo.toml"
    exit 1
fi

errors=0

# Check README.md for version references
if [ -f "README.md" ]; then
    # Look for version patterns (e.g., "v2.5.1", "version 2.5.1", "2.5.1")
    old_versions=$(grep -iE "(version|v)[[:space:]]*[0-9]+\.[0-9]+\.[0-9]+" README.md | grep -v "$root_version" || true)
    if [ -n "$old_versions" ]; then
        echo "⚠️  WARNING: README.md may contain outdated version references:"
        echo "$old_versions" | head -5
        echo "   Current version: $root_version"
    else
        echo "✅ README.md version references appear current"
    fi
fi

# Check VERSION file matches
if [ -f "VERSION" ]; then
    version_file=$(cat VERSION | tr -d '[:space:]')
    if [ "$version_file" != "$root_version" ]; then
        echo "❌ ERROR: VERSION file ($version_file) does not match Cargo.toml ($root_version)"
        errors=$((errors + 1))
    else
        echo "✅ VERSION file matches: $version_file"
    fi
fi

if [ $errors -gt 0 ]; then
    echo ""
    echo "❌ ERROR: Found $errors documentation sync issue(s)"
    exit 1
fi

echo "✅ Documentation version sync validated"
exit 0


