#!/bin/bash
# Release Version Consistency Validation
# FMEA Fix: Prevents version inconsistencies across crates (RPN 504)

set -euo pipefail

echo "🔍 Validating version consistency for release..."

# Get version from root Cargo.toml
root_version=$(grep '^version = ' Cargo.toml | head -1 | sed 's/.*version = "\(.*\)"/\1/' | tr -d '"')
if [ -z "$root_version" ]; then
    echo "❌ ERROR: Could not determine version from Cargo.toml"
    exit 1
fi

echo "Root version: $root_version"

# Check VERSION file
if [ -f "VERSION" ]; then
    version_file=$(cat VERSION | tr -d '[:space:]')
    if [ "$version_file" != "$root_version" ]; then
        echo "❌ ERROR: VERSION file ($version_file) does not match Cargo.toml ($root_version)"
        exit 1
    fi
    echo "✅ VERSION file matches: $version_file"
else
    echo "⚠️  WARNING: VERSION file not found"
fi

# Check all Cargo.toml files in workspace
errors=0
for cargo_toml in $(find . -name "Cargo.toml" -not -path "./target/*" -not -path "./node_modules/*" -not -path "./.git/*" -not -path "./examples/*" -not -path "./marketplace/packages/*"); do
    # Skip if it's a workspace member (check if it's in workspace members list)
    if grep -q "members = \[" Cargo.toml && grep -q "$(dirname $cargo_toml | sed 's|^\./||')" Cargo.toml; then
        crate_version=$(grep '^version = ' "$cargo_toml" | head -1 | sed 's/.*version = "\(.*\)"/\1/' | tr -d '"' || echo "")
        if [ -n "$crate_version" ] && [ "$crate_version" != "$root_version" ]; then
            echo "❌ ERROR: Version mismatch in $cargo_toml: $crate_version (expected $root_version)"
            errors=$((errors + 1))
        fi
    fi
done

# Check workspace crates specifically
workspace_crates=(
    "crates/ggen-utils"
    "crates/ggen-cli"
    "crates/ggen-domain"
    "crates/ggen-core"
    "crates/ggen-ai"
    "crates/ggen-marketplace"
    "crates/ggen-node"
)

for crate in "${workspace_crates[@]}"; do
    if [ -f "$crate/Cargo.toml" ]; then
        crate_version=$(grep '^version = ' "$crate/Cargo.toml" | head -1 | sed 's/.*version = "\(.*\)"/\1/' | tr -d '"' || echo "")
        if [ -z "$crate_version" ]; then
            echo "⚠️  WARNING: Could not determine version in $crate/Cargo.toml"
        elif [ "$crate_version" != "$root_version" ]; then
            echo "❌ ERROR: Version mismatch in $crate/Cargo.toml: $crate_version (expected $root_version)"
            errors=$((errors + 1))
        else
            echo "✅ $crate: $crate_version"
        fi
    fi
done

if [ $errors -gt 0 ]; then
    echo ""
    echo "❌ ERROR: Found $errors version inconsistency(ies)"
    exit 1
fi

echo "✅ All versions are consistent: $root_version"
exit 0


