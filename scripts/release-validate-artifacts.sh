#!/bin/bash
# Release Artifacts Validation
# FMEA Fix: Ensures all required release artifacts exist (RPN 180)

set -euo pipefail

echo "🔍 Validating release artifacts..."

# Get version from root Cargo.toml
root_version=$(grep '^version = ' Cargo.toml | head -1 | sed 's/.*version = "\(.*\)"/\1/' | tr -d '"')
if [ -z "$root_version" ]; then
    echo "❌ ERROR: Could not determine version from Cargo.toml"
    exit 1
fi

errors=0

# Check CHANGELOG.md has entry for this version
if [ ! -f "CHANGELOG.md" ]; then
    echo "❌ ERROR: CHANGELOG.md not found"
    errors=$((errors + 1))
elif ! grep -q "^## \[$root_version\]" CHANGELOG.md && ! grep -q "^## \[$root_version\]" CHANGELOG.md; then
    echo "❌ ERROR: CHANGELOG.md missing entry for version $root_version"
    echo "   Expected: ## [$root_version] - YYYY-MM-DD"
    errors=$((errors + 1))
else
    echo "✅ CHANGELOG.md has entry for $root_version"
fi

# Check release notes exist (optional but recommended)
if [ -f "docs/V${root_version}_RELEASE_NOTES.md" ] || [ -f "docs/V${root_version//./_}_RELEASE_NOTES.md" ]; then
    echo "✅ Release notes found"
else
    echo "⚠️  WARNING: Release notes not found (recommended but not required)"
fi

# Check release checklist exists
if [ -f "RELEASE_v${root_version}_CHECKLIST.md" ]; then
    echo "✅ Release checklist found"
else
    echo "⚠️  WARNING: Release checklist not found (recommended but not required)"
fi

# Check binary exists (if release build was run)
if [ -f "target/release/ggen" ]; then
    echo "✅ Release binary found"
elif [ -f "target/debug/ggen" ]; then
    echo "⚠️  WARNING: Only debug binary found (release build not run)"
else
    echo "⚠️  WARNING: No binary found (run 'cargo make build-release' first)"
fi

if [ $errors -gt 0 ]; then
    echo ""
    echo "❌ ERROR: Found $errors missing artifact(s)"
    exit 1
fi

echo "✅ All required release artifacts are present"
exit 0


