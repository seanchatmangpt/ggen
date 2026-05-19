#!/bin/bash
# Release Version Consistency Validation
# FMEA Fix: Prevents version inconsistencies across crates (RPN 504)

set -euo pipefail

echo "🔍 Validating version consistency for release..."

# Get version from root Cargo.toml [workspace.package] section
root_version=$(awk '
    /^\[/ { in_wpkg = ($0 == "[workspace.package]") }
    in_wpkg && /^version = / { gsub(/version = "|"/, ""); print; exit }
' Cargo.toml)

if [ -z "$root_version" ]; then
    # Fallback: try [package] section (for legacy non-workspace projects)
    root_version=$(awk '
        /^\[/ { in_pkg = ($0 == "[package]") }
        in_pkg && /^version = / { gsub(/version = "|"/, ""); print; exit }
    ' Cargo.toml)
fi

if [ -z "$root_version" ]; then
    echo "❌ ERROR: Could not determine version from Cargo.toml"
    exit 1
fi

echo "Root version: $root_version"

# Check VERSION file (optional; root Cargo.toml is the source of truth)
if [ -f "VERSION" ]; then
    version_file=$(cat VERSION | tr -d '[:space:]')
    if [ "$version_file" != "$root_version" ]; then
        echo "❌ ERROR: VERSION file ($version_file) does not match Cargo.toml ($root_version)"
        exit 1
    fi
    echo "✅ VERSION file matches: $version_file"
fi

# Returns the version a Cargo.toml's [package] section declares, OR
# "WORKSPACE" if it inherits from workspace, OR empty if undetermined.
# Only inspects the [package] section — not [dependencies.*] etc.
get_package_version() {
    awk '
    /^\[/ {
        section = $0
        in_pkg = (section == "[package]")
        in_pkg_version = (section == "[package.version]")
    }
    in_pkg && /^version = / { gsub(/version = "|"/, ""); print; exit }
    in_pkg && /^version\.workspace[[:space:]]*=[[:space:]]*true/ { print "WORKSPACE"; exit }
    in_pkg_version && /^workspace[[:space:]]*=[[:space:]]*true/ { print "WORKSPACE"; exit }
    ' "$1"
}

# Discover workspace members from root Cargo.toml
workspace_crates=()
while IFS= read -r line; do
    workspace_crates+=("$line")
done < <(awk '
    /^\[/ { in_ws = ($0 == "[workspace]") }
    in_ws && /^members[[:space:]]*=[[:space:]]*\[/ { collecting = 1; next }
    collecting && /\]/ { exit }
    collecting {
        gsub(/[",]/, "")
        gsub(/^[[:space:]]+|[[:space:]]+$/, "")
        if (length($0) > 0) print
    }
' Cargo.toml)

errors=0
for crate in "${workspace_crates[@]}"; do
    if [ ! -f "$crate/Cargo.toml" ]; then
        echo "⚠️  WARNING: Workspace member $crate has no Cargo.toml"
        continue
    fi

    crate_version=$(get_package_version "$crate/Cargo.toml")

    if [ -z "$crate_version" ]; then
        echo "⚠️  WARNING: Could not determine version in $crate/Cargo.toml"
    elif [ "$crate_version" = "WORKSPACE" ]; then
        echo "✅ $crate: inherits from workspace ($root_version)"
    elif [ "$crate_version" != "$root_version" ]; then
        echo "❌ ERROR: Version mismatch in $crate/Cargo.toml: $crate_version (expected $root_version)"
        errors=$((errors + 1))
    else
        echo "✅ $crate: $crate_version"
    fi
done

# Also check ggen-prompt-mfg if it exists but isn't in workspace.members
# (this catches orphan crates that might drift)
for extra in crates/ggen-prompt-mfg; do
    skip=0
    for member in "${workspace_crates[@]}"; do
        if [ "$member" = "$extra" ]; then skip=1; break; fi
    done
    if [ $skip -eq 0 ] && [ -f "$extra/Cargo.toml" ]; then
        crate_version=$(get_package_version "$extra/Cargo.toml")
        if [ "$crate_version" != "WORKSPACE" ] && [ "$crate_version" != "$root_version" ] && [ -n "$crate_version" ]; then
            echo "⚠️  WARNING: Orphan crate $extra has version $crate_version (not in workspace.members)"
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
