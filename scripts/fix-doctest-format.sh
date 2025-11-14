#!/bin/bash
# Fix doctest format inconsistencies - apply kaizen improvement pattern
# Updates Result-returning doctests to use standard pattern

set -e

echo "=== Fixing Doctest Format Inconsistencies ==="
echo ""

# Files to fix (from mura analysis)
FILES=(
    "crates/ggen-utils/src/lib.rs"
    "crates/ggen-utils/src/error.rs"  # Already fixed, but verify
    "crates/ggen-utils/src/types.rs"
    "crates/ggen-utils/src/logger.rs"
    "crates/ggen-utils/src/app_config.rs"
    "crates/ggen-utils/src/alert.rs"
    "crates/ggen-core/src/registry.rs"
    "crates/ggen-core/src/lifecycle/state_machine.rs"
    "crates/ggen-core/src/lifecycle/poka_yoke.rs"
    "crates/ggen-core/src/lifecycle/state_validation.rs"
    "crates/ggen-core/src/lifecycle/model.rs"
    "crates/ggen-core/src/cli_generator/dx.rs"
    "crates/ggen-core/src/project_generator/mod.rs"
    "crates/ggen-core/src/cleanroom/policy.rs"
    "crates/ggen-core/src/cleanroom/forensics.rs"
    "crates/ggen-core/src/cleanroom/mod.rs"
    "crates/ggen-core/src/rdf/schema.rs"
    "crates/ggen-marketplace/src/template_search.rs"
    "crates/ggen-marketplace/src/cache/mod.rs"
    "crates/ggen-marketplace/src/traits/registry.rs"
)

echo "Files to check: ${#FILES[@]}"
echo ""

# Check each file for Result doctests that need fixing
for file in "${FILES[@]}"; do
    if [ ! -f "$file" ]; then
        echo "⚠️  File not found: $file"
        continue
    fi
    
    # Check if file has Result in doctests
    if grep -q '```rust.*Result\|Result<.*>' "$file"; then
        # Check if it already has the standard pattern
        if grep -q '# fn main() ->.*Result<()>' "$file" || grep -q '# async fn.*->.*Result<()>' "$file"; then
            echo "✅ $file - Already has standard pattern"
        else
            echo "🔧 $file - Needs fixing"
        fi
    else
        echo "⏭️  $file - No Result doctests"
    fi
done

echo ""
echo "=== Analysis Complete ==="
echo ""
echo "Note: This script identifies files. Manual fixes required for each file."
echo "Pattern to apply:"
echo "  # fn main() -> Result<()> {"
echo "  // ... example code ..."
echo "  # Ok(())"
echo "  # }"

