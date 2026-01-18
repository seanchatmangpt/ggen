#!/bin/bash
# Quick fix for v2.0.0 dependency mismatches
# This script resolves version conflicts to enable full validation

set -e

echo "🔧 Fixing ggen v2.0.0 dependency mismatches..."
echo

# Option 1: Update all to v2.0.0 (if ready for version bump)
# echo "Option 1: Updating all Cargo.toml to v2.0.0"
# find . -name Cargo.toml -type f -exec sed -i '' 's/version = "1.2.0"/version = "2.0.0"/g' {} \;

# Option 2: Keep v1.2.0 and fix internal dependencies (safer for now)
echo "✅ Keeping v1.2.0 and adjusting internal dependencies..."

# Fix ggen-ai dependency on ggen-core
if [ -f "ggen-ai/Cargo.toml" ]; then
    echo "  - Updating ggen-ai/Cargo.toml"
    sed -i '' 's/ggen-core = { version = "^2.0.0"/ggen-core = { version = "^1.2.0"/g' ggen-ai/Cargo.toml
fi

# Fix ggen-project dependency on ggen-core
if [ -f "ggen-project/Cargo.toml" ]; then
    echo "  - Updating ggen-project/Cargo.toml"
    sed -i '' 's/ggen-core = { version = "^2.0.0"/ggen-core = { version = "^1.2.0"/g' ggen-project/Cargo.toml
fi

# Fix cli dependency on ggen-core
if [ -f "cli/Cargo.toml" ]; then
    echo "  - Updating cli/Cargo.toml"
    sed -i '' 's/ggen-core = { version = "^2.0.0"/ggen-core = { version = "^1.2.0"/g' cli/Cargo.toml
fi

# Fix any other workspace dependencies
for toml in */Cargo.toml; do
    if grep -q 'ggen-core = { version = "^2.0.0"' "$toml"; then
        echo "  - Updating $toml"
        sed -i '' 's/ggen-core = { version = "^2.0.0"/ggen-core = { version = "^1.2.0"/g' "$toml"
    fi
done

echo
echo "✅ Dependencies fixed!"
echo
echo "Next steps:"
echo "  1. cargo build --release    # Verify build still works"
echo "  2. cargo bench               # Run performance benchmarks"
echo "  3. cargo test                # Run test suite (after stub fixes)"
echo
