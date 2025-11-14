#!/bin/bash
# Analyze documentation inconsistencies (Mura) in the codebase
# Identifies files that haven't been updated to follow documentation standards

set -e

echo "=== Documentation Mura Analysis ==="
echo ""

# Count public APIs
echo "## Public API Counts"
PUBLIC_APIS=$(grep -r "^pub fn\|^pub struct\|^pub enum\|^pub trait" crates/ --include="*.rs" | wc -l | tr -d ' ')
echo "Total public APIs: $PUBLIC_APIS"

# Count documented items
DOCUMENTED=$(grep -r "^///\|^//!" crates/ --include="*.rs" | wc -l | tr -d ' ')
echo "Total documentation lines: $DOCUMENTED"

# Count doctests
DOCTESTS=$(grep -r '```rust' crates/ --include="*.rs" | wc -l | tr -d ' ')
echo "Total doctests: $DOCTESTS"

# Count no_run doctests
NO_RUN=$(grep -r '```rust,no_run' crates/ --include="*.rs" | wc -l | tr -d ' ')
echo "Non-runnable doctests (no_run): $NO_RUN"

# Count ignored doctests
IGNORED=$(grep -r '```rust,ignore' crates/ --include="*.rs" | wc -l | tr -d ' ')
echo "Ignored doctests: $IGNORED"

echo ""
echo "## Files Without Module-Level Documentation"
echo ""

# Find files without module-level docs (//!)
find crates/ -name "*.rs" -type f | while read file; do
    # Skip test files
    if [[ "$file" == *"/tests/"* ]] || [[ "$file" == *"/test.rs" ]] || [[ "$file" == *"/benches/"* ]]; then
        continue
    fi
    
    # Check if file has module-level docs
    if ! head -20 "$file" | grep -q "^//!"; then
        # Check if file has any public items
        if grep -q "^pub " "$file"; then
            echo "  - $file"
        fi
    fi
done | head -20

echo ""
echo "## Public Functions Without Documentation"
echo ""

# Find public functions without doc comments
find crates/ -name "*.rs" -type f | while read file; do
    # Skip test files
    if [[ "$file" == *"/tests/"* ]] || [[ "$file" == *"/test.rs" ]] || [[ "$file" == *"/benches/"* ]]; then
        continue
    fi
    
    # Extract public functions and check for documentation
    awk '
    /^pub fn/ {
        func = $0
        getline
        if (!/^\/\//) {
            print FILENAME ":" NR-1 ": " func
        }
    }
    ' "$file" 2>/dev/null | head -10
done | head -20

echo ""
echo "## Public Structs/Enums Without Documentation"
echo ""

# Find public types without doc comments
find crates/ -name "*.rs" -type f | while read file; do
    # Skip test files
    if [[ "$file" == *"/tests/"* ]] || [[ "$file" == *"/test.rs" ]] || [[ "$file" == *"/benches/"* ]]; then
        continue
    fi
    
    # Extract public types and check for documentation
    awk '
    /^pub struct|^pub enum/ {
        type_def = $0
        getline
        if (!/^\/\//) {
            print FILENAME ":" NR-1 ": " type_def
        }
    }
    ' "$file" 2>/dev/null | head -10
done | head -20

echo ""
echo "## Documentation Without Doctests"
echo ""

# Find documented functions without doctests
find crates/ -name "*.rs" -type f | while read file; do
    # Skip test files
    if [[ "$file" == *"/tests/"* ]] || [[ "$file" == *"/test.rs" ]] || [[ "$file" == *"/benches/"* ]]; then
        continue
    fi
    
    # Check if file has documentation but no doctests
    if grep -q "^///" "$file" && ! grep -q '```rust' "$file"; then
        # Check if it has public functions
        if grep -q "^pub fn" "$file"; then
            echo "  - $file (has docs, no doctests)"
        fi
    fi
done | head -20

echo ""
echo "## Doctest Format Inconsistencies"
echo ""

# Find doctests that don't follow standard format
find crates/ -name "*.rs" -type f | while read file; do
    # Skip test files
    if [[ "$file" == *"/tests/"* ]] || [[ "$file" == *"/test.rs" ]] || [[ "$file" == *"/benches/"* ]]; then
        continue
    fi
    
    # Check for doctests without proper Result handling pattern
    if grep -q '```rust' "$file"; then
        # Check if doctest uses Result but doesn't have the standard pattern
        if grep -q 'Result<.*>' "$file" && ! grep -q '# fn main() -> anyhow::Result<()>' "$file"; then
            echo "  - $file (Result doctest missing standard pattern)"
        fi
    fi
done | head -20

echo ""
echo "=== Analysis Complete ==="

