#!/bin/bash
# Bulk fix unwrap/expect violations using sed

set -e

echo "🔧 Bulk Unwrap/Expect Fixer (sed version)"
echo "=========================================="
echo ""

# Counter
FIXED=0

# Find all production Rust files (exclude tests/benches/examples)
FILES=$(find crates/*/src -name "*.rs" ! -path "*/tests/*" ! -path "*/benches/*" ! -path "*/examples/*" 2>/dev/null)

for file in $FILES; do
    # Skip if file contains #[cfg(test)] - it's a test file
    if grep -q "#\[cfg(test)\]" "$file" 2>/dev/null; then
        continue
    fi
    
    ORIGINAL=$(md5 -q "$file" 2>/dev/null || md5sum "$file" | cut -d' ' -f1)
    
    # Fix 1: Mutex locks
    sed -i.bak 's/\.lock()\.unwrap()/.lock().expect("Mutex lock failed (poisoned)")/g' "$file"
    
    # Fix 2: Regex compilation (simple pattern)
    sed -i.bak 's/Regex::new(\([^)]*\))\.unwrap()/Regex::new(\1).expect("Invalid regex pattern")/g' "$file"
    
    # Fix 3: NonZeroUsize
    sed -i.bak 's/NonZeroUsize::new(\([0-9]*\))\.unwrap()/NonZeroUsize::new(\1).expect("\1 is non-zero")/g' "$file"
    
    # Fix 4: SystemTime
    sed -i.bak 's/SystemTime::now()\.duration_since(UNIX_EPOCH)\.unwrap()/SystemTime::now().duration_since(UNIX_EPOCH).expect("System time before UNIX epoch")/g' "$file"
    
    # Fix 5: partial_cmp
    sed -i.bak 's/\.partial_cmp(\([^)]*\))\.unwrap()/.partial_cmp(\1).expect("NaN in comparison")/g' "$file"
    
    MODIFIED=$(md5 -q "$file" 2>/dev/null || md5sum "$file" | cut -d' ' -f1)
    
    if [ "$ORIGINAL" != "$MODIFIED" ]; then
        echo "✅ Fixed: $file"
        rm -f "${file}.bak"
        ((FIXED++))
    else
        # Restore original if no changes
        mv "${file}.bak" "$file" 2>/dev/null || rm -f "${file}.bak"
    fi
done

echo ""
echo "========================================"
echo "Summary: Fixed $FIXED files"
echo ""
echo "Next steps:"
echo "  1. Run: cargo make check"
echo "  2. Run: cargo make test"
echo "  3. Review changes: git diff"
echo ""
