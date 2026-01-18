#!/bin/bash
# ggen v5.1.0 → v6.0.0 Automated Migration Script
# This script updates import paths for library users

set -e  # Exit on error

echo "========================================="
echo "ggen v6.0.0 Migration Script"
echo "========================================="
echo ""

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if we're in a Rust project
if [ ! -f "Cargo.toml" ]; then
    echo -e "${RED}❌ Error: Cargo.toml not found in current directory${NC}"
    echo "Please run this script from your Rust project root."
    exit 1
fi

echo "📋 Pre-migration checklist:"
echo "  ✓ Found Cargo.toml"

# Check for .git directory (optional but recommended)
if [ -d ".git" ]; then
    echo "  ✓ Git repository detected"
    echo ""
    echo -e "${YELLOW}⚠️  Recommendation: Commit your changes before migration${NC}"
    echo "   Run: git add -A && git commit -m 'chore: Pre-v6 migration checkpoint'"
    echo ""
    read -p "Continue with migration? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Migration cancelled."
        exit 0
    fi
fi

echo ""
echo "🔄 Starting migration..."
echo ""

# Step 1: Backup Rust files
echo "Step 1/4: Creating backups..."
backup_count=0
while IFS= read -r file; do
    cp "$file" "$file.bak"
    ((backup_count++))
done < <(find . -type f -name "*.rs" -not -path "*/target/*" -not -path "*/.git/*")

echo -e "${GREEN}  ✓ Backed up $backup_count Rust files (.bak)${NC}"

# Step 2: Update Cargo.toml versions
echo "Step 2/4: Updating Cargo.toml..."
if grep -q 'ggen.*=.*"5\.1' Cargo.toml; then
    sed -i.bak 's/ggen\([-_][a-z]*\)\? = "5\.1/ggen\1 = "6.0/g' Cargo.toml
    echo -e "${GREEN}  ✓ Updated ggen crate versions to 6.0${NC}"
else
    echo -e "${YELLOW}  ⚠️  No v5.1 ggen dependencies found in Cargo.toml${NC}"
fi

# Step 3: Apply import path transformations
echo "Step 3/4: Updating import paths..."

transformation_count=0

while IFS= read -r file; do
    # Skip backup files and target directory
    if [[ "$file" == *.bak ]] || [[ "$file" == */target/* ]]; then
        continue
    fi

    # Apply transformations
    if grep -q "ggen_core::types::" "$file" || \
       grep -q "ggen_core::validation::" "$file" || \
       grep -q "ggen_domain::marketplace::" "$file"; then

        sed -i \
            -e 's/ggen_core::types::/ggen_core::protection::/g' \
            -e 's/ggen_core::validation::/ggen_core::validation::rules::/g' \
            -e 's/ggen_domain::marketplace::/ggen_domain::marketplace::client::/g' \
            "$file"

        ((transformation_count++))
    fi
done < <(find . -type f -name "*.rs" -not -path "*/target/*" -not -path "*/.git/*")

echo -e "${GREEN}  ✓ Updated import paths in $transformation_count files${NC}"

# Step 4: Verify with cargo check
echo "Step 4/4: Verifying migration..."
echo ""

if command -v cargo &> /dev/null; then
    echo "Running 'cargo check'..."
    if cargo check --all-targets 2>&1 | tee migration.log; then
        echo ""
        echo -e "${GREEN}✅ Migration successful!${NC}"
        echo ""
        echo "📝 Summary:"
        echo "  - $backup_count files backed up"
        echo "  - $transformation_count files updated"
        echo "  - Cargo check: PASSED"
        echo ""
        echo "🎯 Next steps:"
        echo "  1. cargo test          # Run your test suite"
        echo "  2. Review changes:     # git diff"
        echo "  3. Commit changes:     # git add -A && git commit -m 'chore: Migrate to ggen v6.0.0'"
        echo "  4. Clean backups:      # find . -name '*.bak' -delete"
        echo ""
        echo "📚 Documentation: See UPGRADING_TO_V6.md for details"
        exit 0
    else
        echo ""
        echo -e "${YELLOW}⚠️  Cargo check found issues${NC}"
        echo ""
        echo "🔍 Common issues:"
        echo "  1. Manual import updates may be needed (check migration.log)"
        echo "  2. Additional API changes beyond simple imports"
        echo "  3. Version conflicts in dependencies"
        echo ""
        echo "💡 The Rust compiler will guide you through fixes:"
        echo "   cargo check  # Shows all remaining issues with suggestions"
        echo ""
        echo "🔄 To rollback:"
        echo "   ./rollback-v6-migration.sh"
        echo ""
        exit 1
    fi
else
    echo -e "${YELLOW}⚠️  Cargo not found - skipping verification${NC}"
    echo ""
    echo -e "${GREEN}✅ Migration transformations completed${NC}"
    echo ""
    echo "📝 Summary:"
    echo "  - $backup_count files backed up"
    echo "  - $transformation_count files updated"
    echo ""
    echo "⚠️  Please verify manually:"
    echo "  1. cargo check"
    echo "  2. cargo test"
    echo ""
fi

# Create rollback script
cat > rollback-v6-migration.sh << 'EOF'
#!/bin/bash
# Rollback ggen v6.0.0 migration

set -e

echo "🔄 Rolling back ggen v6.0.0 migration..."
echo ""

rollback_count=0

# Restore .rs files
while IFS= read -r backup; do
    original="${backup%.bak}"
    mv "$backup" "$original"
    ((rollback_count++))
done < <(find . -type f -name "*.rs.bak" -not -path "*/target/*")

# Restore Cargo.toml if backup exists
if [ -f "Cargo.toml.bak" ]; then
    mv Cargo.toml.bak Cargo.toml
    echo "  ✓ Restored Cargo.toml"
fi

echo "✅ Rollback complete!"
echo "   Restored $rollback_count files"
echo ""
echo "Run 'cargo check' to verify."
EOF

chmod +x rollback-v6-migration.sh

echo ""
echo "📄 Rollback script created: rollback-v6-migration.sh"
