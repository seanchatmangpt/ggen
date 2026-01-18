#!/bin/bash
# Migration script for ggen v5.1.0 → v6.0.0
# Automatically fixes import paths for library users

set -e  # Exit on error

echo "🔧 ggen v6.0.0 Migration Script"
echo "================================"
echo ""

# Detect OS and set sed command
if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  if command -v gsed &> /dev/null; then
    SED_CMD="gsed"
    echo "✅ Using GNU sed (gsed)"
  else
    echo "❌ Error: GNU sed not found. Install it with: brew install gnu-sed"
    exit 1
  fi
else
  # Linux
  SED_CMD="sed"
  echo "✅ Using sed"
fi

# Check if we're in a ggen project
if [ ! -f "Cargo.toml" ]; then
  echo "❌ Error: Cargo.toml not found. Run this script from your project root."
  exit 1
fi

echo ""
echo "📂 Scanning for Rust files in crates/"
echo ""

# Count files to migrate
file_count=$(find crates/ -name "*.rs" -type f 2>/dev/null | wc -l | tr -d ' ')

if [ "$file_count" -eq 0 ]; then
  echo "⚠️  No Rust files found in crates/. Nothing to migrate."
  exit 0
fi

echo "Found $file_count Rust files"
echo ""

# Create backup directory
BACKUP_DIR=".ggen_migration_backup_$(date +%Y%m%d_%H%M%S)"
echo "📦 Creating backup at: $BACKUP_DIR"
mkdir -p "$BACKUP_DIR"
cp -r crates/ "$BACKUP_DIR/"
echo "✅ Backup created"
echo ""

echo "🔄 Migrating import paths..."
echo ""

# Counter for changed files
changed_files=0

# Migration patterns
declare -A migrations=(
  ["ProtectedPath"]="protection"
  ["PathError"]="protection"
  ["PathProtectionError"]="protection"
  ["PathProtector"]="protection"
  ["GlobPattern"]="protection"
)

# Process each pattern
for type in "${!migrations[@]}"; do
  module="${migrations[$type]}"
  echo "  Migrating: ggen_core::types::$type → ggen_core::${module}::$type"

  # Find and replace
  find crates/ -name "*.rs" -type f -exec $SED_CMD -i \
    "s/use ggen_core::types::$type/use ggen_core::${module}::$type/g" {} +

  # Also handle grouped imports
  find crates/ -name "*.rs" -type f -exec $SED_CMD -i \
    "s/ggen_core::types::{\\([^}]*\\)$type\\([^}]*\\)}/ggen_core::${module}::{\1$type\2}/g" {} +
done

echo ""
echo "✅ Import path migration complete"
echo ""

# Count actually changed files
changed_files=$(find crates/ -name "*.rs" -type f -exec grep -l "ggen_core::protection" {} + 2>/dev/null | wc -l | tr -d ' ')

echo "📊 Migration Summary"
echo "===================="
echo "Files scanned:  $file_count"
echo "Files changed:  $changed_files"
echo "Backup location: $BACKUP_DIR"
echo ""

# Verify compilation
echo "🔍 Verifying compilation..."
echo ""

if cargo check 2>&1 | tee /tmp/cargo_check_output.txt; then
  echo ""
  echo "✅ SUCCESS: Project compiles with v6.0.0 imports!"
  echo ""
  echo "🎉 Migration complete! You can now:"
  echo "   1. Review changes: git diff"
  echo "   2. Run tests: cargo make test"
  echo "   3. Commit: git commit -am 'chore: migrate to ggen v6.0.0'"
  echo ""
  echo "📁 Backup preserved at: $BACKUP_DIR"
  echo "   (You can delete it after verifying everything works)"
else
  echo ""
  echo "❌ COMPILATION FAILED"
  echo ""
  echo "Some imports may still need manual fixes. Check the output above."
  echo ""
  echo "Common issues:"
  echo "  1. Mixed imports (some from ::types, some from ::protection)"
  echo "  2. Wildcard imports: use ggen_core::types::*"
  echo "  3. Custom type aliases referencing old paths"
  echo ""
  echo "To restore from backup:"
  echo "  rm -rf crates/"
  echo "  cp -r $BACKUP_DIR/crates/ ."
  echo ""
  exit 1
fi
