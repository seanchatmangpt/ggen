#!/bin/bash
# Batch fix all 35 compilation errors in ggen-marketplace
set -e

echo "🔧 Fixing ggen-marketplace compilation errors..."

# Fix 1: backend/local.rs - line 49
# From: MarketplaceError::io_error(format!("create_dir_all: {}", db_path.display()), e)
# To: MarketplaceError::io_error(format!("create_dir_all: {}", db_path.display()), e)
# Already correct format, but wrong - e is String, should be std::io::Error

# Fix backend/local.rs comprehensively
sed -i '' 's/MarketplaceError::io_error(format!("create_dir_all: {}", db_path\.display()), e)/MarketplaceError::io_error("create directory", e)/' \
  ggen-marketplace/src/backend/local.rs

sed -i '' 's/MarketplaceError::io_error(format!("read_to_string: {}", index_path\.display()), e)/MarketplaceError::io_error("read index", e)/' \
  ggen-marketplace/src/backend/local.rs

sed -i '' 's/MarketplaceError::io_error(format!("write: {}", index_path\.display()), e)/MarketplaceError::io_error("write index", e)/' \
  ggen-marketplace/src/backend/local.rs

# Fix storage/filesystem.rs - all io_error calls
sed -i '' 's/MarketplaceError::io_error(format!("create_dir_all: {}", [^)]*), e)/MarketplaceError::io_error("create directory", e)/g' \
  ggen-marketplace/src/storage/filesystem.rs

sed -i '' 's/MarketplaceError::io_error(format!("write: {}", [^)]*), e)/MarketplaceError::io_error("write file", e)/g' \
  ggen-marketplace/src/storage/filesystem.rs

sed -i '' 's/MarketplaceError::io_error(format!("read: {}", [^)]*), e)/MarketplaceError::io_error("read file", e)/g' \
  ggen-marketplace/src/storage/filesystem.rs

sed -i '' 's/MarketplaceError::io_error(format!("remove_file: {}", [^)]*), e)/MarketplaceError::io_error("remove file", e)/g' \
  ggen-marketplace/src/storage/filesystem.rs

sed -i '' 's/MarketplaceError::io_error(format!("read_dir: {}", [^)]*), e)/MarketplaceError::io_error("read directory", e)/g' \
  ggen-marketplace/src/storage/filesystem.rs

# Fix storage/memory.rs - similar patterns
sed -i '' 's/MarketplaceError::io_error(format!("[^"]*", [^)]*), e)/MarketplaceError::io_error("storage operation", e)/g' \
  ggen-marketplace/src/storage/memory.rs

# Fix backend/centralized.rs - similar patterns
sed -i '' 's/MarketplaceError::io_error(format!("[^"]*", [^)]*), e)/MarketplaceError::io_error("registry operation", e)/g' \
  ggen-marketplace/src/backend/centralized.rs

echo "✅ Phase 1 complete: io_error fixes applied"

# Now fix the arity errors - searcher.search() takes 1 arg not 2
sed -i '' 's/searcher\.search(&collector, &\([^)]*\))/searcher.search(\&\1)/g' \
  ggen-marketplace/src/search/tantivy_engine.rs

echo "✅ Phase 2 complete: search() arity fixes applied"

echo "🔍 Verifying fixes..."
cargo build --package ggen-marketplace 2>&1 | grep -c "error\[E" || echo "0"
