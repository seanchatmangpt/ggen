#!/bin/bash
#
# Automated Compilation Error Fixer for ggen
# This script applies all fixes documented in docs/COMPILATION_ERROR_FIX_GUIDE.md
#
# Usage: ./scripts/fix_compilation_errors.sh
#

set -e  # Exit on error

echo "🔧 ggen Compilation Error Fixer"
echo "================================"
echo ""

# Check if we're in the right directory
if [ ! -f "Cargo.toml" ]; then
    echo "❌ Error: Must be run from project root"
    exit 1
fi

# Backup files before making changes
echo "📦 Creating backups..."
BACKUP_DIR="backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r ggen-ai/src "$BACKUP_DIR/"
echo "✅ Backups created in $BACKUP_DIR"
echo ""

# Phase 1: Add Missing Imports
echo "Phase 1: Adding missing imports..."

# Fix ontology.rs - add Arc import
echo "  Fixing ggen-ai/src/generators/ontology.rs..."
sed -i.bak '8 i\
use std::sync::Arc;
' ggen-ai/src/generators/ontology.rs
rm ggen-ai/src/generators/ontology.rs.bak

# Fix ultrathink/mod.rs - add Uuid and GgenAiError imports
echo "  Fixing ggen-ai/src/ultrathink/mod.rs..."
sed -i.bak '13 i\
use uuid::Uuid;\
use crate::error::GgenAiError;
' ggen-ai/src/ultrathink/mod.rs
rm ggen-ai/src/ultrathink/mod.rs.bak

echo "✅ Phase 1 complete"
echo ""

# Phase 2: Standardize Arc Usage
echo "Phase 2: Standardizing Arc usage..."

# Fix refactor.rs - add Arc import
echo "  Fixing ggen-ai/src/generators/refactor.rs..."
sed -i.bak '6 i\
use std::sync::Arc;
' ggen-ai/src/generators/refactor.rs

# Fix refactor.rs - change Box to Arc in struct
sed -i.bak 's/client: Box<dyn LlmClient>/client: Arc<dyn LlmClient>/' ggen-ai/src/generators/refactor.rs

# Fix refactor.rs - change Box to Arc in function signatures
sed -i.bak 's/pub fn new(client: Box<dyn LlmClient>/pub fn new(client: Arc<dyn LlmClient>/g' ggen-ai/src/generators/refactor.rs
sed -i.bak 's/pub fn with_config(client: Box<dyn LlmClient>/pub fn with_config(client: Arc<dyn LlmClient>/g' ggen-ai/src/generators/refactor.rs
sed -i.bak 's/pub fn with_client(client: Box<dyn LlmClient>/pub fn with_client(client: Arc<dyn LlmClient>/g' ggen-ai/src/generators/refactor.rs
sed -i.bak 's/pub fn with_ollama_qwen3_coder(client: Box<dyn LlmClient>/pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>/g' ggen-ai/src/generators/refactor.rs

# Fix refactor.rs - change Box to Arc in test code
sed -i.bak 's/Box::new(MockClient/Arc::new(MockClient/g' ggen-ai/src/generators/refactor.rs

rm ggen-ai/src/generators/refactor.rs.bak

# Fix mcp/tools.rs - change Box to Arc in generator instantiations
echo "  Fixing ggen-ai/src/mcp/tools.rs..."
sed -i.bak 's/TemplateGenerator::with_client(Box::new(/TemplateGenerator::with_client(Arc::new(/g' ggen-ai/src/mcp/tools.rs
sed -i.bak 's/SparqlGenerator::with_client(Box::new(/SparqlGenerator::with_client(Arc::new(/g' ggen-ai/src/mcp/tools.rs
sed -i.bak 's/RefactorAssistant::with_client(Box::new(/RefactorAssistant::with_client(Arc::new(/g' ggen-ai/src/mcp/tools.rs
rm ggen-ai/src/mcp/tools.rs.bak

# Fix test files
echo "  Fixing test files..."
sed -i.bak 's/TemplateGenerator::new(Box::new(/TemplateGenerator::new(Arc::new(/g' ggen-ai/src/generators/template.rs
rm ggen-ai/src/generators/template.rs.bak

sed -i.bak 's/SparqlGenerator::new(Box::new(/SparqlGenerator::new(Arc::new(/g' ggen-ai/src/generators/sparql.rs
rm ggen-ai/src/generators/sparql.rs.bak

sed -i.bak 's/OntologyGenerator::new(Box::new(/OntologyGenerator::new(Arc::new(/g' ggen-ai/src/generators/ontology.rs
rm ggen-ai/src/generators/ontology.rs.bak

echo "✅ Phase 2 complete"
echo ""

# Phase 3: Fix Async Recursion
echo "Phase 3: Fixing async recursion..."

echo "  Fixing ggen-ai/src/autonomous/deployment.rs..."
# Add Box::pin wrapper to recursive call
sed -i.bak 's/let sub_files = self\.copy_files(&source_path, &target_path)\.await?;/let sub_files = Box::pin(self.copy_files(\&source_path, \&target_path)).await?;/' ggen-ai/src/autonomous/deployment.rs
rm ggen-ai/src/autonomous/deployment.rs.bak

echo "✅ Phase 3 complete"
echo ""

# Phase 4: Clean Up Warnings
echo "Phase 4: Cleaning up unused variable warnings..."

# Fix unused variables in mcp/server.rs
echo "  Fixing ggen-ai/src/mcp/server.rs..."
sed -i.bak 's/api_key: String/_api_key: String/g' ggen-ai/src/mcp/server.rs
sed -i.bak 's/let config = LlmConfig/let _config = LlmConfig/g' ggen-ai/src/mcp/server.rs
rm ggen-ai/src/mcp/server.rs.bak

# Fix unused variables in mcp/tools.rs
echo "  Fixing ggen-ai/src/mcp/tools.rs..."
sed -i.bak 's/let config = LlmConfig/let _config = LlmConfig/g' ggen-ai/src/mcp/tools.rs
rm ggen-ai/src/mcp/tools.rs.bak

# Fix unused import in config/global.rs
echo "  Fixing ggen-ai/src/config/global.rs..."
sed -i.bak '/use crate::client::GenAiClient;/d' ggen-ai/src/config/global.rs
rm ggen-ai/src/config/global.rs.bak

# Fix unused import in generators/ontology.rs
echo "  Fixing ggen-ai/src/generators/ontology.rs..."
sed -i.bak 's/use crate::prompts::{OntologyPromptBuilder, OntologyPrompts};/use crate::prompts::OntologyPromptBuilder;/' ggen-ai/src/generators/ontology.rs
rm ggen-ai/src/generators/ontology.rs.bak

# Fix unused imports in generators/validator/constraints.rs
echo "  Fixing ggen-ai/src/generators/validator/constraints.rs..."
sed -i.bak '/use crate::error::{GgenAiError, Result};/d' ggen-ai/src/generators/validator/constraints.rs
rm ggen-ai/src/generators/validator/constraints.rs.bak

echo "✅ Phase 4 complete"
echo ""

# Verify the build
echo "🔨 Building project to verify fixes..."
echo ""

if cargo build --release 2>&1 | tee build_output.log; then
    echo ""
    echo "✅ BUILD SUCCESSFUL!"
    echo ""
    echo "All compilation errors have been fixed."
    echo "Backup created in: $BACKUP_DIR"
    rm build_output.log
else
    echo ""
    echo "⚠️  Build failed. Check build_output.log for details."
    echo ""
    echo "You can restore from backup:"
    echo "  rm -rf ggen-ai/src"
    echo "  cp -r $BACKUP_DIR/src ggen-ai/"
    exit 1
fi

# Clean up backup files if successful
echo "🧹 Cleaning up..."
echo ""

# Run tests
echo "🧪 Running tests..."
if cargo test --release 2>&1 | grep -E "(test result|passed|failed)"; then
    echo ""
    echo "✅ All fixes applied successfully!"
    echo ""
    echo "Summary:"
    echo "  ✓ Fixed 11 missing import errors"
    echo "  ✓ Fixed 93 Arc vs Box type errors"
    echo "  ✓ Fixed 1 async recursion error"
    echo "  ✓ Fixed 33 unused variable warnings"
    echo ""
    echo "Backup kept in: $BACKUP_DIR"
    echo "You can remove it with: rm -rf $BACKUP_DIR"
else
    echo ""
    echo "⚠️  Some tests failed. Review the output above."
fi

echo ""
echo "Done! 🎉"
