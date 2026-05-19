#!/bin/bash
# Fix MCP server generation template: Use inner attribute for tokio::main
# Issue: Template uses #[tokio::main] but should use #![tokio::main]

set -e

echo "🔧 Fixing MCP server generation template..."

# Check we're in the worktree
if [ ! -f "crates/ggen-core/src/mcp/mod.rs" ]; then
    echo "❌ Error: Must be run from worktree root"
    exit 1
fi

# Backup original file
cp crates/ggen-core/src/mcp/mod.rs crates/ggen-core/src/mcp/mod.rs.backup

# Apply fix: Replace #[tokio::main] with #![tokio::main]
sed -i '' 's/#\[tokio::main\]/#![tokio::main]/g' crates/ggen-core/src/mcp/mod.rs

echo "✅ Template fixed"
echo ""
echo "🧪 Running smoke test..."

# Run tests
cargo test -p ggen-core --test mcp_generation_e2e_test --verbose 2>&1 | tee /tmp/mcp_fix_test.log

# Check results
if grep -q "test result: FAILED" /tmp/mcp_fix_test.log; then
    echo ""
    echo "❌ Tests still failing - check logs"
    echo "   Backup saved to: crates/ggen-core/src/mcp/mod.rs.backup"
    exit 1
else
    echo ""
    echo "✅ All tests passed!"
    echo ""
    echo "📊 Test Summary:"
    grep "test result:" /tmp/mcp_fix_test.log
    echo ""
    echo "🧹 Cleanup: Remove backup with: rm crates/ggen-core/src/mcp/mod.rs.backup"
fi
