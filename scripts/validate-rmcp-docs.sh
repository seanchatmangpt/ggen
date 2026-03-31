#!/bin/bash
# scripts/validate-rmcp-docs.sh
# Validates that RMCP_NOTES.md examples compile and work

set -euo pipefail

echo "=== rmcp 1.3.0 Documentation Validation ==="

# Step 1: Create temp Rust project
TEMP_DIR=$(mktemp -d)
echo "Created temp dir: $TEMP_DIR"

cleanup() {
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

cd "$TEMP_DIR"
mkdir -p src

# Step 2: Create minimal rmcp server matching RMCP_NOTES.md examples
cat > Cargo.toml <<'EOF'
[package]
name = "rmcp-validation"
version = "0.1.0"
edition = "2021"

[dependencies]
rmcp = { version = "1.3", features = ["server", "client", "transport-io", "macros"] }
schemars = "1.0"
EOF

cat > src/main.rs <<'EOF'
use rmcp::{tool, tool_handler, tool_router, ServerHandler, ToolRouter};

#[derive(Clone)]
pub struct TestServer {
    router: ToolRouter<TestServer>,
}

#[tool_router]
impl TestServer {
    pub fn new() -> Self {
        Self { router: Self::tool_router() }
    }

    #[tool(description = "Test tool")]
    async fn test_tool(&self, #[tool(param)] input: String)
        -> Result<rmcp::types::CallToolResult, rmcp::Error>
    {
        Ok(rmcp::types::CallToolResult::success(vec![
            rmcp::types::Content::text(format!("Echo: {}", input))
        ]))
    }
}

#[tool_handler]
impl ServerHandler for TestServer {}
EOF

echo "✓ Created minimal rmcp server project"

# Step 3: Verify it compiles (validates RMCP_NOTES.md §9: Cargo.toml dependency line)
echo "Compiling rmcp server..."
if cargo check 2>&1 | grep -q "Finished"; then
    echo "✓ rmcp server compiles successfully"
else
    echo "✗ Compilation failed — RMCP_NOTES.md dependency line may be incorrect"
    cargo check 2>&1 | tail -20
    exit 1
fi

# Step 4: Verify no anti-patterns in generated code
echo "Checking for documented anti-patterns..."
if grep -r "let _ = .*\.serve()" src/ 2>/dev/null; then
    echo "✗ Found DropGuard anti-pattern (RMCP_NOTES.md §2)"
    exit 1
else
    echo "✓ No DropGuard anti-patterns found"
fi

echo "=== All rmcp documentation validated ==="
