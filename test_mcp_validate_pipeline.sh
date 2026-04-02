#!/bin/bash
# Test script for validate_pipeline MCP tool example

set -e

echo "🔧 Building validate_pipeline MCP client example..."

# Navigate to ggen-a2a-mcp crate
cd /Users/sac/ggen/crates/ggen-a2a-mcp

# Build the example
cargo build --example mcp_validate_pipeline_bin 2>&1 | tail -20

echo ""
echo "🔨 Running validate_pipeline MCP client example..."
echo "=================================================="

# Run the example
cargo run --example mcp_validate_pipeline_bin 2>&1

echo ""
echo "✅ Test complete!"
