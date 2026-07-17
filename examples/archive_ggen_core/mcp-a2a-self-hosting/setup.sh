#!/usr/bin/env bash
# Setup and validation script for MCP/A2A self-hosting example
# This script validates the entire zero-touch workflow

set -e  # Exit on error

echo "🚀 MCP/A2A Self-Hosting Example Setup"
echo "======================================"
echo ""

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Check prerequisites
echo "📋 Checking prerequisites..."

# Check Rust
if ! command -v cargo &> /dev/null; then
    echo -e "${RED}❌ Rust not found. Install from https://rustup.rs/${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Rust installed${NC}"

# Check ggen CLI
if ! command -v ggen &> /dev/null; then
    echo -e "${YELLOW}⚠ ggen CLI not found. Installing...${NC}"
    cargo install ggen-cli
fi
echo -e "${GREEN}✓ ggen CLI installed${NC}"

# Check API key
if [[ -z "$OPENAI_API_KEY" && -z "$ANTHROPIC_API_KEY" ]]; then
    echo -e "${YELLOW}⚠ No LLM API key found${NC}"
    echo "Set OPENAI_API_KEY or ANTHROPIC_API_KEY to enable LLM generation"
    echo "Continuing without LLM generation..."
    export LLM_ENABLED="false"
else
    echo -e "${GREEN}✓ LLM API key found${NC}"
    export LLM_ENABLED="true"
fi

echo ""
echo "🔧 Validating ontology..."

# Validate ontology
if ggen validate ontology/agent.ttl 2>&1 | grep -q "valid"; then
    echo -e "${GREEN}✓ Ontology is valid${NC}"
else
    echo -e "${RED}❌ Ontology validation failed${NC}"
    exit 1
fi

echo ""
echo "🧱 Running ggen sync..."

# Run ggen sync
if [[ "$LLLM_ENABLED" == "true" ]]; then
    ggen sync --llm true
else
    ggen sync --llm false
fi

echo -e "${GREEN}✓ Code generation complete${NC}"

echo ""
echo "🔍 Validating generated code..."

# Check if generated files exist
if [[ ! -f "src/generated/agent.rs" ]]; then
    echo -e "${RED}❌ agent.rs not generated${NC}"
    exit 1
fi
echo -e "${GREEN}✓ src/generated/agent.rs exists${NC}"

if [[ ! -f "src/generated/skills.rs" ]]; then
    echo -e "${RED}❌ skills.rs not generated${NC}"
    exit 1
fi
echo -e "${GREEN}✓ src/generated/skills.rs exists${NC}"

if [[ ! -f "src/generated/agent_card.json" ]]; then
    echo -e "${RED}❌ agent_card.json not generated${NC}"
    exit 1
fi
echo -e "${GREEN}✓ src/generated/agent_card.json exists${NC}"

echo ""
echo "🔨 Compiling generated code..."

# Compile check
if cargo check --quiet 2>&1; then
    echo -e "${GREEN}✓ Code compiles successfully${NC}"
else
    echo -e "${RED}❌ Compilation failed${NC}"
    echo "Run 'cargo check' for details"
    exit 1
fi

echo ""
echo "✅ Setup complete!"
echo ""
echo "📦 Generated files:"
echo "  - src/generated/agent.rs"
echo "  - src/generated/skills.rs"
echo "  - src/generated/agent_card.json"
echo ""
echo "🚀 Next steps:"
echo "  1. Run A2A server:   cargo run --bin a2a-server"
echo "  2. Run MCP server:   cargo run --bin mcp-server"
echo "  3. Test agent:       curl http://localhost:8080/.well-known/agent.json"
echo ""
echo "📚 Documentation:"
echo "  - README.md          (This file)"
echo "  - ontology/agent.ttl (Source of truth)"
echo ""
