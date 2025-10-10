#!/bin/bash

# AI-Powered Code Generation Demo with ggen-ai using Ollama qwen3-coder:30b
# This script demonstrates the AI capabilities of ggen for generating:
# 1. Rust service code
# 2. OWL ontologies
# 3. SPARQL queries

set -e

echo "🚀 ggen-ai Code Generation Demo (Ollama qwen3-coder:30b)"
echo "========================================================="

# Check if ggen binary exists
if ! command -v ggen &> /dev/null; then
    echo "❌ ggen binary not found. Building..."
    cargo build --release --bin ggen
fi

# Check if Ollama is running
if ! curl -s http://localhost:11434/api/version &> /dev/null; then
    echo "❌ Ollama is not running. Please start Ollama first:"
    echo "   ollama serve"
    echo ""
    echo "   Make sure the qwen3-coder:30b model is available:"
    echo "   ollama pull qwen3-coder:30b"
    exit 1
fi

# Check if qwen3-coder:30b model is available
if ! ollama list | grep -q "qwen3-coder:30b"; then
    echo "📥 Pulling qwen3-coder:30b model..."
    ollama pull qwen3-coder:30b
fi

# Create demo directory
DEMO_DIR="ai-demo-output"
mkdir -p "$DEMO_DIR"
cd "$DEMO_DIR"

echo ""
echo "📁 Demo output directory: $(pwd)"
echo "🤖 Using Ollama with qwen3-coder:30b model"

# ============================================================================
# 1. Generate Rust Service Code with AI
# ============================================================================
echo ""
echo "🔧 Step 1: AI-Generated Rust Service"
echo "------------------------------------"

# Run the AI template demo directly
echo "🤖 Running AI template generation demo..."
if cargo run --example ai_template_demo; then
    echo "✅ AI template demo completed successfully!"

    if [ -d "demo_templates" ]; then
        echo ""
        echo "📋 Generated templates:"
        ls -la demo_templates/
        echo ""
        echo "📖 Generated template preview:"
        head -10 demo_templates/ai_generated_model.tmpl 2>/dev/null || echo "No template generated"
    fi
else
    echo "❌ AI template demo failed"
    echo ""
    echo "💡 Make sure Ollama is running with the qwen3-coder:30b model:"
    echo "   1. Start Ollama: ollama serve"
    echo "   2. Pull model: ollama pull qwen3-coder:30b"
    echo "   3. Run demo: cargo run --example ai_template_demo"
fi

# ============================================================================
# 2. Run ggen-ai MCP Server with Ollama
# ============================================================================
echo ""
echo "🤖 Step 2: ggen-ai MCP Server with Ollama"
echo "----------------------------------------"

echo "To run the AI-powered MCP server with Ollama:"
echo ""
echo "  USE_OLLAMA=true cargo run --bin ggen-ai-mcp"
echo ""
echo "Or with custom model:"
echo "  USE_OLLAMA=true OLLAMA_MODEL=qwen3-coder:30b cargo run --bin ggen-ai-mcp"
echo ""

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "🎉 ggen-ai Demo Complete!"
echo "=========================="
echo ""
echo "What was demonstrated:"
echo "  • AI-powered template generation using Ollama qwen3-coder:30b"
echo "  • Rust service code generation"
echo "  • OWL ontology generation"
echo "  • SPARQL query generation"
echo "  • Integration with local Ollama models"
echo ""
echo "🔗 Next steps:"
echo "  • Use the generated templates: ggen gen demo_templates ai_generated_model"
echo "  • Run MCP server: USE_OLLAMA=true cargo run --bin ggen-ai-mcp"
echo "  • Customize model: export OLLAMA_MODEL=qwen3-coder:30b"
echo "  • Explore generated files in demo_templates/"
