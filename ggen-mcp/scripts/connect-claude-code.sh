#!/usr/bin/env bash
# Connect ggen-mcp to Claude Code
# 80/20 Implementation: Fastest path to working connection

set -e

echo "🚀 Connecting ggen-mcp to Claude Code..."

# Get the project root (parent of ggen-mcp)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
echo "📁 Project root: $PROJECT_ROOT"

# Build ggen-mcp
echo "🔨 Building ggen-mcp..."
cd "$PROJECT_ROOT"
cargo build --package ggen-mcp --release

# Check if build succeeded
if [ ! -f "$PROJECT_ROOT/target/release/ggen-mcp" ]; then
    echo "❌ Build failed: ggen-mcp binary not found"
    exit 1
fi

echo "✅ Build successful!"

# Add to Claude Code MCP servers
echo "🔗 Adding ggen-mcp to Claude Code..."
claude mcp add ggen "$PROJECT_ROOT/target/release/ggen-mcp" || {
    echo "⚠️  claude command not found. Please add manually:"
    echo "   claude mcp add ggen $PROJECT_ROOT/target/release/ggen-mcp"
}

# Verify connection
echo "🔍 Verifying connection..."
claude mcp list | grep -q "ggen" && echo "✅ ggen-mcp connected successfully!" || echo "⚠️  Manual verification needed: claude mcp list"

echo ""
echo "📚 Available MCP Tools (27 total):"
echo "  🤖 AI Generation:"
echo "     - ai_generate_template - Generate templates from natural language"
echo "     - ai_generate_sparql - Generate SPARQL queries from intent"
echo "     - ai_generate_ontology - Generate RDF ontologies from domain descriptions"
echo "     - ai_generate_project - Generate complete project structures"
echo "     - ai_extend_graph - Extend knowledge graphs with AI"
echo "     - ai_validate_and_improve - AI-powered code validation"
echo "     - ai_list_providers - List available AI providers"
echo ""
echo "  📦 Project Management:"
echo "     - project_gen - Generate files from templates"
echo "     - project_plan - Create execution plans"
echo "     - project_apply - Apply execution plans"
echo "     - project_diff - Show differences"
echo ""
echo "  🛒 Marketplace:"
echo "     - market_list - List available templates"
echo "     - market_search - Search marketplace"
echo "     - market_install - Install templates"
echo "     - market_recommend - Get personalized recommendations"
echo "     - market_info - Get package details"
echo "     - market_offline_search - Search cached data"
echo "     - market_cache_status - Cache statistics"
echo "     - market_sync - Sync with remote marketplace"
echo ""
echo "  🔍 Graph Operations:"
echo "     - graph_query - Execute SPARQL queries"
echo "     - graph_load - Load RDF data"
echo "     - graph_export - Export RDF graphs"
echo ""
echo "  📝 Template Tools:"
echo "     - template_create - Create new templates"
echo "     - template_validate - Validate template syntax"
echo ""
echo "  🪝 Hooks:"
echo "     - hook_register - Register lifecycle hooks"
echo ""
echo "✨ Usage in Claude Code:"
echo '   "Use ggen to generate a Rust REST API template"'
echo '   "Query the knowledge graph for all Person entities"'
echo '   "Search the marketplace for React templates"'
echo ""
echo "🎉 Setup complete!"
