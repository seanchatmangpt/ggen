#!/bin/bash
set -e

echo "🚀 ggen MCP HTTP API Examples with curl"
echo "========================================"
echo ""

# Start ggen MCP HTTP server in background
echo "Starting ggen MCP HTTP server on port 8080..."
ggen mcp start --transport http --port 8080 &
SERVER_PID=$!

# Wait for server to start
sleep 2

BASE_URL="http://localhost:8080"

# Cleanup function
cleanup() {
    echo ""
    echo "Stopping server..."
    kill $SERVER_PID 2>/dev/null || true
}
trap cleanup EXIT

echo "✓ Server started (PID: $SERVER_PID)"
echo ""

# Example 1: List available tools
echo "📋 Example 1: List Available Tools"
echo "-----------------------------------"
curl -s -X POST "$BASE_URL/tools/list" \
  -H "Content-Type: application/json" | jq '.tools[:5] | .[] | {name, description}'
echo ""

# Example 2: Search marketplace
echo "🔍 Example 2: Search Marketplace"
echo "--------------------------------"
curl -s -X POST "$BASE_URL/tools/call" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_market_search",
    "arguments": {
      "query": "rust axum",
      "limit": 3
    }
  }' | jq
echo ""

# Example 3: Generate code
echo "⚙️  Example 3: Generate Code"
echo "---------------------------"
curl -s -X POST "$BASE_URL/tools/call" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_gen_with_vars",
    "arguments": {
      "template": "templates/rust-struct.tmpl",
      "vars": {
        "name": "User",
        "fields": ["id", "email", "name"],
        "determinism": 42
      },
      "output": "/tmp/ggen-http-example.rs"
    }
  }' | jq
echo ""

# Example 4: SPARQL query
echo "🔗 Example 4: RDF Graph Query"
echo "-----------------------------"
curl -s -X POST "$BASE_URL/tools/call" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_graph_load",
    "arguments": {
      "content": "@prefix ex: <http://example.org/> . ex:User a ex:Class .",
      "format": "turtle"
    }
  }' | jq
echo ""

curl -s -X POST "$BASE_URL/tools/call" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_graph_query",
    "arguments": {
      "query": "SELECT * WHERE { ?s ?p ?o }"
    }
  }' | jq
echo ""

# Example 5: Batch operations
echo "📦 Example 5: Batch Operations"
echo "------------------------------"
curl -s -X POST "$BASE_URL/tools/batch" \
  -H "Content-Type: application/json" \
  -d '{
    "calls": [
      {
        "name": "ggen_template_list",
        "arguments": {}
      },
      {
        "name": "ggen_market_categories",
        "arguments": {}
      }
    ]
  }' | jq
echo ""

# Example 6: Health check
echo "❤️  Example 6: Health Check"
echo "---------------------------"
curl -s "$BASE_URL/health" | jq
echo ""

echo "✅ All examples completed successfully!"
