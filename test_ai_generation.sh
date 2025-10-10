#!/bin/bash

# Test script for ggen-ai template generation
set -e

echo "🚀 Testing ggen-ai template generation..."

# Create test output directory
mkdir -p test_output

# Test 1: Generate Rust service template using AI
echo "📝 Generating Rust service template with AI..."
cargo run --bin ggen -- ai generate \
  --description "A REST API service for managing products with CRUD operations" \
  --examples "Include CRUD operations" \
  --examples "Use Rust Axum framework" \
  --language rust \
  --framework axum \
  --output test_output/product_service.tmpl

# Test 2: Generate SPARQL queries using AI
echo "🔍 Generating SPARQL queries with AI..."
cargo run --bin ggen -- ai generate \
  --description "SPARQL queries for product management operations" \
  --examples "Include SELECT queries" \
  --examples "Use proper RDF prefixes" \
  --language sparql \
  --output test_output/product_queries.tmpl

# Test 3: Generate ontology using AI
echo "🏗️ Generating ontology with AI..."
cargo run --bin ggen -- ai generate \
  --description "Product catalog domain ontology with products, categories, and inventory" \
  --examples "Include classes and properties" \
  --examples "Use Turtle format" \
  --language turtle \
  --output test_output/product_ontology.tmpl

# Test 4: Test MCP server (if available)
echo "🤖 Testing MCP server..."
if command -v ggen-ai-mcp &> /dev/null; then
    echo "MCP server binary found, testing..."
    # Note: This would require a proper MCP client to test
    echo "MCP server test requires MCP client - skipping for now"
else
    echo "MCP server binary not found, building..."
    cargo build --bin ggen-ai-mcp
fi

# Test 5: Generate with different variables using AI
echo "🔄 Testing AI template variations..."

# Generate Python service
cargo run --bin ggen -- ai generate \
  --description "A Python FastAPI service for user management" \
  --examples "Include user authentication" \
  --examples "Use FastAPI framework" \
  --language python \
  --framework fastapi \
  --output test_output/user_api.tmpl

# Generate TypeScript service  
cargo run --bin ggen -- ai generate \
  --description "A TypeScript Express service for order management" \
  --examples "Include order processing" \
  --examples "Use Express framework" \
  --language typescript \
  --framework express \
  --output test_output/order_service.tmpl

echo "✅ All tests completed! Check test_output/ directory for generated files."

# Show generated files
echo "📁 Generated files:"
ls -la test_output/

echo ""
echo "🎉 ggen-ai template generation test completed successfully!"
