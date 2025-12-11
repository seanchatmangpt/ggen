#!/bin/bash
set -e

echo "=== ElectricSQL Schema to JSDoc Generation Test ==="
echo ""

# Navigate to project root
cd /Users/sac/ggen

echo "Step 1: Load Electric schema into RDF store"
cargo run --package ggen-cli-lib --bin ggen -- graph load \
  --file examples/electric-schema/electric-api.ttl

echo ""
echo "Step 2: Query loaded schema (verify triples)"
echo "Query: SELECT all triples (limit 10)"
cargo run --package ggen-cli-lib --bin ggen -- graph query \
  --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

echo ""
echo "Step 3: Query Electric classes"
cargo run --package ggen-cli-lib --bin ggen -- graph query \
  --sparql_query "SELECT ?class ?label WHERE { ?class a <http://www.w3.org/2002/07/owl#Class> . ?class <http://www.w3.org/2000/01/rdf-schema#label> ?label . }"

echo ""
echo "Step 4: Visualize graph structure (if available)"
cargo run --package ggen-cli-lib --bin ggen -- graph visualize \
  --input_file examples/electric-schema/electric-api.ttl \
  --output examples/electric-schema/electric-graph.svg \
  2>&1 || echo "Visualize command not fully implemented"

echo ""
echo "Step 5: Export graph to JSON for inspection"
cargo run --package ggen-cli-lib --bin ggen -- graph export \
  --input_file examples/electric-schema/electric-api.ttl \
  --output examples/electric-schema/electric-data.json \
  --format json

echo ""
echo "Step 6: Show exported JSON (first 50 lines)"
head -50 examples/electric-schema/electric-data.json 2>&1 || echo "JSON export not available"

echo ""
echo "=== Test Complete ==="
echo ""
echo "Next steps to fully validate:"
echo "1. Implement 'ggen generate' command to render Tera templates"
echo "2. Pass SPARQL query results as context to template"
echo "3. Verify JSDoc output matches expected format"
echo ""
echo "Expected workflow:"
echo "  ggen generate --schema electric-api.ttl \\"
echo "                --template jsdoc-api.tera \\"
echo "                --output electric-client.js"
