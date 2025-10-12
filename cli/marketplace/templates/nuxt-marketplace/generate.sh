#!/bin/bash
# Graph-Driven Nuxt Marketplace Generation Script
# This demonstrates the CORRECT way to generate Nuxt artifacts from RDF graphs

set -e

echo "🚀 Generating Nuxt Marketplace from RDF Knowledge Graph..."
echo ""

# Ensure we're in the right directory
cd "$(dirname "$0")"

# Create output directory
OUTPUT_DIR="./generated"
mkdir -p "$OUTPUT_DIR"

echo "📊 Step 1: Validating RDF graphs..."
# Validate the RDF graphs exist
if [ ! -f "graphs/nuxt-ontology.ttl" ] || [ ! -f "graphs/marketplace-project.ttl" ]; then
    echo "❌ Error: RDF graph files not found!"
    exit 1
fi
echo "✅ RDF graphs validated"
echo ""

echo "🔍 Step 2: Querying graph for pages..."
# Generate pages from graph
ggen gen templates/page.tmpl \
  --vars page_name=index \
  --output "$OUTPUT_DIR" \
  --graph graphs/nuxt-ontology.ttl \
  --graph graphs/marketplace-project.ttl

ggen gen templates/page.tmpl \
  --vars page_name="package-[id]" \
  --output "$OUTPUT_DIR" \
  --graph graphs/nuxt-ontology.ttl \
  --graph graphs/marketplace-project.ttl

echo "✅ Pages generated from graph"
echo ""

echo "🧩 Step 3: Querying graph for components..."
# Generate components from graph
ggen gen templates/component.tmpl \
  --vars component_name=PackageCard \
  --output "$OUTPUT_DIR" \
  --graph graphs/nuxt-ontology.ttl \
  --graph graphs/marketplace-project.ttl

ggen gen templates/component.tmpl \
  --vars component_name=PackageGrid \
  --output "$OUTPUT_DIR" \
  --graph graphs/nuxt-ontology.ttl \
  --graph graphs/marketplace-project.ttl

echo "✅ Components generated from graph"
echo ""

echo "🎣 Step 4: Querying graph for composables..."
# Generate composables from graph
ggen gen templates/composable.tmpl \
  --vars composable_name=usePackages \
  --output "$OUTPUT_DIR" \
  --graph graphs/nuxt-ontology.ttl \
  --graph graphs/marketplace-project.ttl

ggen gen templates/composable.tmpl \
  --vars composable_name=usePackage \
  --output "$OUTPUT_DIR" \
  --graph graphs/nuxt-ontology.ttl \
  --graph graphs/marketplace-project.ttl

ggen gen templates/composable.tmpl \
  --vars composable_name=useSearch \
  --output "$OUTPUT_DIR" \
  --graph graphs/nuxt-ontology.ttl \
  --graph graphs/marketplace-project.ttl

echo "✅ Composables generated from graph"
echo ""

echo "🎉 Generation complete! All artifacts created from RDF knowledge graph."
echo "📁 Output directory: $OUTPUT_DIR"
echo ""
echo "📖 What just happened:"
echo "  1. RDF graphs defined the semantic structure of Nuxt artifacts"
echo "  2. SPARQL queries extracted metadata from the graph"
echo "  3. Templates rendered Vue/TypeScript code using graph data"
echo "  4. Result: Fully typed, deterministic Nuxt application"
echo ""
echo "🔄 To regenerate: bash generate.sh"
