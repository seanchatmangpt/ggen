#!/bin/bash
set -e

# Benchmark script for reasoner-cli

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "📊 Running reasoner-cli benchmarks..."

# Build optimized binary
echo "🔨 Building optimized binary..."
cargo build --release --all-features

REASONER="$PROJECT_ROOT/target/release/reasoner"

# Create test ontologies
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo "📝 Creating test ontologies..."

# Small ontology (Wine ontology)
cat > "$TEMP_DIR/wine.ttl" << 'EOF'
@prefix : <http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:Wine a owl:Class .
:RedWine a owl:Class ; rdfs:subClassOf :Wine .
:WhiteWine a owl:Class ; rdfs:subClassOf :Wine .
:DryWine a owl:Class ; rdfs:subClassOf :Wine .
:SweetWine a owl:Class ; rdfs:subClassOf :Wine .
:RedDryWine a owl:Class ; rdfs:subClassOf :RedWine, :DryWine .
EOF

echo "⏱️  Benchmark 1: Small ontology classification"
time $REASONER classifier classify --ontology "$TEMP_DIR/wine.ttl" --profile DL 2>&1 | grep -E "(Classification|Time|Memory)"

echo ""
echo "⏱️  Benchmark 2: Consistency checking"
time $REASONER validator check --ontology "$TEMP_DIR/wine.ttl" 2>&1 | grep -E "(Consistent|Time)"

echo ""
echo "⏱️  Benchmark 3: Subsumption inference"
time $REASONER inference subsume --ontology "$TEMP_DIR/wine.ttl" 2>&1 | grep -E "(Result|Time)"

echo ""
echo "⏱️  Benchmark 4: Ontology loading"
time $REASONER ontology load --input "$TEMP_DIR/wine.ttl" 2>&1 | grep -E "(Loaded|Axioms|Time)"

echo ""
echo "✅ Benchmarks complete!"
echo "Results summary:"
echo "  - Classification: ✓"
echo "  - Consistency: ✓"
echo "  - Inference: ✓"
echo "  - Loading: ✓"
