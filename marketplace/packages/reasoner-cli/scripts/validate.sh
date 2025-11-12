#!/bin/bash
set -e

# Validation script for reasoner-cli

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "🔍 Validating reasoner-cli package..."

# Check required files
echo "📁 Checking required files..."
REQUIRED_FILES=(
    "rdf/ontology.ttl"
    "package.toml"
    "Cargo.toml"
    "README.md"
    "LICENSE-MIT"
    "LICENSE-APACHE"
    "src/main.rs"
    "src/lib.rs"
)

for file in "${REQUIRED_FILES[@]}"; do
    if [ ! -f "$file" ]; then
        echo "✗ Missing required file: $file"
        exit 1
    fi
    echo "✓ Found $file"
done

# Validate RDF ontology
echo "🔍 Validating RDF ontology..."
if grep -q "reasoner:Classifier" rdf/ontology.ttl && \
   grep -q "reasoner:Ontology" rdf/ontology.ttl && \
   grep -q "reasoner:Inference" rdf/ontology.ttl && \
   grep -q "reasoner:Validator" rdf/ontology.ttl; then
    echo "✓ Ontology contains all 4 nouns"
else
    echo "✗ Ontology missing required nouns"
    exit 1
fi

# Check for required verbs
REQUIRED_VERBS=("classify" "realize" "materialize" "load" "merge" "derive" "entail" "check" "validate")
for verb in "${REQUIRED_VERBS[@]}"; do
    if grep -q "reasoner:$verb" rdf/ontology.ttl; then
        echo "✓ Found verb: $verb"
    else
        echo "✗ Missing verb: $verb"
        exit 1
    fi
done

# Validate Cargo.toml
echo "📦 Validating Cargo.toml..."
if grep -q 'name = "reasoner-cli"' Cargo.toml && \
   grep -q 'clap' Cargo.toml && \
   grep -q 'oxigraph' Cargo.toml; then
    echo "✓ Cargo.toml is valid"
else
    echo "✗ Cargo.toml validation failed"
    exit 1
fi

# Check code compiles
echo "🔨 Checking code compilation..."
if cargo check --all-features; then
    echo "✓ Code compiles successfully"
else
    echo "✗ Compilation failed"
    exit 1
fi

# Run tests
echo "🧪 Running tests..."
if cargo test; then
    echo "✓ All tests passed"
else
    echo "✗ Tests failed"
    exit 1
fi

# Check documentation
echo "📚 Checking documentation..."
if [ $(wc -l < README.md) -ge 500 ]; then
    echo "✓ README.md is comprehensive ($(wc -l < README.md) lines)"
else
    echo "✗ README.md too short"
    exit 1
fi

echo "✅ All validations passed!"
