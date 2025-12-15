#!/usr/bin/env bash
set -euo pipefail

# E2E Test Script for ggen v2.0 Template Generation
# This script validates the complete workflow: RDF -> Template -> Working Rust Project

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="$SCRIPT_DIR/output"
TEMPLATE="$SCRIPT_DIR/templates/template.hbs"
RDF="$SCRIPT_DIR/project.ttl"

echo "=========================================="
echo "ggen v2.0 E2E Test: Template + TTL"
echo "=========================================="
echo ""

# Clean output directory
echo "[1/6] Cleaning output directory..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"
echo "✓ Output directory ready: $OUTPUT_DIR"
echo ""

# Validate input files exist
echo "[2/6] Validating input files..."
if [[ ! -f "$TEMPLATE" ]]; then
    echo "✗ Template not found: $TEMPLATE"
    exit 1
fi
if [[ ! -f "$RDF" ]]; then
    echo "✗ RDF file not found: $RDF"
    exit 1
fi
echo "✓ Template found: $TEMPLATE"
echo "✓ RDF found: $RDF"
echo ""

# Generate project using ggen
echo "[3/6] Generating project from template + RDF..."
cd "$OUTPUT_DIR"

# Note: Adjust this command based on actual ggen v2.0 CLI syntax
# Assuming: ggen template generate <template> --rdf <rdf-file> --output <dir>
if command -v ggen &> /dev/null; then
    ggen template generate "$TEMPLATE" --rdf "$RDF" --output .
    GENERATION_STATUS=$?
else
    echo "⚠ ggen command not found, using cargo run..."
    cargo run --manifest-path "$SCRIPT_DIR/../../Cargo.toml" -- \
        template generate "$TEMPLATE" --rdf "$RDF" --output .
    GENERATION_STATUS=$?
fi

if [[ $GENERATION_STATUS -ne 0 ]]; then
    echo "✗ Generation failed"
    exit 1
fi
echo "✓ Project generated successfully"
echo ""

# Validate generated files
echo "[4/6] Validating generated files..."
EXPECTED_FILES=(
    "Cargo.toml"
    "src/lib.rs"
    "README.md"
)

for file in "${EXPECTED_FILES[@]}"; do
    if [[ ! -f "$file" ]]; then
        echo "✗ Missing file: $file"
        exit 1
    fi
    echo "✓ Found: $file"
done
echo ""

# Validate file contents
echo "[5/6] Validating file contents..."

# Check Cargo.toml
if grep -q 'name = "calculator"' Cargo.toml && \
   grep -q 'version = "0.1.0"' Cargo.toml && \
   grep -q 'edition = "2021"' Cargo.toml && \
   grep -q 'serde' Cargo.toml && \
   grep -q 'thiserror' Cargo.toml; then
    echo "✓ Cargo.toml contains correct metadata and dependencies"
else
    echo "✗ Cargo.toml validation failed"
    echo "Contents:"
    cat Cargo.toml
    exit 1
fi

# Check src/lib.rs
if grep -q 'pub mod error' src/lib.rs && \
   grep -q 'pub mod operations' src/lib.rs && \
   grep -q 'CalcError' src/lib.rs && \
   grep -q 'fn add' src/lib.rs && \
   grep -q 'fn divide' src/lib.rs; then
    echo "✓ src/lib.rs contains correct modules and functions"
else
    echo "✗ src/lib.rs validation failed"
    echo "Contents:"
    cat src/lib.rs
    exit 1
fi

# Check README.md
if grep -q '# calculator' README.md && \
   grep -q 'operations' README.md && \
   grep -q 'MIT' README.md; then
    echo "✓ README.md contains correct documentation"
else
    echo "✗ README.md validation failed"
    exit 1
fi
echo ""

# Compile the generated project
echo "[6/6] Compiling generated Rust project..."
if cargo build 2>&1 | tee build.log; then
    echo "✓ Project compiled successfully"
else
    echo "✗ Compilation failed"
    echo "Build log:"
    cat build.log
    exit 1
fi

# Run tests
echo ""
echo "Running tests..."
if cargo test 2>&1 | tee test.log; then
    echo "✓ All tests passed"
else
    echo "✗ Tests failed"
    echo "Test log:"
    cat test.log
    exit 1
fi

echo ""
echo "=========================================="
echo "✓ E2E Test PASSED!"
echo "=========================================="
echo ""
echo "Summary:"
echo "  - RDF parsed: project.ttl"
echo "  - Template processed: template.hbs"
echo "  - Files generated: ${#EXPECTED_FILES[@]}"
echo "  - Compilation: SUCCESS"
echo "  - Tests: PASSED"
echo ""
echo "Generated project location:"
echo "  $OUTPUT_DIR"
echo ""
