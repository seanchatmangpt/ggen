#!/usr/bin/env bash
# Generate Erlang code from RDF ontology
set -euo pipefail

echo "=== Erlang Job Processor Code Generation ==="
echo ""

# Check if ggen is available
if ! command -v ggen &> /dev/null; then
    echo "Error: ggen not found. Please build ggen first:"
    echo "  cd ../../ && cargo build --release"
    echo "  export PATH=\$PATH:\$(pwd)/target/release"
    exit 1
fi

echo "Step 1: Validate RDF ontology..."
ggen sync --validate_only true

echo ""
echo "Step 2: Generate code (dry run)..."
ggen sync --dry_run true

echo ""
echo "Step 3: Generate code with audit trail..."
ggen sync --audit true

echo ""
echo "=== Generation Complete ==="
echo ""
echo "Generated files in: ./generated/"
echo ""
echo "Next steps:"
echo "  cd generated"
echo "  rebar3 compile"
echo "  rebar3 shell"
echo ""
