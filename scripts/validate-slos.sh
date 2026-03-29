#!/usr/bin/env bash
set -euo pipefail

# scripts/validate-slos.sh
# Verify performance SLOs from .claude/rules/rust/performance.md

echo "### Validating Performance SLOs"
echo ""

SLO_FAILED=0

# SLO 1: First build ≤15s
echo "#### SLO 1: First build time (≤15s)"
if [[ ! -d target ]]; then
    START=$(date +%s)
    if timeout 30s cargo check --workspace 2>&1 | grep -q "Finished"; then
        END=$(date +%s)
        ELAPSED=$((END - START))
        if [[ $ELAPSED -le 15 ]]; then
            echo "✅ PASSED: ${ELAPSED}s"
        else
            echo "❌ FAILED: ${ELAPSED}s (exceeds 15s)"
            ((SLO_FAILED++))
        fi
    else
        echo "❌ FAILED: build did not complete"
        ((SLO_FAILED++))
    fi
else
    echo "⚠️  SKIPPED: target/ exists (not a clean build)"
fi

# SLO 2: Incremental build ≤2s
echo ""
echo "#### SLO 2: Incremental build time (≤2s)"
START=$(date +%s)
if timeout 10s cargo check --workspace 2>&1 | grep -q "Finished"; then
    END=$(date +%s)
    ELAPSED=$((END - START))
    if [[ $ELAPSED -le 2 ]]; then
        echo "✅ PASSED: ${ELAPSED}s"
    else
        echo "❌ FAILED: ${ELAPSED}s (exceeds 2s)"
        ((SLO_FAILED++))
    fi
else
    echo "❌ FAILED: build did not complete"
    ((SLO_FAILED++))
fi

# SLO 3: Test suite <30s
echo ""
echo "#### SLO 3: Test suite time (<30s)"
START=$(date +%s)
if timeout 60s cargo test --workspace --tests --lib 2>&1 | grep -q "test result"; then
    END=$(date +%s)
    ELAPSED=$((END - START))
    if [[ $ELAPSED -lt 30 ]]; then
        echo "✅ PASSED: ${ELAPSED}s"
    else
        echo "❌ FAILED: ${ELAPSED}s (exceeds 30s)"
        ((SLO_FAILED++))
    fi
else
    echo "❌ FAILED: tests did not complete"
    ((SLO_FAILED++))
fi

# SLO 4: RDF processing ≤5s per 1k triples
echo ""
echo "#### SLO 4: RDF processing (≤5s/1k triples)"
# Create test ontology with 1000 triples
TEST_DIR=$(mktemp -d)
cat > "$TEST_DIR/test.ttl" << 'EOF'
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
EOF

# Generate 1000 triples
for i in {1..1000}; do
    echo "ex:entity$i rdf:type ex:Entity ; ex:id \"$i\" ." >> "$TEST_DIR/test.ttl"
done

cat > "$TEST_DIR/ggen.toml" << EOF
[generation]
output_dir = "output"
[[generation.rules]]
name = "test"
query = { inline = "SELECT ?s WHERE { ?s a ?o } LIMIT 10" }
template = { inline = "{{ sparql_results }}", output_file = "test.txt" }
EOF

START=$(date +%s)
if (cd "$TEST_DIR" && timeout 10s ggen sync > /dev/null 2>&1); then
    END=$(date +%s)
    ELAPSED=$((END - START))
    if [[ $ELAPSED -le 5 ]]; then
        echo "✅ PASSED: ${ELAPSED}s for 1000 triples"
    else
        echo "❌ FAILED: ${ELAPSED}s (exceeds 5s)"
        ((SLO_FAILED++))
    fi
else
    echo "❌ FAILED: sync did not complete"
    ((SLO_FAILED++))
fi

rm -rf "$TEST_DIR"

echo ""
if [[ $SLO_FAILED -gt 0 ]]; then
    echo "**SLO Compliance:** ❌ $SLO_FAILED failed"
    exit 1
else
    echo "**SLO Compliance:** ✅ All passed"
    exit 0
fi
