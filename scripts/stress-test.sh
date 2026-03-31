#!/usr/bin/env bash
set -euo pipefail

# scripts/stress-test.sh
# Run stress tests to validate system stability under load

echo "### Stress Tests"
echo ""

FAILED=0

# Test 1: Concurrent ggen sync operations
echo "#### Test 1: Concurrent sync (10 parallel)"
TEST_DIR=$(mktemp -d)
mkdir -p "$TEST_DIR/examples"

# Create 10 minimal examples
for i in {1..10}; do
    mkdir -p "$TEST_DIR/examples/example_$i"
    cat > "$TEST_DIR/examples/example_$i/ggen.toml" << EOF
[generation]
output_dir = "output"
[[generation.rules]]
name = "test"
query = { inline = "SELECT ?s WHERE { ?s a ?o } LIMIT 1" }
template = { inline = "{{ sparql_results }}", output_file = "test.txt" }
EOF

    cat > "$TEST_DIR/examples/example_$i/ontology.ttl" << EOF
@prefix ex: <http://example.org/> .
ex:entity$i a ex:Entity .
EOF
done

# Run all 10 in parallel
if parallel --jobs 10 ::: \
    "cd $TEST_DIR/examples/example_1 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_2 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_3 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_4 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_5 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_6 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_7 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_8 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_9 && timeout 30s ggen sync" \
    "cd $TEST_DIR/examples/example_10 && timeout 30s ggen sync" \
    > /dev/null 2>&1; then
    echo "✅ PASSED: 10 concurrent sync operations"
else
    echo "❌ FAILED: concurrent sync operations"
    ((FAILED++))
fi

rm -rf "$TEST_DIR"

# Test 2: Large ontology processing (10k triples)
echo ""
echo "#### Test 2: Large ontology (10k triples)"
TEST_DIR=$(mktemp -d)
cat > "$TEST_DIR/large.ttl" << 'EOF'
@prefix ex: <http://example.org/> .
EOF

for i in {1..10000}; do
    echo "ex:entity$i a ex:Entity ; ex:id \"$i\" ; ex:related ex:entity$((i % 100 + 1)) ." >> "$TEST_DIR/large.ttl"
done

cat > "$TEST_DIR/ggen.toml" << EOF
[generation]
output_dir = "output"
[[generation.rules]]
name = "test"
query = { inline = "SELECT ?s WHERE { ?s a ex:Entity } LIMIT 100" }
template = { inline = "{{ sparql_results }}", output_file = "test.txt" }
EOF

START=$(date +%s)
if (cd "$TEST_DIR" && timeout 60s ggen sync > /dev/null 2>&1); then
    END=$(date +%s)
    ELAPSED=$((END - START))
    echo "✅ PASSED: 10k triples processed in ${ELAPSED}s"
else
    echo "❌ FAILED: large ontology processing"
    ((FAILED++))
fi

rm -rf "$TEST_DIR"

# Test 3: Memory stress (repeated operations)
echo ""
echo "#### Test 3: Memory stability (100 iterations)"
TEST_DIR=$(mktemp -d)
mkdir -p "$TEST_DIR/test"

cat > "$TEST_DIR/test/ggen.toml" << EOF
[generation]
output_dir = "output"
[[generation.rules]]
name = "test"
query = { inline = "SELECT ?s WHERE { ?s a ?o }" }
template = { inline = "{{ sparql_results }}", output_file = "test.txt" }
EOF

cat > "$TEST_DIR/test/ontology.ttl" << EOF
@prefix ex: <http://example.org/> .
ex:entity a ex:Entity .
EOF

for i in {1..100}; do
    if ! (cd "$TEST_DIR/test" && timeout 5s ggen sync > /dev/null 2>&1); then
        echo "❌ FAILED: iteration $i"
        ((FAILED++))
        break
    fi
done

if [[ $i -eq 100 ]]; then
    echo "✅ PASSED: 100 iterations completed"
fi

rm -rf "$TEST_DIR"

echo ""
if [[ $FAILED -gt 0 ]]; then
    echo "**Stress Tests:** ❌ $FAILED failed"
    exit 1
else
    echo "**Stress Tests:** ✅ All passed"
    exit 0
fi
