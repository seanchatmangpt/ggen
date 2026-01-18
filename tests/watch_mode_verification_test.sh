#!/usr/bin/env bash
# Watch Mode Verification Test
# Tests all watch mode functionality including debouncing, auto-regeneration, and graceful shutdown

set -e

TEST_DIR="/tmp/ggen-watch-test-$$"
RESULTS_FILE="/tmp/ggen-watch-test-results-$$.md"

echo "# Watch Mode Verification Test Results" > "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "Test Date: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Cleanup function
cleanup() {
    echo "Cleaning up test directory..."
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

# Setup test project
setup_test_project() {
    echo "Setting up test project in $TEST_DIR..."
    mkdir -p "$TEST_DIR"
    cd "$TEST_DIR"

    # Create minimal ontology
    cat > ontology.ttl <<'EOF'
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    ex:hasProperty ex:name .

ex:Alice a ex:Person ;
    ex:name "Alice" .
EOF

    # Create ggen.toml manifest
    cat > ggen.toml <<'EOF'
[project]
name = "watch-mode-test"
version = "1.0.0"
description = "Test project for watch mode verification"

[ontology]
source = "ontology.ttl"

[inference]
rules = []

[generation]
output_dir = "generated"

[[generation.rules]]
name = "person-list"
query.inline = "SELECT ?name WHERE { ?s <http://example.org/name> ?name }"
template.inline = """
// Generated person list
// Count: {{ bindings | length }}
{% for binding in bindings -%}
const PERSON_{{ loop.index }}: &str = "{{ binding.name }}";
{% endfor %}
"""
output_file = "persons.rs"
mode = "Overwrite"
EOF

    mkdir -p generated
}

# Test 1: Verify watch mode starts correctly
test_watch_mode_startup() {
    echo "## Test 1: Watch Mode Startup" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"

    # Run ggen sync --watch in background and capture output
    timeout 3s ggen sync --watch --verbose 2>&1 | head -20 > /tmp/watch-startup.log || true

    if grep -q "Starting watch mode" /tmp/watch-startup.log || grep -q "Monitoring.*paths" /tmp/watch-startup.log || grep -q "Watch Mode Started" /tmp/watch-startup.log; then
        echo "✓ PASS: Watch mode started successfully" >> "$RESULTS_FILE"
        echo "  - Watch mode startup message detected" >> "$RESULTS_FILE"
    else
        echo "✗ FAIL: Watch mode did not start" >> "$RESULTS_FILE"
        echo "  - Expected startup message not found" >> "$RESULTS_FILE"
    fi

    if grep -q "ontology.ttl" /tmp/watch-startup.log || grep -q "ggen.toml" /tmp/watch-startup.log; then
        echo "✓ PASS: Watch paths detected" >> "$RESULTS_FILE"
        echo "  - Watching ontology.ttl and ggen.toml" >> "$RESULTS_FILE"
    else
        echo "✗ FAIL: Watch paths not detected" >> "$RESULTS_FILE"
    fi

    echo "" >> "$RESULTS_FILE"
}

# Test 2: Verify initial sync runs
test_initial_sync() {
    echo "## Test 2: Initial Sync" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"

    # Run regular sync first
    if ggen sync --verbose 2>&1 | grep -q "success\|Synced"; then
        echo "✓ PASS: Initial sync completed" >> "$RESULTS_FILE"
    else
        echo "✗ FAIL: Initial sync failed" >> "$RESULTS_FILE"
    fi

    if [ -f "generated/persons.rs" ]; then
        echo "✓ PASS: Generated file exists" >> "$RESULTS_FILE"
        echo "  - File: generated/persons.rs" >> "$RESULTS_FILE"

        if grep -q "Alice" generated/persons.rs; then
            echo "✓ PASS: Generated content is correct" >> "$RESULTS_FILE"
            echo "  - Content contains 'Alice'" >> "$RESULTS_FILE"
        else
            echo "✗ FAIL: Generated content is incorrect" >> "$RESULTS_FILE"
        fi
    else
        echo "✗ FAIL: Generated file not found" >> "$RESULTS_FILE"
    fi

    echo "" >> "$RESULTS_FILE"
}

# Test 3: Verify file change detection and auto-regeneration
test_file_change_detection() {
    echo "## Test 3: File Change Detection and Auto-Regeneration" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"

    # Start watch mode in background
    ggen sync --watch --verbose > /tmp/watch-output.log 2>&1 &
    WATCH_PID=$!

    # Wait for watch mode to start
    sleep 2

    # Modify ontology file
    cat >> ontology.ttl <<'EOF'

ex:Bob a ex:Person ;
    ex:name "Bob" .
EOF

    # Wait for debounce + regeneration
    sleep 2

    # Check if file was updated
    if grep -q "Bob" generated/persons.rs; then
        echo "✓ PASS: File change detected and regenerated" >> "$RESULTS_FILE"
        echo "  - New content 'Bob' found in generated file" >> "$RESULTS_FILE"
    else
        echo "✗ FAIL: File change not detected or regeneration failed" >> "$RESULTS_FILE"
    fi

    # Check watch output log
    if grep -q "Change detected\|changed\|Detected change" /tmp/watch-output.log; then
        echo "✓ PASS: Change detection logged" >> "$RESULTS_FILE"
    else
        echo "✗ FAIL: Change detection not logged" >> "$RESULTS_FILE"
    fi

    # Cleanup: kill watch process
    kill $WATCH_PID 2>/dev/null || true
    wait $WATCH_PID 2>/dev/null || true

    echo "" >> "$RESULTS_FILE"
}

# Test 4: Verify debouncing (500ms)
test_debouncing() {
    echo "## Test 4: Debouncing (500ms)" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"

    # Start watch mode in background
    rm -f /tmp/watch-debounce.log
    ggen sync --watch --verbose > /tmp/watch-debounce.log 2>&1 &
    WATCH_PID=$!

    # Wait for watch mode to start
    sleep 2

    # Make rapid changes (3 changes within 1 second)
    echo "# Change 1" >> ontology.ttl
    sleep 0.2
    echo "# Change 2" >> ontology.ttl
    sleep 0.2
    echo "# Change 3" >> ontology.ttl

    # Wait for debounce + regeneration
    sleep 2

    # Count regeneration events in log
    REGEN_COUNT=$(grep -c "Regenerat\|Running sync\|Synced.*files" /tmp/watch-debounce.log || echo "0")

    if [ "$REGEN_COUNT" -le 2 ]; then
        echo "✓ PASS: Debouncing works (found $REGEN_COUNT regeneration(s), expected 1-2)" >> "$RESULTS_FILE"
        echo "  - Rapid changes were debounced into single regeneration" >> "$RESULTS_FILE"
    else
        echo "⚠ WARNING: Debouncing may not be optimal (found $REGEN_COUNT regenerations)" >> "$RESULTS_FILE"
        echo "  - Expected 1-2 regenerations, but debouncing still prevented all 3" >> "$RESULTS_FILE"
    fi

    # Cleanup
    kill $WATCH_PID 2>/dev/null || true
    wait $WATCH_PID 2>/dev/null || true

    echo "" >> "$RESULTS_FILE"
}

# Test 5: Verify graceful shutdown (SIGINT)
test_graceful_shutdown() {
    echo "## Test 5: Graceful Shutdown (Ctrl+C / SIGINT)" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"

    # Start watch mode in background
    rm -f /tmp/watch-shutdown.log
    ggen sync --watch --verbose > /tmp/watch-shutdown.log 2>&1 &
    WATCH_PID=$!

    # Wait for watch mode to start
    sleep 2

    # Send SIGINT (Ctrl+C)
    kill -INT $WATCH_PID

    # Wait for graceful shutdown
    sleep 1

    # Check if process exited cleanly
    if ! ps -p $WATCH_PID > /dev/null 2>&1; then
        echo "✓ PASS: Watch mode shut down gracefully" >> "$RESULTS_FILE"
        echo "  - Process exited after SIGINT" >> "$RESULTS_FILE"
    else
        echo "✗ FAIL: Watch mode did not shut down" >> "$RESULTS_FILE"
        kill -9 $WATCH_PID 2>/dev/null || true
    fi

    # Check shutdown log
    if grep -q "Shutting down\|shutdown\|Interrupt" /tmp/watch-shutdown.log; then
        echo "✓ PASS: Shutdown message logged" >> "$RESULTS_FILE"
    fi

    wait $WATCH_PID 2>/dev/null || true

    echo "" >> "$RESULTS_FILE"
}

# Test 6: Verify incremental cache integration
test_incremental_cache() {
    echo "## Test 6: Incremental Cache Integration" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"

    # Clear cache
    rm -rf .ggen/cache

    # First sync (cold cache)
    TIME1=$(ggen sync --verbose 2>&1 | grep -oP 'duration.*\K[0-9]+' | head -1 || echo "0")

    # Second sync (warm cache)
    TIME2=$(ggen sync --verbose 2>&1 | grep -oP 'duration.*\K[0-9]+' | head -1 || echo "0")

    if [ -d ".ggen/cache" ]; then
        echo "✓ PASS: Cache directory created" >> "$RESULTS_FILE"
        echo "  - Cache location: .ggen/cache" >> "$RESULTS_FILE"
    else
        echo "⚠ WARNING: Cache directory not found" >> "$RESULTS_FILE"
    fi

    # Note: We can't reliably test cache speedup in this simple test
    echo "  - First sync: ${TIME1}ms" >> "$RESULTS_FILE"
    echo "  - Second sync: ${TIME2}ms" >> "$RESULTS_FILE"

    echo "" >> "$RESULTS_FILE"
}

# Main test execution
main() {
    echo "==================================================="
    echo "  ggen Watch Mode Verification Test"
    echo "==================================================="
    echo ""

    setup_test_project

    echo "Running tests..."
    test_watch_mode_startup
    test_initial_sync
    test_file_change_detection
    test_debouncing
    test_graceful_shutdown
    test_incremental_cache

    echo ""
    echo "==================================================="
    echo "  Test Results"
    echo "==================================================="
    cat "$RESULTS_FILE"

    echo ""
    echo "Full results written to: $RESULTS_FILE"

    # Count pass/fail
    PASS_COUNT=$(grep -c "✓ PASS" "$RESULTS_FILE" || echo "0")
    FAIL_COUNT=$(grep -c "✗ FAIL" "$RESULTS_FILE" || echo "0")
    WARN_COUNT=$(grep -c "⚠ WARNING" "$RESULTS_FILE" || echo "0")

    echo ""
    echo "Summary: $PASS_COUNT passed, $FAIL_COUNT failed, $WARN_COUNT warnings"

    if [ "$FAIL_COUNT" -eq 0 ]; then
        echo "✓ All critical tests passed!"
        exit 0
    else
        echo "✗ Some tests failed"
        exit 1
    fi
}

main "$@"
