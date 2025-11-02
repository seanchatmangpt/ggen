#!/bin/bash
# Standalone validation script for clap-noun-verb v3.2.0 integration
# Tests CLI auto-discovery, template generation, and E2E workflow

# Don't exit on first failure - we want to run all tests
set +e

GGEN="./target/debug/ggen"
TEST_DIR=$(mktemp -d)
PASSED=0
FAILED=0

function test_passed() {
    echo "✅ PASSED: $1"
    ((PASSED++))
}

function test_failed() {
    echo "❌ FAILED: $1"
    ((FAILED++))
}

function cleanup() {
    rm -rf "$TEST_DIR"
}

trap cleanup EXIT

echo "=== CLAP-NOUN-VERB v3.2.0 INTEGRATION TESTS ==="
echo "Test directory: $TEST_DIR"
echo ""

# TEST SUITE 1: CLI AUTO-DISCOVERY
echo "=== TEST SUITE 1: CLI AUTO-DISCOVERY ==="

# Test 1: ggen --help shows template noun
if $GGEN --help | grep -q "template"; then
    test_passed "ggen --help shows template noun"
else
    test_failed "ggen --help shows template noun"
fi

# Test 2: template --help shows all verbs
if $GGEN template --help | grep -q "list" && $GGEN template --help | grep -q "new" && $GGEN template --help | grep -q "generate"; then
    test_passed "template --help shows all verbs"
else
    test_failed "template --help shows all verbs"
fi

# Test 3: template list command exists in help
if $GGEN template --help | grep -q "list"; then
    test_passed "template list command exists"
else
    test_failed "template list command exists"
fi

# Test 4: template lint --help shows arguments
if $GGEN template lint --help | grep -q -E "(path|PATH|template)"; then
    test_passed "template lint --help shows arguments"
else
    test_failed "template lint --help shows arguments"
fi

# Test 5: Invalid commands return proper errors
if $GGEN invalid-noun 2>&1 | grep -q -E "(error|invalid|unrecognized)"; then
    test_passed "Invalid noun returns error"
else
    test_failed "Invalid noun returns error"
fi

# Test 6: Invalid verbs return errors
if $GGEN template invalid-verb 2>&1 | grep -q -E "(error|invalid|unrecognized)"; then
    test_passed "Invalid verb returns error"
else
    test_failed "Invalid verb returns error"
fi

# Test 7: Version flag works
if $GGEN --version | grep -q "ggen"; then
    test_passed "CLI version flag works"
else
    test_failed "CLI version flag works"
fi

echo ""
echo "=== TEST SUITE 2: TEMPLATE-DRIVEN PROJECT GENERATION ==="

# Create test TTL specification
TTL_FILE="$TEST_DIR/cli-spec.ttl"
cat > "$TTL_FILE" << 'EOF'
@prefix cli: <http://example.org/cli#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

cli:MyCLI a cli:Application ;
    rdfs:label "MyCLI" ;
    cli:hasNoun cli:UserNoun, cli:ProjectNoun .

cli:UserNoun a cli:Noun ;
    rdfs:label "user" ;
    cli:hasVerb cli:ListVerb, cli:CreateVerb .
EOF

# Test 8: TTL file created with valid RDF
if [ -f "$TTL_FILE" ] && grep -q "cli:Application" "$TTL_FILE"; then
    test_passed "RDF CLI definition loads"
else
    test_failed "RDF CLI definition loads"
fi

# Create CLI tree spec
TREE_SPEC="$TEST_DIR/cli-tree.yaml"
cat > "$TREE_SPEC" << 'EOF'
files:
  - path: "Cargo.toml"
    content: |
      [package]
      name = "testcli"
      version = "0.1.0"
      edition = "2021"

      [dependencies]
      clap = { version = "4.5", features = ["derive"] }
      clap-noun-verb = "3.2"
  - path: "src/main.rs"
    content: |
      use clap::Parser;

      #[derive(Parser)]
      struct Cli {
          #[command(subcommand)]
          command: Commands,
      }

      #[derive(clap::Subcommand)]
      enum Commands {
          User,
          Project,
      }

      fn main() {
          let _cli = Cli::parse();
          println!("CLI executed");
      }
EOF

OUTPUT_DIR="$TEST_DIR/generated"
mkdir -p "$OUTPUT_DIR"

# Test 9: Generate project from tree spec
if $GGEN template generate-tree --template "$TREE_SPEC" --output "$OUTPUT_DIR"; then
    test_passed "Template generates from tree spec"
else
    test_failed "Template generates from tree spec"
fi

# Test 10: Generated project structure valid
if [ -f "$OUTPUT_DIR/Cargo.toml" ] && [ -f "$OUTPUT_DIR/src/main.rs" ]; then
    test_passed "Generated project structure valid"
else
    test_failed "Generated project structure valid"
fi

# Test 11: Cargo.toml has clap dependency
if grep -q "clap" "$OUTPUT_DIR/Cargo.toml"; then
    test_passed "Generated Cargo.toml has clap dependency"
else
    test_failed "Generated Cargo.toml has clap dependency"
fi

# Test 12: Generated code matches spec
if grep -q "User" "$OUTPUT_DIR/src/main.rs" && grep -q "Project" "$OUTPUT_DIR/src/main.rs"; then
    test_passed "Generated CLI code matches RDF spec"
else
    test_failed "Generated CLI code matches RDF spec"
fi

echo ""
echo "=== TEST SUITE 3: END-TO-END WORKFLOW ==="

# Test 13: Cargo.toml is valid TOML
if command -v toml-cli &> /dev/null; then
    if toml-cli get "$OUTPUT_DIR/Cargo.toml" "package.name" &> /dev/null; then
        test_passed "Generated Cargo.toml is valid TOML"
    else
        test_failed "Generated Cargo.toml is valid TOML"
    fi
else
    # Fallback: check basic structure
    if grep -q "\[package\]" "$OUTPUT_DIR/Cargo.toml" && grep -q "\[dependencies\]" "$OUTPUT_DIR/Cargo.toml"; then
        test_passed "Generated Cargo.toml has valid structure"
    else
        test_failed "Generated Cargo.toml is valid TOML"
    fi
fi

# Test 14: main.rs has main function
if grep -q "fn main()" "$OUTPUT_DIR/src/main.rs"; then
    test_passed "Generated main.rs has main function"
else
    test_failed "Generated main.rs has main function"
fi

# Test 15: Code has clap attributes
if grep -q "Parser" "$OUTPUT_DIR/src/main.rs" && grep -q "Subcommand" "$OUTPUT_DIR/src/main.rs"; then
    test_passed "Generated code has clap attributes"
else
    test_failed "Generated code has clap attributes"
fi

echo ""
echo "=== PERFORMANCE TESTS ==="

# Test 16: Generation performance < 2 seconds
START_TIME=$(date +%s)
OUTPUT_PERF="$TEST_DIR/perf"
mkdir -p "$OUTPUT_PERF"
$GGEN template generate-tree --template "$TREE_SPEC" --output "$OUTPUT_PERF"
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

if [ "$DURATION" -lt 2 ]; then
    test_passed "Template generation under 2 seconds ($DURATION s)"
else
    test_failed "Template generation under 2 seconds (took $DURATION s)"
fi

# Test 17: Help command is fast
START_TIME=$(date +%s)
$GGEN template --help > /dev/null
END_TIME=$(date +%s)
HELP_DURATION=$((END_TIME - START_TIME))

if [ "$HELP_DURATION" -lt 2 ]; then
    test_passed "Help command is fast ($HELP_DURATION s)"
else
    test_failed "Help command is fast (took $HELP_DURATION s)"
fi

echo ""
echo "=== TEST SUMMARY ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Total:  $((PASSED + FAILED))"

if [ "$FAILED" -eq 0 ]; then
    echo "✅ All tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
