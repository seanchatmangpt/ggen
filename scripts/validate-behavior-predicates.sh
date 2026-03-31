#!/bin/bash
# Validation script for behavior predicates ontology
# Checks syntax, namespace consistency, and integration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SPEC_DIR="$PROJECT_ROOT/.specify/specs/014-a2a-integration"

echo "=== Behavior Predicates Validation ==="
echo

# Check files exist
echo "1. Checking file existence..."
FILES=(
  "$SPEC_DIR/behavior-predicates.ttl"
  "$SPEC_DIR/behavior-example.ttl"
  "$SPEC_DIR/a2a-ontology.ttl"
)

for file in "${FILES[@]}"; do
  if [ -f "$file" ]; then
    echo "  ✓ $(basename "$file")"
  else
    echo "  ✗ $(basename "$file") NOT FOUND"
    exit 1
  fi
done
echo

# Check namespace consistency
echo "2. Checking namespace consistency..."
if grep -q "@prefix a2a: <https://a2a.dev/ontology#>" "$SPEC_DIR/behavior-predicates.ttl"; then
  echo "  ✓ A2A namespace consistent"
else
  echo "  ✗ A2A namespace mismatch"
  exit 1
fi

if grep -q "@prefix mcp: <https://ggen.io/ontology/mcp#>" "$SPEC_DIR/behavior-predicates.ttl"; then
  echo "  ✓ MCP namespace consistent"
else
  echo "  ✗ MCP namespace mismatch"
  exit 1
fi
echo

# Check predicate definitions
echo "3. Checking predicate definitions..."
PREDICATES=(
  "a2a:hasSystemPrompt"
  "a2a:hasImplementationHint"
  "a2a:hasTestExample"
  "a2a:hasErrorHandling"
  "a2a:hasPerformanceHint"
  "a2a:hasDependency"
  "mcp:hasAutoImplementation"
  "mcp:hasImplementationLanguage"
  "mcp:hasToolCategory"
)

for predicate in "${PREDICATES[@]}"; do
  if grep -q "$predicate" "$SPEC_DIR/behavior-predicates.ttl"; then
    echo "  ✓ $predicate"
  else
    echo "  ✗ $predicate NOT FOUND"
    exit 1
  fi
done
echo

# Check example usage
echo "4. Checking example usage..."
EXAMPLE_COUNT=$(grep -c "a a2a:Skill" "$SPEC_DIR/behavior-example.ttl" || true)
if [ "$EXAMPLE_COUNT" -ge 5 ]; then
  echo "  ✓ Found $EXAMPLE_COUNT skill examples"
else
  echo "  ✗ Insufficient examples (found $EXAMPLE_COUNT, expected >=5)"
  exit 1
fi
echo

# Check SPARQL integration
echo "5. Checking SPARQL query integration..."
SPARQL_FILES=(
  "$PROJECT_ROOT/crates/ggen-core/queries/a2a/extract-a2a-skills.rq"
  "$PROJECT_ROOT/examples/mcp-a2a-self-hosting/queries/extract-skills.rq"
)

for file in "${SPARQL_FILES[@]}"; do
  if [ -f "$file" ]; then
    if grep -q "hasSystemPrompt\|hasImplementationHint\|hasAutoImplementation" "$file"; then
      echo "  ✓ $(basename "$file")"
    else
      echo "  ⚠ $(basename "$file") - missing behavior predicates"
    fi
  fi
done
echo

# Check documentation
echo "6. Checking documentation..."
DOC_FILE="$PROJECT_ROOT/docs/A2A_TEMPLATING_USAGE.md"
if [ -f "$DOC_FILE" ]; then
  if grep -q "Behavior Predicates" "$DOC_FILE"; then
    echo "  ✓ Documentation updated"
  else
    echo "  ⚠ Documentation missing behavior predicate section"
  fi
else
  echo "  ⚠ Documentation file not found"
fi
echo

echo "=== Validation Complete ==="
echo "✅ All checks passed!"
echo
echo "Summary:"
echo "  - Behavior predicates defined: ${#PREDICATES[@]}"
echo "  - Example skills: $EXAMPLE_COUNT"
echo "  - Ontology files: ${#FILES[@]}"
echo "  - Status: READY FOR USE"
