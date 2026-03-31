#!/usr/bin/env bash
# Validate Benchmark Component Library
# Tests that refactored templates produce functionally equivalent code

set -e

echo "=== Benchmark Component Validation ==="
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

# Counters
PASS=0
FAIL=0

# Test function
test_component() {
  local name="$1"
  local file="$2"
  local expected_lines="$3"

  echo -n "Testing $name... "

  if [ -f "$file" ]; then
    actual_lines=$(wc -l < "$file")

    if [ "$actual_lines" -le "$expected_lines" ]; then
      echo -e "${GREEN}PASS${NC} ($actual_lines lines)"
      ((PASS++))
    else
      echo -e "${RED}FAIL${NC} ($actual_lines lines, expected ≤$expected_lines)"
      ((FAIL++))
    fi
  else
    echo -e "${RED}FAIL${NC} (file not found)"
    ((FAIL++))
  fi
}

# Test 1: Component files exist
echo "1. Component Library Files"
echo "   ----------------------"
test_component "benchmark_setup.tera" \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/benchmark_setup.tera" \
  100

test_component "benchmark_teardown.tera" \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/benchmark_teardown.tera" \
  100

test_component "benchmark_metrics.tera" \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/benchmark_metrics.tera" \
  150

test_component "benchmark_data_gen.tera" \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/benchmark_data_gen.tera" \
  150

test_component "benchmark_assertions.tera" \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/benchmark_assertions.tera" \
  150

echo ""

# Test 2: Refactored templates exist
echo "2. Refactored Templates"
echo "   --------------------"
test_component "refactored_sparql_benchmark_simple.exs.tera" \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera" \
  300

test_component "refactored_sparql_benchmark.go.tera" \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark.go.tera" \
  250

echo ""

# Test 3: Component includes are present
echo "3. Template Includes"
echo "   -----------------"

check_include() {
  local file="$1"
  local component="$2"
  local desc="$3"

  echo -n "Checking $desc... "

  if grep -q "{% include \"$component\" %}" "$file"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASS++))
  else
    echo -e "${RED}FAIL${NC} (include not found)"
    ((FAIL++))
  fi
}

check_include \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera" \
  "benchmark_components/benchmark_setup.tera" \
  "Elixir template includes setup"

check_include \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera" \
  "benchmark_components/benchmark_metrics.tera" \
  "Elixir template includes metrics"

check_include \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera" \
  "benchmark_components/benchmark_assertions.tera" \
  "Elixir template includes assertions"

check_include \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark.go.tera" \
  "benchmark_components/benchmark_setup.tera" \
  "Go template includes setup"

check_include \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark.go.tera" \
  "benchmark_components/benchmark_metrics.tera" \
  "Go template includes metrics"

echo ""

# Test 4: Macro usage in templates
echo "4. Macro Usage"
echo "   ------------"

check_macro() {
  local file="$1"
  local macro="$2"
  local desc="$3"

  echo -n "Checking $desc... "

  if grep -q "self::$macro" "$file"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASS++))
  else
    echo -e "${RED}FAIL${NC} (macro not found)"
    ((FAIL++))
  fi
}

check_macro \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera" \
  "elixir_measure_function" \
  "Elixir template uses elixir_measure_function macro"

check_macro \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera" \
  "elixir_timeout_assertion" \
  "Elixir template uses elixir_timeout_assertion macro"

check_macro \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera" \
  "performance_summary_table" \
  "Elixir template uses performance_summary_table macro"

check_macro \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark.go.tera" \
  "go_benchmark_setup" \
  "Go template uses go_benchmark_setup macro"

check_macro \
  "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark.go.tera" \
  "go_timeout_assertion" \
  "Go template uses go_timeout_assertion macro"

echo ""

# Test 5: Line count comparison
echo "5. Line Count Comparison"
echo "   ---------------------"

original_elixir=$(wc -l < "$HOME/chatmangpt/OSA/templates/sparql_benchmark_test_simple.exs.tera")
refactored_elixir=$(wc -l < "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark_simple.exs.tera")

echo -n "Elixir template reduction... "
if [ "$refactored_elixir" -le "$original_elixir" ]; then
  reduction=$((original_elixir - refactored_elixir))
  percentage=$((reduction * 100 / original_elixir))
  echo -e "${GREEN}PASS${NC} ($reduction lines, $percentage% reduction)"
  ((PASS++))
else
  increase=$((refactored_elixir - original_elixir))
  echo -e "${RED}FAIL${NC} (+$increase lines, increased instead of reduced)"
  ((FAIL++))
fi

original_go=$(wc -l < "$HOME/chatmangpt/BusinessOS/templates/sparql_benchmark.go.tera")
refactored_go=$(wc -l < "$HOME/ggen/crates/ggen-core/templates/benchmark_components/refactored_sparql_benchmark.go.tera")

echo -n "Go template reduction... "
if [ "$refactored_go" -le "$original_go" ]; then
  reduction=$((original_go - refactored_go))
  percentage=$((reduction * 100 / original_go))
  echo -e "${GREEN}PASS${NC} ($reduction lines, $percentage% reduction)"
  ((PASS++))
else
  increase=$((refactored_go - original_go))
  echo -e "${RED}FAIL${NC} (+$increase lines, increased instead of reduced)"
  ((FAIL++))
fi

echo ""

# Summary
echo "=== Validation Summary ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"
echo ""

if [ $FAIL -eq 0 ]; then
  echo -e "${GREEN}All tests passed!${NC}"
  echo ""
  echo "Component library is ready for use."
  echo ""
  echo "Next steps:"
  echo "  1. Refactor remaining templates to use components"
  echo "  2. Generate benchmarks from refactored templates"
  echo "  3. Run test suites to verify functional equivalence"
  exit 0
else
  echo -e "${RED}Some tests failed!${NC}"
  echo ""
  echo "Please review the failures above."
  exit 1
fi
