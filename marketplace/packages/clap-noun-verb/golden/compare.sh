#!/bin/bash
# Golden File Comparison Script
# Validates generated CLI code against reference outputs
# Part of deterministic validation pipeline
# Usage: ./compare.sh [--update] [--json] [example-name]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$(dirname "$(dirname "$PACKAGE_DIR")")")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
UPDATE_MODE=false
JSON_OUTPUT=false
EXAMPLE_NAME=${1:-}
VERBOSE=${VERBOSE:-0}

# Report variables
TOTAL_COMPARISONS=0
PASSED_COMPARISONS=0
FAILED_COMPARISONS=0
SKIPPED_COMPARISONS=0

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --update)
      UPDATE_MODE=true
      shift
      ;;
    --json)
      JSON_OUTPUT=true
      shift
      ;;
    --verbose)
      VERBOSE=1
      shift
      ;;
    *)
      EXAMPLE_NAME=$1
      shift
      ;;
  esac
done

# Helper functions
log_info() {
  echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
  echo -e "${GREEN}[PASS]${NC} $*"
}

log_error() {
  echo -e "${RED}[FAIL]${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
}

compare_files() {
  local generated_file=$1
  local golden_file=$2
  local file_type=$3

  ((TOTAL_COMPARISONS++))

  # Check if golden file exists
  if [[ ! -f "$golden_file" ]]; then
    if [[ "$UPDATE_MODE" == true ]]; then
      log_warn "Creating golden file: $(basename "$golden_file")"
      mkdir -p "$(dirname "$golden_file")"
      cp "$generated_file" "$golden_file"
      ((PASSED_COMPARISONS++))
      return 0
    else
      log_error "Golden file missing: $(basename "$golden_file")"
      ((FAILED_COMPARISONS++))
      return 1
    fi
  fi

  # Compare files
  if diff -q "$generated_file" "$golden_file" > /dev/null 2>&1; then
    log_success "$(basename "$generated_file") matches golden"
    ((PASSED_COMPARISONS++))
    return 0
  else
    log_error "$(basename "$generated_file") differs from golden"

    if [[ "$VERBOSE" == "1" ]]; then
      echo "--- Golden (expected)"
      echo "+++ Generated (actual)"
      diff -u "$golden_file" "$generated_file" || true
    fi

    if [[ "$UPDATE_MODE" == true ]]; then
      log_info "Updating golden file: $(basename "$golden_file")"
      cp "$generated_file" "$golden_file"
      ((PASSED_COMPARISONS++))
      return 0
    else
      ((FAILED_COMPARISONS++))
      return 1
    fi
  fi
}

validate_example() {
  local example=$1
  local golden_dir="$SCRIPT_DIR/$example"

  log_info "Validating example: $example"

  case $example in
    calculator-simple)
      # Simple calculator golden files
      if [[ -f "$PROJECT_ROOT/target/ggen/$example/src/main.rs" ]]; then
        compare_files \
          "$PROJECT_ROOT/target/ggen/$example/src/main.rs" \
          "$golden_dir/main.rs" \
          "rust"
      else
        log_warn "Generated files not found for $example (expected: target/ggen/$example/)"
        ((SKIPPED_COMPARISONS++))
      fi
      ;;

    calculator-enhanced)
      # Enhanced calculator with 12 operations
      if [[ -f "$PROJECT_ROOT/target/ggen/$example/src/main.rs" ]]; then
        compare_files \
          "$PROJECT_ROOT/target/ggen/$example/src/main.rs" \
          "$golden_dir/main.rs" \
          "rust"
      else
        log_warn "Generated files not found for $example"
        ((SKIPPED_COMPARISONS++))
      fi
      ;;

    enterprise-generated)
      # Multi-noun enterprise CLI
      if [[ -f "$PROJECT_ROOT/target/ggen/$example/src/main.rs" ]]; then
        compare_files \
          "$PROJECT_ROOT/target/ggen/$example/src/main.rs" \
          "$golden_dir/main.rs" \
          "rust"
      else
        log_warn "Generated files not found for $example"
        ((SKIPPED_COMPARISONS++))
      fi
      ;;

    *)
      log_error "Unknown example: $example"
      return 1
      ;;
  esac
}

generate_report() {
  local total=$TOTAL_COMPARISONS
  local passed=$PASSED_COMPARISONS
  local failed=$FAILED_COMPARISONS
  local skipped=$SKIPPED_COMPARISONS

  if [[ "$JSON_OUTPUT" == true ]]; then
    cat > "$SCRIPT_DIR/comparison-report.json" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "total_comparisons": $total,
  "passed": $passed,
  "failed": $failed,
  "skipped": $skipped,
  "success_rate": $(awk "BEGIN {printf \"%.1f\", ($passed / ($total - $skipped)) * 100}")%,
  "status": "$([ $failed -eq 0 ] && echo 'PASS' || echo 'FAIL')"
}
EOF
    log_info "JSON report: $SCRIPT_DIR/comparison-report.json"
  fi

  echo ""
  echo "=========================================="
  echo "Golden File Comparison Summary"
  echo "=========================================="
  echo "Total Comparisons: $total"
  echo -e "Passed: ${GREEN}$passed${NC}"
  echo -e "Failed: ${RED}$failed${NC}"
  echo -e "Skipped: ${YELLOW}$skipped${NC}"

  if [[ $total -gt 0 ]]; then
    success_rate=$((($passed * 100) / ($total - $skipped)))
    echo "Success Rate: $success_rate%"
  fi
  echo "=========================================="

  [[ $failed -eq 0 ]]
}

# Main execution
main() {
  log_info "Golden file comparison started"
  log_info "Update mode: $UPDATE_MODE"

  if [[ -z "$EXAMPLE_NAME" ]]; then
    # Validate all examples
    for example in calculator-simple calculator-enhanced enterprise-generated; do
      validate_example "$example" || true
    done
  else
    # Validate specific example
    validate_example "$EXAMPLE_NAME" || exit 1
  fi

  generate_report
}

main "$@"
