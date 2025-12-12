#!/usr/bin/env bash
#
# Validate Diataxis Case Study Documentation
# Tests: docs/examples/diataxis-case-study/**/*.md
#
# Validates that the case study demonstrates proper Diataxis structure

set -e
set -u

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

log_info() { echo -e "${BLUE}ℹ${NC} $1"; }
log_success() {
    echo -e "${GREEN}✓${NC} $1"
    ((++TESTS_PASSED))
    ((++TESTS_RUN))
    return 0
}
log_error() {
    echo -e "${RED}✗${NC} $1"
    ((++TESTS_FAILED))
    ((++TESTS_RUN))
    return 0
}
log_section() {
    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}$1${NC}"
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
CASE_STUDY_DIR="$REPO_ROOT/docs/examples/diataxis-case-study"
DEFAULT_GGEN_BIN="$REPO_ROOT/target/debug/ggen"
if [ -x "$DEFAULT_GGEN_BIN" ]; then
    GGEN_BIN="${GGEN_BIN:-$DEFAULT_GGEN_BIN}"
else
    GGEN_BIN="${GGEN_BIN:-ggen}"
fi

run_with_timeout() {
    local timeout_secs=$1
    shift
    python3 - "$timeout_secs" "$@" <<'PY'
import subprocess, sys
timeout = int(sys.argv[1])
cmd = sys.argv[2:]
try:
    cp = subprocess.run(cmd, timeout=timeout, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
except subprocess.TimeoutExpired:
    print("Command timed out", file=sys.stderr)
    sys.exit(124)
print(cp.stdout, end="")
print(cp.stderr, end="", file=sys.stderr)
sys.exit(cp.returncode)
PY
}

log_info "Validating Diataxis Case Study at: $CASE_STUDY_DIR"
log_info "Using ggen binary: ${GGEN_BIN}"

# ============================================================================
# Test 1: All Required Files Exist
# ============================================================================
log_section "Test 1: Required Files Exist"

if [ -f "$CASE_STUDY_DIR/README.md" ]; then
    log_success "Main README.md exists"
else
    log_error "Missing README.md"
fi

if [ -f "$CASE_STUDY_DIR/META-GUIDE.md" ]; then
    log_success "META-GUIDE.md exists"
else
    log_error "Missing META-GUIDE.md"
fi

if [ -f "$CASE_STUDY_DIR/tutorials/01-first-todo-app.md" ]; then
    log_success "Tutorial exists"
else
    log_error "Missing tutorial"
fi

if [ -f "$CASE_STUDY_DIR/how-to/setup-electric-sync.md" ]; then
    log_success "How-to guide exists"
else
    log_error "Missing how-to guide"
fi

if [ -f "$CASE_STUDY_DIR/explanations/local-first-architecture.md" ]; then
    log_success "Explanation exists"
else
    log_error "Missing explanation"
fi

if [ -f "$CASE_STUDY_DIR/reference/electric-api.md" ]; then
    log_success "Reference documentation exists"
else
    log_error "Missing reference"
fi

# ============================================================================
# Test 1b: ggen CLI availability
# ============================================================================
log_section "Test 1b: ggen CLI Availability"

if command -v "$GGEN_BIN" >/dev/null 2>&1 || [ -x "$GGEN_BIN" ]; then
    log_success "ggen binary is discoverable"
else
    log_error "ggen binary not found (set GGEN_BIN to override)"
fi

if run_with_timeout 5 "$GGEN_BIN" --version >/dev/null 2>&1; then
    log_success "ggen --version executes"
else
    log_error "ggen --version failed (binary missing or broken)"
fi

HELP_OUTPUT="$(mktemp)"
if run_with_timeout 5 "$GGEN_BIN" --help >"$HELP_OUTPUT" 2>&1; then
    log_success "ggen --help executes"
else
    if grep -q "Usage: ggen" "$HELP_OUTPUT"; then
        log_success "ggen --help shows usage (non-zero exit tolerated)"
    else
        log_error "ggen --help failed"
    fi
fi
rm -f "$HELP_OUTPUT"

# ============================================================================
# Test 1c: ggen Functional Smoke (template list)
# ============================================================================
log_section "Test 1c: ggen Functional Smoke"

TEMPLATE_LIST_OUTPUT="$(mktemp)"
if run_with_timeout 15 "$GGEN_BIN" template list >"$TEMPLATE_LIST_OUTPUT" 2>&1; then
    if python3 - "$TEMPLATE_LIST_OUTPUT" <<'PY'
import json, sys
path = sys.argv[1]
with open(path, "r", encoding="utf-8") as fh:
    data = json.load(fh)
templates = data.get("templates", [])
assert isinstance(templates, list)
assert len(templates) > 0
sys.exit(0)
PY
    then
        log_success "ggen template list returns templates"
    else
        log_error "ggen template list output invalid JSON or empty"
    fi
else
    log_error "ggen template list failed"
fi
rm -f "$TEMPLATE_LIST_OUTPUT"

# ============================================================================
# Test 1d: ggen Template Lint (hello.tmpl)
# ============================================================================
log_section "Test 1d: ggen Template Lint"

LINT_OUTPUT="$(mktemp)"
if run_with_timeout 15 "$GGEN_BIN" template lint --template "$REPO_ROOT/templates/hello.tmpl" >"$LINT_OUTPUT" 2>&1; then
    if python3 - "$LINT_OUTPUT" <<'PY'
import json, sys
path = sys.argv[1]
with open(path, "r", encoding="utf-8") as fh:
    data = json.load(fh)
assert data.get("has_errors") is False
assert isinstance(data.get("errors", []), list)
sys.exit(0)
PY
    then
        log_success "ggen template lint (hello.tmpl) passes"
    else
        log_error "ggen template lint output invalid or reports errors"
    fi
else
    log_error "ggen template lint failed"
fi
rm -f "$LINT_OUTPUT"

# ============================================================================
# Test 2: Tutorial Structure Validation
# ============================================================================
log_section "Test 2: Tutorial Structure"

TUTORIAL="$CASE_STUDY_DIR/tutorials/01-first-todo-app.md"

if grep -q "What You'll Build" "$TUTORIAL"; then
    log_success "Tutorial has clear outcome statement"
else
    log_error "Tutorial missing outcome statement"
fi

if grep -q "Step [0-9]" "$TUTORIAL"; then
    log_success "Tutorial has numbered steps"
else
    log_error "Tutorial missing numbered steps"
fi

if grep -q "What just happened" "$TUTORIAL"; then
    log_success "Tutorial explains steps"
else
    log_error "Tutorial missing step explanations"
fi

if grep -q "Congratulations" "$TUTORIAL"; then
    log_success "Tutorial has completion celebration"
else
    log_error "Tutorial missing completion section"
fi

if grep -q "Next Steps" "$TUTORIAL"; then
    log_success "Tutorial links to next steps"
else
    log_error "Tutorial missing next steps"
fi

# ============================================================================
# Test 3: How-to Guide Structure
# ============================================================================
log_section "Test 3: How-to Guide Structure"

HOWTO="$CASE_STUDY_DIR/how-to/setup-electric-sync.md"

if grep -q "^\\*\\*Problem\\*\\*:" "$HOWTO"; then
    log_success "How-to states problem upfront"
else
    log_error "How-to missing problem statement"
fi

if grep -q "^\\*\\*Solution\\*\\*:" "$HOWTO"; then
    log_success "How-to states solution"
else
    log_error "How-to missing solution statement"
fi

if grep -q "Prerequisites" "$HOWTO"; then
    log_success "How-to lists prerequisites"
else
    log_error "How-to missing prerequisites"
fi

if grep -q "Troubleshooting" "$HOWTO"; then
    log_success "How-to includes troubleshooting"
else
    log_error "How-to missing troubleshooting section"
fi

if grep -q "Production" "$HOWTO"; then
    log_success "How-to includes production considerations"
else
    log_error "How-to missing production section"
fi

# ============================================================================
# Test 4: Explanation Structure
# ============================================================================
log_section "Test 4: Explanation Structure"

EXPLANATION="$CASE_STUDY_DIR/explanations/local-first-architecture.md"

if grep -q "What is" "$EXPLANATION"; then
    log_success "Explanation defines concept"
else
    log_error "Explanation missing definition"
fi

if grep -q "Trade-offs\\|Advantages\\|Challenges" "$EXPLANATION"; then
    log_success "Explanation discusses trade-offs"
else
    log_error "Explanation missing trade-offs"
fi

if grep -q "When to Use\\|Great for\\|Not ideal for" "$EXPLANATION"; then
    log_success "Explanation provides decision criteria"
else
    log_error "Explanation missing decision criteria"
fi

# Check that explanation doesn't have step-by-step instructions
if grep -q "Step [0-9]:" "$EXPLANATION"; then
    log_error "Explanation contains step-by-step instructions (anti-pattern!)"
else
    log_success "Explanation avoids step-by-step instructions"
fi

# ============================================================================
# Test 5: Reference Structure
# ============================================================================
log_section "Test 5: Reference Structure"

REFERENCE="$CASE_STUDY_DIR/reference/electric-api.md"

if grep -q "^###.*(" "$REFERENCE"; then
    log_success "Reference documents functions"
else
    log_error "Reference missing function documentation"
fi

if grep -q "\\*\\*Parameters:\\*\\*" "$REFERENCE"; then
    log_success "Reference documents parameters"
else
    log_error "Reference missing parameter documentation"
fi

if grep -q "\\*\\*Returns:\\*\\*" "$REFERENCE"; then
    log_success "Reference documents return values"
else
    log_error "Reference missing return value documentation"
fi

if grep -q "\\*\\*Example:\\*\\*" "$REFERENCE"; then
    log_success "Reference includes examples"
else
    log_error "Reference missing examples"
fi

# ============================================================================
# Test 6: Cross-Links Between Quadrants
# ============================================================================
log_section "Test 6: Cross-Links Between Quadrants"

# Tutorial should link to how-tos
if grep -q "how-to/" "$TUTORIAL"; then
    log_success "Tutorial links to how-to guides"
else
    log_error "Tutorial missing links to how-tos"
fi

# How-to should link to explanations
if grep -q "explanations/" "$HOWTO"; then
    log_success "How-to links to explanations"
else
    log_error "How-to missing links to explanations"
fi

# Explanation should link to reference
if grep -q "reference/" "$EXPLANATION"; then
    log_success "Explanation links to reference"
else
    log_error "Explanation missing links to reference"
fi

# Reference should link to tutorial
if grep -q "tutorials/" "$REFERENCE"; then
    log_success "Reference links to tutorial"
else
    log_error "Reference missing links to tutorial"
fi

# ============================================================================
# Test 7: Meta-Lessons Included
# ============================================================================
log_section "Test 7: Meta-Lessons"

if grep -q "Meta-Lesson\\|Meta-observation" "$TUTORIAL"; then
    log_success "Tutorial includes meta-lesson"
else
    log_error "Tutorial missing meta-lesson"
fi

if grep -q "Meta-Lesson\\|Meta-observation" "$HOWTO"; then
    log_success "How-to includes meta-lesson"
else
    log_error "How-to missing meta-lesson"
fi

if grep -q "Meta-Lesson\\|Meta-observation" "$EXPLANATION"; then
    log_success "Explanation includes meta-lesson"
else
    log_error "Explanation missing meta-lesson"
fi

# ============================================================================
# Test 8: Code Examples Validity
# ============================================================================
log_section "Test 8: Code Example Structure"

# Check that code blocks are properly formatted
if grep -q '```javascript' "$TUTORIAL"; then
    log_success "Tutorial uses JavaScript code blocks"
else
    log_error "Tutorial missing JavaScript examples"
fi

# Check for TypeScript (should be JavaScript + Zod + JSDoc)
if grep -q '```typescript' "$TUTORIAL"; then
    log_error "Tutorial uses TypeScript (should be JavaScript + Zod + JSDoc)"
else
    log_success "Tutorial avoids TypeScript"
fi

# Check for Zod usage
if grep -q "import.*zod\\|from 'zod'" "$TUTORIAL"; then
    log_success "Tutorial uses Zod for validation"
else
    log_error "Tutorial missing Zod validation"
fi

# ============================================================================
# Test 9: META-GUIDE Completeness
# ============================================================================
log_section "Test 9: META-GUIDE Structure"

METAGUIDE="$CASE_STUDY_DIR/META-GUIDE.md"

if grep -q "Phase 1: Experience" "$METAGUIDE"; then
    log_success "META-GUIDE includes experience phase"
else
    log_error "META-GUIDE missing experience phase"
fi

if grep -q "Phase 2: Analyze" "$METAGUIDE"; then
    log_success "META-GUIDE includes analyze phase"
else
    log_error "META-GUIDE missing analyze phase"
fi

if grep -q "Phase 3: Apply" "$METAGUIDE"; then
    log_success "META-GUIDE includes apply phase"
else
    log_error "META-GUIDE missing apply phase"
fi

if grep -q "Exercises" "$METAGUIDE"; then
    log_success "META-GUIDE includes exercises"
else
    log_error "META-GUIDE missing exercises"
fi

# ============================================================================
# Summary
# ============================================================================
log_section "Validation Summary"

echo ""
echo "Total Tests Run:    $TESTS_RUN"
echo -e "${GREEN}Tests Passed:       $TESTS_PASSED${NC}"
echo -e "${RED}Tests Failed:       $TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}✓ DIATAXIS CASE STUDY: ALL VALIDATIONS PASSED${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo "The case study correctly demonstrates:"
    echo "  ✓ Tutorial structure (learning + practical)"
    echo "  ✓ How-to structure (using + practical)"
    echo "  ✓ Explanation structure (learning + theoretical)"
    echo "  ✓ Reference structure (using + theoretical)"
    echo "  ✓ Cross-links between quadrants"
    echo "  ✓ Meta-lessons for teaching Diataxis"
    echo "  ✓ JavaScript + Zod + JSDoc (not TypeScript)"
    exit 0
else
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}✗ DIATAXIS CASE STUDY: VALIDATION FAILED${NC}"
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 1
fi
