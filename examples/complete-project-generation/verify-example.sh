#!/usr/bin/env bash
set -euo pipefail

# Verification script for complete project generation example
# Ensures all files are present and templates are valid

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

passed=0
failed=0

check() {
    local test_name="$1"
    local test_command="$2"

    if eval "$test_command" > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $test_name"
        ((passed++))
    else
        echo -e "${RED}✗${NC} $test_name"
        ((failed++))
    fi
}

echo "Verifying complete project generation example..."
echo ""

# Check documentation files
check "README.md exists" "test -f '$SCRIPT_DIR/README.md'"
check "QUICKSTART.md exists" "test -f '$SCRIPT_DIR/QUICKSTART.md'"
check "TEMPLATE_REFERENCE.md exists" "test -f '$SCRIPT_DIR/TEMPLATE_REFERENCE.md'"
check "SUMMARY.md exists" "test -f '$SCRIPT_DIR/SUMMARY.md'"
check "project-spec.yaml exists" "test -f '$SCRIPT_DIR/project-spec.yaml'"

# Check generation script
check "generate-project.sh exists" "test -f '$SCRIPT_DIR/generate-project.sh'"
check "generate-project.sh is executable" "test -x '$SCRIPT_DIR/generate-project.sh'"

# Check templates directory
check "templates/ directory exists" "test -d '$SCRIPT_DIR/templates'"

# Check individual templates
check "cargo-toml.tmpl exists" "test -f '$SCRIPT_DIR/templates/cargo-toml.tmpl'"
check "main-rs.tmpl exists" "test -f '$SCRIPT_DIR/templates/main-rs.tmpl'"
check "models-mod.tmpl exists" "test -f '$SCRIPT_DIR/templates/models-mod.tmpl'"
check "user-model.tmpl exists" "test -f '$SCRIPT_DIR/templates/user-model.tmpl'"
check "product-model.tmpl exists" "test -f '$SCRIPT_DIR/templates/product-model.tmpl'"
check "api-mod.tmpl exists" "test -f '$SCRIPT_DIR/templates/api-mod.tmpl'"
check "api-handlers.tmpl exists" "test -f '$SCRIPT_DIR/templates/api-handlers.tmpl'"
check "api-routes.tmpl exists" "test -f '$SCRIPT_DIR/templates/api-routes.tmpl'"
check "config-mod.tmpl exists" "test -f '$SCRIPT_DIR/templates/config-mod.tmpl'"
check "config-settings.tmpl exists" "test -f '$SCRIPT_DIR/templates/config-settings.tmpl'"
check "integration-test.tmpl exists" "test -f '$SCRIPT_DIR/templates/integration-test.tmpl'"

# Check template content
check "cargo-toml.tmpl has [package]" "grep -q '\[package\]' '$SCRIPT_DIR/templates/cargo-toml.tmpl'"
check "main-rs.tmpl has main function" "grep -q 'fn main()' '$SCRIPT_DIR/templates/main-rs.tmpl'"
check "user-model.tmpl has User struct" "grep -q 'pub struct User' '$SCRIPT_DIR/templates/user-model.tmpl'"
check "api-handlers.tmpl has handlers" "grep -q 'pub async fn' '$SCRIPT_DIR/templates/api-handlers.tmpl'"
check "integration-test.tmpl has tests" "grep -q '#\[actix_web::test\]' '$SCRIPT_DIR/templates/integration-test.tmpl'"

# Check for template variables
check "Templates use {{project_name}}" "grep -q '{{project_name}}' '$SCRIPT_DIR/templates/cargo-toml.tmpl'"
check "Templates use {{version}}" "grep -q '{{version}}' '$SCRIPT_DIR/templates/cargo-toml.tmpl'"
check "Templates use {{port}}" "grep -q '{{port}}' '$SCRIPT_DIR/templates/main-rs.tmpl'"

# Validate script functionality
check "Script shows help" "$SCRIPT_DIR/generate-project.sh help | grep -q 'Usage:'"
check "Script validates templates" "$SCRIPT_DIR/generate-project.sh validate"

# Check documentation quality
check "README has Quick Start section" "grep -q '## Quick Start' '$SCRIPT_DIR/README.md'"
check "README has API endpoints" "grep -q 'GET /health' '$SCRIPT_DIR/README.md'"
check "QUICKSTART has 5-minute setup" "grep -q '5-Minute Setup' '$SCRIPT_DIR/QUICKSTART.md'"

echo ""
echo "Results:"
echo -e "  ${GREEN}Passed: $passed${NC}"
if [ $failed -gt 0 ]; then
    echo -e "  ${RED}Failed: $failed${NC}"
    exit 1
else
    echo -e "  ${GREEN}All checks passed!${NC}"
    echo ""
    echo "Example is ready to use:"
    echo "  ./generate-project.sh validate"
    echo "  ./generate-project.sh generate"
fi
