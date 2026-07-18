#!/usr/bin/env bash
# Complete workflow for source code analysis and template generation

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo_step() {
    echo -e "${BLUE}==>${NC} ${1}"
}

echo_success() {
    echo -e "${GREEN}✓${NC} ${1}"
}

echo_error() {
    echo -e "${RED}✗${NC} ${1}"
}

echo_info() {
    echo -e "${YELLOW}ℹ${NC} ${1}"
}

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_DIR="$SCRIPT_DIR/source-code"
TEMPLATES_DIR="$SCRIPT_DIR/templates"
REGENERATED_DIR="$SCRIPT_DIR/regenerated-code"
EXPECTED_DIR="$SCRIPT_DIR/expected-outputs"

# Create directories
mkdir -p "$TEMPLATES_DIR"
mkdir -p "$REGENERATED_DIR"

echo_step "Source Code Analysis Workflow"
echo ""
echo "This script demonstrates the complete workflow:"
echo "1. Analyze source files"
echo "2. Generate templates"
echo "3. Review analysis reports"
echo "4. Use templates to regenerate code"
echo "5. Validate generated code"
echo ""

# Check if ggen is available
if ! command -v ggen &> /dev/null; then
    echo_error "ggen command not found. Please install it first."
    echo "Run: cargo install --path ."
    exit 1
fi

echo_success "Found ggen command"
echo ""

# Function to analyze a source file
analyze_source() {
    local source_file="$1"
    local template_name="$2"
    local description="$3"

    echo_step "Analyzing: $source_file"
    echo_info "Template: $template_name"
    echo_info "Description: $description"

    local template_path="$TEMPLATES_DIR/${template_name}.yaml"
    local analysis_path="$TEMPLATES_DIR/${template_name}-analysis.md"

    # Run analysis (using --mock for demonstration)
    if ggen ai from-source "$source_file" --output "$template_path" --mock; then
        echo_success "Generated template: $template_path"

        if [ -f "$analysis_path" ]; then
            echo_success "Generated analysis: $analysis_path"
            echo_info "Preview of analysis report:"
            head -n 20 "$analysis_path"
            echo "..."
        else
            echo_info "No analysis report generated (expected in mock mode)"
        fi
    else
        echo_error "Failed to analyze $source_file"
        return 1
    fi

    echo ""
    return 0
}

# Function to regenerate code from template
regenerate_code() {
    local template_path="$1"
    local output_path="$2"
    local variables="$3"

    echo_step "Regenerating code from template"
    echo_info "Template: $template_path"
    echo_info "Output: $output_path"
    echo_info "Variables: $variables"

    # Build ggen command with variables
    local cmd="ggen generate \"$template_path\" --output \"$output_path\" --mock"

    # Add variables if provided
    if [ -n "$variables" ]; then
        while IFS=',' read -ra PAIRS; do
            for pair in "${PAIRS[@]}"; do
                IFS='=' read -ra KV <<< "$pair"
                cmd="$cmd --set ${KV[0]}=${KV[1]}"
            done
        done <<< "$variables"
    fi

    # Execute command
    if eval "$cmd"; then
        echo_success "Generated code: $output_path"
        echo_info "Preview of generated code:"
        head -n 30 "$output_path"
        echo "..."
    else
        echo_error "Failed to generate code"
        return 1
    fi

    echo ""
    return 0
}

# Step 1: Analyze configuration pattern
echo_step "STEP 1: Analyze Configuration Pattern"
echo ""
analyze_source \
    "$SOURCE_DIR/config.rs" \
    "config-pattern" \
    "Configuration struct with builder pattern"

# Step 2: Analyze user model
echo_step "STEP 2: Analyze User Model Pattern"
echo ""
analyze_source \
    "$SOURCE_DIR/user-model.rs" \
    "user-model-pattern" \
    "Database model with validation"

# Step 3: Analyze API handler
echo_step "STEP 3: Analyze API Handler Pattern"
echo ""
analyze_source \
    "$SOURCE_DIR/api-handler.rs" \
    "api-handler-pattern" \
    "REST API handler with error handling"

# Step 4: Analyze middleware
echo_step "STEP 4: Analyze Middleware Pattern"
echo ""
analyze_source \
    "$SOURCE_DIR/middleware.rs" \
    "middleware-pattern" \
    "Composable middleware architecture"

# Step 5: Regenerate code from templates
echo_step "STEP 5: Use Templates to Generate Similar Code"
echo ""

echo_step "5a. Generate database configuration"
regenerate_code \
    "$TEMPLATES_DIR/config-pattern.yaml" \
    "$REGENERATED_DIR/database-config.rs" \
    "config_name=DatabaseConfig,component=database"

echo_step "5b. Generate admin user model"
regenerate_code \
    "$TEMPLATES_DIR/user-model-pattern.yaml" \
    "$REGENERATED_DIR/admin-model.rs" \
    "model_name=Admin,entity=admin"

echo_step "5c. Generate product API handler"
regenerate_code \
    "$TEMPLATES_DIR/api-handler-pattern.yaml" \
    "$REGENERATED_DIR/product-handler.rs" \
    "resource=product,endpoint=/api/products"

echo_step "5d. Generate authentication middleware"
regenerate_code \
    "$TEMPLATES_DIR/middleware-pattern.yaml" \
    "$REGENERATED_DIR/auth-middleware.rs" \
    "middleware_name=AuthMiddleware,purpose=authentication"

# Step 6: Validate generated code (basic syntax check)
echo_step "STEP 6: Validate Generated Code"
echo ""

for file in "$REGENERATED_DIR"/*.rs; do
    if [ -f "$file" ]; then
        echo_info "Checking: $(basename "$file")"

        # Try to parse as Rust (this is a simple check)
        # In production, you'd use rustc or cargo check
        if grep -q "pub struct\|pub enum\|pub fn" "$file"; then
            echo_success "✓ $(basename "$file") has valid Rust structure"
        else
            echo_error "✗ $(basename "$file") may have issues"
        fi
    fi
done

echo ""

# Step 7: Summary and comparison
echo_step "STEP 7: Summary"
echo ""

echo "Generated Templates:"
ls -lh "$TEMPLATES_DIR"/*.yaml 2>/dev/null || echo "No templates found"
echo ""

echo "Analysis Reports:"
ls -lh "$TEMPLATES_DIR"/*-analysis.md 2>/dev/null || echo "No analysis reports found"
echo ""

echo "Regenerated Code:"
ls -lh "$REGENERATED_DIR"/*.rs 2>/dev/null || echo "No regenerated files found"
echo ""

echo_step "Workflow Complete!"
echo ""
echo "Next steps:"
echo "1. Review templates in: $TEMPLATES_DIR"
echo "2. Read analysis reports: $TEMPLATES_DIR/*-analysis.md"
echo "3. Examine regenerated code: $REGENERATED_DIR"
echo "4. Compare original vs regenerated code"
echo "5. Refine templates based on results"
echo ""

echo_info "To analyze your own code:"
echo "  ggen ai from-source <your-file> --output templates/my-pattern.yaml"
echo ""

echo_info "To use a template:"
echo "  ggen generate templates/my-pattern.yaml --set variable=value --output generated.rs"
echo ""

echo_success "All steps completed successfully!"
