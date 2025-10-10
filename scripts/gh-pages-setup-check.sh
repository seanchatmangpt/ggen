#!/usr/bin/env bash
# Comprehensive GitHub Pages setup validation

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "gh-pages-setup-check" "Comprehensive GitHub Pages setup validation"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_PAGES_URL="https://seanchatmangpt.github.io/ggen"
readonly DEFAULT_WORKFLOW_FILE=".github/workflows/publish-registry.yml"
readonly DEFAULT_DOCS_DIR="docs/book"
readonly DEFAULT_TIMEOUT="10"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly GITHUB_PAGES_URL="${GITHUB_PAGES_URL:-$DEFAULT_PAGES_URL}"
readonly WORKFLOW_FILE="${WORKFLOW_FILE:-$DEFAULT_WORKFLOW_FILE}"
readonly DOCS_DIR="${DOCS_DIR:-$DEFAULT_DOCS_DIR}"
readonly TIMEOUT="${TIMEOUT:-$DEFAULT_TIMEOUT}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false
FIX_ISSUES=false
ERRORS=0
WARNINGS=0

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Comprehensive GitHub Pages setup validation. Checks local build, workflow configuration, dependencies, and remote accessibility." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -j, --json           Output results in JSON format
    -f, --fix            Attempt to fix common issues automatically
    -w, --workflow FILE  Workflow file to check (default: .github/workflows/publish-registry.yml)
    -d, --docs-dir DIR   Docs directory to check (default: docs/book)
    -t, --timeout SEC    HTTP request timeout in seconds (default: 10)" \
        "    $SCRIPT_NAME                    # Check with defaults
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --json                 # Output in JSON format
    $SCRIPT_NAME --fix                  # Attempt to fix issues
    $SCRIPT_NAME --workflow my.yml      # Check specific workflow file" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    GITHUB_PAGES_URL     GitHub Pages URL (default: $DEFAULT_PAGES_URL)
    WORKFLOW_FILE        Workflow file to check (default: $DEFAULT_WORKFLOW_FILE)
    DOCS_DIR             Docs directory to check (default: $DEFAULT_DOCS_DIR)
    TIMEOUT              HTTP request timeout in seconds (default: $DEFAULT_TIMEOUT)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_repo_format "$REPO"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_url "$GITHUB_PAGES_URL"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$TIMEOUT" =~ ^[0-9]+$ ]] || [ "$TIMEOUT" -lt 1 ] || [ "$TIMEOUT" -gt 300 ]; then
        log_error "Invalid timeout: $TIMEOUT"
        log_error "Timeout must be a number between 1 and 300 seconds"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# Check functions
check_local_build() {
    log_debug "Checking local build..."
    
    if [ -f "$DOCS_DIR/index.html" ]; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"local_build\",\"status\":\"pass\",\"message\":\"Local docs built successfully\"}"
        else
            log_success "Local docs built successfully"
        fi
        return $EXIT_SUCCESS
    else
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"local_build\",\"status\":\"fail\",\"message\":\"Local docs not built\",\"fix\":\"Run: cargo make docs-build\"}"
        else
            log_failure "Local docs not built - run: cargo make docs-build"
        fi
        ERRORS=$((ERRORS + 1))
        return $EXIT_ERROR
    fi
}

check_workflow_file() {
    log_debug "Checking workflow file..."
    
    if [ -f "$WORKFLOW_FILE" ]; then
        local branch
        branch=$(grep -A2 "push:" "$WORKFLOW_FILE" | grep "branches:" -A1 | grep -v "branches:" | tr -d ' -' || echo "")
        
        if [ "$branch" = "master" ]; then
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"workflow_file\",\"status\":\"pass\",\"message\":\"Workflow configured for master branch\"}"
            else
                log_success "Workflow configured for master branch"
            fi
            return $EXIT_SUCCESS
        else
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"workflow_file\",\"status\":\"warn\",\"message\":\"Workflow configured for: $branch\"}"
            else
                log_warn "Workflow configured for: $branch"
            fi
            WARNINGS=$((WARNINGS + 1))
            return $EXIT_ERROR
        fi
    else
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"workflow_file\",\"status\":\"fail\",\"message\":\"Workflow file not found\",\"file\":\"$WORKFLOW_FILE\"}"
        else
            log_failure "Workflow file not found: $WORKFLOW_FILE"
        fi
        ERRORS=$((ERRORS + 1))
        return $EXIT_ERROR
    fi
}

check_mdbook_installation() {
    log_debug "Checking mdbook installation..."
    
    if command -v mdbook &> /dev/null; then
        local version
        version=$(mdbook --version 2>/dev/null || echo "unknown")
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"mdbook_installation\",\"status\":\"pass\",\"message\":\"mdbook installed\",\"version\":\"$version\"}"
        else
            log_success "mdbook installed: $version"
        fi
        return $EXIT_SUCCESS
    else
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"mdbook_installation\",\"status\":\"warn\",\"message\":\"mdbook not installed locally\",\"note\":\"Not required for CI\"}"
        else
            log_warn "mdbook not installed locally (not required for CI)"
        fi
        WARNINGS=$((WARNINGS + 1))
        return $EXIT_ERROR
    fi
}

check_github_cli() {
    log_debug "Checking GitHub CLI..."
    
    if command -v gh &> /dev/null; then
        if gh auth status &> /dev/null; then
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"github_cli\",\"status\":\"pass\",\"message\":\"GitHub CLI authenticated\"}"
            else
                log_success "GitHub CLI authenticated"
            fi
            return $EXIT_SUCCESS
        else
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"github_cli\",\"status\":\"warn\",\"message\":\"GitHub CLI installed but not authenticated\",\"fix\":\"Run: gh auth login\"}"
            else
                log_warn "GitHub CLI installed but not authenticated"
                log_info "      Run: gh auth login"
            fi
            WARNINGS=$((WARNINGS + 1))
            return $EXIT_ERROR
        fi
    else
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"github_cli\",\"status\":\"warn\",\"message\":\"GitHub CLI not installed\",\"fix\":\"Install: brew install gh\"}"
        else
            log_warn "GitHub CLI not installed (recommended)"
            log_info "      Install: brew install gh"
        fi
        WARNINGS=$((WARNINGS + 1))
        return $EXIT_ERROR
    fi
}

check_remote_accessibility() {
    log_debug "Checking remote site accessibility..."
    
    local http_status
    http_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time "$TIMEOUT" "$GITHUB_PAGES_URL" 2>/dev/null || echo "000")
    
    case "$http_status" in
        200)
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"remote_accessibility\",\"status\":\"pass\",\"message\":\"Site is live\",\"url\":\"$GITHUB_PAGES_URL\",\"status_code\":\"$http_status\"}"
            else
                log_success "Site is live: $GITHUB_PAGES_URL"
            fi
            return $EXIT_SUCCESS
            ;;
        404)
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"remote_accessibility\",\"status\":\"fail\",\"message\":\"Site returns 404\",\"url\":\"$GITHUB_PAGES_URL\",\"status_code\":\"$http_status\",\"fix\":\"Visit: https://github.com/$REPO/settings/pages\"}"
            else
                log_failure "Site returns 404 - GitHub Pages may not be configured"
                log_info "      Visit: https://github.com/$REPO/settings/pages"
            fi
            ERRORS=$((ERRORS + 1))
            return $EXIT_ERROR
            ;;
        *)
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"remote_accessibility\",\"status\":\"warn\",\"message\":\"Unexpected HTTP status\",\"url\":\"$GITHUB_PAGES_URL\",\"status_code\":\"$http_status\"}"
            else
                log_warn "Unexpected HTTP status: $http_status"
            fi
            WARNINGS=$((WARNINGS + 1))
            return $EXIT_ERROR
            ;;
    esac
}

# Fix functions
fix_local_build() {
    log_debug "Attempting to fix local build..."
    
    if [ "$FIX_ISSUES" = true ] && [ ! -f "$DOCS_DIR/index.html" ]; then
        log_info "Building documentation..."
        if cargo make docs-build &>/dev/null; then
            log_success "Documentation built successfully"
            return $EXIT_SUCCESS
        else
            log_error "Failed to build documentation"
            return $EXIT_ERROR
        fi
    fi
    
    return $EXIT_ERROR
}

# Summary functions
show_summary() {
    if [ "$JSON_OUTPUT" = true ]; then
        local exit_code=0
        if [ $ERRORS -gt 0 ]; then
            exit_code=1
        elif [ $WARNINGS -gt 0 ]; then
            exit_code=0
        fi
        
        echo "{\"summary\":{\"errors\":$ERRORS,\"warnings\":$WARNINGS,\"total_checks\":5,\"exit_code\":$exit_code}}"
    else
        echo ""
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📊 Summary:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Errors:   $ERRORS"
        echo "Warnings: $WARNINGS"
        echo ""
        
        if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
            log_success "All checks passed!"
        elif [ $ERRORS -eq 0 ]; then
            log_warn "Setup is functional but has warnings"
        else
            log_failure "Setup has errors that need to be fixed"
            echo ""
            log_info "Next steps:"
            log_info "1. Fix errors listed above"
            log_info "2. Run: cargo make docs-deploy (local test)"
            log_info "3. Push changes: git push origin master"
            log_info "4. Configure Pages: https://github.com/$REPO/settings/pages"
        fi
    fi
}

# Main function
main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit $EXIT_SUCCESS
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -j|--json)
                JSON_OUTPUT=true
                shift
                ;;
            -f|--fix)
                FIX_ISSUES=true
                shift
                ;;
            -w|--workflow)
                WORKFLOW_FILE="$2"
                shift 2
                ;;
            -d|--docs-dir)
                DOCS_DIR="$2"
                shift 2
                ;;
            -t|--timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit $EXIT_INVALID_ARGS
                ;;
        esac
    done
    
    # Validate inputs
    if ! validate_inputs; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Check dependencies
    if ! check_dependencies "curl"; then
        exit $EXIT_MISSING_DEPS
    fi
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "Starting GitHub Pages setup validation"
        log_info "Repository: $REPO"
        log_info "Pages URL: $GITHUB_PAGES_URL"
        log_info "Workflow file: $WORKFLOW_FILE"
        log_info "Docs directory: $DOCS_DIR"
        log_info "Timeout: $TIMEOUT"
        echo ""
    fi
    
    # Perform checks
    check_local_build
    check_workflow_file
    check_mdbook_installation
    check_github_cli
    check_remote_accessibility
    
    # Attempt fixes if requested
    if [ "$FIX_ISSUES" = true ]; then
        log_info "Attempting to fix issues..."
        fix_local_build
    fi
    
    # Show summary
    show_summary
    
    # Exit with appropriate code
    if [ $ERRORS -gt 0 ]; then
        exit $EXIT_ERROR
    elif [ $WARNINGS -gt 0 ]; then
        exit $EXIT_SUCCESS
    else
        exit $EXIT_SUCCESS
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
