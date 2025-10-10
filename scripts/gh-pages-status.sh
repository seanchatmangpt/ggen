#!/usr/bin/env bash
# Check GitHub Pages configuration and deployment status via API

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "gh-pages-status" "Check GitHub Pages configuration and deployment status via API"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_API="https://api.github.com"
readonly DEFAULT_PAGES_URL="https://seanchatmangpt.github.io/ggen"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly API="${GITHUB_API:-$DEFAULT_API}"
readonly GITHUB_PAGES_URL="${GITHUB_PAGES_URL:-$DEFAULT_PAGES_URL}"
readonly WORKFLOW_NAME="${WORKFLOW_NAME:-Deploy Documentation to GitHub Pages}"
readonly LIMIT="${LIMIT:-5}"
readonly TIMEOUT="${TIMEOUT:-10}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Check GitHub Pages configuration and deployment status via API. Provides comprehensive status information including configuration, deployments, workflow runs, and site accessibility." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -j, --json           Output results in JSON format
    -w, --workflow NAME  Specify workflow name (default: Deploy Documentation to GitHub Pages)
    -l, --limit NUM      Limit number of workflow runs to show (default: 5)
    -t, --timeout SEC    HTTP request timeout in seconds (default: 10)" \
        "    $SCRIPT_NAME                    # Check with defaults
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --json                 # Output in JSON format
    $SCRIPT_NAME --workflow \"My Workflow\"  # Check specific workflow
    $SCRIPT_NAME --limit 10             # Show more workflow runs" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    GITHUB_API           GitHub API URL (default: $DEFAULT_API)
    GITHUB_PAGES_URL     GitHub Pages URL (default: $DEFAULT_PAGES_URL)
    WORKFLOW_NAME        Workflow name to check (default: Deploy Documentation to GitHub Pages)
    LIMIT                Number of workflow runs to show (default: 5)
    TIMEOUT              HTTP request timeout in seconds (default: 10)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_repo_format "$REPO"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_url "$API"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_url "$GITHUB_PAGES_URL"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$LIMIT" =~ ^[0-9]+$ ]] || [ "$LIMIT" -lt 1 ] || [ "$LIMIT" -gt 100 ]; then
        log_error "Invalid limit: $LIMIT"
        log_error "Limit must be a number between 1 and 100"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$TIMEOUT" =~ ^[0-9]+$ ]] || [ "$TIMEOUT" -lt 1 ] || [ "$TIMEOUT" -gt 300 ]; then
        log_error "Invalid timeout: $TIMEOUT"
        log_error "Timeout must be a number between 1 and 300 seconds"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# GitHub CLI functions
check_pages_configuration() {
    log_debug "Checking GitHub Pages configuration..."
    
    if command -v gh &> /dev/null && gh auth status &> /dev/null; then
        log_info "Using GitHub CLI for authenticated requests"
        
        if [ "$JSON_OUTPUT" = true ]; then
            gh api "repos/$REPO/pages" 2>/dev/null || echo '{"error": "GitHub Pages not configured or not accessible"}'
        else
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "📄 GitHub Pages Configuration:"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            gh api "repos/$REPO/pages" 2>&1 || log_warn "GitHub Pages not configured or not accessible"
            echo ""
        fi
    else
        log_warn "GitHub CLI not available, using curl with public API"
        log_warn "Install gh CLI for authenticated access: brew install gh"
        
        if [ "$JSON_OUTPUT" = true ]; then
            curl -s --max-time "$TIMEOUT" "$API/repos/$REPO/pages" 2>/dev/null || echo '{"error": "Could not fetch Pages configuration"}'
        else
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "📄 GitHub Pages Configuration (public data):"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            curl -s --max-time "$TIMEOUT" "$API/repos/$REPO/pages" | python3 -m json.tool 2>&1 || log_warn "Could not fetch Pages configuration"
            echo ""
        fi
    fi
}

check_latest_deployment() {
    log_debug "Checking latest deployment..."
    
    if command -v gh &> /dev/null && gh auth status &> /dev/null; then
        if [ "$JSON_OUTPUT" = true ]; then
            gh api "repos/$REPO/pages/deployments" --jq '.[0] | {status_url: .status_url, environment: .environment, created_at: .created_at, updated_at: .updated_at}' 2>/dev/null || echo '{"error": "No deployments found"}'
        else
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "🚀 Latest Pages Deployment:"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            gh api "repos/$REPO/pages/deployments" --jq '.[0] | {status_url: .status_url, environment: .environment, created_at: .created_at, updated_at: .updated_at}' 2>&1 || log_warn "No deployments found"
            echo ""
        fi
    fi
}

check_workflow_runs() {
    log_debug "Checking workflow runs..."
    
    if command -v gh &> /dev/null && gh auth status &> /dev/null; then
        if [ "$JSON_OUTPUT" = true ]; then
            gh run list --workflow="$WORKFLOW_NAME" --limit "$LIMIT" --json name,status,conclusion,createdAt,displayTitle 2>/dev/null || echo '{"error": "No workflow runs found"}'
        else
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "⚙️  Recent Workflow Runs ($WORKFLOW_NAME):"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            gh run list --workflow="$WORKFLOW_NAME" --limit "$LIMIT" 2>&1 || log_warn "No workflow runs found"
            echo ""
        fi
    fi
}

check_site_accessibility() {
    log_debug "Checking site accessibility..."
    
    local http_status
    http_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time "$TIMEOUT" "$GITHUB_PAGES_URL" 2>/dev/null || echo "000")
    
    if [ "$JSON_OUTPUT" = true ]; then
        local status_msg
        case "$http_status" in
            200) status_msg="accessible" ;;
            404) status_msg="not_found" ;;
            *) status_msg="unexpected" ;;
        esac
        echo "{\"url\":\"$GITHUB_PAGES_URL\",\"status_code\":\"$http_status\",\"status\":\"$status_msg\"}"
    else
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "🌐 Testing Site Accessibility:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        
        case "$http_status" in
            200)
                log_success "Site is accessible: $GITHUB_PAGES_URL"
                log_info "HTTP Status: $http_status"
                ;;
            404)
                log_failure "Site returns 404: $GITHUB_PAGES_URL"
                log_info "HTTP Status: $http_status"
                echo ""
                log_info "💡 Possible issues:"
                log_info "   1. GitHub Pages not enabled in repository settings"
                log_info "   2. No successful deployment yet"
                log_info "   3. Source not set to 'GitHub Actions'"
                ;;
            *)
                log_warn "Unexpected status: $http_status"
                ;;
        esac
        echo ""
    fi
}

show_setup_checklist() {
    if [ "$JSON_OUTPUT" = false ]; then
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📋 Quick Setup Checklist:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "1. [ ] GitHub Pages enabled in repo settings"
        echo "2. [ ] Source set to 'GitHub Actions'"
        echo "3. [ ] Workflow has run successfully"
        echo "4. [ ] Site returns 200 status code"
        echo ""
        log_info "🔗 Repository Settings: https://github.com/$REPO/settings/pages"
        log_info "🔗 Workflow Runs: https://github.com/$REPO/actions"
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
            -w|--workflow)
                WORKFLOW_NAME="$2"
                shift 2
                ;;
            -l|--limit)
                LIMIT="$2"
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
    
    # Check jq if using GitHub CLI
    if command -v gh &> /dev/null && gh auth status &> /dev/null; then
        if ! check_jq_dependency; then
            exit $EXIT_MISSING_DEPS
        fi
    fi
    
    # Check if python3 is available for JSON formatting
    if [ "$JSON_OUTPUT" = false ] && ! command -v python3 &> /dev/null; then
        log_warn "python3 not available, JSON formatting may be limited"
    fi
    
    if [ "$VERBOSE" = true ]; then
        log_info "Starting GitHub Pages status check"
        log_info "Repository: $REPO"
        log_info "API: $API"
        log_info "Pages URL: $GITHUB_PAGES_URL"
        log_info "Workflow: $WORKFLOW_NAME"
        log_info "Limit: $LIMIT"
        log_info "Timeout: $TIMEOUT"
        echo ""
    fi
    
    # Perform checks
    check_pages_configuration
    check_latest_deployment
    check_workflow_runs
    check_site_accessibility
    show_setup_checklist
    
    if [ "$VERBOSE" = true ]; then
        log_success "GitHub Pages status check completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
