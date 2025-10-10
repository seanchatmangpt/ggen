#!/usr/bin/env bash
# Check GitHub Actions workflow status for Pages deployment

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "gh-workflow-status" "Check GitHub Actions workflow status for Pages deployment"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_WORKFLOW="Deploy Documentation to GitHub Pages"
readonly DEFAULT_LIMIT="10"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly WORKFLOW_NAME="${WORKFLOW_NAME:-$DEFAULT_WORKFLOW}"
readonly LIMIT="${LIMIT:-$DEFAULT_LIMIT}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false
SHOW_DETAILS=true

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Check GitHub Actions workflow status for Pages deployment. Shows recent workflow runs and detailed information about the latest run." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -j, --json           Output results in JSON format
    -w, --workflow NAME  Specify workflow name (default: Deploy Documentation to GitHub Pages)
    -l, --limit NUM      Limit number of workflow runs to show (default: 10)
    -s, --summary        Show summary only, skip detailed view
    -r, --run-id ID      Show details for specific run ID" \
        "    $SCRIPT_NAME                    # Check with defaults
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --json                 # Output in JSON format
    $SCRIPT_NAME --workflow \"My Workflow\"  # Check specific workflow
    $SCRIPT_NAME --limit 20             # Show more workflow runs
    $SCRIPT_NAME --run-id 12345678       # Show specific run details" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    WORKFLOW_NAME        Workflow name to check (default: $DEFAULT_WORKFLOW)
    LIMIT                Number of workflow runs to show (default: $DEFAULT_LIMIT)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_repo_format "$REPO"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$LIMIT" =~ ^[0-9]+$ ]] || [ "$LIMIT" -lt 1 ] || [ "$LIMIT" -gt 100 ]; then
        log_error "Invalid limit: $LIMIT"
        log_error "Limit must be a number between 1 and 100"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# GitHub CLI functions
list_workflow_runs() {
    log_debug "Listing workflow runs for: $WORKFLOW_NAME"
    
    if [ "$JSON_OUTPUT" = true ]; then
        gh run list --workflow="$WORKFLOW_NAME" --limit "$LIMIT" --json name,status,conclusion,createdAt,displayTitle,databaseId,url 2>/dev/null || echo '{"error": "No workflow runs found"}'
    else
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📊 Recent Workflow Runs ($WORKFLOW_NAME):"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        gh run list --workflow="$WORKFLOW_NAME" --limit "$LIMIT" 2>&1 || log_warn "No workflow runs found"
        echo ""
    fi
}

show_latest_run_details() {
    log_debug "Getting latest run details..."
    
    local latest_run
    latest_run=$(gh run list --workflow="$WORKFLOW_NAME" --limit 1 --json databaseId --jq '.[0].databaseId' 2>/dev/null || echo "")
    
    if [ -n "$latest_run" ] && [ "$latest_run" != "null" ]; then
        if [ "$JSON_OUTPUT" = true ]; then
            gh run view "$latest_run" --json name,status,conclusion,createdAt,updatedAt,displayTitle,url,workflowName 2>/dev/null || echo '{"error": "Could not fetch run details"}'
        else
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "🔍 Latest Run Details:"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            gh run view "$latest_run" 2>&1 || log_warn "Could not fetch run details"
            echo ""
            log_info "💡 To view logs: gh run view $latest_run --log"
            log_info "💡 To rerun: gh run rerun $latest_run"
        fi
    else
        if [ "$JSON_OUTPUT" = true ]; then
            echo '{"error": "No workflow runs found"}'
        else
            log_warn "No workflow runs found"
            echo ""
            log_info "💡 Trigger manually with: gh workflow run '$WORKFLOW_NAME'"
        fi
    fi
}

show_specific_run_details() {
    local run_id="$1"
    log_debug "Getting details for run ID: $run_id"
    
    if [ "$JSON_OUTPUT" = true ]; then
        gh run view "$run_id" --json name,status,conclusion,createdAt,updatedAt,displayTitle,url,workflowName 2>/dev/null || echo '{"error": "Could not fetch run details"}'
    else
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "🔍 Run Details (ID: $run_id):"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        gh run view "$run_id" 2>&1 || log_warn "Could not fetch run details"
        echo ""
        log_info "💡 To view logs: gh run view $run_id --log"
        log_info "💡 To rerun: gh run rerun $run_id"
    fi
}

# Main function
main() {
    local run_id=""
    
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
            -s|--summary)
                SHOW_DETAILS=false
                shift
                ;;
            -r|--run-id)
                run_id="$2"
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
    
    # Check GitHub CLI dependency
    if ! check_github_cli; then
        exit $EXIT_MISSING_DEPS
    fi
    
    # Check jq dependency
    if ! check_jq_dependency; then
        exit $EXIT_MISSING_DEPS
    fi
    
    if [ "$VERBOSE" = true ]; then
        log_info "Starting workflow status check"
        log_info "Repository: $REPO"
        log_info "Workflow: $WORKFLOW_NAME"
        log_info "Limit: $LIMIT"
        if [ -n "$run_id" ]; then
            log_info "Run ID: $run_id"
        fi
        echo ""
    fi
    
    # Perform checks
    if [ -n "$run_id" ]; then
        # Show specific run details
        show_specific_run_details "$run_id"
    else
        # List workflow runs
        list_workflow_runs
        
        # Show latest run details if requested
        if [ "$SHOW_DETAILS" = true ]; then
            show_latest_run_details
        fi
    fi
    
    if [ "$VERBOSE" = true ]; then
        log_success "Workflow status check completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
