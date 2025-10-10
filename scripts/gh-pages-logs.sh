#!/usr/bin/env bash
# View logs from latest GitHub Pages deployment

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "gh-pages-logs" "View logs from latest GitHub Pages deployment"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_WORKFLOW="Deploy Documentation to GitHub Pages"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly WORKFLOW_NAME="${WORKFLOW_NAME:-$DEFAULT_WORKFLOW}"

# Global variables
VERBOSE=false
FOLLOW=false
RUN_ID=""

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "View logs from latest GitHub Pages deployment. Shows detailed logs from workflow runs to help debug deployment issues." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -f, --follow         Follow logs in real-time (live streaming)
    -r, --run-id ID      Show logs for specific run ID
    -w, --workflow NAME  Specify workflow name (default: Deploy Documentation to GitHub Pages)" \
        "    $SCRIPT_NAME                    # Show latest logs
    $SCRIPT_NAME --follow               # Follow logs in real-time
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --run-id 12345678       # Show specific run logs
    $SCRIPT_NAME --workflow \"My Workflow\"  # Show logs for specific workflow" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    WORKFLOW_NAME        Workflow name to check (default: $DEFAULT_WORKFLOW)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_repo_format "$REPO"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if [ -z "$WORKFLOW_NAME" ]; then
        log_error "Workflow name cannot be empty"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if [ -n "$RUN_ID" ] && ! [[ "$RUN_ID" =~ ^[0-9]+$ ]]; then
        log_error "Invalid run ID: $RUN_ID"
        log_error "Run ID must be a number"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# Log viewing functions
get_latest_run_id() {
    log_debug "Getting latest run ID for workflow: $WORKFLOW_NAME"
    
    local latest_run
    latest_run=$(gh run list --workflow="$WORKFLOW_NAME" --limit 1 --json databaseId --jq '.[0].databaseId' 2>/dev/null || echo "")
    
    if [ -n "$latest_run" ] && [ "$latest_run" != "null" ]; then
        echo "$latest_run"
        return $EXIT_SUCCESS
    else
        return $EXIT_ERROR
    fi
}

check_run_exists() {
    local run_id="$1"
    log_debug "Checking if run exists: $run_id"
    
    if gh run view "$run_id" --json databaseId &>/dev/null; then
        return $EXIT_SUCCESS
    else
        log_error "Run ID $run_id not found"
        return $EXIT_VALIDATION_FAILED
    fi
}

show_run_logs() {
    local run_id="$1"
    log_debug "Showing logs for run: $run_id"
    
    if [ "$VERBOSE" = true ]; then
        log_info "Fetching logs for run ID: $run_id"
        echo ""
    fi
    
    if [ "$FOLLOW" = true ]; then
        log_info "Following logs in real-time (press Ctrl+C to stop)..."
        echo ""
        gh run view "$run_id" --log --follow 2>/dev/null || {
            log_error "Failed to follow logs"
            return $EXIT_ERROR
        }
    else
        gh run view "$run_id" --log 2>/dev/null || {
            log_error "Failed to fetch logs"
            return $EXIT_ERROR
        }
    fi
    
    return $EXIT_SUCCESS
}

show_run_summary() {
    local run_id="$1"
    log_debug "Showing run summary for: $run_id"
    
    if [ "$VERBOSE" = true ]; then
        echo ""
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📊 Run Summary:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        gh run view "$run_id" --json name,status,conclusion,createdAt,updatedAt,displayTitle,url,workflowName | jq -r '
            "Workflow: \(.workflowName)
Name: \(.displayTitle)
Status: \(.status)
Conclusion: \(.conclusion // "running")
Created: \(.createdAt)
Updated: \(.updatedAt)
URL: \(.url)"
        ' 2>/dev/null || log_warn "Could not fetch run summary"
        echo ""
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
            -f|--follow)
                FOLLOW=true
                shift
                ;;
            -r|--run-id)
                RUN_ID="$2"
                shift 2
                ;;
            -w|--workflow)
                WORKFLOW_NAME="$2"
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
        log_info "Starting log viewer"
        log_info "Repository: $REPO"
        log_info "Workflow: $WORKFLOW_NAME"
        if [ -n "$RUN_ID" ]; then
            log_info "Run ID: $RUN_ID"
        fi
        if [ "$FOLLOW" = true ]; then
            log_info "Mode: Follow (real-time)"
        else
            log_info "Mode: Static (one-time)"
        fi
        echo ""
    fi
    
    # Determine run ID
    if [ -z "$RUN_ID" ]; then
        log_info "Fetching logs from latest deployment..."
        echo ""
        
        RUN_ID=$(get_latest_run_id)
        if [ $? -ne $EXIT_SUCCESS ]; then
            log_warn "No workflow runs found for '$WORKFLOW_NAME'"
            log_info "💡 Trigger a deployment with: cargo make gh-pages-trigger"
            exit $EXIT_ERROR
        fi
        
        if [ "$VERBOSE" = true ]; then
            log_info "Using latest run ID: $RUN_ID"
        fi
    else
        # Validate provided run ID
        if ! check_run_exists "$RUN_ID"; then
            exit $EXIT_VALIDATION_FAILED
        fi
    fi
    
    # Show run summary if verbose
    if [ "$VERBOSE" = true ]; then
        show_run_summary "$RUN_ID"
    fi
    
    # Show logs
    if ! show_run_logs "$RUN_ID"; then
        exit $EXIT_ERROR
    fi
    
    if [ "$VERBOSE" = true ] && [ "$FOLLOW" = false ]; then
        log_success "Log viewing completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
