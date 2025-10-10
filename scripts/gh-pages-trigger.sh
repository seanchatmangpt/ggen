#!/usr/bin/env bash
# Trigger GitHub Pages deployment workflow manually

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "gh-pages-trigger" "Trigger GitHub Pages deployment workflow manually"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_WORKFLOW="Deploy Documentation to GitHub Pages"
readonly DEFAULT_BRANCH="master"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly WORKFLOW_NAME="${WORKFLOW_NAME:-$DEFAULT_WORKFLOW}"
readonly BRANCH="${BRANCH:-$DEFAULT_BRANCH}"

# Global variables
VERBOSE=false
FORCE=false
YES=false

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Trigger GitHub Pages deployment workflow manually. This will start a new deployment of the documentation to GitHub Pages." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -y, --yes            Skip confirmation prompt
    -f, --force          Force trigger without confirmation
    -w, --workflow NAME  Specify workflow name (default: Deploy Documentation to GitHub Pages)
    -b, --branch BRANCH  Specify branch to deploy (default: master)" \
        "    $SCRIPT_NAME                    # Trigger with confirmation
    $SCRIPT_NAME --yes                  # Skip confirmation prompt
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --workflow \"My Workflow\"  # Trigger specific workflow
    $SCRIPT_NAME --branch main          # Deploy from main branch" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    WORKFLOW_NAME        Workflow name to trigger (default: $DEFAULT_WORKFLOW)
    BRANCH               Branch to deploy (default: $DEFAULT_BRANCH)
    FORCE                Force trigger without confirmation (default: false)
    YES                  Skip confirmation prompt (default: false)
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
    
    if [ -z "$BRANCH" ]; then
        log_error "Branch cannot be empty"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# Workflow triggering functions
check_workflow_exists() {
    log_debug "Checking if workflow exists: $WORKFLOW_NAME"
    
    if gh workflow list --json name | jq -e ".[] | select(.name == \"$WORKFLOW_NAME\")" &>/dev/null; then
        return $EXIT_SUCCESS
    else
        log_error "Workflow '$WORKFLOW_NAME' not found"
        log_error "Available workflows:"
        gh workflow list --json name | jq -r '.[].name' | sed 's/^/  - /' || true
        return $EXIT_VALIDATION_FAILED
    fi
}

trigger_workflow() {
    log_debug "Triggering workflow: $WORKFLOW_NAME"
    
    if [ "$VERBOSE" = true ]; then
        log_info "Triggering workflow '$WORKFLOW_NAME' on branch '$BRANCH'"
    fi
    
    if gh workflow run "$WORKFLOW_NAME" --ref "$BRANCH" 2>/dev/null; then
        log_success "Workflow triggered successfully"
        
        if [ "$VERBOSE" = true ]; then
            echo ""
            log_info "💡 Check status with: cargo make gh-workflow-status"
            log_info "💡 Or visit: https://github.com/$REPO/actions"
            log_info "💡 View logs with: cargo make gh-pages-logs"
        fi
        return $EXIT_SUCCESS
    else
        log_error "Failed to trigger workflow"
        log_error "Check that the workflow exists and you have permission to trigger it"
        return $EXIT_ERROR
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
            -y|--yes)
                YES=true
                shift
                ;;
            -f|--force)
                FORCE=true
                shift
                ;;
            -w|--workflow)
                WORKFLOW_NAME="$2"
                shift 2
                ;;
            -b|--branch)
                BRANCH="$2"
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
        log_info "Starting workflow trigger"
        log_info "Repository: $REPO"
        log_info "Workflow: $WORKFLOW_NAME"
        log_info "Branch: $BRANCH"
        echo ""
    fi
    
    # Check if workflow exists
    if ! check_workflow_exists; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Confirmation prompt
    if [ "$FORCE" = false ] && [ "$YES" = false ]; then
        if ! confirm "Trigger workflow '$WORKFLOW_NAME' on branch '$BRANCH'?" "n"; then
            log_info "Workflow trigger cancelled"
            exit $EXIT_SUCCESS
        fi
    fi
    
    # Trigger the workflow
    if ! trigger_workflow; then
        exit $EXIT_ERROR
    fi
    
    if [ "$VERBOSE" = true ]; then
        log_success "Workflow trigger completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
