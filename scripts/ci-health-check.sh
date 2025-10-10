#!/usr/bin/env bash
# Check GitHub Actions workflow health and report failures

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "ci-health-check" "Check GitHub Actions workflow health and report failures"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_WORKFLOWS_DIR=".github/workflows"
readonly DEFAULT_RECENT_LIMIT="10"
readonly DEFAULT_FAILED_LIMIT="20"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly WORKFLOWS_DIR="${WORKFLOWS_DIR:-$DEFAULT_WORKFLOWS_DIR}"
readonly RECENT_LIMIT="${RECENT_LIMIT:-$DEFAULT_RECENT_LIMIT}"
readonly FAILED_LIMIT="${FAILED_LIMIT:-$DEFAULT_FAILED_LIMIT}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false
WORKFLOW_FILTER=""
SHOW_FIXES=false
ISSUES_FOUND=0
MISSING_TIMEOUT=0
MISSING_CONCURRENCY=0

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Check GitHub Actions workflow health and report failures. Analyzes workflow files and recent runs to identify issues." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -j, --json           Output results in JSON format
    -w, --workflow NAME  Filter by specific workflow name
    -r, --recent NUM     Number of recent runs to check (default: 10)
    -f, --failed NUM     Number of failed runs to check (default: 20)
    --fix                Show suggested fixes for common issues
    --workflows-dir DIR  Workflows directory to check (default: .github/workflows)" \
        "    $SCRIPT_NAME                    # Check with defaults
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --json                 # Output in JSON format
    $SCRIPT_NAME --workflow \"My Workflow\"  # Check specific workflow
    $SCRIPT_NAME --recent 20             # Check more recent runs
    $SCRIPT_NAME --fix                   # Show suggested fixes" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    WORKFLOWS_DIR        Workflows directory (default: $DEFAULT_WORKFLOWS_DIR)
    RECENT_LIMIT         Number of recent runs to check (default: $DEFAULT_RECENT_LIMIT)
    FAILED_LIMIT         Number of failed runs to check (default: $DEFAULT_FAILED_LIMIT)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_repo_format "$REPO"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_directory_exists "$WORKFLOWS_DIR"; then
        log_error "Workflows directory not found: $WORKFLOWS_DIR"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$RECENT_LIMIT" =~ ^[0-9]+$ ]] || [ "$RECENT_LIMIT" -lt 1 ] || [ "$RECENT_LIMIT" -gt 100 ]; then
        log_error "Invalid recent limit: $RECENT_LIMIT"
        log_error "Limit must be between 1 and 100"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$FAILED_LIMIT" =~ ^[0-9]+$ ]] || [ "$FAILED_LIMIT" -lt 1 ] || [ "$FAILED_LIMIT" -gt 100 ]; then
        log_error "Invalid failed limit: $FAILED_LIMIT"
        log_error "Limit must be between 1 and 100"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# CI health checking functions
check_workflow_files() {
    log_debug "Checking workflow files..."
    
    local workflow_count=0
    local workflows=()
    
    for workflow in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow" ]; then
            workflow_count=$((workflow_count + 1))
            workflows+=("$(basename "$workflow" .yml)")
        fi
    done
    
    if [ "$JSON_OUTPUT" = true ]; then
        echo "{\"check\":\"workflow_files\",\"status\":\"pass\",\"message\":\"Found $workflow_count workflow files\",\"count\":$workflow_count,\"workflows\":$(printf '%s\n' "${workflows[@]}" | jq -R . | jq -s .)}"
    else
        if [ "$VERBOSE" = true ]; then
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "📋 Workflow Files:"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            for i in "${!workflows[@]}"; do
                echo "  $((i+1)). ${workflows[i]}.yml"
            done
            echo ""
        fi
        log_info "📊 Found $workflow_count workflow files"
    fi
    
    return $EXIT_SUCCESS
}

check_deprecated_actions() {
    log_debug "Checking for deprecated actions..."
    
    local deprecated_actions=(
        "actions/checkout@v1"
        "actions/checkout@v2"
        "actions/cache@v1"
        "actions/cache@v2"
        "actions/upload-artifact@v1"
        "actions/upload-artifact@v2"
        "actions/download-artifact@v1"
        "actions/create-release@v1"
        "actions/upload-release-asset@v1"
        "actions-rs/toolchain"
        "actions-rs/cargo"
    )
    
    local found_actions=()
    
    for action in "${deprecated_actions[@]}"; do
        if grep -r "$action" "$WORKFLOWS_DIR" --include="*.yml" -q; then
            local files
            files=$(grep -r "$action" "$WORKFLOWS_DIR" --include="*.yml" -l | xargs basename -a)
            found_actions+=("{\"action\":\"$action\",\"files\":\"$files\"}")
            ISSUES_FOUND=$((ISSUES_FOUND + 1))
        fi
    done
    
    if [ "$JSON_OUTPUT" = true ]; then
        if [ ${#found_actions[@]} -eq 0 ]; then
            echo "{\"check\":\"deprecated_actions\",\"status\":\"pass\",\"message\":\"No deprecated actions found\"}"
        else
            echo "{\"check\":\"deprecated_actions\",\"status\":\"fail\",\"message\":\"Found ${#found_actions[@]} deprecated actions\",\"actions\":[$(IFS=','; echo "${found_actions[*]}")]}"
        fi
    else
        if [ ${#found_actions[@]} -eq 0 ]; then
            log_success "No deprecated actions found"
        else
            log_failure "Found ${#found_actions[@]} deprecated actions"
            for action in "${deprecated_actions[@]}"; do
                if grep -r "$action" "$WORKFLOWS_DIR" --include="*.yml" -q; then
                    local files
                    files=$(grep -r "$action" "$WORKFLOWS_DIR" --include="*.yml" -l | xargs basename -a)
                    log_info "      Found deprecated: $action"
                    log_info "      In: $files"
                fi
            done
        fi
    fi
    
    return $EXIT_SUCCESS
}

check_deprecated_output() {
    log_debug "Checking for deprecated ::set-output..."
    
    if grep -r "::set-output" "$WORKFLOWS_DIR" --include="*.yml" -q; then
        local files
        files=$(grep -r "::set-output" "$WORKFLOWS_DIR" --include="*.yml" -l | xargs basename -a)
        ISSUES_FOUND=$((ISSUES_FOUND + 1))
        
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"deprecated_output\",\"status\":\"fail\",\"message\":\"Found deprecated ::set-output command\",\"files\":\"$files\",\"fix\":\"Replace with: echo \\\"key=value\\\" >> \\\$GITHUB_OUTPUT\"}"
        else
            log_failure "Found deprecated ::set-output command"
            log_info "      In: $files"
            log_info "      Replace with: echo \"key=value\" >> \$GITHUB_OUTPUT"
        fi
        return $EXIT_ERROR
    else
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"deprecated_output\",\"status\":\"pass\",\"message\":\"No deprecated ::set-output usage\"}"
        else
            log_success "No deprecated ::set-output usage"
        fi
        return $EXIT_SUCCESS
    fi
}

check_missing_timeouts() {
    log_debug "Checking for missing timeout-minutes..."
    
    MISSING_TIMEOUT=0
    local missing_workflows=()
    
    for workflow in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow" ]; then
            local workflow_name
            workflow_name=$(basename "$workflow" .yml)
            if ! grep -q "timeout-minutes:" "$workflow"; then
                MISSING_TIMEOUT=$((MISSING_TIMEOUT + 1))
                missing_workflows+=("$workflow_name")
            fi
        fi
    done
    
    if [ "$JSON_OUTPUT" = true ]; then
        if [ $MISSING_TIMEOUT -eq 0 ]; then
            echo "{\"check\":\"missing_timeouts\",\"status\":\"pass\",\"message\":\"All workflows have timeouts\",\"count\":0}"
        else
            echo "{\"check\":\"missing_timeouts\",\"status\":\"warn\",\"message\":\"$MISSING_TIMEOUT workflows missing timeout-minutes\",\"count\":$MISSING_TIMEOUT,\"workflows\":$(printf '%s\n' "${missing_workflows[@]}" | jq -R . | jq -s .)}"
        fi
    else
        if [ $MISSING_TIMEOUT -eq 0 ]; then
            log_success "All workflows have timeouts"
        else
            log_warn "$MISSING_TIMEOUT workflows missing timeout-minutes"
            for workflow in "${missing_workflows[@]}"; do
                log_info "      Missing timeout in: $workflow.yml"
            done
        fi
    fi
    
    return $EXIT_SUCCESS
}

check_missing_concurrency() {
    log_debug "Checking for missing concurrency control..."
    
    MISSING_CONCURRENCY=0
    local missing_workflows=()
    
    for workflow in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow" ]; then
            local workflow_name
            workflow_name=$(basename "$workflow" .yml)
            if ! grep -q "concurrency:" "$workflow"; then
                MISSING_CONCURRENCY=$((MISSING_CONCURRENCY + 1))
                missing_workflows+=("$workflow_name")
            fi
        fi
    done
    
    if [ "$JSON_OUTPUT" = true ]; then
        if [ $MISSING_CONCURRENCY -eq 0 ]; then
            echo "{\"check\":\"missing_concurrency\",\"status\":\"pass\",\"message\":\"All workflows have concurrency control\",\"count\":0}"
        else
            echo "{\"check\":\"missing_concurrency\",\"status\":\"warn\",\"message\":\"$MISSING_CONCURRENCY workflows missing concurrency control\",\"count\":$MISSING_CONCURRENCY,\"workflows\":$(printf '%s\n' "${missing_workflows[@]}" | jq -R . | jq -s .)}"
        fi
    else
        if [ $MISSING_CONCURRENCY -eq 0 ]; then
            log_success "All workflows have concurrency control"
        else
            log_warn "$MISSING_CONCURRENCY workflows missing concurrency control"
            for workflow in "${missing_workflows[@]}"; do
                log_info "      Missing concurrency in: $workflow.yml"
            done
        fi
    fi
    
    return $EXIT_SUCCESS
}

check_live_workflow_status() {
    log_debug "Checking live workflow status..."
    
    if ! check_github_cli; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"live_status\",\"status\":\"warn\",\"message\":\"GitHub CLI not available\",\"fix\":\"Install: brew install gh\"}"
        else
            log_warn "GitHub CLI not available - checking local workflow files only"
            log_info "      Install: brew install gh && gh auth login"
        fi
        return $EXIT_ERROR
    fi
    
    local gh_cmd="gh run list --limit $RECENT_LIMIT --json name,status,conclusion,createdAt,displayTitle"
    if [ -n "$WORKFLOW_FILTER" ]; then
        gh_cmd="$gh_cmd --workflow=\"$WORKFLOW_FILTER\""
    fi
    
    local recent_runs
    recent_runs=$(eval "$gh_cmd" 2>/dev/null || echo "")
    
    if [ -z "$recent_runs" ]; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"live_status\",\"status\":\"warn\",\"message\":\"No recent runs found\"}"
        else
            log_warn "No recent runs found"
        fi
        return $EXIT_ERROR
    fi
    
    if [ "$JSON_OUTPUT" = true ]; then
        echo "$recent_runs" | jq -r '.[] | {name: .name, status: .status, conclusion: .conclusion, created_at: .createdAt, display_title: .displayTitle}'
    else
        if [ "$VERBOSE" = true ]; then
            echo ""
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "🚀 Live Workflow Status (Last $RECENT_LIMIT runs):"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo ""
        fi
        
        echo "$recent_runs" | jq -r ".[] | \"\(.name): \(.status) (\(.conclusion // \"running\")) - \(.displayTitle)\"" | \
            while IFS= read -r line; do
                if echo "$line" | grep -q "failure"; then
                    log_failure "$line"
                elif echo "$line" | grep -q "success"; then
                    log_success "$line"
                elif echo "$line" | grep -q "cancelled"; then
                    log_info "⚪ $line"
                else
                    log_info "🔄 $line"
                fi
            done
    fi
    
    return $EXIT_SUCCESS
}

check_failed_workflows() {
    log_debug "Checking for failed workflows..."
    
    if ! check_github_cli; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"failed_workflows\",\"status\":\"warn\",\"message\":\"GitHub CLI not available\",\"fix\":\"Install: brew install gh\"}"
        else
            log_warn "GitHub CLI not available"
        fi
        return $EXIT_ERROR
    fi
    
    local gh_cmd="gh run list --limit $FAILED_LIMIT --json name,status,conclusion,databaseId,url"
    if [ -n "$WORKFLOW_FILTER" ]; then
        gh_cmd="$gh_cmd --workflow=\"$WORKFLOW_FILTER\""
    fi
    
    local failed_runs
    failed_runs=$(eval "$gh_cmd" 2>/dev/null | jq -r '.[] | select(.conclusion == "failure") | {name: .name, database_id: .databaseId, url: .url}' 2>/dev/null || echo "")
    
    if [ -z "$failed_runs" ]; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"failed_workflows\",\"status\":\"pass\",\"message\":\"No recent workflow failures\",\"count\":0}"
        else
            if [ "$VERBOSE" = true ]; then
                echo ""
                echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                echo "❌ Failed Workflows (if any):"
                echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                echo ""
            fi
            log_success "No recent workflow failures"
        fi
        return $EXIT_SUCCESS
    fi
    
    local failed_count
    failed_count=$(echo "$failed_runs" | jq -s 'length')
    
    if [ "$JSON_OUTPUT" = true ]; then
        echo "{\"check\":\"failed_workflows\",\"status\":\"fail\",\"message\":\"Found $failed_count failed workflows\",\"count\":$failed_count,\"runs\":$failed_runs}"
    else
        if [ "$VERBOSE" = true ]; then
            echo ""
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "❌ Failed Workflows (if any):"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo ""
        fi
        
        echo "$failed_runs" | jq -r '.[] | "\(.name) (ID: \(.database_id)) - \(.url)"' | \
            while IFS= read -r run; do
                log_failure "$run"
                ISSUES_FOUND=$((ISSUES_FOUND + 1))
            done
        
        if [ "$VERBOSE" = true ]; then
            echo ""
            log_info "💡 View logs: gh run view <ID> --log"
            log_info "💡 Rerun failed: gh run rerun <ID>"
        fi
    fi
    
    return $EXIT_ERROR
}

show_suggested_fixes() {
    if [ "$SHOW_FIXES" = true ] && [ "$JSON_OUTPUT" = false ]; then
        echo ""
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "🔧 Suggested Fixes:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "1. Update deprecated actions to latest versions"
        echo "2. Add timeout-minutes to prevent hung jobs"
        echo "3. Add concurrency control to save CI minutes"
        echo "4. Check failed workflow logs: gh run view <ID> --log"
        echo ""
        log_info "🔗 View all runs: https://github.com/$REPO/actions"
    fi
}

show_summary() {
    # Count workflow files
    local workflow_count=0
    for workflow in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow" ]; then
            workflow_count=$((workflow_count + 1))
        fi
    done
    
    if [ "$JSON_OUTPUT" = true ]; then
        local exit_code=0
        if [ $ISSUES_FOUND -gt 0 ]; then
            exit_code=1
        elif [ $MISSING_TIMEOUT -gt 0 ] || [ $MISSING_CONCURRENCY -gt 0 ]; then
            exit_code=0
        fi
        
        echo "{\"summary\":{\"workflows\":$workflow_count,\"issues_found\":$ISSUES_FOUND,\"missing_timeouts\":$MISSING_TIMEOUT,\"missing_concurrency\":$MISSING_CONCURRENCY,\"exit_code\":$exit_code}}"
    else
        echo ""
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📊 Summary:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo ""
        echo "  Workflows:             $workflow_count"
        echo "  Issues Found:          $ISSUES_FOUND"
        echo "  Missing Timeouts:      $MISSING_TIMEOUT"
        echo "  Missing Concurrency:   $MISSING_CONCURRENCY"
        echo ""
        
        if [ $ISSUES_FOUND -eq 0 ] && [ $MISSING_TIMEOUT -eq 0 ]; then
            log_success "All workflow health checks passed!"
        elif [ $ISSUES_FOUND -eq 0 ]; then
            log_warn "No critical issues, but some improvements recommended"
        else
            log_failure "Found $ISSUES_FOUND issues that should be addressed"
            echo ""
            log_info "💡 Next steps:"
            log_info "   1. Update deprecated actions to latest versions"
            log_info "   2. Add timeout-minutes to prevent hung jobs"
            log_info "   3. Add concurrency control to save CI minutes"
            log_info "   4. Check failed workflow logs: gh run view <ID> --log"
        fi
        echo ""
        log_info "🔗 View all runs: https://github.com/$REPO/actions"
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
                WORKFLOW_FILTER="$2"
                shift 2
                ;;
            -r|--recent)
                RECENT_LIMIT="$2"
                shift 2
                ;;
            -f|--failed)
                FAILED_LIMIT="$2"
                shift 2
                ;;
            --fix)
                SHOW_FIXES=true
                shift
                ;;
            --workflows-dir)
                WORKFLOWS_DIR="$2"
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
    if ! check_dependencies "find" "grep"; then
        exit $EXIT_MISSING_DEPS
    fi
    
    # Check jq dependency
    if ! check_jq_dependency; then
        exit $EXIT_MISSING_DEPS
    fi
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "Starting CI health check"
        log_info "Repository: $REPO"
        log_info "Workflows directory: $WORKFLOWS_DIR"
        log_info "Recent limit: $RECENT_LIMIT"
        log_info "Failed limit: $FAILED_LIMIT"
        if [ -n "$WORKFLOW_FILTER" ]; then
            log_info "Workflow filter: $WORKFLOW_FILTER"
        fi
        echo ""
    fi
    
    # Perform checks
    check_workflow_files
    check_deprecated_actions
    check_deprecated_output
    check_missing_timeouts
    check_missing_concurrency
    check_live_workflow_status
    check_failed_workflows
    
    # Show suggested fixes if requested
    show_suggested_fixes
    
    # Show summary
    show_summary
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_success "CI health check completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
