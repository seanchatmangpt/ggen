#!/usr/bin/env bash
set -euo pipefail

# Act Release Script
# Runs release and homebrew-release workflows concurrently using act
# Follows core team best practices for CI/CD automation

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "act-release" "Run release workflows locally with act (parallel execution)"

# Script metadata
readonly SCRIPT_VERSION="2.0.0"

# Additional exit codes
readonly EXIT_WORKFLOW_FAILED=6
readonly EXIT_TIMEOUT=7
readonly EXIT_CONCURRENT_LIMIT=8

# Configuration
readonly DEFAULT_TIMEOUT="1800"  # 30 minutes
readonly DEFAULT_MAX_RETRIES="3"
readonly DEFAULT_CONCURRENT_LIMIT="2"

# Environment variables
TIMEOUT="${ACT_TIMEOUT:-$DEFAULT_TIMEOUT}"
MAX_RETRIES="${ACT_MAX_RETRIES:-$DEFAULT_MAX_RETRIES}"
CONCURRENT_LIMIT="${ACT_CONCURRENT_LIMIT:-$DEFAULT_CONCURRENT_LIMIT}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false
DRY_RUN=false
WORKFLOW_ONLY=false
HOMEBREW_ONLY=false
METRICS_ENABLED=false
RETRY_FAILED=false
TIMEOUT_ENABLED=false
WORKFLOW_TIMEOUT=0
START_TIME=0
ERRORS=0
WARNINGS=0

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Run release and homebrew-release workflows concurrently using act. Supports parallel execution, retry logic, and comprehensive monitoring." \
        "    -h, --help              Show this help message
    -v, --version           Show version information
    -d, --debug             Enable debug output
    -j, --json              Output results in JSON format
    --dry-run               Show what would be executed without running
    --workflow-only         Run only the release workflow
    --homebrew-only         Run only the homebrew-release workflow
    --retry-failed          Retry failed workflows automatically
    --timeout SEC           Set workflow timeout in seconds (default: 1800)
    --max-retries N         Maximum retry attempts (default: 3)
    --metrics               Enable performance metrics collection
    --concurrent-limit N    Limit concurrent workflows (default: 2)" \
        "    $SCRIPT_NAME                        # Run both workflows in parallel
    $SCRIPT_NAME --workflow-only         # Run only release workflow
    $SCRIPT_NAME --dry-run              # Show execution plan
    $SCRIPT_NAME --json                 # Output in JSON format
    $SCRIPT_NAME --retry-failed         # Retry failed workflows
    $SCRIPT_NAME --timeout 3600         # Set 1 hour timeout
    DEBUG=true $SCRIPT_NAME             # Enable debug output" \
        "    ACT_TIMEOUT               Workflow timeout in seconds (default: 1800)
    ACT_MAX_RETRIES          Maximum retry attempts (default: 3)
    ACT_CONCURRENT_LIMIT     Concurrent workflow limit (default: 2)
    DEBUG                    Enable debug output (true/false)"
}

# Version information
version() {
    echo "$SCRIPT_NAME version $SCRIPT_VERSION"
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit $EXIT_SUCCESS
                ;;
            -v|--version)
                version
                exit $EXIT_SUCCESS
                ;;
            -d|--debug)
                export DEBUG=true
                shift
                ;;
            -j|--json)
                JSON_OUTPUT=true
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --workflow-only)
                WORKFLOW_ONLY=true
                shift
                ;;
            --homebrew-only)
                HOMEBREW_ONLY=true
                shift
                ;;
            --retry-failed)
                RETRY_FAILED=true
                shift
                ;;
            --timeout)
                TIMEOUT="$2"
                TIMEOUT_ENABLED=true
                shift 2
                ;;
            --max-retries)
                MAX_RETRIES="$2"
                shift 2
                ;;
            --metrics)
                METRICS_ENABLED=true
                shift
                ;;
            --concurrent-limit)
                CONCURRENT_LIMIT="$2"
                shift 2
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit $EXIT_INVALID_ARGS
                ;;
        esac
    done
    
    # Validate mutually exclusive options
    if [[ "$WORKFLOW_ONLY" == "true" && "$HOMEBREW_ONLY" == "true" ]]; then
        log_error "Cannot specify both --workflow-only and --homebrew-only"
        exit $EXIT_INVALID_ARGS
    fi
    
    # Validate numeric options
    if ! [[ "$TIMEOUT" =~ ^[0-9]+$ ]] || [ "$TIMEOUT" -lt 60 ]; then
        log_error "Invalid timeout: $TIMEOUT (minimum 60 seconds)"
        exit $EXIT_INVALID_ARGS
    fi
    
    if ! [[ "$MAX_RETRIES" =~ ^[0-9]+$ ]] || [ "$MAX_RETRIES" -gt 10 ]; then
        log_error "Invalid max retries: $MAX_RETRIES (maximum 10)"
        exit $EXIT_INVALID_ARGS
    fi
    
    if ! [[ "$CONCURRENT_LIMIT" =~ ^[0-9]+$ ]] || [ "$CONCURRENT_LIMIT" -lt 1 ] || [ "$CONCURRENT_LIMIT" -gt 5 ]; then
        log_error "Invalid concurrent limit: $CONCURRENT_LIMIT (range 1-5)"
        exit $EXIT_INVALID_ARGS
    fi
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    # Check if we're in the right directory
    if ! validate_file_exists "$PROJECT_ROOT/Cargo.toml"; then
        log_error "Not in a Rust project root. Cargo.toml not found."
        return $EXIT_VALIDATION_FAILED
    fi
    
    # Validate workflow files exist
    local release_workflow="$PROJECT_ROOT/.github/workflows/release.yml"
    local homebrew_workflow="$PROJECT_ROOT/.github/workflows/homebrew-release.yml"
    
    if ! validate_file_exists "$release_workflow"; then
        log_error "Release workflow not found: $release_workflow"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_file_exists "$homebrew_workflow"; then
        log_error "Homebrew release workflow not found: $homebrew_workflow"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# Check system requirements
check_requirements() {
    log_info "Checking system requirements..."
    
    # Validate inputs first
    if ! validate_inputs; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Check dependencies
    if ! check_dependencies "docker" "git"; then
        exit $EXIT_MISSING_DEPS
    fi
    
    # Check Docker daemon
    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running"
        exit $EXIT_ERROR
    fi
    
    # Check available disk space (minimum 5GB)
    local available_space
    available_space=$(df "$PROJECT_ROOT" | awk 'NR==2 {print $4}')
    if [ "$available_space" -lt 5242880 ]; then  # 5GB in KB
        log_warn "Low disk space: $(($available_space / 1024 / 1024))GB available (minimum 5GB recommended)"
        WARNINGS=$((WARNINGS + 1))
    fi
    
    # Check available memory (minimum 4GB)
    if command -v free &> /dev/null; then
        local available_memory
        available_memory=$(free -m | awk 'NR==2{print $7}')
        if [ "$available_memory" -lt 4096 ]; then
            log_warn "Low memory: ${available_memory}MB available (minimum 4GB recommended)"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi
    
    log_success "System requirements satisfied"
}

# Metrics collection functions
start_metrics() {
    if [ "$METRICS_ENABLED" = true ]; then
        START_TIME=$(date +%s)
        log_debug "Metrics collection enabled"
    fi
}

collect_metrics() {
    if [ "$METRICS_ENABLED" = true ]; then
        local end_time
        end_time=$(date +%s)
        local duration=$((end_time - START_TIME))
        
        # Collect system metrics
        local cpu_usage=""
        local memory_usage=""
        local disk_usage=""
        
        if command -v top &> /dev/null; then
            if [[ "$OSTYPE" == "darwin"* ]]; then
                cpu_usage=$(top -l 1 | grep "CPU usage" | awk '{print $3}' | sed 's/%//')
            else
                cpu_usage=$(top -bn1 | grep "Cpu(s)" | awk '{print $2}' | sed 's/%us,//')
            fi
        fi
        
        if command -v free &> /dev/null; then
            memory_usage=$(free | awk 'NR==2{printf "%.1f", $3*100/$2}')
        fi
        
        if command -v df &> /dev/null; then
            disk_usage=$(df "$PROJECT_ROOT" | awk 'NR==2{print $5}' | sed 's/%//')
        fi
        
        METRICS_DATA="{\"duration\":$duration,\"cpu_usage\":\"$cpu_usage\",\"memory_usage\":\"$memory_usage\",\"disk_usage\":\"$disk_usage\",\"errors\":$ERRORS,\"warnings\":$WARNINGS}"
    fi
}

# JSON output functions
output_json_start() {
    if [ "$JSON_OUTPUT" = true ]; then
        echo "{"
        echo "  \"script\": \"$SCRIPT_NAME\","
        echo "  \"version\": \"$SCRIPT_VERSION\","
        echo "  \"timestamp\": \"$(date -Iseconds)\","
        echo "  \"project_root\": \"$PROJECT_ROOT\","
        echo "  \"options\": {"
        echo "    \"dry_run\": $DRY_RUN,"
        echo "    \"workflow_only\": $WORKFLOW_ONLY,"
        echo "    \"homebrew_only\": $HOMEBREW_ONLY,"
        echo "    \"retry_failed\": $RETRY_FAILED,"
        echo "    \"timeout\": $TIMEOUT,"
        echo "    \"max_retries\": $MAX_RETRIES,"
        echo "    \"concurrent_limit\": $CONCURRENT_LIMIT"
        echo "  },"
        echo "  \"workflows\": ["
    fi
}

output_json_workflow() {
    local workflow_name="$1"
    local status="$2"
    local exit_code="$3"
    local duration="$4"
    local message="$5"
    
    if [ "$JSON_OUTPUT" = true ]; then
        echo "    {"
        echo "      \"name\": \"$workflow_name\","
        echo "      \"status\": \"$status\","
        echo "      \"exit_code\": $exit_code,"
        echo "      \"duration\": $duration,"
        echo "      \"message\": \"$message\""
        echo "    }"
    fi
}

output_json_end() {
    if [ "$JSON_OUTPUT" = true ]; then
        echo "  ],"
        echo "  \"summary\": {"
        echo "    \"total_errors\": $ERRORS,"
        echo "    \"total_warnings\": $WARNINGS,"
        echo "    \"overall_status\": \"$([ $ERRORS -eq 0 ] && echo "success" || echo "failed")\""
        if [ "$METRICS_ENABLED" = true ] && [ -n "${METRICS_DATA:-}" ]; then
            echo "    ,\"metrics\": $METRICS_DATA"
        fi
        echo "  }"
        echo "}"
    fi
}

# Check/install act following core team practices
check_act() {
    log_info "Checking act installation..."
    
    if command -v act &> /dev/null; then
        local act_version
        act_version=$(act --version 2>/dev/null | head -n1 || echo "unknown")
        log_success "act is installed: $act_version"
        return
    fi
    
    log_warn "act is not installed"
    
    # Auto-install based on platform
    case "$OSTYPE" in
        darwin*)
            if command -v brew &> /dev/null; then
                log_info "Installing act via Homebrew..."
                if brew install act; then
                    log_success "act installed successfully via Homebrew"
                else
                    log_error "Failed to install act via Homebrew"
                    exit $EXIT_ERROR
                fi
            else
                log_error "Homebrew not found. Please install act manually: https://github.com/nektos/act"
                exit $EXIT_ERROR
            fi
            ;;
        linux*)
            log_info "Installing act via curl..."
            if curl -s https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash; then
                log_success "act installed successfully via install script"
            else
                log_error "Failed to install act. Please install manually: https://github.com/nektos/act"
                exit $EXIT_ERROR
            fi
            ;;
        *)
            log_error "Unsupported platform: $OSTYPE. Please install act manually: https://github.com/nektos/act"
            exit $EXIT_ERROR
            ;;
    esac
}

# Verify workflow files exist
check_workflows() {
    log_info "Verifying workflow files..."
    
    local release_workflow="$PROJECT_ROOT/.github/workflows/release.yml"
    local homebrew_workflow="$PROJECT_ROOT/.github/workflows/homebrew-release.yml"
    
    if [[ ! -f "$release_workflow" ]]; then
        log_error "Release workflow not found: $release_workflow"
        exit $EXIT_ERROR
    fi
    
    if [[ ! -f "$homebrew_workflow" ]]; then
        log_error "Homebrew release workflow not found: $homebrew_workflow"
        exit $EXIT_ERROR
    fi
    
    log_success "Workflow files verified"
}

# Get act configuration from Makefile.toml
get_act_config() {
    local makefile="$PROJECT_ROOT/Makefile.toml"
    
    if [[ ! -f "$makefile" ]]; then
        log_warn "Makefile.toml not found, using defaults"
        echo "ubuntu-latest=catthehacker/ubuntu:act-latest"
        echo "linux/amd64"
        return
    fi
    
    # Extract platform and container architecture
    local platform
    local container_arch
    
    platform=$(grep 'ACT_PLATFORM' "$makefile" | head -1 | cut -d'"' -f2 || echo "ubuntu-latest=catthehacker/ubuntu:act-latest")
    container_arch=$(grep 'ACT_CONTAINER_ARCH' "$makefile" | head -1 | cut -d'"' -f2 || echo "linux/amd64")
    
    echo "$platform"
    echo "$container_arch"
}

# Run workflow with proper error handling and retry logic
run_workflow() {
    local workflow_name="$1"
    local workflow_file="$2"
    local event_type="$3"
    local platform="$4"
    local container_arch="$5"
    
    local start_time
    start_time=$(date +%s)
    local attempt=1
    local max_attempts=$MAX_RETRIES
    
    log_info "Starting $workflow_name workflow..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "DRY RUN: Would execute: act $event_type -W $workflow_file --platform $platform --container-architecture $container_arch"
        if [ "$JSON_OUTPUT" = true ]; then
            output_json_workflow "$workflow_name" "dry_run" 0 0 "Would execute workflow"
        fi
        return $EXIT_SUCCESS
    fi
    
    # Create temporary log file for this workflow
    local log_file
    log_file=$(mktemp "/tmp/act-${workflow_name}-XXXXXX.log")
    
    # Retry loop
    while [ $attempt -le $max_attempts ]; do
        log_debug "Attempt $attempt/$max_attempts for $workflow_name workflow"
        
        # Run act with timeout if enabled
        local act_cmd="act $event_type -W $workflow_file --platform $platform --container-architecture $container_arch --verbose"
        
        if [ "$TIMEOUT_ENABLED" = true ]; then
            if timeout "$TIMEOUT" bash -c "$act_cmd" > "$log_file" 2>&1; then
                local end_time
                end_time=$(date +%s)
                local duration=$((end_time - start_time))
                
                log_success "$workflow_name workflow completed successfully (attempt $attempt, duration: ${duration}s)"
                
                if [ "$JSON_OUTPUT" = true ]; then
                    output_json_workflow "$workflow_name" "success" 0 "$duration" "Workflow completed successfully"
                fi
                
                rm -f "$log_file"
                return $EXIT_SUCCESS
            else
                local exit_code=$?
                if [ $exit_code -eq 124 ]; then
                    log_error "$workflow_name workflow timed out after ${TIMEOUT}s (attempt $attempt)"
                    ERRORS=$((ERRORS + 1))
                else
                    log_error "$workflow_name workflow failed (exit code: $exit_code, attempt $attempt)"
                    ERRORS=$((ERRORS + 1))
                fi
            fi
        else
            if eval "$act_cmd" > "$log_file" 2>&1; then
                local end_time
                end_time=$(date +%s)
                local duration=$((end_time - start_time))
                
                log_success "$workflow_name workflow completed successfully (attempt $attempt, duration: ${duration}s)"
                
                if [ "$JSON_OUTPUT" = true ]; then
                    output_json_workflow "$workflow_name" "success" 0 "$duration" "Workflow completed successfully"
                fi
                
                rm -f "$log_file"
                return $EXIT_SUCCESS
            else
                local exit_code=$?
                log_error "$workflow_name workflow failed (exit code: $exit_code, attempt $attempt)"
                ERRORS=$((ERRORS + 1))
            fi
        fi
        
        # Check if we should retry
        if [ $attempt -lt $max_attempts ] && [ "$RETRY_FAILED" = true ]; then
            log_warn "Retrying $workflow_name workflow in 5 seconds..."
            sleep 5
            attempt=$((attempt + 1))
        else
            break
        fi
    done
    
    # All attempts failed
    local end_time
    end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    log_error "$workflow_name workflow failed after $max_attempts attempts (total duration: ${duration}s)"
    log_error "Check log file: $log_file"
    
    if [ "$VERBOSE" = true ] || [ "$DEBUG" = true ]; then
        echo "Last 50 lines of log:"
        tail -n 50 "$log_file"
    fi
    
    if [ "$JSON_OUTPUT" = true ]; then
        output_json_workflow "$workflow_name" "failed" $exit_code "$duration" "Workflow failed after $max_attempts attempts"
    fi
    
    rm -f "$log_file"
    return $EXIT_WORKFLOW_FAILED
}

# Run workflows based on options
run_workflows() {
    log_info "Starting workflow execution..."
    
    # Get configuration
    local config
    config=$(get_act_config)
    local platform
    platform=$(echo "$config" | head -n1)
    local container_arch
    container_arch=$(echo "$config" | tail -n1)
    
    log_debug "Platform: $platform"
    log_debug "Container architecture: $container_arch"
    
    # Global variables for exit codes
    release_exit=$EXIT_SUCCESS
    homebrew_exit=$EXIT_SUCCESS
    
    # Run workflows based on options
    if [[ "${HOMEBREW_ONLY}" == "true" ]]; then
        # Run only homebrew workflow
        homebrew_exit=$(run_workflow "homebrew-release" \
            "$PROJECT_ROOT/.github/workflows/homebrew-release.yml" \
            "release" "$platform" "$container_arch")
    elif [[ "${WORKFLOW_ONLY}" == "true" ]]; then
        # Run only release workflow
        release_exit=$(run_workflow "release" \
            "$PROJECT_ROOT/.github/workflows/release.yml" \
            "push" "$platform" "$container_arch")
    else
        # Run both workflows in parallel
        log_info "Running workflows in parallel..."
        
        # Start release workflow in background
        (
            run_workflow "release" \
                "$PROJECT_ROOT/.github/workflows/release.yml" \
                "push" "$platform" "$container_arch"
        ) &
        local release_pid=$!
        
        # Start homebrew workflow in background
        (
            run_workflow "homebrew-release" \
                "$PROJECT_ROOT/.github/workflows/homebrew-release.yml" \
                "release" "$platform" "$container_arch"
        ) &
        local homebrew_pid=$!
        
        # Wait for both workflows to complete
        log_info "Waiting for workflows to complete..."
        wait $release_pid
        release_exit=$?
        wait $homebrew_pid
        homebrew_exit=$?
    fi
}

# Summary function
show_summary() {
    if [ "$JSON_OUTPUT" = false ]; then
        echo
        log_info "Workflow Results Summary:"
        
        if [ "$WORKFLOW_ONLY" != "true" ]; then
            if [ $homebrew_exit -eq $EXIT_SUCCESS ]; then
                log_success "Homebrew workflow: SUCCESS"
            else
                log_error "Homebrew workflow: FAILED (exit $homebrew_exit)"
            fi
        fi
        
        if [ "$HOMEBREW_ONLY" != "true" ]; then
            if [ $release_exit -eq $EXIT_SUCCESS ]; then
                log_success "Release workflow: SUCCESS"
            else
                log_error "Release workflow: FAILED (exit $release_exit)"
            fi
        fi
        
        if [ $WARNINGS -gt 0 ]; then
            log_warn "Total warnings: $WARNINGS"
        fi
        
        if [ $ERRORS -gt 0 ]; then
            log_error "Total errors: $ERRORS"
        fi
    fi
}

# Main execution function
main() {
    # Parse command line arguments first
    parse_args "$@"
    
    # Start JSON output if enabled
    if [ "$JSON_OUTPUT" = true ]; then
        output_json_start
    fi
    
    # Start metrics collection
    start_metrics
    
    if [ "$JSON_OUTPUT" = false ]; then
        log_info "Act Release Script v$SCRIPT_VERSION"
        log_info "Project root: $PROJECT_ROOT"
    fi
    
    # Check requirements and setup
    if [ "$JSON_OUTPUT" = false ]; then
        check_requirements
        check_act
    else
        # For JSON output, suppress logs and only check requirements
        check_requirements >/dev/null 2>&1
        check_act >/dev/null 2>&1
    fi
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "Configuration:"
        log_info "  Timeout: ${TIMEOUT}s"
        log_info "  Max retries: $MAX_RETRIES"
        log_info "  Concurrent limit: $CONCURRENT_LIMIT"
        log_info "  Retry failed: $RETRY_FAILED"
        log_info "  Metrics enabled: $METRICS_ENABLED"
        echo
    fi
    
    # Run workflows
    if [ "$JSON_OUTPUT" = false ]; then
        run_workflows
    else
        run_workflows >/dev/null 2>&1
    fi
    
    # Collect metrics
    collect_metrics
    
    # Show summary
    show_summary
    
    # End JSON output if enabled
    if [ "$JSON_OUTPUT" = true ]; then
        output_json_end
    fi
    
    # Overall result
    local total_exit=$((release_exit + homebrew_exit))
    if [ $total_exit -eq $EXIT_SUCCESS ]; then
        if [ "$JSON_OUTPUT" = false ]; then
            log_success "All workflows completed successfully!"
        fi
        exit $EXIT_SUCCESS
    else
        if [ "$JSON_OUTPUT" = false ]; then
            log_error "One or more workflows failed (total exit code: $total_exit)"
        fi
        exit $total_exit
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
