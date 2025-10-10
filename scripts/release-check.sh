#!/usr/bin/env bash
# Check if release artifacts are ready

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "release-check" "Check if release artifacts are ready"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_CARGO_FILE="Cargo.toml"
readonly DEFAULT_TIMEOUT="10"
readonly DEFAULT_MAX_WAIT="300"
readonly DEFAULT_TARGETS="aarch64-apple-darwin,x86_64-apple-darwin,x86_64-unknown-linux-gnu,aarch64-unknown-linux-gnu"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly CARGO_FILE="${CARGO_FILE:-$DEFAULT_CARGO_FILE}"
readonly TIMEOUT="${TIMEOUT:-$DEFAULT_TIMEOUT}"
readonly MAX_WAIT="${MAX_WAIT:-$DEFAULT_MAX_WAIT}"
readonly TARGETS="${TARGETS:-$DEFAULT_TARGETS}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false
WAIT_FOR_READY=false
VERSION=""
ALL_READY=true

# Help function
show_help() {
    show_help_template \
        "[VERSION] [OPTIONS]" \
        "Check if release artifacts are ready. Verifies that a GitHub release exists and has the expected assets for all target platforms." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -j, --json           Output results in JSON format
    -w, --wait           Wait until all artifacts are ready (polling)
    -t, --timeout SEC    HTTP request timeout in seconds (default: 10)
    -m, --max-wait SEC   Maximum wait time for --wait option (default: 300)
    -c, --cargo FILE     Cargo.toml file to read version from (default: Cargo.toml)
    --targets LIST       Comma-separated list of targets to check" \
        "    $SCRIPT_NAME                    # Check with version from Cargo.toml
    $SCRIPT_NAME 1.2.3               # Check specific version
    $SCRIPT_NAME --verbose            # Enable verbose output
    $SCRIPT_NAME --json               # Output in JSON format
    $SCRIPT_NAME --wait               # Wait until all artifacts are ready
    $SCRIPT_NAME --max-wait 600       # Wait up to 10 minutes
    $SCRIPT_NAME --targets \"target1,target2\" # Check specific targets" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    CARGO_FILE           Cargo.toml file to read version from (default: $DEFAULT_CARGO_FILE)
    TIMEOUT              HTTP request timeout in seconds (default: $DEFAULT_TIMEOUT)
    MAX_WAIT             Maximum wait time for --wait option (default: $DEFAULT_MAX_WAIT)
    TARGETS              Comma-separated list of targets (default: $DEFAULT_TARGETS)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_repo_format "$REPO"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if [ -z "$VERSION" ]; then
        log_error "Version cannot be empty"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_version "$VERSION"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$TIMEOUT" =~ ^[0-9]+$ ]] || [ "$TIMEOUT" -lt 1 ] || [ "$TIMEOUT" -gt 300 ]; then
        log_error "Invalid timeout: $TIMEOUT"
        log_error "Timeout must be a number between 1 and 300 seconds"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$MAX_WAIT" =~ ^[0-9]+$ ]] || [ "$MAX_WAIT" -lt 30 ] || [ "$MAX_WAIT" -gt 3600 ]; then
        log_error "Invalid max wait time: $MAX_WAIT"
        log_error "Max wait must be between 30 and 3600 seconds"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_file_exists "$CARGO_FILE"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# Release checking functions
check_tag_exists() {
    local version="$1"
    log_debug "Checking if tag exists: v$version"
    
    if git rev-parse "v$version" >/dev/null 2>&1; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"tag_exists\",\"status\":\"pass\",\"message\":\"Tag v$version exists\"}"
        else
            log_success "Tag v$version exists"
        fi
        return $EXIT_SUCCESS
    else
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"tag_exists\",\"status\":\"fail\",\"message\":\"Tag v$version does not exist\",\"fix\":\"Create with: git tag v$version && git push origin v$version\"}"
        else
            log_failure "Tag v$version does not exist"
            log_info "      Create with: git tag v$version && git push origin v$version"
        fi
        return $EXIT_ERROR
    fi
}

check_release_artifacts() {
    local version="$1"
    log_debug "Checking release artifacts for version: $version"
    
    local release_url="https://github.com/$REPO/releases/download/v$version"
    local targets_array
    IFS=',' read -ra targets_array <<< "$TARGETS"
    
    local all_ready=true
    local results=()
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "Checking release artifacts for v$version..."
        echo ""
    fi
    
    for target in "${targets_array[@]}"; do
        local artifact_url="$release_url/ggen-$target.tar.gz"
        local status="fail"
        local message="Artifact not found"
        
        if curl -s -f -I --max-time "$TIMEOUT" "$artifact_url" > /dev/null 2>&1; then
            status="pass"
            message="Artifact ready"
        else
            all_ready=false
        fi
        
        if [ "$JSON_OUTPUT" = true ]; then
            results+=("{\"target\":\"$target\",\"status\":\"$status\",\"message\":\"$message\",\"url\":\"$artifact_url\"}")
        else
            if [ "$VERBOSE" = true ]; then
                if [ "$status" = "pass" ]; then
                    log_success "$target: ✅"
                else
                    log_failure "$target: ❌"
                fi
            else
                echo -n "  $target: "
                if [ "$status" = "pass" ]; then
                    echo "✅"
                else
                    echo "❌"
                fi
            fi
        fi
    done
    
    if [ "$JSON_OUTPUT" = true ]; then
        echo "["
        local first=true
        for result in "${results[@]}"; do
            if [ "$first" = true ]; then
                first=false
            else
                echo ","
            fi
            echo -n "$result"
        done
        echo ""
        echo "]"
    fi
    
    if [ "$all_ready" = true ]; then
        ALL_READY=true
        return $EXIT_SUCCESS
    else
        ALL_READY=false
        return $EXIT_ERROR
    fi
}

wait_for_artifacts() {
    local version="$1"
    log_debug "Waiting for all artifacts to be ready: v$version"
    
    local start_time
    local elapsed_time
    start_time=$(date +%s)
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "Waiting for all artifacts to be ready..."
        log_info "Maximum wait time: $MAX_WAIT seconds"
        echo ""
    fi
    
    while true; do
        elapsed_time=$(($(date +%s) - start_time))
        
        if [ $elapsed_time -ge $MAX_WAIT ]; then
            if [ "$JSON_OUTPUT" = true ]; then
                echo "{\"check\":\"artifacts_wait\",\"status\":\"timeout\",\"message\":\"Artifacts not ready after $MAX_WAIT seconds\",\"elapsed\":$elapsed_time}"
            else
                log_error "Artifacts not ready after $MAX_WAIT seconds"
            fi
            return $EXIT_ERROR
        fi
        
        # Check if all artifacts are ready
        if check_release_artifacts "$version" >/dev/null 2>&1; then
            if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
                log_success "All artifacts are ready (waited $elapsed_time seconds)"
            fi
            return $EXIT_SUCCESS
        fi
        
        if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
            log_info "Some artifacts not ready yet, waiting 10 seconds... (elapsed: $elapsed_time/$MAX_WAIT)"
        fi
        
        sleep 10
    done
}

show_completion_message() {
    local version="$1"
    
    if [ "$JSON_OUTPUT" = false ]; then
        echo ""
        if [ "$ALL_READY" = true ]; then
            log_success "All release artifacts are ready!"
            echo ""
            log_info "Update Homebrew formula with:"
            log_info "  cargo make brew-update-formula $version"
        else
            log_warn "Some artifacts are not ready yet"
            log_info "      Check: https://github.com/$REPO/releases/tag/v$version"
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
            -w|--wait)
                WAIT_FOR_READY=true
                shift
                ;;
            -t|--timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            -m|--max-wait)
                MAX_WAIT="$2"
                shift 2
                ;;
            -c|--cargo)
                CARGO_FILE="$2"
                shift 2
                ;;
            --targets)
                TARGETS="$2"
                shift 2
                ;;
            -*)
                log_error "Unknown option: $1"
                show_help
                exit $EXIT_INVALID_ARGS
                ;;
            *)
                if [ -z "$VERSION" ]; then
                    VERSION="$1"
                else
                    log_error "Multiple versions specified: $VERSION and $1"
                    exit $EXIT_INVALID_ARGS
                fi
                shift
                ;;
        esac
    done
    
    # Get version from Cargo.toml if not provided
    if [ -z "$VERSION" ]; then
        VERSION=$(get_version_from_cargo "$CARGO_FILE")
        if [ $? -ne $EXIT_SUCCESS ] || [ -z "$VERSION" ]; then
            exit $EXIT_VALIDATION_FAILED
        fi
    fi
    
    # Validate inputs
    if ! validate_inputs; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Check dependencies
    if ! check_dependencies "curl"; then
        exit $EXIT_MISSING_DEPS
    fi
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "Starting release check"
        log_info "Repository: $REPO"
        log_info "Version: $VERSION"
        log_info "Targets: $TARGETS"
        log_info "Timeout: $TIMEOUT seconds"
        if [ "$WAIT_FOR_READY" = true ]; then
            log_info "Max wait: $MAX_WAIT seconds"
        fi
        echo ""
    fi
    
    # Check if tag exists
    if ! check_tag_exists "$VERSION"; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Check release artifacts
    if ! check_release_artifacts "$VERSION"; then
        if [ "$WAIT_FOR_READY" = true ]; then
            if ! wait_for_artifacts "$VERSION"; then
                exit $EXIT_ERROR
            fi
        else
            show_completion_message "$VERSION"
            exit $EXIT_ERROR
        fi
    fi
    
    # Show completion message
    show_completion_message "$VERSION"
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_success "Release check completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
