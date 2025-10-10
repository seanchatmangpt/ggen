#!/usr/bin/env bash
set -euo pipefail

# Act Release Script
# Runs release and homebrew-release workflows concurrently using act

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if act is installed
check_act() {
    if ! command -v act &> /dev/null; then
        log_warn "act is not installed"
        
        if [[ "$OSTYPE" == "darwin"* ]]; then
            log_info "Installing act via Homebrew..."
            if command -v brew &> /dev/null; then
                brew install act
                log_success "act installed successfully"
            else
                log_error "Homebrew not found. Please install act manually: https://github.com/nektos/act"
                exit 1
            fi
        else
            log_error "Please install act manually: https://github.com/nektos/act"
            exit 1
        fi
    else
        log_success "act is already installed"
    fi
}

# Verify workflow files exist
check_workflows() {
    local release_workflow=".github/workflows/release.yml"
    local homebrew_workflow=".github/workflows/homebrew-release.yml"
    
    if [[ ! -f "$release_workflow" ]]; then
        log_error "Release workflow not found: $release_workflow"
        exit 1
    fi
    
    if [[ ! -f "$homebrew_workflow" ]]; then
        log_error "Homebrew release workflow not found: $homebrew_workflow"
        exit 1
    fi
    
    log_success "Workflow files found"
}

# Run workflows in parallel
run_workflows() {
    log_info "Starting parallel workflow execution..."
    
    # Act configuration from Makefile.toml
    local platform="ubuntu-latest=catthehacker/ubuntu:act-latest"
    local container_arch="linux/amd64"
    
    # Run release workflow in background
    log_info "Starting release workflow..."
    act push -W .github/workflows/release.yml \
        --platform "$platform" \
        --container-architecture "$container_arch" \
        --verbose &
    RELEASE_PID=$!
    
    # Run homebrew-release workflow in background
    log_info "Starting homebrew-release workflow..."
    act release -W .github/workflows/homebrew-release.yml \
        --platform "$platform" \
        --container-architecture "$container_arch" \
        --verbose &
    HOMEBREW_PID=$!
    
    # Wait for both workflows to complete
    log_info "Waiting for workflows to complete..."
    wait $RELEASE_PID
    RELEASE_EXIT=$?
    
    wait $HOMEBREW_PID
    HOMEBREW_EXIT=$?
    
    # Report results
    echo
    log_info "Workflow Results:"
    if [[ $RELEASE_EXIT -eq 0 ]]; then
        log_success "Release workflow: SUCCESS (exit $RELEASE_EXIT)"
    else
        log_error "Release workflow: FAILED (exit $RELEASE_EXIT)"
    fi
    
    if [[ $HOMEBREW_EXIT -eq 0 ]]; then
        log_success "Homebrew workflow: SUCCESS (exit $HOMEBREW_EXIT)"
    else
        log_error "Homebrew workflow: FAILED (exit $HOMEBREW_EXIT)"
    fi
    
    # Overall result
    local total_exit=$((RELEASE_EXIT + HOMEBREW_EXIT))
    if [[ $total_exit -eq 0 ]]; then
        log_success "All workflows completed successfully!"
        exit 0
    else
        log_error "One or more workflows failed (total exit code: $total_exit)"
        exit $total_exit
    fi
}

# Main execution
main() {
    log_info "Act Release Script - Running workflows in parallel"
    echo
    
    check_act
    check_workflows
    run_workflows
}

# Run main function
main "$@"
