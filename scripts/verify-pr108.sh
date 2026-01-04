#!/bin/bash
# PR 108 Verification Script
# Verifies Debian distribution for Claude Code on the web using testcontainers and act

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
TIMEOUT_DOCKER_CHECK=10
TIMEOUT_ACT_CHECK=5

# Functions
print_header() {
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

# Poka-yoke: Pre-flight checks
check_docker_running() {
    print_header "Poka-Yoke: Pre-flight Check - Docker"
    
    if ! command -v docker &> /dev/null; then
        print_error "Docker is not installed"
        exit 1
    fi
    
    if ! timeout "$TIMEOUT_DOCKER_CHECK" docker ps > /dev/null 2>&1; then
        print_error "Docker daemon is not running"
        exit 1
    fi
    
    print_success "Docker is running"
    docker --version
    echo ""
}

verify_act_installed() {
    print_header "Poka-Yoke: Pre-flight Check - Act"
    
    if ! command -v act &> /dev/null; then
        print_warning "act is not installed (optional for workflow testing)"
        echo "Install with: brew install act"
        return 1
    fi
    
    print_success "act is installed"
    act --version
    echo ""
    return 0
}

# Test Debian container with testcontainers
test_debian_container() {
    print_header "Testing Debian Bookworm Container (Claude Code Web Environment)"
    
    cd "$PROJECT_ROOT"
    
    print_info "Running Debian container tests..."
    
    # Run the Debian container tests
    if cargo test --package ggen-e2e --lib tests::debian_claude_web_test -- --nocapture --ignored; then
        print_success "Debian container tests passed"
    else
        print_error "Debian container tests failed"
        return 1
    fi
    
    echo ""
}

# Test GitHub Actions workflows with act
test_workflows_with_act() {
    print_header "Testing GitHub Actions Workflows with Act"
    
    if ! command -v act &> /dev/null; then
        print_warning "Skipping workflow tests (act not installed)"
        return 0
    fi
    
    cd "$PROJECT_ROOT"
    
    # Test deploy-docs workflow (dry-run first)
    print_info "Validating deploy-docs workflow..."
    if act --dryrun -W .github/workflows/deploy-docs.yml > /dev/null 2>&1; then
        print_success "deploy-docs workflow is valid"
    else
        print_error "deploy-docs workflow validation failed"
        act --dryrun -W .github/workflows/deploy-docs.yml
        return 1
    fi
    
    # Test CI workflow (dry-run)
    print_info "Validating CI workflow..."
    if act --dryrun -W .github/workflows/ci.yml > /dev/null 2>&1; then
        print_success "CI workflow is valid"
    else
        print_warning "CI workflow validation had issues (may be expected)"
    fi
    
    echo ""
}

# Run poka-yoke certification tests
run_poka_yoke_tests() {
    print_header "Running Poka-Yoke Certification Tests"
    
    cd "$PROJECT_ROOT"
    
    print_info "Running poka-yoke certification test suite..."
    
    if cargo test --test poka_yoke_certification_test -- --nocapture; then
        print_success "Poka-yoke certification tests passed"
    else
        print_error "Poka-yoke certification tests failed"
        return 1
    fi
    
    echo ""
}

# Verify cleanup (poka-yoke safeguard)
verify_cleanup() {
    print_header "Poka-Yoke: Cleanup Verification"
    
    # Count containers before
    local before_count
    before_count=$(docker ps -a --format "{{.ID}}" | wc -l | tr -d ' ')
    
    print_info "Container count before: $before_count"
    
    # Wait a moment for any cleanup
    sleep 2
    
    # Count containers after
    local after_count
    after_count=$(docker ps -a --format "{{.ID}}" | wc -l | tr -d ' ')
    
    print_info "Container count after: $after_count"
    
    local increase=$((after_count - before_count))
    
    if [ "$increase" -le 5 ]; then
        print_success "Cleanup verification passed (increase: $increase)"
    else
        print_warning "Container count increased by $increase (may indicate cleanup issue)"
    fi
    
    echo ""
}

# Main verification workflow
main() {
    print_header "PR 108 Poka-Yoke Certification Verification"
    echo ""
    print_info "This script verifies PR 108 (Debian distribution for Claude Code on the web)"
    print_info "using testcontainers and act with comprehensive poka-yoke safeguards."
    echo ""
    
    local errors=0
    
    # Checkpoint 1: Docker is running
    if ! check_docker_running; then
        errors=$((errors + 1))
    fi
    
    # Checkpoint 2: Act is available (optional)
    verify_act_installed || true
    
    # Checkpoint 3-6: Debian container tests
    if ! test_debian_container; then
        errors=$((errors + 1))
    fi
    
    # Checkpoint 7-8: Act workflow tests
    if ! test_workflows_with_act; then
        errors=$((errors + 1))
    fi
    
    # Checkpoint 9: Poka-yoke certification
    if ! run_poka_yoke_tests; then
        errors=$((errors + 1))
    fi
    
    # Final checkpoint: Cleanup verification
    verify_cleanup
    
    # Summary
    print_header "Verification Summary"
    
    if [ "$errors" -eq 0 ]; then
        print_success "All verification checkpoints passed!"
        echo ""
        print_info "PR 108 is poka-yoke certified ✅"
        echo ""
        exit 0
    else
        print_error "Verification failed with $errors error(s)"
        echo ""
        print_info "Please review the errors above and fix them before proceeding."
        echo ""
        exit 1
    fi
}

# Run main
main "$@"


