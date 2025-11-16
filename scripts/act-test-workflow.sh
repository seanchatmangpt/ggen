#!/bin/bash
# Helper script for testing GitHub Actions workflows with act
# Usage: ./scripts/act-test-workflow.sh [workflow] [job]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration from Makefile.toml
ACT_CONTAINER_ARCH="${ACT_CONTAINER_ARCH:-linux/amd64}"
ACT_PLATFORM="${ACT_PLATFORM:-ubuntu-latest=catthehacker/ubuntu:act-latest}"
ACT_MEMORY="${ACT_MEMORY:-2g}"
ACT_CPUS="${ACT_CPUS:-2}"

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

# Check prerequisites
check_prerequisites() {
    print_header "Checking Prerequisites"
    
    if ! command -v act &> /dev/null; then
        print_error "act is not installed"
        echo ""
        echo "Installation instructions:"
        echo "  macOS: brew install act"
        echo "  Linux: curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash"
        exit 1
    fi
    print_success "act is installed ($(act --version))"
    
    if ! docker ps > /dev/null 2>&1; then
        print_error "Docker daemon is not running"
        echo "Start Docker Desktop or Docker daemon"
        exit 1
    fi
    print_success "Docker is running"
    
    echo ""
}

# List available workflows
list_workflows() {
    print_header "Available Workflows"
    
    if [ ! -d ".github/workflows" ]; then
        print_error ".github/workflows directory not found"
        exit 1
    fi
    
    echo "Workflows:"
    for workflow in .github/workflows/*.yml .github/workflows/*.yaml; do
        if [ -f "$workflow" ]; then
            basename "$workflow"
        fi
    done | sort
    
    echo ""
    print_info "Use: $0 [workflow] [job]"
    echo "Example: $0 ci.yml test"
    echo ""
}

# Validate workflow
validate_workflow() {
    local workflow="$1"
    print_header "Validating Workflow: $workflow"
    
    if [ ! -f ".github/workflows/$workflow" ]; then
        print_error "Workflow not found: .github/workflows/$workflow"
        exit 1
    fi
    
    if act --dryrun -W ".github/workflows/$workflow" > /dev/null 2>&1; then
        print_success "Workflow is valid"
        return 0
    else
        print_error "Workflow has errors"
        act --dryrun -W ".github/workflows/$workflow"
        return 1
    fi
}

# Run workflow
run_workflow() {
    local workflow="$1"
    local job="${2:-}"
    
    print_header "Running Workflow: $workflow${job:+ (job: $job)}"
    
    # Validate first
    if ! validate_workflow "$workflow"; then
        exit 1
    fi
    
    echo ""
    print_info "Configuration:"
    echo "  Container Architecture: $ACT_CONTAINER_ARCH"
    echo "  Platform: $ACT_PLATFORM"
    echo "  Memory: $ACT_MEMORY"
    echo "  CPUs: $ACT_CPUS"
    echo ""
    
    # Build act command
    local act_cmd=(
        act
        --container-architecture "$ACT_CONTAINER_ARCH"
        --platform "$ACT_PLATFORM"
        --memory "$ACT_MEMORY"
        --cpus "$ACT_CPUS"
        -W ".github/workflows/$workflow"
    )
    
    if [ -n "$job" ]; then
        act_cmd+=(-j "$job")
        print_info "Running job: $job"
    else
        print_info "Running all jobs"
    fi
    
    echo ""
    print_info "Executing: ${act_cmd[*]}"
    echo ""
    
    # Run act
    if "${act_cmd[@]}"; then
        echo ""
        print_success "Workflow completed successfully"
        return 0
    else
        echo ""
        print_error "Workflow failed"
        return 1
    fi
}

# Main
main() {
    # If no arguments, list workflows
    if [ $# -eq 0 ]; then
        check_prerequisites
        list_workflows
        exit 0
    fi
    
    # Check prerequisites
    check_prerequisites
    
    # Parse arguments
    local workflow="$1"
    local job="${2:-}"
    
    # Run workflow
    if run_workflow "$workflow" "$job"; then
        exit 0
    else
        exit 1
    fi
}

# Run main
main "$@"

