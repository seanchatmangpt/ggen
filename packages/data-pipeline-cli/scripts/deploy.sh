#!/bin/bash
set -euo pipefail

# Data Pipeline CLI - Deployment Script
# Deploys the data-pipeline-cli tool with production configuration

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo "=========================================="
echo "Data Pipeline CLI - Deployment"
echo "=========================================="
echo

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    if ! command -v cargo &> /dev/null; then
        log_error "Rust/Cargo not found. Please install from https://rustup.rs/"
        exit 1
    fi

    log_info "Rust version: $(rustc --version)"
    log_info "Cargo version: $(cargo --version)"
}

# Build release binary
build_release() {
    log_info "Building release binary..."
    cd "$PROJECT_DIR"

    cargo build --release --all-features

    if [ $? -eq 0 ]; then
        log_info "Build successful!"
        log_info "Binary location: $PROJECT_DIR/target/release/data-pipeline"
    else
        log_error "Build failed!"
        exit 1
    fi
}

# Run tests
run_tests() {
    log_info "Running tests..."
    cd "$PROJECT_DIR"

    cargo test --all-features

    if [ $? -eq 0 ]; then
        log_info "All tests passed!"
    else
        log_error "Tests failed!"
        exit 1
    fi
}

# Install binary
install_binary() {
    log_info "Installing binary..."
    cd "$PROJECT_DIR"

    cargo install --path . --force

    if [ $? -eq 0 ]; then
        log_info "Installation successful!"
        log_info "Binary installed to: $(which data-pipeline)"
    else
        log_error "Installation failed!"
        exit 1
    fi
}

# Verify installation
verify_installation() {
    log_info "Verifying installation..."

    if command -v data-pipeline &> /dev/null; then
        log_info "data-pipeline is accessible"
        data-pipeline --version
    else
        log_error "data-pipeline not found in PATH"
        exit 1
    fi
}

# Create example data
create_example_data() {
    log_info "Creating example data directory..."

    mkdir -p "$PROJECT_DIR/examples/data"

    # Create sample CSV
    cat > "$PROJECT_DIR/examples/data/users.csv" << 'EOF'
firstName,lastName,email,age,status,country
Alice,Smith,alice@example.com,30,active,US
Bob,Jones,bob@example.com,25,active,UK
Charlie,Brown,charlie@example.com,17,inactive,CA
Diana,Wilson,diana@example.com,42,active,AU
Eve,Davis,eve@example.com,35,active,US
Frank,Miller,,28,active,UK
EOF

    log_info "Sample data created in examples/data/"
}

# Display usage information
show_usage() {
    echo
    log_info "Deployment complete! Try these commands:"
    echo
    echo "  # Create a pipeline"
    echo "  data-pipeline pipeline create \\"
    echo "    --name 'csv-to-rdf' \\"
    echo "    --sources 'csv:users.csv' \\"
    echo "    --transforms 'map:user-schema' \\"
    echo "    --sinks 'rdf:oxigraph://store.db'"
    echo
    echo "  # Run the pipeline"
    echo "  data-pipeline pipeline run --name 'csv-to-rdf'"
    echo
    echo "  # Monitor execution"
    echo "  data-pipeline pipeline monitor --name 'csv-to-rdf' --metrics all"
    echo
    log_info "For more examples, see: $PROJECT_DIR/examples/"
}

# Main deployment flow
main() {
    check_prerequisites
    build_release
    run_tests
    install_binary
    verify_installation
    create_example_data
    show_usage

    echo
    log_info "Deployment completed successfully!"
}

# Run main
main
