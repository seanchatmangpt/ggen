#!/bin/bash
# SHACL CLI - Deployment Script
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check dependencies
check_dependencies() {
    log_info "Checking dependencies..."

    if ! command -v cargo &> /dev/null; then
        log_error "Cargo is not installed. Please install Rust."
        exit 1
    fi

    if ! command -v rustc &> /dev/null; then
        log_error "Rustc is not installed. Please install Rust."
        exit 1
    fi

    log_success "All dependencies satisfied"
}

# Build project
build_project() {
    log_info "Building shacl-cli..."

    cd "$PROJECT_ROOT"

    # Clean previous builds
    cargo clean

    # Build in release mode
    cargo build --release --all-features

    if [ $? -eq 0 ]; then
        log_success "Build completed successfully"
    else
        log_error "Build failed"
        exit 1
    fi
}

# Run tests
run_tests() {
    log_info "Running tests..."

    cd "$PROJECT_ROOT"

    cargo test --all-features

    if [ $? -eq 0 ]; then
        log_success "All tests passed"
    else
        log_error "Tests failed"
        exit 1
    fi
}

# Install binary
install_binary() {
    log_info "Installing shacl-cli..."

    cd "$PROJECT_ROOT"

    cargo install --path . --force

    if [ $? -eq 0 ]; then
        log_success "Installation completed"
        log_info "shacl-cli installed to: $(which shacl-cli)"
    else
        log_error "Installation failed"
        exit 1
    fi
}

# Create sample shapes and data
create_samples() {
    log_info "Creating sample shapes and data..."

    SAMPLES_DIR="$PROJECT_ROOT/samples"
    mkdir -p "$SAMPLES_DIR"

    # Create sample shape
    cat > "$SAMPLES_DIR/person-shape.ttl" <<'EOF'
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string ;
    sh:minLength 1 ;
    sh:severity sh:Violation ;
    sh:message "Person must have exactly one name" ;
  ] ;
  sh:property [
    sh:path foaf:mbox ;
    sh:datatype xsd:string ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    sh:severity sh:Violation ;
    sh:message "Email must be valid format" ;
  ] ;
  sh:property [
    sh:path foaf:age ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150 ;
    sh:severity sh:Warning ;
    sh:message "Age should be between 0 and 150" ;
  ] .
EOF

    # Create valid sample data
    cat > "$SAMPLES_DIR/valid-person.ttl" <<'EOF'
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:john a foaf:Person ;
  foaf:name "John Doe" ;
  foaf:mbox "john.doe@example.com" ;
  foaf:age 30 .
EOF

    # Create invalid sample data
    cat > "$SAMPLES_DIR/invalid-person.ttl" <<'EOF'
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:jane a foaf:Person ;
  foaf:name "Jane Doe" ;
  foaf:name "J. Doe" ;
  foaf:mbox "invalid-email" ;
  foaf:age 200 .
EOF

    log_success "Sample files created in $SAMPLES_DIR"
}

# Run validation demo
run_demo() {
    log_info "Running validation demo..."

    SAMPLES_DIR="$PROJECT_ROOT/samples"

    if [ ! -d "$SAMPLES_DIR" ]; then
        create_samples
    fi

    echo ""
    log_info "Validating valid data..."
    shacl-cli validation validate "$SAMPLES_DIR/valid-person.ttl" \
        --shapes "$SAMPLES_DIR/person-shape.ttl" || true

    echo ""
    log_info "Validating invalid data..."
    shacl-cli validation validate "$SAMPLES_DIR/invalid-person.ttl" \
        --shapes "$SAMPLES_DIR/person-shape.ttl" || true

    echo ""
    log_success "Demo completed"
}

# Package for distribution
package_release() {
    log_info "Packaging release..."

    cd "$PROJECT_ROOT"

    VERSION=$(grep '^version' Cargo.toml | head -1 | cut -d'"' -f2)
    PACKAGE_NAME="shacl-cli-v${VERSION}"
    PACKAGE_DIR="target/package/${PACKAGE_NAME}"

    mkdir -p "$PACKAGE_DIR"

    # Copy binary
    cp target/release/shacl-cli "$PACKAGE_DIR/"

    # Copy documentation
    cp README.md LICENSE-MIT LICENSE-APACHE "$PACKAGE_DIR/"

    # Copy samples
    cp -r samples "$PACKAGE_DIR/" 2>/dev/null || true

    # Create archive
    cd target/package
    tar -czf "${PACKAGE_NAME}.tar.gz" "$PACKAGE_NAME"

    log_success "Package created: target/package/${PACKAGE_NAME}.tar.gz"
}

# Generate documentation
generate_docs() {
    log_info "Generating documentation..."

    cd "$PROJECT_ROOT"

    cargo doc --no-deps --all-features

    if [ $? -eq 0 ]; then
        log_success "Documentation generated at target/doc/shacl_cli/index.html"
    else
        log_error "Documentation generation failed"
        exit 1
    fi
}

# Main deployment workflow
main() {
    echo ""
    log_info "Starting SHACL CLI deployment..."
    echo ""

    check_dependencies
    build_project
    run_tests

    # Ask user if they want to install
    read -p "Install shacl-cli to system? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        install_binary
    fi

    # Ask user if they want to create samples
    read -p "Create sample files? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        create_samples
    fi

    # Ask user if they want to run demo
    read -p "Run validation demo? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        run_demo
    fi

    # Ask user if they want to package
    read -p "Package for distribution? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        package_release
    fi

    # Ask user if they want to generate docs
    read -p "Generate documentation? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        generate_docs
    fi

    echo ""
    log_success "Deployment completed successfully!"
    echo ""
}

# Script entry point
if [ "${1:-}" == "--help" ] || [ "${1:-}" == "-h" ]; then
    echo "SHACL CLI Deployment Script"
    echo ""
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  build        Build the project"
    echo "  test         Run tests"
    echo "  install      Install binary to system"
    echo "  samples      Create sample files"
    echo "  demo         Run validation demo"
    echo "  package      Package for distribution"
    echo "  docs         Generate documentation"
    echo "  all          Run complete deployment (default)"
    echo ""
    exit 0
fi

case "${1:-all}" in
    build)
        check_dependencies
        build_project
        ;;
    test)
        run_tests
        ;;
    install)
        install_binary
        ;;
    samples)
        create_samples
        ;;
    demo)
        run_demo
        ;;
    package)
        package_release
        ;;
    docs)
        generate_docs
        ;;
    all)
        main
        ;;
    *)
        log_error "Unknown command: $1"
        echo "Run '$0 --help' for usage information"
        exit 1
        ;;
esac
