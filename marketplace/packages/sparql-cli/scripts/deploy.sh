#!/bin/bash
set -euo pipefail

# SPARQL CLI Deployment Script
# Builds, tests, and deploys the SPARQL CLI tool

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
VERSION="${VERSION:-1.0.0}"
TARGET="${TARGET:-release}"

echo "=========================================="
echo "SPARQL CLI Deployment Script"
echo "=========================================="
echo "Version: $VERSION"
echo "Target: $TARGET"
echo "Project: $PROJECT_ROOT"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Step 1: Environment check
info "Checking environment..."
command -v cargo >/dev/null 2>&1 || error "cargo not found. Please install Rust."
command -v git >/dev/null 2>&1 || warn "git not found. Version info may be incomplete."

# Check Rust version
RUST_VERSION=$(rustc --version | awk '{print $2}')
info "Rust version: $RUST_VERSION"

# Step 2: Clean previous builds
info "Cleaning previous builds..."
cd "$PROJECT_ROOT"
cargo clean

# Step 3: Validate RDF ontology
info "Validating RDF ontology..."
if [ -f "rdf/ontology.ttl" ]; then
    # Basic Turtle syntax check
    grep -q "@prefix" rdf/ontology.ttl || error "Invalid Turtle syntax in ontology.ttl"

    # Count nouns and verbs
    NOUN_COUNT=$(grep -c "a cnv:Noun" rdf/ontology.ttl || true)
    VERB_COUNT=$(grep -c "a cnv:Verb" rdf/ontology.ttl || true)

    info "Ontology contains $NOUN_COUNT nouns and $VERB_COUNT verbs"

    [ "$NOUN_COUNT" -ge 4 ] || error "Expected at least 4 nouns, found $NOUN_COUNT"
    [ "$VERB_COUNT" -ge 16 ] || error "Expected at least 16 verbs, found $VERB_COUNT"
else
    error "Ontology file not found: rdf/ontology.ttl"
fi

# Step 4: Build
info "Building SPARQL CLI..."
if [ "$TARGET" = "release" ]; then
    cargo build --release
    BINARY_PATH="target/release/sparql-cli"
else
    cargo build
    BINARY_PATH="target/debug/sparql-cli"
fi

[ -f "$BINARY_PATH" ] || error "Build failed: binary not found at $BINARY_PATH"

# Get binary size
BINARY_SIZE=$(du -h "$BINARY_PATH" | cut -f1)
info "Binary size: $BINARY_SIZE"

# Step 5: Run tests
info "Running tests..."
cargo test --all-features

# Step 6: Run integration tests
info "Running integration tests..."
cargo test --test integration_test -- --nocapture

# Step 7: Run benchmarks (optional)
if [ "${RUN_BENCHMARKS:-false}" = "true" ]; then
    info "Running benchmarks..."
    cargo bench
fi

# Step 8: Check code quality
info "Checking code quality..."
cargo fmt -- --check || warn "Code formatting issues found. Run 'cargo fmt' to fix."
cargo clippy -- -D warnings || warn "Clippy warnings found."

# Step 9: Generate documentation
info "Generating documentation..."
cargo doc --no-deps

# Step 10: Package
info "Creating package..."
PACKAGE_DIR="$PROJECT_ROOT/target/package"
mkdir -p "$PACKAGE_DIR"

# Copy binary
cp "$BINARY_PATH" "$PACKAGE_DIR/"

# Copy RDF ontology
mkdir -p "$PACKAGE_DIR/rdf"
cp rdf/ontology.ttl "$PACKAGE_DIR/rdf/"

# Copy documentation
cp README.md "$PACKAGE_DIR/"
cp LICENSE-MIT "$PACKAGE_DIR/" 2>/dev/null || true
cp LICENSE-APACHE "$PACKAGE_DIR/" 2>/dev/null || true

# Copy diagrams
if [ -d "docs/diagrams" ]; then
    mkdir -p "$PACKAGE_DIR/docs/diagrams"
    cp docs/diagrams/*.puml "$PACKAGE_DIR/docs/diagrams/" 2>/dev/null || true
fi

# Create tarball
info "Creating distribution tarball..."
cd "$PROJECT_ROOT/target"
tar -czf "sparql-cli-$VERSION-$(uname -s)-$(uname -m).tar.gz" -C package .
TARBALL_SIZE=$(du -h "sparql-cli-$VERSION-$(uname -s)-$(uname -m).tar.gz" | cut -f1)
info "Tarball size: $TARBALL_SIZE"

# Step 11: Install (optional)
if [ "${INSTALL:-false}" = "true" ]; then
    info "Installing SPARQL CLI..."
    cargo install --path "$PROJECT_ROOT"

    # Verify installation
    command -v sparql-cli >/dev/null 2>&1 || error "Installation failed"
    INSTALLED_VERSION=$(sparql-cli --version | awk '{print $2}')
    info "Installed version: $INSTALLED_VERSION"
fi

# Step 12: Create checksums
info "Creating checksums..."
cd "$PROJECT_ROOT/target"
shasum -a 256 "sparql-cli-$VERSION-$(uname -s)-$(uname -m).tar.gz" > \
    "sparql-cli-$VERSION-$(uname -s)-$(uname -m).tar.gz.sha256"

# Step 13: Summary
echo ""
echo "=========================================="
echo "Deployment Summary"
echo "=========================================="
echo "Version: $VERSION"
echo "Binary: $BINARY_PATH ($BINARY_SIZE)"
echo "Package: target/package/"
echo "Tarball: target/sparql-cli-$VERSION-$(uname -s)-$(uname -m).tar.gz ($TARBALL_SIZE)"
echo "Checksum: target/sparql-cli-$VERSION-$(uname -s)-$(uname -m).tar.gz.sha256"
echo ""
echo "Next steps:"
echo "  1. Test binary: $BINARY_PATH --version"
echo "  2. Install: cargo install --path ."
echo "  3. Publish: cargo publish"
echo "=========================================="

info "Deployment completed successfully!"
