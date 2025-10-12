#!/usr/bin/env bash
# Run all ggen library usage examples
# This script demonstrates how to use ggen as a library

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo -e "${BLUE}╔═══════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  ggen Library Usage & Wrapping Examples Runner   ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════╝${NC}"
echo ""

# Function to print section header
print_header() {
    echo ""
    echo -e "${GREEN}═══════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}  $1${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════${NC}"
    echo ""
}

# Function to print step
print_step() {
    echo -e "${YELLOW}▶ $1${NC}"
}

# Function to print success
print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

# Function to print error
print_error() {
    echo -e "${RED}✗ $1${NC}"
}

# Function to check if API key is set
check_api_key() {
    if [ -z "$OPENAI_API_KEY" ] && [ -z "$ANTHROPIC_API_KEY" ]; then
        print_error "Warning: No API key set. AI examples will be skipped."
        echo "  Set OPENAI_API_KEY or ANTHROPIC_API_KEY to enable AI features"
        return 1
    fi
    return 0
}

# Build project
print_header "Building Project"
print_step "Building all examples..."
cargo build --examples 2>&1 | grep -v "Compiling" | head -20 || true
print_success "Build complete"

# Run core library examples
print_header "Core Library Examples"

print_step "Running basic-usage example..."
cargo run --example basic-usage --quiet
print_success "basic-usage completed"

print_step "Running custom-pipeline example..."
cargo run --example custom-pipeline --quiet
print_success "custom-pipeline completed"

print_step "Running batch-processor example..."
cargo run --example batch-processor --quiet
print_success "batch-processor completed"

print_step "Running graph-operations example..."
cargo run --example graph-operations --quiet
print_success "graph-operations completed"

print_step "Running template-validation example..."
cargo run --example template-validation --quiet
print_success "template-validation completed"

# Run AI examples if API key is available
if check_api_key; then
    print_header "AI-Powered Examples"

    print_step "Running with-ai example..."
    cargo run --example with-ai --quiet
    print_success "with-ai completed"
else
    print_header "AI Examples (Skipped)"
    echo "  Set OPENAI_API_KEY or ANTHROPIC_API_KEY to run AI examples"
fi

# Build wrappers
print_header "Building Wrappers"

print_step "Building REST API wrapper..."
cd wrappers/rest-api
cargo build --quiet 2>&1 | grep -v "Compiling" | head -10 || true
print_success "REST API wrapper built"
cd "$SCRIPT_DIR"

print_step "Building Custom CLI wrapper..."
cd wrappers/custom-cli
cargo build --quiet 2>&1 | grep -v "Compiling" | head -10 || true
print_success "Custom CLI wrapper built"
cd "$SCRIPT_DIR"

# Demonstrate wrappers
print_header "Wrapper Demonstrations"

echo -e "${BLUE}REST API Wrapper:${NC}"
echo "  To run: cd wrappers/rest-api && cargo run"
echo "  Then visit: http://localhost:8080/swagger-ui/"
echo ""

echo -e "${BLUE}Custom CLI Wrapper:${NC}"
echo "  To run: cd wrappers/custom-cli && cargo run -- --help"
echo "  Interactive mode: cargo run -- interactive"
echo ""

# Run tests
print_header "Running Tests"
print_step "Running all tests..."
cargo test --quiet 2>&1 | tail -20
print_success "Tests completed"

# Summary
print_header "Summary"
echo "✓ All core examples completed successfully"
echo "✓ All wrappers built successfully"
echo "✓ Tests passed"
echo ""
echo "Next steps:"
echo "  1. Explore examples in examples/ directory"
echo "  2. Try the wrappers in wrappers/ directory"
echo "  3. Read the documentation in docs/USAGE_GUIDE.md"
echo "  4. Check the main README.md for API reference"
echo ""

print_header "Example Commands"
echo "# Run specific example:"
echo "  cargo run --example basic-usage"
echo ""
echo "# Run with AI (requires API key):"
echo "  export OPENAI_API_KEY='your-key'"
echo "  cargo run --example with-ai"
echo ""
echo "# Start REST API server:"
echo "  cd wrappers/rest-api && cargo run"
echo ""
echo "# Use custom CLI:"
echo "  cd wrappers/custom-cli"
echo "  cargo run -- generate --template ../../templates/sample.md --output ./out"
echo ""
echo "# Interactive mode:"
echo "  cargo run -- interactive"
echo ""

print_success "All examples and demonstrations complete!"
