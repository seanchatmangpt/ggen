#!/bin/bash
set -e

echo "========================================"
echo "Ggen Marketplace Build Verification"
echo "========================================"
echo ""

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_step() {
    echo -e "${BLUE}▶ $1${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

# 1. Build with default features
print_step "Building with default features..."
cargo build --verbose
print_success "Default features build complete"
echo ""

# 2. Build with p2p feature
print_step "Building with p2p feature..."
cargo build --verbose --features p2p
print_success "P2P feature build complete"
echo ""

# 3. Build with graphql feature
print_step "Building with graphql feature..."
cargo build --verbose --features graphql
print_success "GraphQL feature build complete"
echo ""

# 4. Build with crypto feature
print_step "Building with crypto feature..."
cargo build --verbose --features crypto
print_success "Crypto feature build complete"
echo ""

# 5. Build with all features
print_step "Building with all features..."
cargo build --verbose --all-features
print_success "All features build complete"
echo ""

# 6. Run tests with default features
print_step "Running tests with default features..."
cargo test --verbose
print_success "Default features tests passed"
echo ""

# 7. Run tests with all features
print_step "Running tests with all features..."
cargo test --verbose --all-features
print_success "All features tests passed"
echo ""

# 8. Check formatting
print_step "Checking code formatting..."
cargo fmt -- --check
print_success "Code formatting check passed"
echo ""

# 9. Run clippy with all features
print_step "Running clippy with all features..."
cargo clippy --all-features -- -D warnings
print_success "Clippy checks passed"
echo ""

# 10. Check documentation builds
print_step "Checking documentation builds..."
cargo doc --all-features --no-deps
print_success "Documentation builds successfully"
echo ""

# 11. Summary
echo "========================================"
echo -e "${GREEN}✓ Build verification complete!${NC}"
echo "========================================"
echo ""
echo "Summary:"
echo "  - Default features: ✓"
echo "  - P2P feature: ✓"
echo "  - GraphQL feature: ✓"
echo "  - Crypto feature: ✓"
echo "  - All features: ✓"
echo "  - Tests: ✓"
echo "  - Formatting: ✓"
echo "  - Clippy: ✓"
echo "  - Documentation: ✓"
echo ""
echo "All checks passed! Ready for deployment."
