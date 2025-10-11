#!/usr/bin/env bash
# Development environment setup script

set -e

echo "🚀 Setting up ggen development environment..."

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Install Rust tools
echo ""
echo "📦 Installing Rust development tools..."

# Essential tools
TOOLS=(
    "cargo-nextest"
    "cargo-watch"
    "cargo-expand"
    "cargo-audit"
)

for tool in "${TOOLS[@]}"; do
    if command_exists "$tool"; then
        echo -e "${GREEN}✓${NC} $tool already installed"
    else
        echo -e "${YELLOW}⚙${NC}  Installing $tool..."
        cargo install "$tool" --locked || echo -e "${RED}✗${NC} Failed to install $tool"
    fi
done

# Optional tools
echo ""
echo "📦 Installing optional tools..."

OPTIONAL_TOOLS=(
    "sccache"
    "cargo-tarpaulin"
    "cargo-outdated"
    "cargo-udeps"
)

for tool in "${OPTIONAL_TOOLS[@]}"; do
    if command_exists "$tool"; then
        echo -e "${GREEN}✓${NC} $tool already installed"
    else
        echo -e "${YELLOW}⚙${NC}  Installing $tool (optional)..."
        cargo install "$tool" --locked 2>/dev/null || echo -e "${YELLOW}⚠${NC}  Skipped $tool (optional)"
    fi
done

# Setup pre-commit hooks
echo ""
echo "🪝 Setting up pre-commit hooks..."

if command_exists pre-commit; then
    pre-commit install
    echo -e "${GREEN}✓${NC} Pre-commit hooks installed"
else
    echo -e "${YELLOW}⚠${NC}  pre-commit not found. Install with: pip install pre-commit"
fi

# Setup sccache if installed
if command_exists sccache; then
    echo ""
    echo "⚡ Configuring sccache..."
    export RUSTC_WRAPPER=sccache
    echo "export RUSTC_WRAPPER=sccache" >> ~/.bashrc 2>/dev/null || true
    echo "export RUSTC_WRAPPER=sccache" >> ~/.zshrc 2>/dev/null || true
    echo -e "${GREEN}✓${NC} sccache configured"
fi

# Generate Cargo.lock if missing
if [ ! -f "Cargo.lock" ]; then
    echo ""
    echo "🔒 Generating Cargo.lock..."
    cargo generate-lockfile
fi

# Run initial build
echo ""
echo "🔨 Running initial build..."
cargo check --workspace

# Run tests
echo ""
echo "🧪 Running tests..."
if command_exists cargo-nextest; then
    cargo nextest run --workspace
else
    cargo test --workspace
fi

# Show summary
echo ""
echo -e "${GREEN}✅ Development environment setup complete!${NC}"
echo ""
echo "Available commands:"
echo "  cargo watch -c -x 'check --workspace'  # Watch and check on changes"
echo "  cargo nextest run --workspace           # Run tests with nextest"
echo "  cargo clippy --workspace -- -D warnings # Lint code"
echo "  cargo audit                              # Security audit"
echo "  pre-commit run --all-files              # Run pre-commit hooks"
echo ""
echo "See docs/CORE_TEAM_RECOMMENDATIONS.md for more information."
