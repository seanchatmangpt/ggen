#!/bin/bash
set -e

# Reasoner CLI Deployment Script
# Deploys reasoner-cli to production

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "🚀 Deploying reasoner-cli..."

# Build release binary
echo "📦 Building release binary..."
cargo build --release --all-features

# Run tests
echo "🧪 Running tests..."
cargo test --release

# Run benchmarks
echo "📊 Running benchmarks..."
cargo bench --no-run

# Install binary
echo "💾 Installing binary..."
cargo install --path . --force

# Verify installation
echo "✅ Verifying installation..."
if command -v reasoner &> /dev/null; then
    echo "✓ reasoner-cli installed successfully"
    reasoner --version
else
    echo "✗ Installation failed"
    exit 1
fi

# Test basic commands
echo "🔍 Testing basic commands..."
reasoner --help > /dev/null
echo "✓ Help command works"

echo "✅ Deployment complete!"
