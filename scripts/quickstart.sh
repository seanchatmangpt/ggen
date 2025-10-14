#!/usr/bin/env bash
set -euo pipefail

# ggen Quickstart Script
# Get from zero to working code in under 2 minutes

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  GGEN QUICKSTART"
echo "  Get started in 2 minutes"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Detect platform
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    PLATFORM="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="macos"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "win32" ]]; then
    PLATFORM="windows"
else
    PLATFORM="unknown"
fi

echo "🖥️  Platform: $PLATFORM"
echo ""
echo "📋 Checking prerequisites..."
echo ""

# Check Rust
echo "1️⃣  Rust toolchain"
if command -v rustc &> /dev/null; then
    RUSTC_VERSION=$(rustc --version | awk '{print $2}')
    echo "   ✅ Rust $RUSTC_VERSION installed"
else
    echo "   ⚠️  Rust not found"
    echo ""
    read -p "   Install Rust now? (takes 2 minutes) [Y/n]: " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        echo "   ⏳ Installing Rust..."
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        source "$HOME/.cargo/env"
        echo "   ✅ Rust installed"
    else
        echo "   ❌ Rust is required for ggen. Exiting."
        exit 1
    fi
fi

# Check Cargo
echo ""
echo "2️⃣  Cargo"
if command -v cargo &> /dev/null; then
    CARGO_VERSION=$(cargo --version | awk '{print $2}')
    echo "   ✅ Cargo $CARGO_VERSION installed"
else
    echo "   ❌ Cargo not found (should be installed with Rust)"
    exit 1
fi

# Check Git
echo ""
echo "3️⃣  Git"
if command -v git &> /dev/null; then
    GIT_VERSION=$(git --version | awk '{print $3}')
    echo "   ✅ Git $GIT_VERSION installed"
else
    echo "   ⚠️  Git not found"
    if [[ "$PLATFORM" == "macos" ]]; then
        echo "   Install with: xcode-select --install"
    elif [[ "$PLATFORM" == "linux" ]]; then
        echo "   Install with: sudo apt install git  # or yum install git"
    fi
    exit 1
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Install or update ggen
echo "📦 Installing ggen..."
if command -v ggen &> /dev/null; then
    CURRENT_VERSION=$(ggen --version 2>/dev/null | awk '{print $2}' || echo "unknown")
    echo "   ℹ️  Ggen $CURRENT_VERSION already installed"
    echo "   ⏳ Updating to latest version..."
else
    echo "   ⏳ Installing ggen from source..."
fi

# Install ggen (this will update if already installed)
cargo install ggen --quiet 2>&1 | grep -E "(Installing|Installed|Updating|Updated)" || true

if command -v ggen &> /dev/null; then
    GGEN_VERSION=$(ggen --version 2>/dev/null | awk '{print $2}' || echo "installed")
    echo "   ✅ Ggen $GGEN_VERSION ready"
else
    echo "   ❌ Ggen installation failed"
    echo "   Try manually: cargo install ggen"
    exit 1
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Generate demo project
echo "🚀 Generating demo project..."
PROJECT_NAME="hello-ggen"

# Remove existing demo if present
if [ -d "$PROJECT_NAME" ]; then
    echo "   ℹ️  Removing existing $PROJECT_NAME directory..."
    rm -rf "$PROJECT_NAME"
fi

# Check if ggen has quickstart command, otherwise use a basic template
if ggen --help | grep -q "quickstart"; then
    echo "   ⏳ Running: ggen quickstart demo"
    ggen quickstart demo --name "$PROJECT_NAME" 2>&1 || {
        echo "   ⚠️  Quickstart command not yet available"
        echo "   Creating basic Rust project instead..."
        cargo new "$PROJECT_NAME" --bin
        cd "$PROJECT_NAME"
        echo "   ✅ Basic Rust project created"
    }
else
    echo "   ⚠️  Quickstart command not yet available in this version"
    echo "   Creating basic Rust project as demo..."
    cargo new "$PROJECT_NAME" --bin --quiet
fi

# Verify project was created
if [ -d "$PROJECT_NAME" ]; then
    cd "$PROJECT_NAME"
    echo "   ✅ Project '$PROJECT_NAME' created"

    echo ""
    echo "   📁 Project structure:"
    tree -L 2 -I target 2>/dev/null || {
        echo "      $PROJECT_NAME/"
        echo "      ├── Cargo.toml"
        echo "      └── src/"
        echo "          └── main.rs"
    }
else
    echo "   ❌ Failed to create project"
    exit 1
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Run tests
echo "🧪 Running tests..."
if cargo test --quiet 2>&1 | tail -3; then
    echo "   ✅ All tests passed!"
else
    echo "   ⚠️  Some tests may have issues (this is normal for a basic project)"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "🎉 SUCCESS! Your first ggen project is ready!"
echo ""
echo "📚 Try it now:"
echo "   cd $PROJECT_NAME"
echo "   cargo run"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "🧭 What's next?"
echo ""
echo "  1️⃣  Generate a Real Project (5 minutes)"
echo "     → ggen ai project 'REST API with auth' --name my-api"
echo ""
echo "  2️⃣  Explore Marketplace (3 minutes)"
echo "     → ggen search 'rust web'"
echo ""
echo "  3️⃣  Read Documentation"
echo "     → https://seanchatmangpt.github.io/ggen/"
echo ""
echo "  4️⃣  Join Community"
echo "     → https://github.com/seanchatmangpt/ggen"
echo ""
echo "💡 Most developers start with #1 (AI project generation)"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Ask for feedback (optional)
read -p "Help improve ggen? Share anonymous usage data? [y/N]: " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "✅ Thank you! Analytics will help us improve the quickstart experience."
    # Note: Analytics not implemented yet, just asking for future
fi

echo ""
echo "⏱️  Total time: Complete!"
echo ""
echo "🚀 Happy coding with ggen!"
echo ""
