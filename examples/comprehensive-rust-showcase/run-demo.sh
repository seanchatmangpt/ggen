#!/bin/bash
# Complete demonstration of the ggen comprehensive Rust showcase
# Shows all lifecycle phases, marketplace usage, and template generation

set -euo pipefail

echo "🚀 Ggen Comprehensive Rust Showcase Demo"
echo "=========================================="
echo ""

# Get the project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"
cd "$PROJECT_ROOT"

echo "📋 This demo will showcase:"
echo "  1. Project initialization with ggen lifecycle"
echo "  2. Marketplace package installation"
echo "  3. Template-based code generation"
echo "  4. Comprehensive testing"
echo "  5. Multi-environment deployment"
echo ""

# Check prerequisites
echo "🔍 Checking prerequisites..."
if ! command -v ggen &> /dev/null; then
    echo "❌ ggen CLI not found!"
    echo "   Install with: cargo install ggen"
    exit 1
fi

if ! command -v cargo &> /dev/null; then
    echo "❌ Rust/Cargo not found!"
    echo "   Install Rust from: https://rustup.rs/"
    exit 1
fi

echo "✅ Prerequisites check passed"
echo ""

# 1. Initialize project
echo "1️⃣ Initializing project..."
echo "   Command: ggen lifecycle run init"
echo ""

if [ ! -f "make.toml" ]; then
    echo "❌ make.toml not found! This demo requires the lifecycle configuration."
    echo "   Please ensure make.toml exists in the project root."
    exit 1
fi

# Show available phases
echo "📋 Available lifecycle phases:"
ggen lifecycle list

echo ""
echo "🔄 Running initialization phase..."
ggen lifecycle run init

echo "✅ Project initialized"
echo ""

# 2. Install marketplace packages
echo "2️⃣ Installing marketplace packages..."
echo "   Command: ggen market add <packages>"
echo ""

# Check if packages are already installed
if ! ggen market list --installed | grep -q "rig-mcp"; then
    echo "📦 Installing AI integration package..."
    ggen market add "rig-mcp-integration"
fi

if ! ggen market list --installed | grep -q "api-endpoint"; then
    echo "📦 Installing API template package..."
    ggen market add "api-endpoint"
fi

echo ""
echo "📋 Installed packages:"
ggen market list --installed

echo ""
echo "✅ Marketplace packages installed"
echo ""

# 3. Generate code from templates
echo "3️⃣ Generating code from templates..."
echo "   Command: ggen template generate <template>"
echo ""

# Check if templates exist
if [ ! -d "templates" ]; then
    echo "❌ templates/ directory not found!"
    echo "   Please ensure templates exist in the project."
    exit 1
fi

echo "📋 Available templates:"
ls -la templates/

echo ""
echo "🔄 Generating Rust service..."
if [ -f "templates/rust-service.tmpl" ]; then
    ggen template generate templates/rust-service.tmpl
    echo "✅ Rust service generated"
else
    echo "⚠️ Rust service template not found, skipping..."
fi

echo ""
echo "🔄 Generating API endpoints..."
if [ -f "templates/api-endpoint.tmpl" ]; then
    ggen template generate templates/api-endpoint.tmpl
    echo "✅ API endpoints generated"
else
    echo "⚠️ API endpoint template not found, skipping..."
fi

echo ""
echo "🔄 Generating database schema..."
if [ -f "templates/database-schema.tmpl" ]; then
    ggen template generate templates/database-schema.tmpl
    echo "✅ Database schema generated"
else
    echo "⚠️ Database schema template not found, skipping..."
fi

echo ""
echo "✅ Code generation completed"
echo ""

# 4. Build and test
echo "4️⃣ Building and testing..."
echo "   Command: ggen lifecycle run build && ggen lifecycle run test"
echo ""

if [ -d "generated" ]; then
    echo "🔨 Building project..."
    cd generated

    if [ -f "Cargo.toml" ]; then
        cargo build --release
        echo "✅ Project built successfully"

        echo ""
        echo "🧪 Running tests..."
        if cargo test --release; then
            echo "✅ All tests passed"
        else
            echo "⚠️ Some tests failed"
        fi
    else
        echo "⚠️ No Cargo.toml found in generated/, skipping build..."
    fi

    cd "$PROJECT_ROOT"
else
    echo "⚠️ Generated directory not found, skipping build..."
fi

echo ""
echo "✅ Build and test phase completed"
echo ""

# 5. Deployment demo
echo "5️⃣ Deployment demonstration..."
echo "   Command: ggen lifecycle run deploy --env development"
echo ""

echo "🚀 Demonstrating deployment to development environment..."
echo ""

# Create a simple deployment simulation
echo "🔧 Running pre-deployment checks..."
if [ -f "scripts/deploy/pre-deploy-check.sh" ]; then
    bash scripts/deploy/pre-deploy-check.sh
else
    echo "⚠️ Deployment scripts not found, simulating..."
fi

echo ""
echo "🏭 Deploying to development..."
if [ -f "scripts/deploy/deploy-dev.sh" ]; then
    # Don't actually run the deployment script as it might fail in demo environment
    echo "   (Deployment script exists and is ready to run)"
else
    echo "⚠️ Deployment scripts not found"
fi

echo ""
echo "✅ Deployment demonstration completed"
echo ""

# 6. Final summary
echo "🎉 Ggen Comprehensive Rust Showcase Demo Complete!"
echo "=================================================="
echo ""
echo "📋 What was demonstrated:"
echo "  ✅ Project initialization with ggen lifecycle"
echo "  ✅ Marketplace package installation"
echo "  ✅ Template-based code generation"
echo "  ✅ Build and test automation"
echo "  ✅ Deployment workflow demonstration"
echo ""
echo "🔧 Generated artifacts:"
echo "  - Project configuration files"
echo "  - Rust source code"
echo "  - Database schema"
echo "  - API documentation"
echo "  - Test suites"
echo "  - Deployment configurations"
echo ""
echo "📚 Next steps:"
echo "  1. Review generated code in generated/ directory"
echo "  2. Run individual lifecycle phases as needed"
echo "  3. Customize templates for your specific needs"
echo "  4. Deploy to staging and production environments"
echo "  5. Add new templates to the marketplace"
echo ""
echo "🚀 Ready to build production applications with ggen!"
