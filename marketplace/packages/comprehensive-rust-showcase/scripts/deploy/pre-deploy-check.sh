#!/bin/bash
# Pre-deployment validation script
# Ensures the application is ready for deployment

set -euo pipefail

echo "🔍 Running pre-deployment checks..."

# Get the project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Check if generated code exists
if [ ! -d "generated" ]; then
    echo "❌ Generated code directory not found!"
    echo "   Run 'ggen lifecycle run generate' first"
    exit 1
fi

# Check if binary exists
if [ ! -f "generated/target/release/comprehensive-rust-showcase" ]; then
    echo "❌ Release binary not found!"
    echo "   Run 'ggen lifecycle run build' first"
    exit 1
fi

# Check if tests pass
echo "🧪 Running test validation..."
cd generated
if ! cargo test --release --quiet; then
    echo "❌ Tests failed!"
    echo "   Fix test failures before deploying"
    exit 1
fi

# Check for security vulnerabilities
echo "🔒 Running security audit..."
if ! cargo audit --quiet; then
    echo "⚠️ Security vulnerabilities found!"
    echo "   Review vulnerabilities before deploying"
fi

# Check code quality
echo "📏 Running code quality checks..."
if ! cargo clippy --all-targets -- -D warnings --quiet; then
    echo "❌ Code quality issues found!"
    echo "   Fix clippy warnings before deploying"
    exit 1
fi

# Check formatting
echo "🎨 Checking code formatting..."
if ! cargo fmt --check --quiet; then
    echo "❌ Code formatting issues found!"
    echo "   Format code before deploying"
    exit 1
fi

# Check for required configuration files
echo "📋 Checking configuration..."
if [ ! -f "ggen.toml" ]; then
    echo "❌ ggen.toml configuration file missing!"
    exit 1
fi

if [ ! -f "make.toml" ]; then
    echo "❌ make.toml lifecycle file missing!"
    exit 1
fi

# Check environment-specific requirements
if [ -n "${GGEN_ENV:-}" ]; then
    echo "🌍 Checking environment: $GGEN_ENV"

    case "$GGEN_ENV" in
        "production")
            echo "🏭 Production environment checks..."

            # Check for production-ready features
            if ! grep -q "production" generated/Cargo.toml; then
                echo "❌ Production optimizations not enabled!"
                exit 1
            fi

            # Check for security hardening
            if ! grep -q "deny" generated/Cargo.toml; then
                echo "❌ Security hardening not enabled!"
                exit 1
            fi
            ;;

        "staging")
            echo "🎭 Staging environment checks..."

            # Check for staging-specific features
            if ! grep -q "staging" generated/Cargo.toml; then
                echo "❌ Staging optimizations not enabled!"
                exit 1
            fi
            ;;

        "development")
            echo "🔧 Development environment checks..."
            # More lenient for development
            ;;
    esac
fi

# Check system resources
echo "💾 Checking system resources..."
FREE_MEMORY=$(free -m | awk 'NR==2{printf "%.0f", $7/1024}')
FREE_DISK=$(df . | awk 'NR==2{printf "%.0f", $4/1024}')

echo "  Available memory: ${FREE_MEMORY}GB"
echo "  Available disk: ${FREE_DISK}GB"

if [ "$FREE_MEMORY" -lt 2 ]; then
    echo "❌ Insufficient memory for deployment!"
    exit 1
fi

if [ "$FREE_DISK" -lt 5 ]; then
    echo "❌ Insufficient disk space for deployment!"
    exit 1
fi

# Check network connectivity (if deploying to external services)
echo "🌐 Checking network connectivity..."
if ping -c 1 -W 5 8.8.8.8 >/dev/null 2>&1; then
    echo "  ✅ Internet connectivity available"
else
    echo "⚠️ No internet connectivity detected"
fi

# Validate deployment configuration
echo "⚙️ Validating deployment configuration..."

# Check if deployment files exist
if [ ! -f "generated/deployment/deployment.yaml" ]; then
    echo "❌ Deployment configuration not found!"
    echo "   Run 'ggen template generate templates/deployment.tmpl' first"
    exit 1
fi

# Validate Kubernetes manifests (if using kubectl)
if command -v kubectl &> /dev/null; then
    echo "🔧 Validating Kubernetes manifests..."
    if ! kubectl apply --dry-run=client -f generated/deployment/deployment.yaml >/dev/null 2>&1; then
        echo "❌ Kubernetes manifest validation failed!"
        exit 1
    fi
    echo "  ✅ Kubernetes manifests are valid"
fi

# Final pre-deployment summary
echo ""
echo "✅ Pre-deployment checks completed successfully!"
echo ""
echo "📋 Deployment Summary:"
echo "  Environment: ${GGEN_ENV:-development}"
echo "  Binary size: $(du -sh generated/target/release/comprehensive-rust-showcase | cut -f1)"
echo "  Build time: $(date)"
echo "  Configuration: Ready"
echo "  Tests: ✅ Passed"
echo "  Security: ✅ Checked"
echo "  Quality: ✅ Validated"
echo ""
echo "🚀 Ready for deployment!"
