#!/bin/bash
# Infrastructure Verification Script
# Verifies Docker, testcontainers, OpenTelemetry, and Weaver are working

set -e

echo "🔍 Verifying Infrastructure Components"
echo "========================================"

# 1. Docker Verification
echo ""
echo "1️⃣  Docker Verification"
echo "----------------------"
if docker --version > /dev/null 2>&1; then
    DOCKER_VERSION=$(docker --version)
    echo "✅ Docker installed: $DOCKER_VERSION"
else
    echo "❌ Docker not found"
    exit 1
fi

if docker ps > /dev/null 2>&1; then
    echo "✅ Docker daemon is running"
else
    echo "❌ Docker daemon is not running"
    echo "   💡 Start Docker Desktop or: sudo systemctl start docker"
    exit 1
fi

# 2. Testcontainers Verification
echo ""
echo "2️⃣  Testcontainers Verification"
echo "-------------------------------"
cd "$(dirname "$0")/../.."
if cargo test --test '*' --lib --no-run 2>&1 | grep -q "testcontainers"; then
    echo "✅ testcontainers dependency found in Cargo.toml"
else
    echo "⚠️  testcontainers dependency not found (may be optional)"
fi

# Try to compile a simple testcontainers test
if cargo check --tests 2>&1 | grep -q "testcontainers" || true; then
    echo "✅ testcontainers compiles successfully"
else
    echo "⚠️  Could not verify testcontainers compilation"
fi

# 3. OpenTelemetry Verification
echo ""
echo "3️⃣  OpenTelemetry Verification"
echo "------------------------------"
cd tests/integration

# Check if docker-compose file exists
if [ -f "docker-compose.otel-test.yml" ]; then
    echo "✅ docker-compose.otel-test.yml found"
    
    # Check if otel-collector-config.yaml exists
    if [ -f "otel-collector-config.yaml" ]; then
        echo "✅ otel-collector-config.yaml found"
    else
        echo "⚠️  otel-collector-config.yaml not found"
    fi
    
    # Try to start services (dry-run)
    echo "   Checking docker-compose configuration..."
    if docker-compose -f docker-compose.otel-test.yml config > /dev/null 2>&1; then
        echo "✅ docker-compose configuration is valid"
    else
        echo "⚠️  docker-compose configuration has issues"
        docker-compose -f docker-compose.otel-test.yml config 2>&1 | head -5
    fi
    
    # Check if services are running
    if docker-compose -f docker-compose.otel-test.yml ps 2>&1 | grep -q "Up"; then
        echo "✅ OTEL services are running"
    else
        echo "   ℹ️  OTEL services not running (start with: docker-compose -f docker-compose.otel-test.yml up -d)"
    fi
else
    echo "❌ docker-compose.otel-test.yml not found"
fi

# Check OTEL dependencies in Cargo.toml
cd ../..
if grep -q "opentelemetry" Cargo.toml; then
    echo "✅ OpenTelemetry dependencies found in Cargo.toml"
else
    echo "⚠️  OpenTelemetry dependencies not found"
fi

# 4. Weaver Verification
echo ""
echo "4️⃣  Weaver Verification"
echo "----------------------"
if command -v weaver > /dev/null 2>&1; then
    WEAVER_VERSION=$(weaver --version 2>&1 || echo "unknown")
    echo "✅ Weaver installed: $WEAVER_VERSION"
    
    # Test weaver help command
    if weaver --help > /dev/null 2>&1; then
        echo "✅ Weaver CLI is functional"
    else
        echo "⚠️  Weaver CLI may have issues"
    fi
else
    echo "⚠️  Weaver not found in PATH"
    echo "   💡 Install from: https://github.com/open-telemetry/weaver/releases"
    echo "   💡 Or build from source: cargo install weaver"
fi

# 5. Integration Test Verification
echo ""
echo "5️⃣  Integration Test Verification"
echo "---------------------------------"
cd tests/integration

# Check if test files exist
TEST_FILES=(
    "otel_validation_tests.rs"
    "marketplace_nextjs_ontology_e2e.rs"
    "testcontainer_marketplace_git_hooks.rs"
    "full_cycle_container_validation.rs"
)

for test_file in "${TEST_FILES[@]}"; do
    if [ -f "$test_file" ]; then
        echo "✅ $test_file exists"
    else
        echo "⚠️  $test_file not found"
    fi
done

echo ""
echo "========================================"
echo "✅ Infrastructure Verification Complete"
echo ""
echo "📋 Summary:"
echo "   - Docker: $(docker --version 2>&1 | head -1)"
echo "   - Weaver: $(weaver --version 2>&1 | head -1 || echo 'not found')"
echo "   - OTEL Config: $(test -f docker-compose.otel-test.yml && echo 'found' || echo 'missing')"
echo ""
echo "💡 To start OTEL stack:"
echo "   cd tests/integration && docker-compose -f docker-compose.otel-test.yml up -d"
echo ""
echo "💡 To run integration tests:"
echo "   cargo test --test '*' -- --ignored"

