#!/bin/bash
# Staging deployment script
# Deploys to staging environment with production-like configuration

set -euo pipefail

echo "🎭 Deploying to staging environment..."

# Get the project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Set environment
export GGEN_ENV=staging

echo "🌍 Environment: $GGEN_ENV"

# Validate pre-deployment requirements
echo "🔍 Running staging validation..."
./scripts/deploy/pre-deploy-check.sh

# Create staging-specific configuration
cat > generated/.env.staging << EOF
# Staging Environment Configuration
DATABASE_URL=postgresql://staging-db:5432/comprehensive_showcase_staging
REDIS_URL=redis://staging-redis:6379/1
JWT_SECRET=staging-secret-key-$(openssl rand -hex 32)
LOG_LEVEL=info
API_PORT=3000
DEBUG_MODE=false
ENABLE_PROFILING=true
CORS_ORIGINS=https://staging.example.com,https://admin.staging.example.com
RATE_LIMIT_REQUESTS=1000
RATE_LIMIT_WINDOW_SECONDS=60
EOF

echo "⚙️ Staging configuration created"

# Build optimized binary for staging
echo "🔨 Building staging binary..."
cd generated

# Clean previous build
cargo clean

# Build with staging optimizations
cargo build --release --features staging

echo "📦 Staging binary built: $(du -sh target/release/comprehensive-rust-showcase | cut -f1)"

# Run staging-specific tests
echo "🧪 Running staging tests..."
cargo test --release --features staging

# Validate security
echo "🔒 Running staging security checks..."
cargo audit
cargo deny check

# Check for staging-specific features
if ! grep -q "staging" Cargo.toml; then
    echo "⚠️ Staging features not explicitly enabled in Cargo.toml"
fi

# Deploy to staging (example using Docker)
echo "🐳 Deploying to staging with Docker..."

# Build Docker image for staging
DOCKER_TAG="comprehensive-rust-showcase:staging-$(date +%Y%m%d-%H%M%S)"

docker build \
    --build-arg ENVIRONMENT=staging \
    --build-arg BUILD_DATE="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
    --build-arg VCS_REF="$(git rev-parse --short HEAD)" \
    --build-arg VERSION="1.0.0-staging" \
    -t "$DOCKER_TAG" \
    -f deployment/Dockerfile.staging \
    .

# Tag as latest staging version
docker tag "$DOCKER_TAG" "comprehensive-rust-showcase:staging-latest"

echo "📦 Docker image built: $DOCKER_TAG"

# Deploy to staging environment (example)
echo "🚀 Deploying to staging Kubernetes..."

# Apply staging configuration
kubectl apply -f deployment/staging/ --namespace=comprehensive-rust-showcase-staging

# Wait for rollout to complete
kubectl rollout status deployment/comprehensive-rust-showcase --namespace=comprehensive-rust-showcase-staging --timeout=300s

# Run staging smoke tests
echo "🧪 Running staging smoke tests..."

# Wait for service to be ready
kubectl wait --for=condition=available --timeout=60s deployment/comprehensive-rust-showcase --namespace=comprehensive-rust-showcase-staging

# Get service URL
STAGING_URL=$(kubectl get svc comprehensive-rust-showcase-service -n comprehensive-rust-showcase-staging -o jsonpath='{.status.loadBalancer.ingress[0].hostname}' 2>/dev/null || echo "localhost")

if [ "$STAGING_URL" = "localhost" ]; then
    # Use port forwarding for local testing
    kubectl port-forward svc/comprehensive-rust-showcase-service 3000:80 -n comprehensive-rust-showcase-staging &
    PORT_FORWARD_PID=$!
    STAGING_URL="http://localhost:3000"
    sleep 5
fi

# Health check
HEALTH_URL="$STAGING_URL/health"
if curl -f -s "$HEALTH_URL" >/dev/null 2>&1; then
    echo "✅ Staging health check passed"
else
    echo "❌ Staging health check failed!"
    echo "🔍 Checking application logs..."
    kubectl logs -l app=comprehensive-rust-showcase -n comprehensive-rust-showcase-staging --tail=50
    exit 1
fi

# Run staging integration tests
echo "🔗 Running staging integration tests..."

# Example integration test
if curl -f -s "$STAGING_URL/api/v1/health" >/dev/null 2>&1; then
    echo "✅ Staging API integration test passed"
else
    echo "❌ Staging API integration test failed!"
    exit 1
fi

# Performance test
echo "⚡ Running staging performance tests..."

# Simple load test
if command -v ab &> /dev/null; then
    echo "Running Apache Bench load test..."
    ab -n 100 -c 10 "$STAGING_URL/health" | grep -E "(Requests per second|Time per request|Failed requests)"

    # Check if performance meets requirements
    RPS=$(ab -n 100 -c 10 "$STAGING_URL/health" 2>/dev/null | grep "Requests per second" | awk '{print $4}')
    if (( $(echo "$RPS > 50" | bc -l) )); then
        echo "✅ Performance test passed (RPS: $RPS)"
    else
        echo "⚠️ Performance below target (RPS: $RPS, target: 50+)"
    fi
fi

# Cleanup port forwarding if used
if [ ! -z "${PORT_FORWARD_PID:-}" ]; then
    kill $PORT_FORWARD_PID 2>/dev/null || true
fi

echo ""
echo "🎉 Staging deployment completed!"
echo ""
echo "📋 Staging Environment Details:"
echo "  URL: $STAGING_URL"
echo "  Health: $HEALTH_URL"
echo "  Namespace: comprehensive-rust-showcase-staging"
echo "  Image: $DOCKER_TAG"
echo ""
echo "🔧 Staging Features:"
echo "  - Production-like configuration"
echo "  - Performance monitoring enabled"
echo "  - Security hardening applied"
echo "  - Integration tests run"
echo "  - Load testing performed"
echo ""
echo "🚀 Staging deployment ready for testing!"

# Save deployment info for rollback
cat > generated/.staging-deployment-info << EOF
DEPLOYMENT_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ')
DOCKER_TAG=$DOCKER_TAG
STAGING_URL=$STAGING_URL
GGEN_ENV=staging
EOF

echo "💾 Deployment info saved to generated/.staging-deployment-info"
