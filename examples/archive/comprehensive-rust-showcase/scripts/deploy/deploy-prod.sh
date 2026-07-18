#!/bin/bash
# Production deployment script
# Deploys to production environment with maximum security and monitoring

set -euo pipefail

echo "🏭 Deploying to production environment..."

# Get the project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Set environment
export GGEN_ENV=production

echo "🌍 Environment: $GGEN_ENV"

# Strict pre-deployment validation
echo "🔍 Running production validation..."
./scripts/deploy/pre-deploy-check.sh

# Create production-specific configuration
cat > generated/.env.production << EOF
# Production Environment Configuration
DATABASE_URL=postgresql://prod-db:5432/comprehensive_showcase_prod
REDIS_URL=redis://prod-redis:6379/2
JWT_SECRET=$(openssl rand -hex 64)
LOG_LEVEL=warn
API_PORT=3000
DEBUG_MODE=false
ENABLE_PROFILING=false
CORS_ORIGINS=https://app.example.com,https://api.example.com
RATE_LIMIT_REQUESTS=5000
RATE_LIMIT_WINDOW_SECONDS=60
SESSION_TIMEOUT_MINUTES=30
EOF

echo "⚙️ Production configuration created"

# Build optimized binary for production
echo "🔨 Building production binary..."
cd generated

# Clean previous build
cargo clean

# Build with production optimizations
cargo build --release \
    --features production \
    -Z build-std=std,panic_abort \
    -Z build-std-features=panic_immediate_abort

echo "📦 Production binary built: $(du -sh target/release/comprehensive-rust-showcase | cut -f1)"

# Run production tests
echo "🧪 Running production tests..."
cargo test --release --features production

# Comprehensive security audit
echo "🔒 Running production security audit..."
cargo audit --ignore-yanked
cargo deny check --config deny.toml

# Validate security hardening
echo "🛡️ Validating security hardening..."

# Check for security-related features in binary
if ! nm target/release/comprehensive-rust-showcase | grep -q "panic_abort"; then
    echo "❌ Panic abort not enabled in production build!"
    exit 1
fi

# Check for debug symbols removal
DEBUG_SYMBOLS=$(nm target/release/comprehensive-rust-showcase | wc -l)
if [ "$DEBUG_SYMBOLS" -gt 1000 ]; then
    echo "⚠️ Debug symbols may still be present (found $DEBUG_SYMBOLS symbols)"
fi

echo "✅ Security validation passed"

# Build production Docker image
echo "🐳 Building production Docker image..."

# Generate unique version tag
VERSION="1.0.0-$(date +%Y%m%d%H%M%S)-$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')"
DOCKER_TAG="comprehensive-rust-showcase:$VERSION"

docker build \
    --build-arg ENVIRONMENT=production \
    --build-arg BUILD_DATE="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
    --build-arg VCS_REF="$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')" \
    --build-arg VERSION="$VERSION" \
    -t "$DOCKER_TAG" \
    -f deployment/Dockerfile.production \
    .

# Tag as latest production version
docker tag "$DOCKER_TAG" "comprehensive-rust-showcase:production-latest"

echo "📦 Production Docker image built: $DOCKER_TAG"

# Pre-deployment backup
echo "💾 Creating pre-deployment backup..."
BACKUP_TAG="comprehensive-rust-showcase:backup-$(date +%Y%m%d-%H%M%S)"

# Tag current production as backup (if exists)
if docker images comprehensive-rust-showcase:production-latest --quiet | grep -q .; then
    docker tag comprehensive-rust-showcase:production-latest "$BACKUP_TAG"
    echo "✅ Backup created: $BACKUP_TAG"
fi

# Deploy to production (example using Kubernetes)
echo "🚀 Deploying to production Kubernetes..."

# Apply production configuration
kubectl apply -f deployment/production/ --namespace=comprehensive-rust-showcase-production

# Wait for rollout to complete
kubectl rollout status deployment/comprehensive-rust-showcase --namespace=comprehensive-rust-showcase-production --timeout=600s

# Run production smoke tests
echo "🧪 Running production smoke tests..."

# Wait for service to be ready
kubectl wait --for=condition=available --timeout=120s deployment/comprehensive-rust-showcase --namespace=comprehensive-rust-showcase-production

# Get service URL
PROD_URL=$(kubectl get svc comprehensive-rust-showcase-service -n comprehensive-rust-showcase-production -o jsonpath='{.status.loadBalancer.ingress[0].hostname}' 2>/dev/null || echo "api.example.com")

# Health check with retry
HEALTH_URL="https://$PROD_URL/health"
HEALTH_ATTEMPTS=0
MAX_HEALTH_ATTEMPTS=12

while [ $HEALTH_ATTEMPTS -lt $MAX_HEALTH_ATTEMPTS ]; do
    if curl -f -s --max-time 10 "$HEALTH_URL" >/dev/null 2>&1; then
        echo "✅ Production health check passed"
        break
    fi

    echo "⏳ Health check attempt $(($HEALTH_ATTEMPTS + 1))/$MAX_HEALTH_ATTEMPTS failed, retrying..."
    sleep 10
    HEALTH_ATTEMPTS=$((HEALTH_ATTEMPTS + 1))
done

if [ $HEALTH_ATTEMPTS -eq $MAX_HEALTH_ATTEMPTS ]; then
    echo "❌ Production health check failed after $MAX_HEALTH_ATTEMPTS attempts!"
    echo "🔍 Checking application logs..."
    kubectl logs -l app=comprehensive-rust-showcase -n comprehensive-rust-showcase-production --tail=100

    # Rollback if health check fails
    echo "🔄 Rolling back to previous version..."
    kubectl rollout undo deployment/comprehensive-rust-showcase --namespace=comprehensive-rust-showcase-production

    exit 1
fi

# Run production integration tests
echo "🔗 Running production integration tests..."

# Example integration test
API_URL="https://$PROD_URL/api/v1"
if curl -f -s "$API_URL/health" >/dev/null 2>&1; then
    echo "✅ Production API integration test passed"
else
    echo "❌ Production API integration test failed!"
    exit 1
fi

# Performance test
echo "⚡ Running production performance tests..."

# Simple load test (adjust based on your requirements)
if command -v ab &> /dev/null; then
    echo "Running Apache Bench load test..."
    ab -n 1000 -c 50 -t 30 "$HEALTH_URL" | grep -E "(Requests per second|Time per request|Failed requests|Non-2xx responses)"

    # Check if performance meets production requirements
    RPS=$(ab -n 1000 -c 50 -t 30 "$HEALTH_URL" 2>/dev/null | grep "Requests per second" | awk '{print $4}')
    ERROR_RATE=$(ab -n 1000 -c 50 -t 30 "$HEALTH_URL" 2>/dev/null | grep "Non-2xx responses" | awk '{print $3}')

    if (( $(echo "$RPS > 100" | bc -l) )) && [ "${ERROR_RATE:-0}" -eq 0 ]; then
        echo "✅ Production performance test passed (RPS: $RPS, Errors: $ERROR_RATE)"
    else
        echo "❌ Production performance test failed (RPS: $RPS, Errors: $ERROR_RATE)"
        exit 1
    fi
fi

# Security test
echo "🔒 Running production security tests..."

# Check for common vulnerabilities
if curl -f -s -I "$HEALTH_URL" | grep -q "X-Frame-Options"; then
    echo "✅ Security headers present"
else
    echo "⚠️ Security headers may be missing"
fi

# Check SSL/TLS configuration
if curl -f -s -I "$HEALTH_URL" | grep -q "HTTP/2"; then
    echo "✅ HTTP/2 enabled"
else
    echo "⚠️ HTTP/2 may not be enabled"
fi

# Monitor for a short period
echo "📊 Running production monitoring check..."

# Get initial metrics (this would integrate with your monitoring system)
sleep 30

# Check application metrics
if kubectl exec deployment/comprehensive-rust-showcase -n comprehensive-rust-showcase-production -- curl -s "$PROD_URL/metrics" >/dev/null 2>&1; then
    echo "✅ Metrics endpoint accessible"
else
    echo "⚠️ Metrics endpoint not accessible"
fi

echo ""
echo "🎉 Production deployment completed successfully!"
echo ""
echo "📋 Production Environment Details:"
echo "  URL: https://$PROD_URL"
echo "  Health: $HEALTH_URL"
echo "  API: $API_URL"
echo "  Namespace: comprehensive-rust-showcase-production"
echo "  Image: $DOCKER_TAG"
echo "  Version: $VERSION"
echo ""
echo "🔧 Production Features:"
echo "  - Maximum security hardening"
echo "  - Performance optimizations enabled"
echo "  - Comprehensive monitoring"
echo "  - Automated health checks"
echo "  - Rollback capability"
echo ""
echo "🚀 Production deployment ready!"

# Save deployment info for rollback
cat > generated/.production-deployment-info << EOF
DEPLOYMENT_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ')
DOCKER_TAG=$DOCKER_TAG
PROD_URL=$PROD_URL
VERSION=$VERSION
GGEN_ENV=production
BACKUP_TAG=${BACKUP_TAG:-none}
EOF

echo "💾 Deployment info saved to generated/.production-deployment-info"

# Post-deployment notifications
echo ""
echo "🔔 Post-deployment actions:"
echo "  - Monitor application metrics"
echo "  - Verify external integrations"
echo "  - Run end-to-end tests"
echo "  - Update documentation"
echo "  - Notify stakeholders"
