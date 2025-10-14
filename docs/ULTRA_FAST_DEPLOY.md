# Ultra-Fast Deployment Workflow (<60 Seconds)

## 🚀 Quick Navigation

- [Overview](#overview) - Concept to deployment in <60s
- [Quick Start](#quick-start) - Get started in 5 minutes
- [Examples](#examples) - 3 complete examples
- [Workflow Guide](#workflow-guide) - Step-by-step process
- [Best Practices](#best-practices) - Optimization tips
- [Troubleshooting](#troubleshooting) - Common issues and fixes
- [Performance](#performance) - Benchmarks and timings

---

## Overview

**The ultra-fast deployment workflow combines ggen's generation capabilities with cleanroom's hermetic testing to achieve <60 second concept-to-deployment cycles.**

### The Magic Formula

```
ggen (generate) + cleanroom (test) + lifecycle (deploy) = <60s production deployment
```

### Key Benefits

- ⚡ **<60s Total Time** - From idea to deployed production service
- 🧪 **100% Tested** - Hermetic cleanroom tests ensure quality
- 🔒 **Production Safe** - Zero `.expect()` or `.unwrap()` calls
- 🎯 **Deterministic** - Reproducible builds and deployments
- 📦 **Complete** - Templates, tests, Docker, CI/CD all generated
- 🚀 **Scalable** - Works for microservices, APIs, CLIs, and more

### Performance Comparison

| Workflow | Time | Quality | Reproducibility |
|----------|------|---------|-----------------|
| **Ultra-Fast** | <60s | ✅ Tested | ✅ Deterministic |
| Traditional | 30-60 min | ⚠️ Manual | ❌ Variable |
| Manual Setup | 2-4 hours | ❌ Untested | ❌ Custom |

---

## Quick Start (5 Minutes)

### Prerequisites

```bash
# Install ggen
cargo install ggen

# Install cleanroom
cd ggen/cleanroom
cargo build --release
export PATH="$PATH:$(pwd)/target/release"

# Verify installations
ggen --version
cleanroom --version
```

### Your First Ultra-Fast Deployment

**Goal:** Deploy a Rust web service in <60 seconds.

```bash
# 1. Search marketplace (5s)
ggen market search "rust web service"

# 2. Install template (3s)
ggen market add "rust-axum-service"

# 3. Initialize project (2s)
ggen lifecycle run init

# 4. Generate service (8s)
ggen template generate rust-axum-service:user-service.tmpl \
  --vars name=user-api \
  --vars port=3000

# 5. Run cleanroom tests (15s)
cleanroom environment create --name test-env
cleanroom container start postgres --db testdb
cleanroom test run --file tests/integration_test.rs

# 6. Validate production readiness (5s)
ggen lifecycle readiness

# 7. Deploy to production (20s)
ggen lifecycle run deploy --env production

# Total: ~58 seconds
```

**🎉 Congratulations!** You just deployed a production-ready web service in under 60 seconds.

---

## Examples

### Example 1: REST API Service (52 seconds)

**Scenario:** Deploy a REST API with authentication, database, and tests.

```bash
#!/bin/bash
# ultra_fast_api_deploy.sh

set -euo pipefail
start_time=$(date +%s)

echo "🚀 Ultra-Fast API Deployment Starting..."

# 1. Search and install (5s)
echo "📦 Installing templates..."
ggen market add "rust-axum-auth"
ggen market add "postgresql-database"

# 2. Initialize project (3s)
echo "🏗️ Initializing project..."
ggen lifecycle run init --name "my-api"

# 3. Generate API code (10s)
echo "⚙️ Generating API code..."
ggen template generate rust-axum-auth:api-service.tmpl \
  --vars name=user-api \
  --vars port=8080 \
  --vars db=postgresql

# 4. Create cleanroom test environment (8s)
echo "🧪 Setting up test environment..."
cleanroom environment create --name api-test
cleanroom container start postgres --db apidb --user admin

# 5. Run integration tests (15s)
echo "🔬 Running integration tests..."
cleanroom test run --file tests/api_integration_test.rs

# 6. Validate production requirements (5s)
echo "✅ Validating production readiness..."
ggen lifecycle validate --env production

# 7. Deploy to production (11s)
echo "🚢 Deploying to production..."
ggen lifecycle run deploy --env production

# Cleanup test environment
cleanroom environment delete --name api-test

end_time=$(date +%s)
duration=$((end_time - start_time))

echo "✨ Deployment complete in ${duration}s!"
```

**Output:**
```
🚀 Ultra-Fast API Deployment Starting...
📦 Installing templates... (5s)
🏗️ Initializing project... (3s)
⚙️ Generating API code... (10s)
🧪 Setting up test environment... (8s)
🔬 Running integration tests... (15s)
  ✅ test_user_create ... ok
  ✅ test_user_auth ... ok
  ✅ test_db_connection ... ok
✅ Validating production readiness... (5s)
🚢 Deploying to production... (11s)
✨ Deployment complete in 52s!
```

### Example 2: Microservice with Queue (58 seconds)

**Scenario:** Deploy a microservice with message queue, cache, and monitoring.

```bash
#!/bin/bash
# ultra_fast_microservice_deploy.sh

set -euo pipefail
start_time=$(date +%s)

echo "🚀 Ultra-Fast Microservice Deployment..."

# 1. Install marketplace packages (6s)
ggen market add "rust-microservice"
ggen market add "rabbitmq-queue"
ggen market add "redis-cache"
ggen market add "prometheus-monitoring"

# 2. Initialize project structure (3s)
ggen lifecycle run init --name "order-service"

# 3. Generate microservice code (12s)
ggen template generate rust-microservice:service.tmpl \
  --vars name=order-service \
  --vars queue=rabbitmq \
  --vars cache=redis

# 4. Create cleanroom environment with services (10s)
cleanroom environment create --name microservice-test
cleanroom container start rabbitmq
cleanroom container start redis
cleanroom container start postgres --db orders

# 5. Run comprehensive tests (18s)
cleanroom test run --file tests/microservice_test.rs

# 6. Validate production (5s)
ggen lifecycle validate --env production

# 7. Deploy (9s)
ggen lifecycle run deploy --env production

# Cleanup
cleanroom environment delete --name microservice-test

end_time=$(date +%s)
echo "✨ Microservice deployed in $((end_time - start_time))s!"
```

### Example 3: CLI Tool with Docker (45 seconds)

**Scenario:** Generate, test, and deploy a CLI tool with Docker packaging.

```bash
#!/bin/bash
# ultra_fast_cli_deploy.sh

set -euo pipefail
start_time=$(date +%s)

echo "🚀 Ultra-Fast CLI Tool Deployment..."

# 1. Install templates (4s)
ggen market add "rust-cli-clap"
ggen market add "docker-multi-stage"

# 2. Initialize (2s)
ggen lifecycle run init --name "mytool"

# 3. Generate CLI code (8s)
ggen template generate rust-cli-clap:cli.tmpl \
  --vars name=mytool \
  --vars description="My awesome CLI tool"

# 4. Generate Dockerfile (3s)
ggen template generate docker-multi-stage:rust.tmpl

# 5. Create cleanroom and test (12s)
cleanroom environment create --name cli-test
cleanroom test run --file tests/cli_test.rs

# 6. Build Docker image in cleanroom (10s)
cleanroom container build --tag mytool:latest

# 7. Test Docker image (4s)
cleanroom container run mytool:latest --version

# 8. Deploy (5s)
ggen lifecycle run deploy --env production

cleanroom environment delete --name cli-test

end_time=$(date +%s)
echo "✨ CLI tool deployed in $((end_time - start_time))s!"
```

---

## Workflow Guide

### Step-by-Step Process

#### Phase 1: Discovery & Setup (8-10 seconds)

**Goal:** Find and install the right templates from the marketplace.

```bash
# Search for what you need
ggen market search "rust web" --tags "production,tested"

# View available categories
ggen market categories

# Install required packages
ggen market add "rust-axum-service"
ggen market add "postgresql-database"
ggen market add "docker-compose"

# Verify installation
ggen market list --installed
```

**Tips:**
- Use specific search terms for better results
- Check template tags for quality indicators (`production`, `tested`)
- Install related packages together for better integration

#### Phase 2: Generation (10-15 seconds)

**Goal:** Generate production-ready code from marketplace templates.

```bash
# Initialize project structure
ggen lifecycle run init --name "my-service"

# Generate main service
ggen template generate rust-axum-service:main.tmpl \
  --vars name=user-service \
  --vars port=8080 \
  --vars db=postgresql \
  --output-dir src/

# Generate tests
ggen template generate rust-axum-service:tests.tmpl \
  --vars name=user-service \
  --output-dir tests/

# Generate Docker configuration
ggen template generate docker-compose:services.tmpl \
  --vars services=postgres,redis \
  --output-dir docker/

# Generate CI/CD
ggen template generate github-actions:rust-ci.tmpl \
  --output-dir .github/workflows/
```

**Tips:**
- Use `--vars` for template customization
- Generate tests alongside production code
- Include Docker and CI/CD in initial generation

#### Phase 3: Cleanroom Testing (15-20 seconds)

**Goal:** Test in hermetic, reproducible environment.

```bash
# Create isolated test environment
cleanroom environment create --name test-env --isolated

# Start required services
cleanroom container start postgres \
  --db testdb \
  --user testuser \
  --password testpass

cleanroom container start redis

# Run integration tests
cleanroom test run \
  --file tests/integration_test.rs \
  --parallel \
  --timeout 60

# Check test results
cleanroom test results --format json

# Get test metrics
cleanroom metrics show --output json
```

**Tips:**
- Use `--parallel` for faster test execution
- Set reasonable timeouts to catch hung tests
- Always check metrics for performance regression

#### Phase 4: Validation (5-8 seconds)

**Goal:** Ensure production readiness before deployment.

```bash
# Check production readiness
ggen lifecycle readiness

# Validate deployment requirements
ggen lifecycle validate --env production

# Check for common issues
ggen lifecycle validate \
  --check-errors \
  --check-warnings \
  --check-security

# Update requirement status
ggen lifecycle readiness-update auth-basic complete
ggen lifecycle readiness-update logging complete
```

**Tips:**
- Fix all critical issues before deploying
- Document any warnings that can't be fixed
- Use `readiness-update` to track progress

#### Phase 5: Deployment (10-15 seconds)

**Goal:** Deploy to production with confidence.

```bash
# Deploy to staging first
ggen lifecycle run deploy --env staging

# Run smoke tests on staging
cleanroom test run --file tests/smoke_test.rs --env staging

# Deploy to production
ggen lifecycle run deploy --env production

# Verify deployment
ggen lifecycle run verify --env production
```

**Tips:**
- Always deploy to staging first
- Run smoke tests after each deployment
- Keep deployment scripts in version control

#### Phase 6: Cleanup (2-3 seconds)

**Goal:** Clean up test environments and resources.

```bash
# Delete test environment
cleanroom environment delete --name test-env

# Clean up containers
cleanroom container stop --all
cleanroom container prune

# Verify cleanup
cleanroom environment list
```

**Tips:**
- Always cleanup test environments
- Use `--force` if cleanup fails gracefully
- Monitor for leaked resources

---

## Best Practices

### 1. Optimize Marketplace Usage

**Search strategically:**
```bash
# Use specific queries
ggen market search "rust axum rest" --tags production

# Check package quality before installing
ggen market info "rust-axum-service" --show-quality

# Install related packages together
ggen market add "rust-axum-service" "postgresql-database" "redis-cache"
```

### 2. Template Generation Efficiency

**Batch generate related files:**
```bash
# Generate all components at once
ggen template batch-generate \
  rust-axum-service:main.tmpl \
  rust-axum-service:tests.tmpl \
  docker-compose:services.tmpl \
  --vars-file project-config.yaml
```

**Use configuration files:**
```yaml
# project-config.yaml
name: user-service
port: 8080
database: postgresql
cache: redis
auth: jwt
monitoring: prometheus
```

### 3. Cleanroom Testing Speed

**Parallel test execution:**
```bash
# Run multiple test files in parallel
cleanroom test run \
  --files "tests/*.rs" \
  --parallel \
  --max-parallel 4
```

**Reuse containers when possible:**
```bash
# Create persistent test environment
cleanroom environment create --name dev --persistent

# Reuse for multiple test runs
cleanroom test run --env dev --file tests/test1.rs
cleanroom test run --env dev --file tests/test2.rs
```

### 4. Production Validation

**Create validation checklist:**
```bash
# Define production requirements
cat > .ggen/production-requirements.yaml <<EOF
requirements:
  - id: auth
    name: Authentication
    required: true
  - id: logging
    name: Structured logging
    required: true
  - id: metrics
    name: Prometheus metrics
    required: true
  - id: health
    name: Health check endpoint
    required: true
EOF

# Validate against checklist
ggen lifecycle validate --requirements .ggen/production-requirements.yaml
```

### 5. Deployment Speed

**Use deployment profiles:**
```toml
# Makefile.toml
[tasks.deploy-fast]
dependencies = ["build", "test", "package"]
command = "ggen"
args = ["lifecycle", "run", "deploy", "--profile", "fast"]

[tasks.deploy-safe]
dependencies = ["build", "test", "integration-test", "security-scan", "package"]
command = "ggen"
args = ["lifecycle", "run", "deploy", "--profile", "safe"]
```

### 6. Error Prevention

**Production code rules:**
```rust
// ❌ NEVER in production
let result = some_operation().expect("This will crash");
let value = optional_value.unwrap();

// ✅ ALWAYS use proper error handling
let result = some_operation()
    .map_err(|e| anyhow::anyhow!("Context: {}", e))?;

let value = optional_value
    .ok_or_else(|| anyhow::anyhow!("Value required"))?;
```

### 7. Monitoring and Metrics

**Track deployment metrics:**
```bash
# Enable metrics collection
export GGEN_METRICS=1
export CLEANROOM_METRICS=1

# Run deployment with metrics
time ggen lifecycle run deploy --env production --metrics

# Review metrics
ggen lifecycle metrics --show-all
cleanroom metrics show --detailed
```

---

## Troubleshooting

### Issue: Deployment Takes >60s

**Diagnosis:**
```bash
# Enable verbose output
ggen lifecycle run deploy --env production --verbose

# Check which phase is slow
ggen lifecycle metrics --breakdown

# Profile cleanroom tests
cleanroom test run --file tests/*.rs --profile
```

**Solutions:**

1. **Slow template generation:**
   ```bash
   # Use simpler templates
   ggen template generate --optimize speed

   # Cache generated code
   ggen template cache --enable
   ```

2. **Slow cleanroom tests:**
   ```bash
   # Run tests in parallel
   cleanroom test run --parallel --max-parallel 8

   # Skip heavy integration tests
   cleanroom test run --skip-slow
   ```

3. **Slow deployment:**
   ```bash
   # Use faster deployment profile
   ggen lifecycle run deploy --profile fast

   # Skip optional validations
   ggen lifecycle run deploy --skip-optional
   ```

### Issue: Tests Fail in Cleanroom

**Diagnosis:**
```bash
# Check cleanroom environment
cleanroom environment status --name test-env

# Check container logs
cleanroom container logs postgres

# Run tests with debugging
cleanroom test run --file tests/*.rs --debug --verbose
```

**Solutions:**

1. **Container startup issues:**
   ```bash
   # Wait for containers to be ready
   cleanroom container start postgres --wait-ready --timeout 30

   # Check container health
   cleanroom container health postgres
   ```

2. **Test isolation issues:**
   ```bash
   # Use completely isolated environment
   cleanroom environment create --name test --isolated --clean

   # Reset state between tests
   cleanroom test run --reset-between
   ```

3. **Resource constraints:**
   ```bash
   # Increase resource limits
   cleanroom environment create --memory 4G --cpus 4
   ```

### Issue: Production Validation Fails

**Diagnosis:**
```bash
# Check detailed validation output
ggen lifecycle validate --env production --verbose

# See which requirements are not met
ggen lifecycle readiness --show-unmet

# Check for specific issues
ggen lifecycle validate --check security --check errors
```

**Solutions:**

1. **Missing requirements:**
   ```bash
   # Install missing dependencies
   ggen market add "prometheus-metrics"

   # Generate missing components
   ggen template generate monitoring:prometheus.tmpl

   # Update readiness status
   ggen lifecycle readiness-update metrics complete
   ```

2. **Security issues:**
   ```bash
   # Run security audit
   ggen lifecycle audit --security

   # Fix identified issues
   ggen lifecycle fix --security-issues
   ```

### Issue: Generated Code Has Errors

**Diagnosis:**
```bash
# Validate generated code
cargo check

# Check template quality
ggen market info "rust-axum-service" --check-quality

# Review template variables
ggen template inspect rust-axum-service:main.tmpl
```

**Solutions:**

1. **Template issues:**
   ```bash
   # Use vetted marketplace templates
   ggen market search --tags "production,verified"

   # Update to latest template version
   ggen market update "rust-axum-service"
   ```

2. **Variable substitution issues:**
   ```bash
   # Use configuration file
   ggen template generate --vars-file config.yaml

   # Validate variables before generation
   ggen template validate-vars config.yaml
   ```

---

## Performance

### Benchmarks

**Average timings for ultra-fast workflow:**

| Phase | Time | % of Total |
|-------|------|------------|
| Marketplace search & install | 8s | 13% |
| Template generation | 12s | 20% |
| Cleanroom environment setup | 8s | 13% |
| Integration tests | 18s | 30% |
| Production validation | 5s | 8% |
| Deployment | 10s | 17% |
| **Total** | **60s** | **100%** |

### Optimization Targets

**To achieve <60s consistently:**

1. **Marketplace (Target: <8s)**
   - Cache marketplace metadata locally
   - Use direct package IDs instead of search
   - Install multiple packages in parallel

2. **Generation (Target: <12s)**
   - Use optimized templates (`--optimize speed`)
   - Enable template caching
   - Generate files in parallel

3. **Testing (Target: <18s)**
   - Run tests in parallel (`--parallel`)
   - Use lightweight test fixtures
   - Skip slow tests in fast mode

4. **Validation (Target: <5s)**
   - Cache validation results
   - Skip optional checks in fast mode
   - Use incremental validation

5. **Deployment (Target: <10s)**
   - Use pre-built images
   - Deploy to staging and production in parallel
   - Skip optional deployment steps

### Performance Monitoring

**Track metrics over time:**
```bash
# Enable metrics collection
export GGEN_METRICS_FILE=metrics.json

# Run deployment and collect metrics
ggen lifecycle run deploy --env production

# Analyze performance trends
ggen lifecycle metrics analyze --file metrics.json --trends

# Compare against baseline
ggen lifecycle metrics compare --baseline baseline.json
```

**Sample metrics output:**
```json
{
  "deployment_id": "deploy-2025-01-13-001",
  "total_time_seconds": 58,
  "phases": {
    "marketplace": 7,
    "generation": 11,
    "testing": 17,
    "validation": 5,
    "deployment": 9
  },
  "test_metrics": {
    "total_tests": 42,
    "passed": 42,
    "failed": 0,
    "duration_seconds": 17
  },
  "resource_usage": {
    "peak_memory_mb": 512,
    "cpu_percent": 75
  }
}
```

---

## Advanced Usage

### Multi-Environment Deployments

**Deploy to multiple environments sequentially:**
```bash
#!/bin/bash
# multi_env_deploy.sh

environments=("dev" "staging" "production")

for env in "${environments[@]}"; do
  echo "Deploying to $env..."

  ggen lifecycle run deploy --env "$env"

  cleanroom test run \
    --file tests/smoke_test.rs \
    --env "$env"

  ggen lifecycle run verify --env "$env"
done
```

### Continuous Deployment Pipeline

**GitHub Actions workflow:**
```yaml
# .github/workflows/ultra-fast-deploy.yml
name: Ultra-Fast Deployment

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    timeout-minutes: 2

    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Install cleanroom
        run: |
          cd cleanroom
          cargo build --release
          echo "$PWD/target/release" >> $GITHUB_PATH

      - name: Ultra-Fast Deploy
        run: |
          set -euo pipefail
          start_time=$(date +%s)

          ggen market add "rust-axum-service"
          ggen lifecycle run init
          ggen template generate rust-axum-service:main.tmpl

          cleanroom environment create --name ci-test
          cleanroom test run --file tests/*.rs

          ggen lifecycle validate --env production
          ggen lifecycle run deploy --env production

          end_time=$(date +%s)
          duration=$((end_time - start_time))
          echo "Deployed in ${duration}s"

          if [ $duration -gt 60 ]; then
            echo "Warning: Deployment took longer than 60s"
            exit 1
          fi
```

### Custom Deployment Profiles

**Create optimized deployment profiles:**
```toml
# .ggen/deployment-profiles.toml

[profiles.ultra-fast]
marketplace_cache = true
template_optimize = "speed"
parallel_tests = true
max_parallel_tests = 8
skip_optional_validation = true
deployment_mode = "fast"

[profiles.production-safe]
marketplace_cache = false
template_optimize = "quality"
parallel_tests = true
max_parallel_tests = 4
skip_optional_validation = false
deployment_mode = "safe"
run_security_scan = true
```

**Use profiles:**
```bash
# Ultra-fast deployment
ggen lifecycle run deploy --profile ultra-fast

# Safe production deployment
ggen lifecycle run deploy --profile production-safe
```

---

## Related Documentation

- **[Technical Reference](ULTRA_FAST_REFERENCE.md)** - Deep dive into architecture and implementation
- **[Cleanroom Testing Guide](../cleanroom/docs/ggen-test-strategy.md)** - Comprehensive cleanroom documentation
- **[Lifecycle System](LIFECYCLE_INDEX.md)** - Complete lifecycle documentation
- **[Marketplace Guide](marketplace.md)** - Template marketplace documentation
- **[Production Deployment](PRODUCTION_DEPLOYMENT.md)** - Production best practices

---

## Appendix: Complete Example Script

```bash
#!/bin/bash
# complete_ultra_fast_deploy.sh
# Complete ultra-fast deployment example with error handling and metrics

set -euo pipefail

# Configuration
PROJECT_NAME="my-service"
SERVICE_TYPE="rust-axum-service"
ENV_NAME="test-env"
START_TIME=$(date +%s)

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

cleanup() {
    log_info "Cleaning up test environment..."
    cleanroom environment delete --name "$ENV_NAME" 2>/dev/null || true
}

trap cleanup EXIT

# Phase 1: Marketplace
log_info "Phase 1: Installing marketplace packages..."
phase_start=$(date +%s)
ggen market add "$SERVICE_TYPE" || {
    log_error "Failed to install marketplace package"
    exit 1
}
phase_duration=$(($(date +%s) - phase_start))
log_info "Marketplace phase completed in ${phase_duration}s"

# Phase 2: Generation
log_info "Phase 2: Generating project code..."
phase_start=$(date +%s)
ggen lifecycle run init --name "$PROJECT_NAME"
ggen template generate "$SERVICE_TYPE:main.tmpl" \
    --vars name="$PROJECT_NAME" \
    --vars port=8080
phase_duration=$(($(date +%s) - phase_start))
log_info "Generation phase completed in ${phase_duration}s"

# Phase 3: Testing
log_info "Phase 3: Running cleanroom tests..."
phase_start=$(date +%s)
cleanroom environment create --name "$ENV_NAME"
cleanroom container start postgres --db testdb
cleanroom test run --file tests/*.rs --parallel
phase_duration=$(($(date +%s) - phase_start))
log_info "Testing phase completed in ${phase_duration}s"

# Phase 4: Validation
log_info "Phase 4: Validating production readiness..."
phase_start=$(date +%s)
ggen lifecycle validate --env production
phase_duration=$(($(date +%s) - phase_start))
log_info "Validation phase completed in ${phase_duration}s"

# Phase 5: Deployment
log_info "Phase 5: Deploying to production..."
phase_start=$(date +%s)
ggen lifecycle run deploy --env production
phase_duration=$(($(date +%s) - phase_start))
log_info "Deployment phase completed in ${phase_duration}s"

# Summary
total_duration=$(($(date +%s) - START_TIME))
log_info "========================================"
log_info "Ultra-Fast Deployment Complete!"
log_info "Total time: ${total_duration}s"
log_info "========================================"

if [ $total_duration -gt 60 ]; then
    log_warn "Deployment took longer than 60s target"
    exit 1
fi

exit 0
```

---

**Last Updated:** 2025-01-13
**Version:** 1.0.0
**Maintained By:** Core Team

For questions or issues, see [Troubleshooting](#troubleshooting) or open an issue with the `ultra-fast-deploy` label.
