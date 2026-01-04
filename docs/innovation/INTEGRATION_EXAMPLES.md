<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Andon Signal Validation Framework - Integration Examples](#andon-signal-validation-framework---integration-examples)
  - [Real-World Integration Patterns](#real-world-integration-patterns)
    - [Example 1: CI/CD Pipeline Integration](#example-1-cicd-pipeline-integration)
    - [Example 2: Scheduled Monitoring](#example-2-scheduled-monitoring)
    - [Example 3: Pre-Deployment Validation](#example-3-pre-deployment-validation)
    - [Example 4: Development Workflow](#example-4-development-workflow)
    - [Example 5: Team Dashboard](#example-5-team-dashboard)
    - [Example 6: Automated Remediation](#example-6-automated-remediation)
    - [Example 7: Validation Metrics Collection](#example-7-validation-metrics-collection)
    - [Example 8: Git Hook Integration](#example-8-git-hook-integration)
    - [Example 9: Multi-Project Validation](#example-9-multi-project-validation)
    - [Example 10: Validation as a Service](#example-10-validation-as-a-service)
  - [Best Practices](#best-practices)
    - [1. Incremental Validation](#1-incremental-validation)
    - [2. Fail Fast](#2-fail-fast)
    - [3. Clear Error Messages](#3-clear-error-messages)
    - [4. Validation Caching](#4-validation-caching)
    - [5. Parallel Execution](#5-parallel-execution)
  - [Integration Checklist](#integration-checklist)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Andon Signal Validation Framework - Integration Examples

## Real-World Integration Patterns

### Example 1: CI/CD Pipeline Integration

**Use Case**: Validate all code before merging to main branch.

**GitHub Actions Workflow**:
```yaml
name: Validation Gate

on:
  pull_request:
    branches: [main]

jobs:
  validation:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      
      # Run validation framework
      - name: Run Validation
        run: |
          cargo make validation-report
          cargo make monitor-validation
```

**Local Testing**:
```bash
# Test before pushing
cargo make act-validation
```

---

### Example 2: Scheduled Monitoring

**Use Case**: Daily validation monitoring with alerts.

**Cron Job**:
```bash
# Run daily at 9 AM
0 9 * * * cd /path/to/ggen && \
  cargo make validation-report && \
  cargo make monitor-validation && \
  [ $? -ne 0 ] && \
  echo "Validation failed" | mail -s "Validation Alert" team@example.com
```

**Script** (`scripts/daily-validation.sh`):
```bash
#!/bin/bash
cd /path/to/ggen
cargo make validation-report
cargo make monitor-validation

if [ $? -ne 0 ]; then
    # Send alert (Slack, email, etc.)
    curl -X POST https://hooks.slack.com/services/YOUR/WEBHOOK/URL \
      -d '{"text":"🚨 Validation failed - check validation-report.txt"}'
fi
```

---

### Example 3: Pre-Deployment Validation

**Use Case**: Validate before deploying to production.

**Script** (`scripts/pre-deploy-validation.sh`):
```bash
#!/bin/bash
set -e

echo "🔍 Pre-deployment validation..."

# Run all validation layers
cargo make check || { echo "❌ Compilation failed"; exit 1; }
cargo make lint || { echo "❌ Linting failed"; exit 1; }
cargo make test-unit || { echo "❌ Tests failed"; exit 1; }
cargo make verify-cli || { echo "❌ CLI verification failed"; exit 1; }

echo "✅ All validation passed - ready for deployment"
```

**Integration**:
```bash
# In deployment script
./scripts/pre-deploy-validation.sh || exit 1
# Continue with deployment...
```

---

### Example 4: Development Workflow

**Use Case**: Fast feedback during development.

**Quick Validation** (fast, <30s):
```bash
# Quick check before commit
cargo make check        # Layer 1 only
cargo make verify-cli   # Layer 3 only (if binary exists)
```

**Full Validation** (comprehensive, <5min):
```bash
# Before pushing
cargo make pre-commit   # All layers
```

**Local CI Testing**:
```bash
# Test GitHub Actions workflow locally
cargo make act-validation JOB=compile-time
```

---

### Example 5: Team Dashboard

**Use Case**: Track validation status across team.

**Script** (`scripts/validation-dashboard.sh`):
```bash
#!/bin/bash

# Generate report
cargo make validation-report

# Parse and format for dashboard
cat validation-report.txt | \
  grep -E "(PASSED|FAILED)" | \
  awk '{print $1, $2}' | \
  jq -R -s 'split("\n") | map(select(length > 0)) | map(split(" ")) | map({layer: .[0], status: .[1]})'

# Output JSON for dashboard consumption
```

**Dashboard Integration**:
```javascript
// Fetch validation status
fetch('/api/validation-status')
  .then(r => r.json())
  .then(data => {
    // Display in dashboard
    updateDashboard(data);
  });
```

---

### Example 6: Automated Remediation

**Use Case**: Auto-fix common validation failures.

**Script** (`scripts/auto-fix-validation.sh`):
```bash
#!/bin/bash

# Run validation
cargo make validation-report

# Check for auto-fixable issues
if grep -q "Format check.*FAILED" validation-report.txt; then
    echo "🔧 Auto-fixing formatting..."
    cargo make fmt
fi

if grep -q "Linting.*FAILED" validation-report.txt; then
    echo "🔧 Checking for auto-fixable lint issues..."
    cargo clippy --fix --allow-dirty --allow-staged
fi

# Re-run validation
cargo make validation-report
```

---

### Example 7: Validation Metrics Collection

**Use Case**: Track validation trends over time.

**Script** (`scripts/collect-validation-metrics.sh`):
```bash
#!/bin/bash

METRICS_DIR=".metrics/validation"
mkdir -p "$METRICS_DIR"

# Generate report
cargo make validation-report

# Extract metrics
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
LAYER1=$(grep -c "✅.*Compilation: PASSED" validation-report.txt || echo "0")
LAYER2=$(grep -c "✅.*Unit Tests: PASSED" validation-report.txt || echo "0")
LAYER3=$(grep -c "✅.*CLI Verification: PASSED" validation-report.txt || echo "0")

# Store metrics
cat > "$METRICS_DIR/$TIMESTAMP.json" << EOF
{
  "timestamp": "$TIMESTAMP",
  "layer1_passed": $LAYER1,
  "layer2_passed": $LAYER2,
  "layer3_passed": $LAYER3,
  "overall_status": "$([ $LAYER1 -eq 1 ] && [ $LAYER2 -eq 1 ] && [ $LAYER3 -eq 1 ] && echo "pass" || echo "fail")"
}
EOF

echo "📊 Metrics collected: $METRICS_DIR/$TIMESTAMP.json"
```

---

### Example 8: Git Hook Integration

**Use Case**: Automatic validation on every commit.

**Pre-Commit Hook** (`.git/hooks/pre-commit`):
```bash
#!/bin/bash

# Run validation framework
cargo make pre-commit

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Pre-commit validation failed"
    echo "   Fix issues above and try again"
    echo "   Or skip with: git commit --no-verify (not recommended)"
    exit 1
fi
```

**Installation**:
```bash
chmod +x .git/hooks/pre-commit
# Or use existing installation script
./scripts/install-git-hooks.sh
```

---

### Example 9: Multi-Project Validation

**Use Case**: Validate multiple projects in a monorepo.

**Script** (`scripts/validate-all-projects.sh`):
```bash
#!/bin/bash

PROJECTS=("project1" "project2" "project3")

for project in "${PROJECTS[@]}"; do
    echo "🔍 Validating $project..."
    cd "$project"
    
    cargo make validation-report
    cargo make monitor-validation
    
    if [ $? -ne 0 ]; then
        echo "❌ $project validation failed"
        FAILED+=("$project")
    fi
    
    cd ..
done

if [ ${#FAILED[@]} -gt 0 ]; then
    echo "❌ Failed projects: ${FAILED[*]}"
    exit 1
else
    echo "✅ All projects validated successfully"
fi
```

---

### Example 10: Validation as a Service

**Use Case**: Expose validation as HTTP endpoint.

**Simple HTTP Server** (`scripts/validation-server.sh`):
```bash
#!/bin/bash

# Generate validation report
cargo make validation-report > /dev/null 2>&1

# Parse status
if grep -q "✅.*PASSED" validation-report.txt; then
    STATUS="pass"
    HTTP_CODE=200
else
    STATUS="fail"
    HTTP_CODE=500
fi

# Return JSON
cat << EOF
HTTP/1.1 $HTTP_CODE OK
Content-Type: application/json

{"status": "$STATUS", "report": "$(cat validation-report.txt | jq -Rs .)"}
EOF
```

**Integration with monitoring tools**:
```bash
# Health check endpoint
curl http://localhost:8080/validation/status
```

---

## Best Practices

### 1. Incremental Validation

Run fast checks frequently, full validation before commits:
```bash
# During development (fast)
cargo make check

# Before commit (comprehensive)
cargo make pre-commit
```

### 2. Fail Fast

Stop on first failure in CI/CD:
```bash
set -e
cargo make check || exit 1
cargo make lint || exit 1
cargo make verify-cli || exit 1
```

### 3. Clear Error Messages

Provide actionable feedback:
```bash
if ! cargo make verify-cli; then
    echo "❌ CLI verification failed"
    echo "   Run 'cargo make verify-cli' to see details"
    echo "   Common fixes:"
    echo "   - Build binary: cargo make build-release"
    echo "   - Check command syntax: ggen --help"
    exit 1
fi
```

### 4. Validation Caching

Cache validation results when appropriate:
```bash
# Only re-validate if code changed
if [ "validation-report.txt" -ot "Cargo.lock" ]; then
    cargo make validation-report
fi
```

### 5. Parallel Execution

Run independent layers in parallel:
```bash
# Run layers 1 and 2 in parallel
cargo make check &
cargo make test-unit &
wait
```

---

## Integration Checklist

- [ ] Prerequisites installed (Rust, cargo-make, Docker, act)
- [ ] Validation framework tested locally
- [ ] Pre-commit hooks installed
- [ ] CI/CD pipeline configured
- [ ] Monitoring set up (if needed)
- [ ] Team trained on framework usage
- [ ] Documentation reviewed
- [ ] Troubleshooting guide accessible

---

**Last Updated**: 2025-12-12  
**Framework Version**: v1.0.0







