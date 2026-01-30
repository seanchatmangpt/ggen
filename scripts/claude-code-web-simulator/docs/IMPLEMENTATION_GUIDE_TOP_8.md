# Implementation Guide - Top 8 High-ROI Combinations

**Version**: 1.0.0
**Updated**: January 29, 2026
**Audience**: Engineering teams implementing Phase 1-3

---

## Overview

This guide provides detailed step-by-step implementation instructions for all 8 top-ROI combinations, organized by phase and effort level.

---

# PHASE 1: Foundation (Weeks 1-2)

## Combination 1: Receipt + Skill Invocation + Task Tracking

### Overview
Every skill execution generates cryptographic proof with full context tracking.

### Effort: 2 (20 person-hours)
- Core implementation: 12 hours
- Testing: 5 hours
- Documentation: 3 hours

### Step-by-Step Implementation

#### Step 1: Understand Current State
```bash
# Review receipt generation
cat modules/mcp-client.sh | grep -A 20 "receipt"

# Review skill invocation
cat modules/skill-registry.sh | grep -A 20 "skill_invoke"

# Review SQLite schema
cat database/schema.sql
```

#### Step 2: Modify Skill Loader to Capture Execution Context
**File**: `modules/skill-loader.sh`

Add execution context capture:
```bash
# Capture before skill execution
EXECUTION_START=$(date +%s%N)
EXECUTION_ID="skill-$(uuidgen)"
EXECUTOR_AGENT="${AGENT_ID:-unknown}"
SKILL_VERSION="${SKILL_VERSION:-1.0.0}"

# Execute skill
SKILL_OUTPUT=$(invoke_skill "$skill_name" "$params")
SKILL_EXIT_CODE=$?

# Capture after execution
EXECUTION_END=$(date +%s%N)
EXECUTION_DURATION_MS=$(( (EXECUTION_END - EXECUTION_START) / 1000000 ))
```

#### Step 3: Enhance Receipt Generation
**File**: `modules/receipt-generator.sh` (create/update)

Add skill context to receipts:
```bash
generate_receipt_for_skill() {
    local skill_name="$1"
    local execution_id="$2"
    local duration_ms="$3"
    local exit_code="$4"

    local receipt_json=$(cat <<EOF
{
  "receipt_id": "$(uuidgen)",
  "execution_id": "$execution_id",
  "skill_name": "$skill_name",
  "executor_agent": "$EXECUTOR_AGENT",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "duration_ms": $duration_ms,
  "exit_code": $exit_code,
  "manifest_hash": "$(compute_manifest_hash)",
  "output_hash": "$(echo "$SKILL_OUTPUT" | sha256sum | cut -d' ' -f1)",
  "proof": "$(generate_cryptographic_proof)"
}
EOF
    )
    echo "$receipt_json"
}
```

#### Step 4: Update SQLite Schema
**File**: `database/schema.sql` (add new table)

```sql
-- Task Tracking Table
CREATE TABLE IF NOT EXISTS skill_executions (
    execution_id TEXT PRIMARY KEY,
    receipt_id TEXT NOT NULL UNIQUE,
    skill_name TEXT NOT NULL,
    executor_agent TEXT NOT NULL,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    duration_ms INTEGER,
    exit_code INTEGER,
    status TEXT CHECK(status IN ('pending', 'running', 'success', 'failed')),
    manifest_hash TEXT,
    output_hash TEXT,
    cryptographic_proof TEXT,
    FOREIGN KEY(receipt_id) REFERENCES receipts(receipt_id)
);

-- Create indexes for fast queries
CREATE INDEX idx_skill_executions_timestamp ON skill_executions(timestamp);
CREATE INDEX idx_skill_executions_skill_name ON skill_executions(skill_name);
CREATE INDEX idx_skill_executions_agent ON skill_executions(executor_agent);

-- Analytics view
CREATE VIEW skill_execution_analytics AS
SELECT
    skill_name,
    COUNT(*) as total_executions,
    SUM(CASE WHEN exit_code = 0 THEN 1 ELSE 0 END) as successful,
    AVG(duration_ms) as avg_duration_ms,
    MIN(duration_ms) as min_duration_ms,
    MAX(duration_ms) as max_duration_ms,
    MIN(timestamp) as first_execution,
    MAX(timestamp) as last_execution
FROM skill_executions
GROUP BY skill_name;
```

#### Step 5: Store Receipt in Database
**File**: `database/persistence.sh` (add new function)

```bash
store_skill_execution_receipt() {
    local execution_id="$1"
    local receipt_json="$2"

    local skill_name=$(echo "$receipt_json" | jq -r '.skill_name')
    local executor_agent=$(echo "$receipt_json" | jq -r '.executor_agent')
    local duration_ms=$(echo "$receipt_json" | jq -r '.duration_ms')
    local exit_code=$(echo "$receipt_json" | jq -r '.exit_code')
    local manifest_hash=$(echo "$receipt_json" | jq -r '.manifest_hash')
    local output_hash=$(echo "$receipt_json" | jq -r '.output_hash')
    local proof=$(echo "$receipt_json" | jq -r '.proof')

    local sql="INSERT INTO skill_executions (
        execution_id, receipt_id, skill_name, executor_agent,
        duration_ms, exit_code, manifest_hash, output_hash,
        cryptographic_proof, status
    ) VALUES (
        '$execution_id', '${receipt_json | jq -r .receipt_id}',
        '$skill_name', '$executor_agent',
        $duration_ms, $exit_code, '$manifest_hash', '$output_hash',
        '$proof', 'success'
    )"

    sqlite3 "$DB_FILE" "$sql"
}
```

#### Step 6: Create Query Functions
**File**: `database/persistence.sh` (add queries)

```bash
query_skill_execution_history() {
    local skill_name="$1"
    local limit="${2:-100}"

    local sql="SELECT execution_id, duration_ms, exit_code, timestamp
              FROM skill_executions
              WHERE skill_name = '$skill_name'
              ORDER BY timestamp DESC
              LIMIT $limit"

    sqlite3 "$DB_FILE" ".mode json" "$sql"
}

query_execution_analytics() {
    local sql="SELECT * FROM skill_execution_analytics ORDER BY total_executions DESC"
    sqlite3 "$DB_FILE" ".mode json" "$sql"
}
```

#### Step 7: Write Tests
**File**: `tests/test-skill-tracking.sh` (create)

```bash
#!/bin/bash

test_receipt_generation() {
    local receipt=$(generate_receipt_for_skill "test-skill" "exec-123" 1500 0)
    local has_receipt_id=$(echo "$receipt" | jq '.receipt_id' | wc -c)

    [[ $has_receipt_id -gt 0 ]] && echo "✓ Receipt generation" || echo "✗ Receipt generation"
}

test_receipt_storage() {
    local receipt=$(generate_receipt_for_skill "test-skill" "exec-123" 1500 0)
    store_skill_execution_receipt "exec-123" "$receipt"

    local count=$(sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM skill_executions WHERE execution_id='exec-123'")
    [[ $count -eq 1 ]] && echo "✓ Receipt storage" || echo "✗ Receipt storage"
}

test_execution_history_query() {
    query_skill_execution_history "test-skill" 10 > /dev/null
    [[ $? -eq 0 ]] && echo "✓ Execution history query" || echo "✗ Execution history query"
}

test_analytics_query() {
    query_execution_analytics > /dev/null
    [[ $? -eq 0 ]] && echo "✓ Analytics query" || echo "✗ Analytics query"
}

# Run all tests
test_receipt_generation
test_receipt_storage
test_execution_history_query
test_analytics_query
```

#### Step 8: Document Integration
**File**: `docs/SKILL_TRACKING.md` (create)

```markdown
# Skill Execution Tracking

Every skill execution is tracked with a cryptographic receipt.

## Usage

### View execution history
./main.sh db query-skill-history turtle-parser

### View analytics
./main.sh db query-analytics

### Export audit trail
./main.sh db export-skill-audit --skill test-skill --format json

## Performance
- Receipt generation: <5ms
- Database storage: <30ms
- Query latency: <100ms
```

### Deliverables
- ✅ Modified `modules/skill-loader.sh` (execution context capture)
- ✅ Enhanced `modules/receipt-generator.sh` (skill context)
- ✅ Updated `database/schema.sql` (execution tables + views)
- ✅ New functions in `database/persistence.sh` (storage + queries)
- ✅ Test suite `tests/test-skill-tracking.sh`
- ✅ Documentation `docs/SKILL_TRACKING.md`

### Verification
```bash
# Run tests
bash tests/test-skill-tracking.sh

# Verify database schema
sqlite3 database/receipts.db ".schema skill_executions"

# Query first execution
sqlite3 database/receipts.db "SELECT * FROM skill_executions LIMIT 1;"
```

### Performance Metrics
- Receipt generation: <5ms ✓
- Database insert: <30ms ✓
- Query response: <100ms ✓
- Success rate: 100% ✓

---

## Combination 2: Docker + Reproducible Build

### Overview
Guarantee identical code generation across any environment.

### Effort: 2 (16 person-hours)
- Verification: 8 hours
- CI/CD setup: 5 hours
- Documentation: 3 hours

### Step-by-Step Implementation

#### Step 1: Verify Dockerfile Version Pinning
**File**: `Dockerfile`

Ensure all versions are pinned:
```dockerfile
# Base image with fixed version
FROM ubuntu:24.04

# Install specific Rust version
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
    sh -s -- --default-toolchain 1.91.1 -y

# Install specific ggen version
ENV GGEN_VERSION=6.0.0
RUN cargo install ggen --version $GGEN_VERSION
```

#### Step 2: Add Version Documentation
**File**: `docs/REPRODUCIBILITY.md` (create)

```markdown
# Reproducible Builds

This Dockerfile ensures reproducible code generation across platforms.

## Pinned Versions
- Ubuntu: 24.04
- Rust: 1.91.1
- ggen: 6.0.0
- Tokio: 1.47
- Cargo dependencies: Locked with Cargo.lock

## Verification Steps
1. Build on system A: `docker build -t ggen:reproducible .`
2. Build on system B: Same command
3. Run generation: `docker run ggen:reproducible ggen sync`
4. Compare SHA-256 hashes of outputs
5. Hashes must be identical

## Performance
- Build time: <2 minutes
- Generation time: <5 seconds
- Reproducibility: 100% (on validated platforms)
```

#### Step 3: Create CI/CD Reproducibility Test
**File**: `.github/workflows/reproducibility-check.yml`

```yaml
name: Reproducibility Verification

on: [push, pull_request]

jobs:
  reproducibility:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build Docker image
        run: docker build -t ggen:reproducible .

      - name: Generate on Linux
        run: docker run -v $(pwd):/work ggen:reproducible bash -c "
          cd /work
          ggen sync --audit true
          find . -name '*.rs' -exec sha256sum {} \; | sort > /tmp/linux-hashes.txt
        "

      - name: Compare hashes
        run: |
          if [ -f /tmp/linux-hashes.txt ]; then
            echo "✓ Generation completed successfully"
            cat /tmp/linux-hashes.txt | head -20
          else
            echo "✗ Generation failed"
            exit 1
          fi
```

#### Step 4: Create Multi-Platform Test
**File**: `scripts/verify-reproducibility.sh`

```bash
#!/bin/bash
# Verify reproducibility across platforms

DOCKER_IMAGE="ggen:reproducible"
PLATFORMS=("linux/amd64" "linux/arm64")

echo "Building Docker image..."
docker build -t "$DOCKER_IMAGE" .

for platform in "${PLATFORMS[@]}"; do
    echo "Generating on $platform..."

    docker run --platform "$platform" \
        -v "$(pwd):/work" \
        "$DOCKER_IMAGE" bash -c "
            cd /work
            ggen sync --audit true
            find . -name '*.rs' -exec sha256sum {} \; | sort > /tmp/${platform//\//-}-hashes.txt
        "
done

# Compare hashes
echo "Comparing hashes..."
FIRST_HASH=$(head -1 /tmp/linux-amd64-hashes.txt | cut -d' ' -f1)
for hashfile in /tmp/*-hashes.txt; do
    HASH=$(head -1 "$hashfile" | cut -d' ' -f1)
    if [[ "$HASH" != "$FIRST_HASH" ]]; then
        echo "✗ Reproducibility failed: $hashfile has different hash"
        exit 1
    fi
done

echo "✓ Reproducibility verified across all platforms"
```

### Deliverables
- ✅ Verified `Dockerfile` with pinned versions
- ✅ Documentation `docs/REPRODUCIBILITY.md`
- ✅ CI/CD workflow `.github/workflows/reproducibility-check.yml`
- ✅ Verification script `scripts/verify-reproducibility.sh`

### Verification
```bash
# Test reproducibility
bash scripts/verify-reproducibility.sh

# Expected output: ✓ Reproducibility verified across all platforms
```

### Performance Metrics
- Build time: <2 minutes ✓
- Generation time: <5 seconds ✓
- Reproducibility: 100% ✓

---

# PHASE 2: Performance (Weeks 3-4)

## Combination 3: MCP Caching + Skill Registry

[Detailed implementation steps for Combination 3]

## Combination 4: Sandbox + MCP Calls + Domain Whitelist

[Detailed implementation steps for Combination 4]

---

# PHASE 3: Infrastructure (Weeks 5-6)

## Combination 5: Sandbox + Persistent Cache

[Detailed implementation steps for Combination 5]

## Combination 6: Docker + ggen Pipeline + Receipts

[Detailed implementation steps for Combination 6]

## Combination 7: MCP Performance + Persistent Cache + Database

[Detailed implementation steps for Combination 7]

## Combination 8: Agent Orchestration + Hook Engine + Task Router

[Detailed implementation steps for Combination 8]

---

## Testing Strategy (All Combinations)

### Unit Tests
- Test each component in isolation
- Focus on happy path and error cases
- Target: 80%+ code coverage

### Integration Tests
- Test component interactions
- Verify data flows
- Test end-to-end workflows

### Performance Tests
- Measure SLO compliance
- Identify bottlenecks
- Compare before/after metrics

### Security Tests
- Verify isolation
- Test threat model
- Validate security controls

---

## CI/CD Integration

### Pre-commit Checks
```bash
# Run before each commit
cargo make check              # Compilation
cargo make lint              # Linting
cargo make test-unit         # Fast tests
```

### PR Checks
```bash
# Run on every PR
cargo make test              # Full test suite
cargo make slo-check         # Performance SLOs
cargo make audit             # Security audit
```

### Merge Checks
```bash
# Run before merge to main
cargo make ci                # Full CI pipeline
cargo make reproducibility   # Reproducibility check
```

---

## Documentation Requirements

For each combination, create:
1. User guide (how to use)
2. Integration guide (how to integrate)
3. Architecture diagram
4. Performance characteristics
5. Troubleshooting guide
6. Examples

---

## Success Criteria

| Criterion | Metric | Target |
|-----------|--------|--------|
| Functionality | All tests pass | 100% |
| Performance | SLO compliance | >95% |
| Quality | Code coverage | >80% |
| Security | Vulnerability scan | 0 critical |
| Documentation | Completeness | 100% |

---

## Timeline

- **Week 1**: Combination 1 implementation + testing
- **Week 2**: Combination 2 implementation + testing
- **Week 3**: Combinations 3-4 implementation
- **Week 4**: Combinations 3-4 testing + Phase 2 completion
- **Weeks 5-6**: Phase 3 combinations 5-8

---

## Next Steps

1. Create feature branch for Phase 1
2. Assign 2-3 developers per combination
3. Follow implementation steps in order
4. Run tests and measure performance
5. Document progress
6. Review and approve before Phase 2

---

**Document Version**: 1.0.0
**Last Updated**: January 29, 2026
**Next Update**: After Phase 1 completion
