# CLNRM Quick Start Guide

## 🚀 Get Started in 5 Minutes

This guide will get you running CLNRM tests with comprehensive 7-layer validation in under 5 minutes.

### Prerequisites

1. **Docker** - For OTEL collector
2. **Rust** - For building ggen and clnrm
3. **ggen** - Built at `/Users/sac/ggen/target/release/ggen`
4. **clnrm** - Built at `~/dev/clnrm/target/release/cleanroom`

### Step 1: Start OTEL Collector (30 seconds)

```bash
# Start OpenTelemetry collector for trace collection
docker run -d \
  --name otel-collector \
  -p 4318:4318 \
  -p 4317:4317 \
  otel/opentelemetry-collector:latest

# Verify it's running
curl http://localhost:4318/v1/traces
```

**Expected output**: `405 Method Not Allowed` (means collector is running)

### Step 2: Build CLNRM CLI (2 minutes)

```bash
cd ~/dev/clnrm
cargo build --release --bin cleanroom
```

**Expected output**: `Finished release [optimized] target(s) in ...`

### Step 3: Build ggen (if not already done) (2 minutes)

```bash
cd /Users/sac/ggen
cargo build --release
```

### Step 4: Run Your First CLNRM Test (30 seconds)

```bash
cd /Users/sac/ggen

# Run marketplace search test
~/dev/clnrm/target/release/cleanroom run \
  tests/clnrm/marketplace/search.clnrm.toml \
  --otel-endpoint http://localhost:4318 \
  --report-json /tmp/search-report.json

# View results
jq . /tmp/search-report.json
```

**Expected output**: JSON report showing test results and validation layers

### Step 5: Run All Tests (2 minutes)

```bash
# Run the complete test suite
./tests/clnrm/run-all-tests.sh
```

**Expected output**:
```
╔══════════════════════════════════════════════════════════╗
║         CLNRM Test Suite Runner for ggen                ║
╚══════════════════════════════════════════════════════════╝

✅ Checking prerequisites...
✅ CLNRM binary found
✅ OTEL collector is running
✅ ggen binary ready

✅ Validating CLNRM test configurations...
  Validating search.clnrm.toml... ✅
  Validating install.clnrm.toml... ✅
  Validating init.clnrm.toml... ✅
  Validating deploy.clnrm.toml... ✅

═══════════════════════════════════════════════════════════
           Running CLNRM Test Suite
═══════════════════════════════════════════════════════════

📦 Marketplace Tests
▶ Running marketplace/search...
  ✅ PASSED (2s)
     Reports: /Users/sac/ggen/target/clnrm-reports/marketplace_search.*

▶ Running marketplace/install...
  ✅ PASSED (5s)
     Reports: /Users/sac/ggen/target/clnrm-reports/marketplace_install.*

🔄 Lifecycle Tests
▶ Running lifecycle/init...
  ✅ PASSED (3s)
     Reports: /Users/sac/ggen/target/clnrm-reports/lifecycle_init.*

▶ Running lifecycle/deploy...
  ✅ PASSED (8s)
     Reports: /Users/sac/ggen/target/clnrm-reports/lifecycle_deploy.*

═══════════════════════════════════════════════════════════
           Test Suite Summary
═══════════════════════════════════════════════════════════
  Total Tests: 4
  Passed: 4
  Failed: 0
  Duration: 18s
  Reports: /Users/sac/ggen/target/clnrm-reports/

🎉 All tests passed!
```

---

## 📊 Understanding Test Results

### JSON Report Structure

```json
{
  "test_name": "marketplace_search_with_otel_proof",
  "result": "passed",
  "duration_seconds": 2.143,
  "validation": {
    "lifecycle_events": { "status": "passed", "expected": 4, "found": 4 },
    "span_graph": { "status": "passed" },
    "span_counts": { "status": "passed", "total_spans": 7 },
    "temporal_ordering": { "status": "passed", "violations": 0 },
    "window_containment": { "status": "passed", "violations": 0 },
    "status_validation": { "status": "passed", "final_status": "ok" },
    "hermeticity": { "status": "passed", "violations": 0 }
  }
}
```

### 7-Layer Validation Explained

Each test validates 7 layers to prevent false positives:

1. **Lifecycle Events** ✅ - Did the expected events occur?
2. **Span Graph** ✅ - Are parent-child relationships correct?
3. **Span Counts** ✅ - Did we get the expected number of operations?
4. **Temporal Ordering** ✅ - Did events happen in the right order?
5. **Window Containment** ✅ - Are child spans within parent time bounds?
6. **Status Validation** ✅ - Did operations succeed/fail as expected?
7. **Hermeticity** ✅ - Was execution isolated and hermetic?

---

## 🎯 Test Categories

### Marketplace Tests (2 files, 16 scenarios)

**`marketplace/search.clnrm.toml`** (285 lines)
- Basic search functionality
- Search by tag
- Case-insensitive search
- Empty results handling
- Special characters in queries
- Performance tests (large registry, rapid searches)
- Concurrent search operations

**`marketplace/install.clnrm.toml`** (400 lines)
- Package resolution
- Version management
- Metadata validation
- Update checking
- Category listing
- Error handling (non-existent packages, invalid versions)
- Empty registry handling
- Malformed index recovery

### Lifecycle Tests (2 files, 18 scenarios)

**`lifecycle/init.clnrm.toml`** (327 lines)
- Basic initialization
- Multi-step initialization
- Template generation during init
- State persistence
- Failure handling with rollback
- Rust project initialization
- Marketplace integration during init
- Concurrent initialization isolation

**`lifecycle/deploy.clnrm.toml`** (550 lines)
- Staging deployment with validation
- Production deployment with comprehensive checks
- Validation failure prevents deploy
- Blue-green deployment
- Canary deployment
- Multi-region deployment
- Artifact publishing and deployment
- Database migrations
- Zero-downtime deployment
- Smoke tests and verification

**Total**: 4 files, 34 test scenarios, 1,562 lines

---

## 🔍 Analyzing Test Results

### View Trace Data

```bash
# Pretty-print trace JSON
jq . /Users/sac/ggen/target/clnrm-reports/marketplace_search_traces.json

# Extract specific spans
jq '.traces[].spans[] | select(.name == "ggen.market.search")' \
  /Users/sac/ggen/target/clnrm-reports/marketplace_search_traces.json

# Show span hierarchy
jq '.traces[].spans[] | "\(.name) -> \(.parentSpanId // "root")"' \
  /Users/sac/ggen/target/clnrm-reports/marketplace_search_traces.json
```

### Verify SHA-256 Digests

```bash
# Check trace digest
cat /Users/sac/ggen/target/clnrm-reports/marketplace_search.sha256

# Verify digest
sha256sum /Users/sac/ggen/target/clnrm-reports/marketplace_search_traces.json
```

### View JUnit XML

```bash
# View test results in JUnit format (for CI/CD)
cat /Users/sac/ggen/target/clnrm-reports/marketplace_search.xml
```

---

## 🐛 Troubleshooting

### Problem: OTEL collector not responding

**Symptoms**: Tests fail with "connection refused" or timeout

**Solution**:
```bash
# Check if collector is running
docker ps | grep otel-collector

# Check collector logs
docker logs otel-collector

# Restart collector
docker restart otel-collector

# Or start new collector
docker rm -f otel-collector
docker run -d --name otel-collector -p 4318:4318 -p 4317:4317 \
  otel/opentelemetry-collector:latest
```

### Problem: CLNRM binary not found

**Symptoms**: `command not found: cleanroom`

**Solution**:
```bash
# Build clnrm
cd ~/dev/clnrm
cargo build --release --bin cleanroom

# Verify build
ls -lh ~/dev/clnrm/target/release/cleanroom
```

### Problem: ggen binary not found

**Symptoms**: Test execution fails with "ggen not found"

**Solution**:
```bash
# Build ggen
cd /Users/sac/ggen
cargo build --release

# Verify build
./target/release/ggen --version
```

### Problem: Test validation fails

**Symptoms**: Tests fail with "validation layer failed"

**Solution**:
```bash
# Check specific validation layer in report
jq '.validation' /tmp/report.json

# View detailed error logs
cat /Users/sac/ggen/target/clnrm-reports/marketplace_search_stderr.log

# Review trace data for missing spans
jq '.traces[].spans[].name' /tmp/traces.json
```

---

## 📚 Next Steps

1. **Read the full documentation**: [CLNRM_MIGRATION_RESULTS.md](./CLNRM_MIGRATION_RESULTS.md)
2. **Explore test configurations**: `tests/clnrm/**/*.clnrm.toml`
3. **Integrate with CI/CD**: See [GitHub Actions example](./CLNRM_MIGRATION_RESULTS.md#cicd-integration)
4. **Write your own tests**: Use existing tests as templates

---

## 💡 Tips

- **Start simple**: Run individual tests first before the full suite
- **Check traces**: Always review trace JSON to understand execution
- **Use filters**: Filter JQ queries to focus on specific spans
- **Monitor performance**: Track test execution times in reports
- **Verify determinism**: Run tests multiple times to ensure reproducibility

---

## 🎉 Success Criteria

You'll know CLNRM is working correctly when:

- ✅ All 4 test files validate successfully
- ✅ All 34 test scenarios pass
- ✅ 7-layer validation shows 0 violations
- ✅ JSON, JUnit, and SHA-256 reports are generated
- ✅ Trace data shows expected span hierarchy
- ✅ Tests are reproducible (same results on re-run)

---

**Need help?** See [CLNRM README](../../tests/clnrm/README.md) for detailed documentation.
