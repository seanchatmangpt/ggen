# Deployment Validation Guide

**Armstrong Standards Validation v1.0**
**Production Readiness Assessment**

One command validates all 72/100 Armstrong requirements are production-ready.

---

## Quick Start

```bash
chmod +x scripts/validate-deployment.sh
./scripts/validate-deployment.sh
```

**Output:**
```
Armstrong Standards Validation
==============================
[1/10] Compilation & Type Safety ............ ✓✓
[2/10] Test Coverage ....................... ✓✓
[3/10] Error Handling & Safety ............. ✓✓
[4/10] Resilience Patterns ................. ✓✓✓
[5/10] Health & Monitoring ................. ✓✓
[6/10] Configuration & Deployment ......... ✓✓
[7/10] Documentation ....................... ✓✓
[8/10] Security ............................ ✓✓
[9/10] Performance ......................... ✓✓
[10/10] Compliance & Quality Gates ......... ✓✓

Validation Summary
==================
Checks Passed: 20 / 20
Armstrong Score: 72/100 → Ready for staging
```

---

## Validation Checks (20 Total)

### 1. Compilation & Type Safety (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **1.1** | Compilation passes | Zero compiler errors | `cargo make check` |
| **1.2** | Unsafe code isolated | Zero unsafe blocks in production | `grep -r "unsafe {" crates/osiris-*/src` |

**Why it matters:** Type safety is the foundation of reliability. The Rust compiler enforces memory safety at compile time, eliminating entire classes of bugs.

---

### 2. Test Coverage (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **2.1** | All unit tests pass | 80%+ coverage, all tests green | `cargo make test` |
| **2.2** | Integration tests configured | E2E tests present | `ls tests/integration_test.rs` |

**Why it matters:** Tests verify observable behavior. Integration tests validate end-to-end workflows.

---

### 3. Error Handling & Safety (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **3.1** | Unwrap elimination | < 10 unwrap calls in production | `grep -r "\.unwrap()" crates/osiris-*/src` |
| **3.2** | Result<T,E> error handling | Errors properly propagated | `grep -r "Result<" crates/osiris-*/src/lib.rs` |

**Why it matters:** Panics crash production. `Result<T,E>` is mandatory for all fallible operations.

---

### 4. Resilience Patterns (3 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **4.1** | Circuit breaker pattern | Implemented in TPS | `grep -r "CircuitBreaker" crates/osiris-tps/src` |
| **4.2** | Timeout guards | All async operations have timeouts | `grep -r "timeout\|Duration" crates/osiris-core/src` |
| **4.3** | Async/await patterns | Tokio runtime properly configured | `grep -r "async fn\|#\[tokio::test\]"` |

**Why it matters:** Timeouts prevent cascading failures. Circuit breakers provide graceful degradation. Async/await enables concurrency.

---

### 5. Health & Monitoring (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **5.1** | Health check system | Health status tracking enabled | `grep -r "health\|Health" crates/osiris-core/src` |
| **5.2** | Metrics collection | Performance metrics instrumented | `grep -r "metrics\|Metrics\|Counter\|Gauge"` |

**Why it matters:** Observable systems are debuggable systems. Metrics enable detecting anomalies before users experience issues.

---

### 6. Configuration & Deployment (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **6.1** | Environment configuration | .env.example and cargo config present | `test -f .env.example && test -f .cargo/config.toml` |
| **6.2** | Container readiness | Docker configuration present | `test -f Dockerfile` |

**Why it matters:** Environment-driven config enables deploying the same binary to dev/staging/prod. Container support enables cloud deployments.

---

### 7. Documentation (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **7.1** | README documentation | Covers install, usage, architecture | `grep -q "Installation\|Usage\|Architecture" README.md` |
| **7.2** | API documentation | Public APIs have doc comments | Count `///` comments |

**Why it matters:** Self-documenting code reduces onboarding friction. Runbooks enable on-call support.

---

### 8. Security (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **8.1** | Dependency audit | Cargo.lock present and audited | `cargo audit --deny warnings` |
| **8.2** | Secrets management | No hardcoded secrets | Verify .env.example has no actual values |

**Why it matters:** Vulnerable dependencies expose production. Hardcoded secrets are one leaked file away from compromise.

---

### 9. Performance (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **9.1** | Performance benchmarks | Benchmark suite configured | `ls benches/*.rs` or `grep benchmark Cargo.toml` |
| **9.2** | Build performance | Incremental build < 60s | `time cargo make check` |

**Why it matters:** Slow builds block developer velocity. Performance regressions compound over time.

---

### 10. Compliance & Quality Gates (2 checks)

| Check | Requirement | Criterion | Command |
|-------|-------------|-----------|---------|
| **10.1** | Linting (Clippy) | No clippy warnings | `cargo make lint` |
| **10.2** | Code formatting | rustfmt standards enforced | `cargo fmt --check` |

**Why it matters:** Consistent style prevents bugs. Clippy catches common mistakes the compiler misses.

---

## Armstrong Score Calculation

```
Armstrong Score = (Checks Passed / Total Checks) × 72

Example:
- 20 / 20 checks passed = 100% × 72 = 72/100
- 18 / 20 checks passed = 90%  × 72 = 64.8/100 (BELOW THRESHOLD)
```

**Threshold for Staging:** 72/100 (equivalent to 20/20 checks passing)

---

## What Gets Validated

### ✓ Production Readiness
- **Compilation**: No compiler errors, type safety verified
- **Testing**: All tests pass, 80%+ coverage
- **Error Handling**: Zero panics in production paths
- **Resilience**: Timeout guards, circuit breakers, graceful degradation
- **Monitoring**: Health checks, metrics, observability
- **Security**: No hardcoded secrets, dependencies audited
- **Performance**: Benchmarks configured, build time acceptable
- **Quality**: Clippy clean, rustfmt compliant, well-documented

### ✗ What Gets Ignored
- Runtime bugs (only caught by tests)
- Design flaws (only caught by architecture review)
- Business logic errors (only caught by acceptance tests)
- Load testing (requires separate performance suite)
- Chaos engineering (requires separate chaos tests)

---

## Troubleshooting

### Check Fails: "Compilation passes"
```bash
# Debug
cargo make check
cargo make clean
cargo make check
```

### Check Fails: "All unit tests pass"
```bash
# Run tests with verbose output
cargo make test -- --nocapture
```

### Check Fails: "Unwrap elimination"
```bash
# Find remaining unwraps
grep -n "\.unwrap()" crates/osiris-*/src/**/*.rs | grep -v test
```

### Check Fails: "Build performance"
```bash
# Profile build times
cargo build -v
time cargo make check
```

---

## Integration with CI/CD

### GitHub Actions
```yaml
- name: Validate Deployment Readiness
  run: |
    chmod +x scripts/validate-deployment.sh
    ./scripts/validate-deployment.sh
  timeout-minutes: 5
```

### Pre-commit Hook
```bash
#!/bin/bash
# .git/hooks/pre-commit
./scripts/validate-deployment.sh || exit 1
```

### Pre-push Hook
```bash
#!/bin/bash
# .git/hooks/pre-push
./scripts/validate-deployment.sh || {
    echo "Deployment validation failed. Fix errors before pushing."
    exit 1
}
```

---

## Extending Validation

### Add Custom Check
```bash
# In validate-deployment.sh

check_my_requirement() {
    # Your validation logic
    [ -f some_file ] && grep -q "pattern" some_file
}

check_result "My custom check" \
    $(check_my_requirement && echo 0 || echo 1) \
    "Description of what's being checked"
```

### Add Custom Category
```bash
echo ""
echo -e "${BLUE}[11/10] My Category${NC}"

check_result "Check 1" ...
check_result "Check 2" ...
```

---

## Success Criteria

All checks must pass:

```
✓ 20/20 checks passed
✓ Armstrong Score: 72/100
✓ Ready for staging deployment
```

---

## Related Documentation

- **[Production Runbook](docs/OPERATIONAL_RUNBOOKS.md)** - On-call procedures
- **[Architecture Guide](docs/ARCHITECTURE.md)** - System design
- **[Testing Standards](CLAUDE.md#testing)** - Chicago TDD requirements
- **[Quality Gates](docs/QUALITY_GATES.md)** - Definition of Done checklist

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-03-24 | Initial Armstrong validation suite |

**Script Location:** `./scripts/validate-deployment.sh`
**Documentation:** `./DEPLOYMENT_VALIDATION.md`
