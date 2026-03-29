# ~/ggen Template Generation Pattern for YAWL v6

## Overview

Every runtime bug fix in YAWL v6 should generate a ~/ggen template documenting:
1. **The Problem** (with runtime evidence)
2. **The Fix** (with code snippets)
3. **Validation** (with test commands)
4. **Files** (with exact paths)

This meta-pattern ensures that runtime fixes are:
- **Reproducible** — Evidence captured from actual engine logs
- **Testable** — JUnit tests prove the fix works
- **Documented** — Pattern docs explain the "why"
- **Validated** — CI checks that patterns aren't violated

## Template Structure

```
~/ggen/templates/yawlv6/
├── README.md                                    # This file (meta-pattern)
├── hibernate-marked-rollback.md                 # Hibernate 7 ROLLBACK_ONLY recovery
├── wcp-predicate-truthy.md                      # XOR split predicate truthiness
├── spec-cascade-delete-identifiers.md           # YIdentifier cascade delete handling
├── docker-db-url-resolution.md                  # host.docker.internal pattern
└── <pattern-name>.md                            # Future pattern docs
```

Each pattern doc follows this structure:

```markdown
# <Pattern Name>

## Runtime Evidence
- **File:** `docs/validation/evidence/<pattern-name>.log`
- **Test Case:** `test/org/yawlfoundation/yawl/validation/<PatternName>Test.java`
- **Engine Version:** YAWL 6.0.0 GA
- **Hibernate:** 7.0.0.Final (or relevant version)

## The Problem

[Code snippet showing the broken behavior]

## The Fix

[Code snippet showing the correct implementation]

## Why This Works

[Explanation of the root cause and why the fix solves it]

## Validation

Run: `mvn test -Dtest=<PatternName>Test`
Expected: 100% pass rate, FATAL flag never set

## Files
- **Implementation:** `src/org/yawlfoundation/yawl/...`
- **Test:** `test/org/yawlfoundation/yawl/validation/<PatternName>Test.java`
- **Evidence:** `docs/validation/evidence/<pattern-name>.log`
```

## Generating a New Template

When you fix a runtime bug, follow this 6-step process:

### Step 1: Extract Runtime Evidence

Capture the bug from actual engine logs:

```bash
# Start engine with DEBUG logging
make docker-up

# Run a case that triggers the bug
curl -s -X POST "http://localhost:8080/ib/ib" \
  -d "action=launchCase&specidentifier=<SpecName>&sessionHandle=$SESSION" \
  > /dev/null

# Extract logs from container
docker logs yawl-engine > /tmp/yawl-engine.log 2>&1

# Filter for relevant error patterns
grep -A 20 "ERROR" /tmp/yawl-engine.log > docs/validation/evidence/<bug-name>.log
```

**What to capture:**
- Exception stack traces
- Hibernate SQL logs (if persistence bug)
- XPath evaluation errors (if predicate bug)
- Timestamps for timeline analysis

### Step 2: Write the Failing Test (RED)

Create a JUnit test that reproduces the bug:

```bash
# Create test file
cat > test/org/yawlfoundation/yawl/validation/<BugName>Test.java << 'EOF'
package org.yawlfoundation.yawl.validation;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Validates <pattern description>.
 *
 * Runtime evidence: docs/validation/evidence/<bug-name>.log
 * Pattern doc: ~/ggen/templates/yawlv6/<pattern-name>.md
 */
public class <BugName>Test {

    @Test
    public void test<BugName>() throws Exception {
        // Arrange: Set up the scenario
        // Act: Trigger the bug
        // Assert: Verify the fix
    }
}
EOF

# Run test — should FAIL (RED phase)
./mvnw test -Dtest=<BugName>Test
```

**Test requirements:**
- Test name describes the claim (e.g., `testMarkedRollbackRecovery`)
- Assertion directly checks the behavior (not a proxy)
- Test FAILS before fix, PASSES after fix
- Test is deterministic (run 10x, same result)

### Step 3: Implement the Fix

Edit the source code to fix the bug:

```bash
# Edit: src/org/yawlfoundation/yawl/...
vim src/org/yawlfoundation/yawl/engine/<ClassName>.java
```

**Fix requirements:**
- Minimal change to make test pass (GREEN phase)
- No speculative features (YAGNI)
- Preserves existing behavior (no regressions)

### Step 4: Verify Test Passes (GREEN)

```bash
# Run test — should PASS (GREEN phase)
./mvnw test -Dtest=<BugName>Test

# Run full test suite — no regressions
./mvnw test
```

**Expected output:**
```
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
```

### Step 5: Write the Pattern Doc

Create the pattern documentation:

```bash
cat > ~/ggen/templates/yawlv6/<pattern-name>.md << 'EOF'
# <Pattern Name>

## Runtime Evidence
- **File:** `docs/validation/evidence/<bug-name>.log`
- **Test Case:** `test/org/yawlfoundation/yawl/validation/<BugName>Test.java`
- **Engine Version:** YAWL 6.0.0 GA
- **Hibernate:** 7.0.0.Final

## The Problem

[Code snippet showing the broken behavior]

## The Fix

[Code snippet showing the correct implementation]

## Why This Works

[Explanation of the root cause]

## Validation

Run: `mvn test -Dtest=<BugName>Test`
Expected: 100% pass rate

## Files
- **Implementation:** `src/org/yawlfoundation/yawl/...`
- **Test:** `test/org/yawlfoundation/yawl/validation/<BugName>Test.java`
- **Evidence:** `docs/validation/evidence/<bug-name>.log`
EOF
```

### Step 6: Add to CI

Create validation script and add to CI:

```bash
# Create validation script
cat > scripts/runtime-validation/validate-<pattern>.sh << 'EOF'
#!/usr/bin/env bash
# Validates <pattern description>

set -euo pipefail

# Exit 1 if pattern is violated
# Exit 0 if pattern is satisfied
EOF

chmod +x scripts/runtime-validation/validate-<pattern>.sh

# Add to Makefile validate-runtime target
echo "	@bash scripts/runtime-validation/validate-<pattern>.sh" >> Makefile

# Commit all artifacts
git add ~/ggen/templates/yawlv6/<pattern-name>.md
git add test/org/yawlfoundation/yawl/validation/<BugName>Test.java
git add scripts/runtime-validation/validate-<pattern>.sh
git commit -m "docs(runtime): add <pattern-name> pattern

Pattern doc: ~/ggen/templates/yawlv6/<pattern-name>.md
Test: test/org/yawlfoundation/yawl/validation/<BugName>Test.java
Validation: scripts/runtime-validation/validate-<pattern>.sh
"
```

## Example Templates

### Hibernate MARKED_ROLLBACK Recovery

**Pattern:** `hibernate-marked-rollback.md`

**Problem:** When `session.merge()` throws `ObjectDeletedException` (cascade-deleted YIdentifier children), Hibernate marks the transaction as ROLLBACK_ONLY. Subsequent `INSERT` operations fail with "MARKED_ROLLBACK", putting the engine in FATAL state.

**Fix:** Detect "MARKED_ROLLBACK" in exception message, rollback the contaminated transaction, start fresh, and retry the INSERT once.

**Evidence:** `docs/validation/evidence/rollback-exceptions.log`

**Test:** `HibernateMarkedRollbackTest.testMergeAfterCascadeDeleteCausesRollbackOnly`

### WCP Predicate Truthiness

**Pattern:** `wcp-predicate-truthy.md`

**Problem:** Variable-based predicates like `/Net/counter >= 3` evaluate against empty task output data (`<data/>`), causing false → infinite loop.

**Fix:** Use XPath `true()` function for XOR split ordering=0 predicates, which always evaluates true regardless of data.

**Evidence:** `docs/validation/evidence/wcp-predicate-timeouts.log`

**Test:** `WCPPredicateTruthinessTest.testVariableBasedPredicateCausesInfiniteLoop`

### YIdentifier Cascade Delete

**Pattern:** `spec-cascade-delete-identifiers.md`

**Problem:** Database FK constraints cascade-delete YIdentifier children when parent is deleted, but Hibernate session doesn't know → `ObjectDeletedException` on merge.

**Fix:** Refresh parent from database before merge to sync with DB state.

**Evidence:** `docs/validation/evidence/cascade-delete-exceptions.log`

**Test:** `YIdentifierCascadeDeleteTest.testCascadeDeleteCausesObjectDeletedException`

## Validation

All templates must pass runtime validation:

```bash
# Validate all patterns
make validate-runtime

# Run Van der Aalst simulation with validation
make test-van-der-aalst-validate
```

Expected output:
```
Pass: 52, Timeouts: 0, Warns: 0
✅ WCP predicate validation: PASS
✅ Hibernate MARKED_ROLLBACK recovery: PASS
```

## CI Integration

GitHub Actions workflow (`.github/workflows/runtime-validation.yml`) runs `make validate-runtime` on every push/PR, failing the build if any pattern is violated.

## Related Documentation

- **Plan:** `docs/superpowers/plans/2026-03-28-doc-validation-runtime-proof.md`
- **Validation Scripts:** `scripts/runtime-validation/`
- **Validation Evidence:** `docs/validation/evidence/`
- **Test Suite:** `test/org/yawlfoundation/yawl/validation/`

## Template Lifecycle

1. **Discover** — Bug found in production or testing
2. **Extract** — Capture runtime evidence from logs
3. **Test** — Write failing test (RED)
4. **Fix** — Implement minimal fix (GREEN)
5. **Document** — Write pattern doc with evidence
6. **Validate** — Add to CI, prevent regression

## Template Quality Checklist

Before committing a new template, verify:

- [ ] **Runtime evidence captured** — Logs saved to `docs/validation/evidence/`
- [ ] **Test FAILS before fix** — Red phase confirmed
- [ ] **Test PASSES after fix** — Green phase confirmed
- [ ] **Pattern doc complete** — All sections filled (Problem, Fix, Why, Validation, Files)
- [ ] **Validation script exists** — Exits 1 if violated, 0 if satisfied
- [ ] **CI integration** — Added to `make validate-runtime`
- [ ] **No regressions** — Full test suite passes
- [ ] **Git commit clean** — All artifacts committed together

---

**Meta-Pattern Version:** 1.0.0
**Last Updated:** 2026-03-29
**Maintainer:** YAWL v6 Development Team
