# Testing Gap Quick Reference
**🚨 For Immediate Action - 1-Page Summary**

---

## 🎯 80/20 Rule: Focus Here First

### Top 4 Gaps = 80% of Risk

```
┌─────────────────────────────────────────────────────────────┐
│ 1. AI Governance (35% of total risk)                       │
│    📁 crates/ggen-ai/src/governance/                        │
│    📊 0% coverage | 100KB code | 0 tests                    │
│    ⏰ Fix: 3-4 days | Impact: Prevent AI safety incidents  │
├─────────────────────────────────────────────────────────────┤
│ 2. Marketplace Domain (28% of total risk)                  │
│    📁 crates/ggen-domain/src/marketplace/                   │
│    📊 18% coverage | 152KB code | 3 tests                   │
│    ⏰ Fix: 2-3 days | Impact: Prevent install failures     │
├─────────────────────────────────────────────────────────────┤
│ 3. Error Handling (22% of total risk)                      │
│    🔍 654 unwrap() + 79 expect() in production code        │
│    📊 ~60% error paths untested                             │
│    ⏰ Fix: 2-3 weeks | Impact: Graceful degradation        │
├─────────────────────────────────────────────────────────────┤
│ 4. Async/Stream Code (15% of total risk)                   │
│    🔍 630 async functions, inconsistent coverage            │
│    📊 Race conditions, edge cases untested                  │
│    ⏰ Fix: 1-2 weeks | Impact: Reliability under load      │
└─────────────────────────────────────────────────────────────┘
```

---

## 📊 Key Metrics At-a-Glance

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **Overall Coverage** | 49% | 70% | 🔴 -21% gap |
| **Critical Modules** | 12% | 80% | 🔴 -68% gap |
| **Source Files** | 281 | - | - |
| **Test Files** | 138 | 197+ | 🔴 -59 gap |
| **unwrap() Count** | 654 | <100 | 🔴 -554 excess |
| **Gap Accumulation** | +35/mo | <5/mo | 🔴 7x target |

**Trend:** 📈 Gaps **increasing** month-over-month

---

## 🚀 This Week's Action Items

### Monday-Tuesday: Critical Safety (8-12 hours)
```bash
# 1. AI Governance Safety Tests
cd crates/ggen-ai/src/governance/
✅ Create tests/safety_tests.rs
✅ Test: policy validation, content filtering, rate limiting
✅ Coverage goal: 50% → 60%

# 2. Marketplace Install Tests
cd crates/ggen-domain/src/marketplace/
✅ Create tests/install_tests.rs
✅ Test: dependency resolution, error recovery, validation
✅ Coverage goal: 18% → 40%
```

### Wednesday-Thursday: Error Handling (8-12 hours)
```bash
# 3. Refactor Top 50 unwrap() Calls
find crates/*/src -name "*.rs" | xargs grep -n "unwrap()"
✅ Replace unwrap() → proper error handling + tests
✅ Priority: marketplace/, governance/, lifecycle/

# 4. Add Error Path Tests
✅ Test all Err() return paths
✅ Test panic scenarios
✅ Test timeout/network failures
```

### Friday: Infrastructure (4-6 hours)
```bash
# 5. Pre-commit Hook
✅ Block unwrap() in src/ (not tests/)
✅ Require test file for new source file
✅ Enforce coverage doesn't decrease

# 6. CI Gate
✅ Fail build if coverage < 40%
✅ Fail if critical modules < 60%
```

---

## 📁 Module Priority Matrix

### Fix Order (Next 30 Days)

```
High Impact, Quick Win (DO FIRST)
├── ggen-domain/marketplace/update.rs     [1.5 days, HIGH impact]
├── ggen-domain/marketplace/install.rs    [2 days, HIGH impact]
└── ggen-ai/governance/safety.rs          [1 day, CRITICAL impact]

High Impact, More Effort (DO SECOND)
├── ggen-ai/governance/policy.rs          [2 days, HIGH impact]
├── ggen-ai/governance/workflow.rs        [2 days, HIGH impact]
└── ggen-ai/swarm/coordinator.rs          [2 days, MEDIUM impact]

Refactoring (ONGOING)
├── Refactor 654 unwrap() → 100           [3 weeks, incremental]
├── Add async edge case tests             [2 weeks, incremental]
└── Property-based testing                [2 weeks, incremental]
```

---

## 🔍 Detection Patterns (Automated Alerts)

### Red Flags (Block Immediately)
```rust
// ❌ BLOCK: unwrap() in production code
let value = some_result.unwrap(); // NO! Use ? or proper error handling

// ❌ BLOCK: New file without test file
src/new_feature.rs → REQUIRES tests/new_feature_tests.rs

// ❌ BLOCK: Public API without integration test
pub async fn execute_critical_operation(...) // Needs #[tokio::test]
```

### Yellow Flags (Require Explanation)
```rust
// ⚠️ REVIEW: Complex logic (>50 LOC) without unit tests
pub fn complex_algorithm(...) { // 150 lines, 0 tests
    // ... lots of logic
}

// ⚠️ REVIEW: Error path without test
if error_condition {
    return Err("Failed".into()); // Test this path!
}

// ⚠️ REVIEW: Async function without edge case tests
pub async fn stream_data(...) { // Race conditions tested?
}
```

---

## 📈 Success Indicators (Weekly Check)

### Green Signals (Keep Going!)
- ✅ Test file committed within 1 day of feature file
- ✅ Test LOC ≥ 50% of source LOC
- ✅ All public functions have integration tests
- ✅ Error paths have explicit test cases
- ✅ Coverage increasing week-over-week

### Red Signals (Course Correct!)
- 🔴 Feature commit without test commit
- 🔴 PR > 1000 LOC without proportional tests
- 🔴 File modified 5+ times without test update
- 🔴 Coverage decreasing week-over-week
- 🔴 New unwrap() added to production code

---

## 🎯 Coverage Targets by Module

| Module | Current | Week 1 | Week 2 | Week 4 | Q1 2025 |
|--------|---------|--------|--------|--------|---------|
| `ggen-ai/governance/` | 0% | 30% | 50% | 60% | **80%** |
| `ggen-domain/marketplace/` | 18% | 30% | 40% | 50% | **70%** |
| `ggen-marketplace/backend/` | 21% | 30% | 40% | 50% | **70%** |
| `ggen-core/lifecycle/` | 25% | 35% | 45% | 55% | **70%** |
| `ggen-cli/` | 65% | 68% | 70% | 72% | **80%** |
| **Overall** | **49%** | **52%** | **55%** | **60%** | **70%** |

---

## 💡 Quick Wins (< 4 Hours Each)

### Highest ROI Test Additions

1. **`marketplace/update.rs`** (0 tests → 10 tests)
   - Test update logic, version conflicts, rollbacks
   - **Impact:** Prevent update failures in production

2. **`governance/safety.rs`** (0 tests → 8 tests)
   - Test content filtering, rate limits, policy validation
   - **Impact:** Prevent AI safety incidents

3. **`lifecycle/validation.rs`** (partial → full)
   - Test state transitions, validation rules
   - **Impact:** Prevent invalid state bugs

4. **Error Path Coverage** (0% → 50% for top files)
   - Test all Err() paths in high-risk modules
   - **Impact:** Graceful error handling

5. **Async Edge Cases** (0% → 30%)
   - Test timeouts, cancellations, race conditions
   - **Impact:** Reliability under load

---

## 🛠️ Tools & Commands

### Measure Current State
```bash
# Count tests vs source
find crates -name "*.rs" -path "*/src/*" | wc -l  # Source files
find crates -name "*.rs" -path "*/tests/*" | wc -l  # Test files

# Find unwrap() usage
grep -r "unwrap()" crates/*/src --include="*.rs" | wc -l

# Find untested modules
comm -23 \
  <(find crates/*/src -name "*.rs" | sort) \
  <(find crates/*/tests -name "*_test.rs" | sed 's/_test//' | sort)

# Run tests with coverage (requires cargo-tarpaulin)
cargo tarpaulin --out Html --output-dir coverage/
```

### Enforce Standards
```bash
# Pre-commit hook (add to .git/hooks/pre-commit)
#!/bin/bash
git diff --cached --name-only | grep "\.rs$" | while read file; do
  if [[ $file == crates/*/src/* ]]; then
    if grep -q "unwrap()" "$file"; then
      echo "❌ ERROR: unwrap() found in $file"
      exit 1
    fi
  fi
done

# CI gate (add to .github/workflows/ci.yml)
- name: Check coverage
  run: |
    cargo tarpaulin --out Xml
    if [ $(grep line-rate coverage.xml | cut -d'"' -f2) < 0.40 ]; then
      echo "❌ Coverage below 40%"
      exit 1
    fi
```

---

## 📞 Escalation Path

### When to Escalate (to Coordinator Agent)

| Issue | Severity | Action |
|-------|----------|--------|
| Coverage drops below 40% | 🔴 CRITICAL | Immediate escalation |
| New unwrap() in critical module | 🔴 HIGH | Block PR, escalate |
| Gap accumulation > 10/week | 🟡 MEDIUM | Weekly review |
| Test execution time > 10min | 🟡 MEDIUM | Optimize, report |

---

## 🎓 Learning Resources

### Best Practices (Internal)
- ✅ `tests/chicago_tdd/` - Example of comprehensive testing
- ✅ `tests/bdd/` - BDD integration test patterns
- ✅ `cli/tests/conventions/` - Unit test structure

### External Resources
- [Rust Testing Guide](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Chicago TDD vs London TDD](https://softwareengineering.stackexchange.com/q/123627)
- [Property-Based Testing](https://github.com/BurntSushi/quickcheck)

---

**Last Updated:** 2025-11-14
**Next Review:** 2025-11-21 (1 week)
**Owner:** Analyst Agent
**Status:** 🔴 CRITICAL - Immediate action required
