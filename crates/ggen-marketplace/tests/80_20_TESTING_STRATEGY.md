# 80/20 Testing Strategy - ggen-marketplace

**Philosophy**: Focus on the 20% of tests that provide 80% of confidence and coverage.

## 🎯 Critical 20% - Highest Value Tests

### 1. **Core User Journeys** (40% of value)
- Search → Install → Verify flow
- Publish → Search → Download flow
- Offline-first: Local registry operations
- Error recovery scenarios

### 2. **Data Integrity** (30% of value)
- Content-addressable storage correctness
- Version resolution accuracy
- Package metadata consistency
- Cryptographic hash verification

### 3. **Error Paths** (20% of value)
- Network failures and retries
- Corrupted data handling
- Missing dependencies
- Authorization failures

### 4. **Invariants** (10% of value)
- Property-based tests for core logic
- Idempotency guarantees
- Thread-safety verification

## 📋 Test Categories

### Priority 1: Integration Tests (60% focus)
Test real component interactions, not mocks.

### Priority 2: Property-Based Tests (20% focus)
Verify mathematical invariants and business rules.

### Priority 3: Unit Tests (15% focus)
Only for complex algorithms or pure functions.

### Priority 4: Performance Tests (5% focus)
Basic benchmarks for regression detection.

## 🚫 What We DON'T Test (The 80%)

- Internal implementation details
- Trivial getters/setters
- Third-party library behavior
- Generated code
- Simple delegating functions
- Obvious error messages

## ✅ Best Practices

1. **No `.unwrap()` or `.expect()` in tests** - Use `?` and proper error handling
2. **Descriptive test names** - `test_search_returns_packages_matching_query`
3. **Arrange-Act-Assert** pattern - Clear test structure
4. **Minimal setup** - Use builders and helpers
5. **Fast tests** - <100ms per integration test
6. **Deterministic** - No flaky tests, no time dependencies
7. **Isolated** - Tests don't depend on each other
8. **Real dependencies** - Prefer testcontainers over mocks

## 📊 Coverage Targets

| Category | Target | Rationale |
|----------|--------|-----------|
| Critical paths | 100% | Must work in production |
| Public APIs | 90% | User-facing contracts |
| Error handling | 80% | Common failure modes |
| Internal utils | 50% | Lower risk |
| Generated code | 0% | Not our code |

---

**Total test count target**: ~30-50 high-value tests (not 1000s of low-value tests)
