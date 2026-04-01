---
auto_load: false
category: rust
priority: critical
version: 6.0.1
---

# Chicago TDD

## What You Do

- Use real collaborators: actual databases, filesystems, HTTP clients, LLM APIs
- Assert on observable state: return values, side effects, persisted data
- Follow the AAA pattern: Arrange, Act, Assert
- Prove your claims with evidence. Evidence hierarchy: PROVEN > OBSERVED > INFERRED > UNVERIFIED
- Provide three-layer proof for every test: Execution (you ran it) + Semantics (assertions are meaningful) + Evaluation (you checked what you asserted)

## What You Do Not Do

- You do not import mockall or automock
- You do not create test doubles (InMemoryStorage, FakeDatabase, MockHttpClient)
- You do not verify behavior (call counts, invocation order, argument matching)
- You do not use dependency injection as a vehicle for mock substitution
- You do not claim a test passes without running it

## Your Testing Failure Modes

| Failure Mode | What It Looks Like | Why It Kills You |
|---|---|---|
| NARRATION | Test reads like documentation but asserts nothing of consequence | You feel confident. The code is untested. |
| SELF-CERT | You write the test, run it, see green, and stop investigating | Green does not mean correct. It means you stopped looking. |
| SHALLOW GREEN | `assert!(result.is_ok())` instead of checking the actual invariant | You proved success happened. You did not prove the right thing happened. |
| MOCK COMFORT | You reach for mockall because the output is predictable | Predictable output means predictable bugs stay hidden. |
| TEST MURDER | You soften a failing assertion so the test passes | You just deleted the only thing protecting that invariant. |
| LAZY JUDGE | You assert on shape (keys exist, status is 200) but not on value | Structural correctness is not semantic correctness. |

## Test Types

| Type | Timeout | Framework |
|---|---|---|
| Unit | <150s | Standard Rust |
| Integration | <30s | Workspace tests |
| BDD | - | Cucumber |
| Property | - | proptest |
| Snapshot | - | insta |
| Security | - | Custom |
| Determinism | - | RNG_SEED=42 |
| Performance | - | Criterion |

## Definition of Done

1. All public APIs have tests that assert on actual values, not just absence of error
2. Error paths and edge cases are covered. Target 80%+ coverage.
3. Every test you write follows the AAA pattern with meaningful assertions
4. You ran the tests. You saw the output. You confirmed the assertions prove something real.
5. No meaningless tests: each test must have a reason to fail that corresponds to a real defect
6. For LLM and external service features, OTEL spans exist and contain required attributes

## Commands

```bash
cargo make test-unit     # Fast (<16s)
cargo make test          # Full (<30s)
cargo make slo-check     # Performance validation
```

## 80/20 Focus Areas

- Error paths and resource cleanup
- Concurrency edge cases
- Real dependency integration
- Deterministic behavior verification
