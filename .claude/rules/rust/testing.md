---
auto_load: false
category: rust
priority: critical
version: 6.1.0
paths:
  - "crates/**/*.rs"
  - "src/**/*.rs"
  - "tests/**/*.rs"
---

# 🧪 Chicago TDD (MANDATORY)

## Principles
- State-based verification
- Real collaborators (not mocks)
- Behavior verification
- AAA pattern: Arrange/Act/Assert

## Test Types
| Type | Timeout | Framework |
|------|---------|-----------|
| Unit | <150s | Standard Rust |
| Integration | <30s | Workspace tests |
| BDD | - | Cucumber |
| Property | - | proptest |
| Snapshot | - | insta |
| Security | - | Custom |
| Determinism | - | RNG_SEED=42 |
| Performance | - | Criterion |

## Requirements (Definition of Done)
- ✅ All public APIs tested
- ✅ Error paths + edge cases (80%+ coverage)
- ✅ Tests verify observable outputs/state changes
- ✅ NEVER claim completion without running tests
- ✅ AAA pattern enforced
- ✅ No meaningless tests

## Commands
```bash
cargo make test-unit     # Fast (<16s)
cargo make test          # Full (<30s)
cargo make slo-check     # Performance validation
```

## 80/20 Focus Areas
- Error paths and resource cleanup
- Concurrency edge cases
- Real dependencies integration
- Deterministic behavior verification
