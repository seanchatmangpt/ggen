---
name: chicago-tdd-pattern
description: "Master Chicago TDD (state-based testing). Verify observable behavior with real collaborators using AAA pattern."
allowed_tools: "Read, Write, Bash(cargo make test:*)"
---

# Chicago TDD Pattern

## Core Philosophy

**Chicago TDD = State-based testing with real collaborators**

Verify observable behavior changes (return values, state mutations, side effects), NOT internal implementation or mocks.

## AAA Pattern

```rust
#[test]
fn test_feature() {
    // ARRANGE: Set up real objects (no mocks)
    let manager = Manager::new(temp_dir.path());

    // ACT: Call the public API
    manager.execute("command").unwrap();

    // ASSERT: Verify observable state
    assert_eq!(manager.status(), Status::Complete);
}
```

## Key Principles
- Use real objects (not mocks)
- Test observable behavior
- Avoid testing internal implementation
- One assertion = one test (focused)
- No flaky tests (deterministic)

## Reference
See CLAUDE.md sections:
- Development Workflow (Chicago TDD)
- Stack: chicago-tdd-tools 1.4.0
- Agents: test-engineer
