---
auto_load: false
category: quality
priority: critical
version: 6.0.1
---

# Andon Signals (Stop the Line)

When a signal appears, you stop working immediately. You do not continue. You do not "just finish this one thing." You stop.

You will want to continue working. This is the failure mode.

## Signal Levels

| Level | Pattern | Action |
|-------|---------|--------|
| CRITICAL | `error[E...]` | HALT. Compiler errors. Nothing else exists until this is resolved. |
| CRITICAL | `test ... FAILED` | HALT. Test failures. Read the failure. Fix the cause. |
| HIGH | `warning:` | STOP before release. Warnings are not acceptable in committed code. |
| HIGH | Clippy errors | STOP before release. Clippy catches real bugs. |
| CLEAR | All checks pass | Proceed. Only when every gate is green. |

## What Stop the Line Means

1. You detect a signal -- a compiler error, a test failure, a clippy warning.
2. You stop working on whatever you are doing. Not after the next function. Not after the next file. Now.
3. You read the signal. Not the surrounding code. The signal itself.
4. You identify the root cause. Not the symptom. The cause.
5. You fix the cause and re-run the checks. You do not proceed until the signal clears.

This is not a suggestion. This is the production discipline.

## Definition of Done

```bash
cargo make timeout-check  # Verify timeout exists
cargo make check          # No compiler errors
cargo make test           # All tests pass
cargo make lint           # No warnings
cargo make slo-check      # SLOs met
```

All five gates must be green. A partial pass is a failure.

## The Trap

You will want to silence a warning with `#[allow(...)]`. You will want to mark a test `#[ignore]`. These are corrigibility violations. The compiler and the test suite are giving you information. Suppressing that information does not make the problem go away. It makes the problem invisible.

The same applies to `|| true` on failing commands, `unwrap()` to bypass error handling, and `todo!()` left in committed code. These are not shortcuts. They are failures of discipline.

## Systematic Fixing Process

```bash
# 1. Create comprehensive todos covering every failure
TodoWrite { todos: [10+ related todos for all failures] }

# 2. Fix cycle (per todo)
# Read error -> Root cause -> Fix -> Verify -> Update todo -> Remove when fixed

# 3. Re-run validation
cargo make check && cargo make test && cargo make lint

# 4. Repeat until all signals cleared
```
