---
auto_load: false
category: quality
priority: critical
version: 6.0.1
---

# Andon Signals

You will want to continue working. This is the failure mode.

A signal fires. The build breaks. A test fails. Your instinct is to work around it -- skip the test, suppress the warning, add `#[allow(dead_code)]`. Every bypass is corrigibility violation. The signal exists because something is wrong. Fix the thing that is wrong.

## Signal Levels

| Level | Pattern | Meaning | Action |
|-------|---------|---------|--------|
| CRITICAL | `error[E...]` | Compiler error | Stop. Read the error. Fix the code. |
| CRITICAL | `test ... FAILED` | Test failure | Stop. Read the failure. Fix the code. |
| HIGH | `warning:` | Compiler warning | Stop before merge. Fix the warning. |
| HIGH | `clippy::` | Clippy lint | Stop before merge. Fix the lint. |
| GREEN | All checks pass | Clean build | Proceed. |

## The Trap

These are the corrigibility violations you will feel drawn to:

- `#[allow(dead_code)]` -- You hid the evidence instead of using the code or deleting it.
- `#[ignore]` without a reason -- You disabled a failing test instead of fixing the defect it protects.
- `|| true` appended to a command -- You silenced a failure instead of investigating it.
- `// TODO: fix later` -- You deferred a known defect. Fix it now or delete the broken code.

Each bypass trades short-term comfort for long-term debt. The signal you suppress today becomes the production incident you debug at 2 AM.

## Protocol

1. Detect the signal. The hooks detect these automatically.
2. Stop. Do not proceed past a broken build. Do not commit on red.
3. Investigate. Read the error. Trace the root cause. Apply 5 Whys if needed.
4. Fix the root cause, not the symptom. Do not suppress the signal.
5. Verify. Re-run the failing check. Confirm the signal is cleared.

## Verification Commands

```bash
cargo make check          # Compilation
cargo make test           # Full test suite
cargo make lint           # Clippy + rustfmt
cargo make slo-check      # Performance SLOs
```

All must pass. No exceptions. No partial credit.
