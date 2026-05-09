---
name: Andon Stop
description: Stop the line when Andon signals appear. Trigger on compiler errors (error[E...]), test failures (FAILED), or clippy errors. Fix immediately before proceeding.
---

# Skill: andon-stop

## The Protocol

**When a signal appears, you stop.**

Signals:
- `error[E...]` — Compiler errors
- `test ... FAILED` — Test failures
- `warning:` — Compiler warnings
- `clippy::` — Clippy errors

You will want to continue. This is the failure mode. Do not.

## When Signal Appears

1. **STOP** — Stop working on whatever you're doing. Now.
2. **READ** — Read the signal itself. Not the surrounding code. The signal.
3. **UNDERSTAND** — What is the root cause? Not the symptom. The cause.
4. **FIX** — Apply a targeted fix to the root cause.
5. **RERUN** — Run the check again. Verify the signal cleared.
6. **ONLY THEN PROCEED** — Continue to next task.

## Definition of Done (All Green)

```bash
cargo make timeout-check  # Verify timeout exists
cargo make check          # ✅ No compiler errors
cargo make test           # ✅ All tests pass
cargo make lint           # ✅ No warnings
cargo make slo-check      # ✅ SLOs met
```

All five gates must return exit code 0. A partial pass is a failure.

## The Trap

You will want to silence a warning with `#[allow(...)]`. You will want to mark a test `#[ignore]`. You will want to use `|| true` to hide a failure.

These are corrigibility violations.

The compiler and the test suite are giving you information. Suppressing that information does not make the problem go away. It makes the problem invisible.

## Systematic Fixing

1. **Create comprehensive todos** covering every signal
2. **Fix cycle** (per todo):
   - Read error message
   - Identify root cause
   - Apply fix
   - Verify fix
   - Update todo
   - Remove when fixed
3. **Rerun validation**: `cargo make check && cargo make test && cargo make lint`
4. **Iterate until all signals cleared**

## Never

❌ Ignore errors with `|| true`
❌ Silence warnings with `#[allow(...)]`
❌ Mark tests `#[ignore]`
❌ Use `unwrap()` or `expect()` to bypass error handling
❌ Leave `todo!()` in committed code
❌ Proceed despite failures

## Only

✅ Stop and fix immediately
✅ Understand root cause
✅ Verify fix with rerun
✅ Continue only when all gates green
