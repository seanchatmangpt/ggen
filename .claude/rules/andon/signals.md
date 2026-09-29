---
auto_load: false
category: quality
priority: critical
version: 26.5.4
---

# Andon Signals (Stop the Line)

A signal appears → stop immediately. Read the signal, find root cause, fix, re-run.

| Level | Pattern | Action |
|---|---|---|
| CRITICAL | `error[E...]` / `test ... FAILED` | HALT until resolved |
| HIGH | `warning:` / clippy errors | STOP before release |
| CLEAR | all checks pass | proceed |

**DoD**: `just pre-commit` (gate list = `justfile`'s recipe, not restated here — has drifted
before). `just timeout-check`/`slo-check`/full `cargo test` are separate, not part of
`pre-commit`.

**The trap**: `#[allow(...)]`, `#[ignore]`, `|| true`, `unwrap()` to bypass, `todo!()` in
committed code suppress information, don't fix it.

**Fixing**: TodoWrite 10+ todos per failure → read error→root cause→fix→verify → `just check &&
just test && just lint` → repeat until clear.
