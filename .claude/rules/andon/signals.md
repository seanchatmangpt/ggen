---
auto_load: false
category: quality
priority: critical
version: 6.0.0
---

# 🚨 Andon Signals (Stop the Line)

## Protocol
**Signal = STOP WORK IMMEDIATELY. Fix root cause, not symptom.**

## Signal Levels
| Level | Pattern | Action |
|-------|---------|--------|
| 🔴 **CRITICAL** | `error[E...]` | HALT - Compiler errors |
| 🔴 **CRITICAL** | `test ... FAILED` | HALT - Test failures |
| 🟡 **HIGH** | `warning:` | STOP before release |
| 🟡 **HIGH** | Clippy errors | STOP before release |
| 🟢 **GREEN** | All checks pass | Proceed |

## Workflow
1. **Monitor** - Run `cargo make check/test/lint`
2. **Stop** - Detect signal → HALT immediately
3. **Investigate** - 5 Whys root cause analysis
4. **Fix** - Address root cause, not symptom
5. **Verify** - Re-run checks until cleared

## Systematic Fixing Process
```bash
# 1. Create comprehensive todos
TodoWrite { todos: [10+ related todos for all failures] }

# 2. Fix cycle (per todo)
# Read error → Root cause → Fix → Verify → Update todo → Remove when fixed

# 3. Re-run validation
cargo make check && cargo make test && cargo make lint

# 4. Repeat until all signals cleared
```

## Definition of Done Validation
```bash
cargo make timeout-check  # Verify timeout exists
cargo make check          # No compiler errors
cargo make test           # All tests pass
cargo make lint           # No warnings
cargo make slo-check      # SLOs met
```

**ONLY mark complete when ALL checks pass**
