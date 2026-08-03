---
name: Cargo Make Protocol
description: Reference catalog of `cargo make` targets and Andon quality-gate signals. Historical — ggen's actual entry point is `just` (see .claude/rules/_core/absolute.md rule 4 and the cargo-make-runner skill); kept only as a target-name reference, not an instruction to run `cargo make` directly.
---

# Cargo Make Protocol

## Overview

**CRITICAL**: NEVER use direct `cargo` commands. ALWAYS use `cargo make` targets.

## Core Commands

### Quick Checks (< 5s)
```bash
cargo make check        # Compilation check - Andon monitoring
cargo make timeout-check # Verify timeout wrapper exists
```

### Testing (< 30s)
```bash
cargo make test-unit    # Fast unit tests (< 16s)
cargo make test         # Full test suite (< 30s)
```

### Quality Gates
```bash
cargo make lint         # Clippy + rustfmt
cargo make pre-commit   # check → lint → test-unit (< 2min)
cargo make ci           # Full CI pipeline
```

### Performance & Security
```bash
cargo make slo-check    # Verify SLOs (build ≤15s, incremental ≤2s, RDF ≤5s/1k+ triples)
cargo make audit        # Security vulnerability scan
cargo make bench        # Run benchmarks
```

### Specification Tools
```bash
cargo make speckit-validate  # SHACL validation on .specify/*.ttl
cargo make speckit-render    # Generate markdown from TTL
```

## Andon Signals

| Signal | Pattern | Action |
|--------|---------|--------|
| 🔴 CRITICAL | `error[E...]` | STOP THE LINE - Fix immediately |
| 🔴 CRITICAL | `test ... FAILED` | STOP THE LINE - Fix immediately |
| 🟡 HIGH | `warning:` | STOP before release |
| 🟢 GREEN | Clean output | Proceed |

## Workflow Pattern

```bash
# BEFORE: Verify setup
cargo make timeout-check

# DURING: Quick feedback loop
cargo make check    # Andon monitoring
cargo make lint     # Andon monitoring

# AFTER: Full validation
cargo make test     # CRITICAL signal
cargo make slo-check
cargo make audit
```

## Why Cargo Make?

1. **Enforces SLOs**: Automatic timeout handling
2. **Quality Gates**: Integrated checks prevent defects
3. **Consistency**: Same commands across all environments
4. **Visibility**: Clear feedback on all quality dimensions
5. **DfLSS**: Design for Lean Six Sigma - prevent defects AND waste

## Definition of Done

✅ `cargo make check` - Clean
✅ `cargo make test` - All pass
✅ `cargo make lint` - Clean
✅ `cargo make slo-check` - SLOs met
✅ No pending test todos

**ONLY mark complete when ALL checks pass**
