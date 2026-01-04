---
name: cargo-make-protocol
description: "Master Cargo Make build orchestration. Use for running ggen builds safely. Covers: poka-yoke error-proofing, SLO enforcement (check <5s, test <30s, lint <60s), timeout mechanisms, quality gates, andon signal detection. Essential skill - always use cargo make, never direct cargo commands. When building, testing, linting, or validating code."
allowed_tools: "Bash(cargo make:*)"
---

# Cargo Make Protocol Skill

## Overview

**Poka-Yoke Build System** from Toyota Production System

The ggen project uses Cargo Make (68K+ lines) to enforce build discipline:
- **Error-proofing**: Prevents mistakes through system design
- **SLO Enforcement**: Timeout-based quality gates
- **Andon Signals**: Stop-the-line on RED signals
- **Deterministic**: Reproducible builds and outputs

## Golden Rule

```
ALWAYS use: cargo make [target]
NEVER use:  cargo [command]
```

Direct cargo commands bypass:
- Timeout enforcement
- Warning-as-error compilation flags
- Quality gate dependencies
- Andon signal monitoring

## Quick Reference: Essential Commands

### Fast Feedback Loop (< 5min)

```bash
cargo make check        # Compile only (~5s) - checks for errors
cargo make test-unit    # Unit tests (~10s) - quick validation
cargo make lint         # Clippy linter (~60s) - catch mistakes early
```

### Full Validation (< 5min)

```bash
cargo make pre-commit   # Format + lint + tests (<60s)
cargo make test         # All tests (30s timeout, 120s escalation)
cargo make ci           # Full CI pipeline
```

### Performance Assurance

```bash
cargo make slo-check    # Verify SLO compliance
cargo make bench        # Run 14 benchmark suites
cargo make bench-compare # Compare performance across commits
```

### Specification System

```bash
cargo make speckit-check    # Validate .ttl syntax
cargo make speckit-validate # Check .ttl → .md generation
cargo make speckit-render   # Regenerate .md from .ttl
```

## SLO Targets (Service Level Objectives)

```
Metric                          Target
────────────────────────────────────────
First build                      ≤ 15s
Incremental compilation          ≤ 2s
Unit test execution             ≤ 10s
Full test suite                 ≤ 30s (120s escalation)
Clippy lint check               ≤ 60s
RDF processing (1k+ triples)    ≤ 5s
CLI scaffolding (end-to-end)    ≤ 3s
Generation memory usage         ≤ 100MB
```

## Poka-Yoke Mechanisms

### 1. Timeout Enforcement

Each target has timeout wrapper:
- **Quick timeout**: Detects deadlocks/hangs quickly
- **Escalation timeout**: Allows for contention on slow systems
- **Exit on timeout**: Prevents indefinite waits

```bash
# Example: test target
cargo make test         # 30s quick timeout
                        # 120s escalation timeout (lock contention)
```

### 2. Warnings as Errors

Compilation enforces zero-warning builds:

```rust
RUSTFLAGS="-D warnings"  // Force compiler to reject warnings
```

Prevents warning accumulation from hiding real problems.

### 3. Quality Gates

Pre-commit depends on three checks:

```
cargo make pre-commit
├─ cargo make check      # Must pass (RED if errors)
├─ cargo make lint       # Must pass (RED if warnings)
└─ cargo make test-unit  # Must pass (RED if failures)
```

Blocks commits with RED Andon signals.

### 4. Andon Signal Escalation

**Stop-the-line protocol**:

| Signal | Meaning | Action |
|--------|---------|--------|
| **RED** | ERROR | **STOP** - Fix immediately |
| **YELLOW** | WARNING | INVESTIGATE - Before release |
| **GREEN** | SUCCESS | CONTINUE - Ready for next step |

## Build Profile Optimization

### Development Profile
```toml
[profile.dev]
opt_level = 0              # Fast compilation
debug = true              # Debugging info
incremental = true        # Fast incremental builds
codegen_units = 256       # Parallel compilation
```

### Release Profile
```toml
[profile.release]
opt_level = 3             # Optimize aggressively
lto = "thin"             # Link-time optimization
strip = true             # Remove debug symbols
codegen_units = 16       # Balance speed/optimization
```

## Lint Configuration

**Zero-tolerance policy**:

```
[workspace.lints.rust]
warnings = "deny"                # All warnings → errors
unsafe_code = "deny"            # No unsafe without justification
missing_docs = "warn"           # Document public API

[workspace.lints.clippy]
all = { level = "deny" }        # All clippy checks
pedantic = { level = "deny" }   # Extra scrutiny
unwrap_used = "deny"            # Critical: NO unwrap!
expect_used = "deny"            # Critical: NO expect!
panic = "deny"                  # Panic-free requirement
```

## Common Workflows

### Making Changes

```bash
# 1. Make code changes
# 2. Quick feedback
cargo make check    # Did I break compilation?
cargo make lint     # Any clippy warnings?

# 3. Run tests
cargo make test-unit    # Quick unit tests

# 4. Ready to commit?
cargo make pre-commit   # Full validation before commit
```

### Before Pushing

```bash
# Verify everything works
cargo make ci           # Full CI pipeline

# Check performance
cargo make slo-check    # SLO compliance
cargo make bench        # Performance baseline

# Specification compliance
cargo make speckit-check # RDF ontology syntax valid
```

### Performance Work

```bash
cargo make bench                # Baseline
# ... make optimizations ...
cargo make bench-compare        # Compare performance
cargo make slo-check           # Verify SLOs met
```

## Timeout Escalation Details

For long-running operations, Cargo Make implements smart escalation:

```
Initial Timeout (Quick)
    ↓
[Operation still running]
    ↓
Wait period (allows slow systems)
    ↓
Escalation Timeout (If lock contention detected)
    ↓
[Timeout event]
```

Allows ggen to work on:
- Fast development machines (quick feedback)
- CI systems with resource contention (escalation gives more time)
- Resource-constrained environments (still enforces limits)

## Exit Codes & Interpretation

```
Exit Code    Meaning               Action
─────────────────────────────────────────
0            Success              Continue
1            Failure              Fix issue
2            Timeout              Investigate contention
124          Hard timeout         Deadlock detected
```

## Integration with Git Hooks

Pre-commit hook runs:
```bash
cargo make check    # RED if compilation fails
cargo make format   # Auto-fix formatting
cargo make lint     # RED if clippy warnings
```

Prevents committing broken code.

Pre-push hook runs:
```bash
cargo make test     # RED if tests fail
cargo make lint     # RED if warnings
```

Prevents pushing broken code.

**NEVER use `git push --no-verify`** - Defeats defect prevention

## Environment Variables

```bash
RUST_BACKTRACE=1           # Full panic backtrace
RUST_LOG=info              # Tracing level
CARGO_TERM_COLOR=always    # Force color output
```

Set these via `.claude/settings.json` environment section.

## Troubleshooting

### Hanging Builds

```bash
# Check for infinite loops
RUST_BACKTRACE=1 cargo make check

# If timeout triggered, investigate:
# - Check for deadlocks in concurrent code
# - Verify no infinite loops in macros
# - Profile with: cargo make bench
```

### Timeout Failures

```bash
# Escalation timeout occurred - system under heavy load
# Options:
1. Retry after system calm
2. Run on less loaded machine
3. Profile to optimize further
```

### Clippy Warnings Blocking Build

```bash
# Fix warning (don't suppress):
cargo make lint              # See warnings
# Edit code to fix root cause
cargo make check             # Re-verify
```

## Best Practices

1. **Always use cargo make**
   - Enforces build discipline
   - Prevents broken commits
   - Maintains consistency

2. **Run pre-commit before committing**
   ```bash
   cargo make pre-commit
   ```

3. **Check SLOs regularly**
   ```bash
   cargo make slo-check
   ```

4. **Use appropriate target for your need**
   - `check`: Quick sanity check
   - `test-unit`: Fast validation
   - `pre-commit`: Before commit
   - `ci`: Full validation

5. **Respect timeout signals**
   - RED timeout = critical issue
   - Investigate immediately
   - Don't bypass with `--timeout 0`

## Reference Files

- See: `reference.md` - Detailed command reference
- See: `examples.md` - Real-world build scenarios
- See: `troubleshooting.md` - Common issues and fixes
