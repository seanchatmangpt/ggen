---
name: Cargo Make Runner
description: Run cargo make targets for ggen. Never use direct cargo — always route through cargo make. Andon stop-the-line on failures.
paths: ["crates/**/*.rs", "tests/**/*.rs", "Makefile*"]
---

# Skill: cargo-make-runner

## Purpose
Route all Rust build, test, lint, and check commands through `cargo make` (never direct cargo). Enforce Andon protocol: STOP THE LINE on errors.

## When to Trigger
- User asks to "build", "check", "test", "lint", "run", or any cargo command
- Compiler errors appear
- Test failures occur
- Clippy warnings detected
- Performance SLO validation needed

## Do Not Trigger
- Explanations of cargo vs cargo-make
- Generic Rust discussion without build context

## Available Targets
```
cargo make check        # Compilation check (fast, <5s)
cargo make test         # Full test suite (unit + integration, <30s)
cargo make test-unit    # Unit tests only
cargo make lint         # Clippy + rustfmt
cargo make pre-commit   # check → lint → test-unit
cargo make slo-check    # Performance SLO validation
cargo make audit        # Security audit
cargo make bench        # Run benchmarks
```

## Protocol: Andon Stop-the-Line

When a command fails:
1. **STOP** — Do not continue to next command
2. **READ** — Understand the error message (not surrounding code)
3. **IDENTIFY** — Root cause, not symptom
4. **FIX** — Apply targeted fix
5. **RERUN** — Verify the command passes
6. **ITERATE** — Repeat until all green

Never:
- ❌ Ignore errors with `|| true`
- ❌ Use `#[allow(...)]` to silence warnings
- ❌ Mark test `#[ignore]`
- ❌ Proceed until ALL gates pass

## Required Before Claiming Completion
```bash
cargo make check
cargo make test
cargo make lint
cargo make slo-check
```

All four must return exit code 0.
