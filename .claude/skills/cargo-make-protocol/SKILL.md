---
name: cargo-make-protocol
description: "Master Cargo Make build orchestration. Poka-yoke error-proofing, SLO enforcement (check <5s, test <30s, lint <60s), andon signals."
allowed_tools: "Bash(cargo make:*)"
---

# Cargo Make Protocol

## Golden Rule

```bash
ALWAYS: cargo make [target]
NEVER:  cargo [command]
```

Direct cargo bypasses timeouts, quality gates, and andon signals.

## Quick Reference

```bash
cargo make check       # <5s   (compile check)
cargo make test-unit   # <16s  (unit tests)
cargo make test        # <30s  (full suite)
cargo make lint        # <60s  (clippy + rustfmt)
cargo make pre-commit  # <2min (check + lint + test-unit)
```

## SLO Targets

| Target | SLO | Escalation |
|--------|-----|------------|
| check | <5s | 30s |
| test-unit | <16s | 150s |
| test | <30s | 120s |
| lint | <60s | - |

## Reference
See CLAUDE.md sections:
- Constitutional Rules (Cargo Make Only)
- Essential Commands
- SLO Targets
