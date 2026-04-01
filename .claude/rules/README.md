---
version: 6.0.1
last_updated: 2026-04-01
---

# Claude Code Rules - ggen v6.0.1

Modular, auto-loading rules for specification-driven Rust code generation.

## Directory Structure

```
.claude/rules/
├── _core/                  # Auto-loaded (always active)
│   ├── absolute.md         # Non-negotiable rules
│   └── workflow.md         # Development workflow
├── rust/                   # Lazy-loaded
│   ├── elite-mindset.md    # Type-first, zero-cost abstractions
│   ├── lsp.md              # LSP-first navigation for .rs files
│   ├── testing.md          # Chicago TDD requirements
│   ├── testing-forbidden.md # Forbidden London TDD patterns
│   └── performance.md      # SLOs, benchmarks, optimization
├── andon/
│   └── signals.md          # Stop the line protocol
├── build/
│   └── cargo-make.md       # Build commands, quality gates
├── architecture.md         # Crate map, patterns, trait index
├── otel-validation.md      # OpenTelemetry span/trace verification
└── README.md               # This file
```

## Rules by Category

### Always Active

| Rule | File | What It Governs |
|------|------|----------------|
| Absolute Rules | [_core/absolute.md](_core/absolute.md) | Concurrent operations, cargo make only, task tool, todo batching, andon protocol |
| Workflow | [_core/workflow.md](_core/workflow.md) | 4-step development cycle (spec, TDD, generate, commit) |

### Rust Development

| Rule | File | What It Governs |
|------|------|----------------|
| Elite Mindset | [rust/elite-mindset.md](rust/elite-mindset.md) | Type-first design, zero-cost abstractions, API patterns |
| LSP Navigation | [rust/lsp.md](rust/lsp.md) | Mandatory LSP-first navigation for .rs files |
| Testing | [rust/testing.md](rust/testing.md) | Chicago TDD, AAA pattern, coverage requirements |
| Testing (Forbidden) | [rust/testing-forbidden.md](rust/testing-forbidden.md) | Banned London TDD patterns |
| Performance | [rust/performance.md](rust/performance.md) | Build time SLOs, memory targets, optimization |

### Verification

| Rule | File | What It Governs |
|------|------|----------------|
| Andon Signals | [andon/signals.md](andon/signals.md) | Stop the line protocol, signal levels, fixing process |
| Cargo Make | [build/cargo-make.md](build/cargo-make.md) | Build commands and quality gates |
| OTEL Validation | [otel-validation.md](otel-validation.md) | OpenTelemetry span/trace verification for LLM and external services |

### Reference

| Rule | File | What It Governs |
|------|------|----------------|
| Architecture | [architecture.md](architecture.md) | Full crate map, cross-cutting patterns, trait index |

CLAUDE.md is the constitution; these files are the statute.
