---
version: 26.7.2
last_updated: 2026-07-03
---

# Claude Code Rules - ggen

Modular, auto-loading rules for specification-driven Rust code generation.

## 📁 Directory Structure

```
.claude/rules/
├── _core/           # Auto-loaded (always active)
│   ├── absolute.md  # Non-negotiable rules
│   └── workflow.md  # 4-step development workflow
├── rust/            # Lazy-loaded
│   ├── elite-mindset.md
│   ├── lsp.md              # LSP-first navigation for .rs files
│   ├── testing.md
│   ├── testing-forbidden.md
│   └── performance.md
├── andon/
│   └── signals.md   # Stop the line protocol
├── architecture.md          # Crate map, cross-cutting patterns
├── coding-agent-mistakes.md # Mandatory gate: mistake classes, patch contract
├── otel-validation.md       # OpenTelemetry span/trace verification
└── README.md        # This file
```

## 🚨 Quick Reference

### Core Rules (Auto-Loaded)
- [Absolute Rules](_core/absolute.md) - non-negotiable rules (ALWAYS active)
- [Workflow](_core/workflow.md) - 4-step development cycle

### Rust Development
- [LSP Navigation](rust/lsp.md) - ALWAYS use LSP over Grep for .rs files
- [Elite Mindset](rust/elite-mindset.md) - Type-first, zero-cost, performance patterns
- [Testing](rust/testing.md) / [Testing Forbidden](rust/testing-forbidden.md) - Chicago TDD, AAA pattern
- [Performance](rust/performance.md) - SLOs, benchmarks, optimization

### Quality Control
- [Andon Signals](andon/signals.md) - Stop the line protocol, validation checklist
- [OTEL Validation](otel-validation.md) - OpenTelemetry span/trace verification for LLM/external services
- [Coding-Agent Mistakes](coding-agent-mistakes.md) - Mandatory gate: mistake classes, 6-question patch contract

## 🎯 The Golden Rule

**`just <task>` is the single entry point. Never `cargo make`, never bare `cargo` for gated tasks.**

## 🚨 Critical Reminders

1. **STOP THE LINE** when Andon signals appear (compiler errors, test failures)
2. **`just <task>`** — never `cargo make`, never bare `cargo` for gated tasks
3. **RDF is source of truth** - Edit `.specify/*.ttl`, not generated `.md`
4. **Test results are truth** - Code doesn't work if tests don't pass
5. **OTEL spans are proof** - LLM/external services require OTEL trace verification
6. **Chicago TDD ONLY** - No mocks, no test doubles, no behavior verification

## 📋 Definition of Done

```bash
just check   # No compiler errors
just test    # All tests pass
just lint    # No warnings
just doctor  # Fast local health check

# For LLM/external service features, ALSO verify:
RUST_LOG=trace,ggen_ai=trace cargo test <test_name> 2>&1 | grep -E "llm\.|mcp\."
# ✅ Required OTEL spans exist, attributes populated
```

## 🏗️ Project Context

- **Stack**: Rust (nightly, pinned) | Tokio | Oxigraph | Tera | Clap
- **Architecture**: 12-crate workspace (see [architecture.md](architecture.md))
- **Testing**: Chicago TDD ONLY (no mocks, no test doubles)
- **Paradigm**: Specification-driven | RDF ontologies → Code

## 📚 Support

- **ggen**: https://github.com/seanchatmangpt/ggen
