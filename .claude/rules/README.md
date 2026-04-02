---
version: 6.0.0
last_updated: 2026-02-08
---

# Claude Code Rules - ggen v6.0.0

Modular, auto-loading rules for specification-driven Rust code generation.

## 📁 Directory Structure

```
.claude/rules/
├── _core/           # Auto-loaded (always active)
│   ├── absolute.md  # 6 non-negotiable rules
│   └── workflow.md  # 4-step development workflow
├── rust/            # Lazy-loaded
│   ├── elite-mindset.md
│   ├── lsp.md      # LSP-first navigation for .rs files
│   ├── testing.md
│   ├── testing-forbidden.md
│   └── performance.md
├── andon/
│   └── signals.md   # Stop the line protocol
├── build/
│   └── cargo-make.md
├── otel-validation.md  # OpenTelemetry span/trace verification
└── README.md        # This file
```

## 🚨 Quick Reference

### Core Rules (Auto-Loaded)
- [Absolute Rules](_core/absolute.md) - 6 non-negotiable rules (ALWAYS active)
- [Workflow](_core/workflow.md) - 4-step development cycle

### Rust Development
- [LSP Navigation](rust/lsp.md) - ALWAYS use LSP over Grep for .rs files
- [Elite Mindset](rust/elite-mindset.md) - Type-first, zero-cost, performance patterns
- [Testing](rust/testing.md) - Chicago TDD, AAA pattern, 80%+ coverage
- [Performance](rust/performance.md) - SLOs, benchmarks, optimization

### Quality Control
- [Andon Signals](andon/signals.md) - Stop the line protocol, validation checklist
- [Cargo Make](build/cargo-make.md) - Build commands, quality gates
- [OTEL Validation](otel-validation.md) - OpenTelemetry span/trace verification for LLM/external services
- [Coding-Agent Mistakes](coding-agent-mistakes.md) - Mandatory gate: 5 mistake classes, 6-question patch contract, invariants, sabotage tests

## 🎯 The Golden Rule

**1 MESSAGE = ALL RELATED OPERATIONS**

Batch everything:
- TodoWrite: 10+ todos minimum
- Task tool: ALL agents together
- File ops: ALL reads/writes/edits together
- Bash: Chain with `&&`
- Memory: ALL store/retrieve together

## 🚨 Critical Reminders

1. **STOP THE LINE** when Andon signals appear
2. **ALWAYS use `cargo make`** - NEVER direct cargo
3. **TodoWrite always 10+ todos** in ONE batch
4. **RDF is source of truth** - Edit `.ttl`, not generated `.md`
5. **Test results are truth** - Code doesn't work if tests don't pass
6. **OTEL spans are proof** - LLM/external services require OTEL trace verification
7. **Chicago TDD ONLY** - No mocks, no test doubles, no behavior verification

## 📋 Definition of Done

```bash
cargo make timeout-check  # Verify timeout exists
cargo make check          # ✅ No compiler errors
cargo make test           # ✅ All tests pass
cargo make lint           # ✅ No warnings
cargo make slo-check      # ✅ SLOs met

# For LLM/external service features, ALSO verify:
RUST_LOG=trace,ggen_ai=trace cargo test <test_name> 2>&1 | grep -E "llm\.|mcp\."
# ✅ Required OTEL spans exist
# ✅ Required attributes populated
```

## 🏗️ Project Context

- **Stack**: Rust 1.91.1 | Tokio | Oxigraph | Tera | Clap
- **Architecture**: 30 crates | Workspace layout
- **Testing**: Chicago TDD ONLY (no mocks, no test doubles) | 87% coverage
- **Performance**: <15s first build | <2s incremental
- **Paradigm**: Specification-driven | RDF ontologies → Code

## 📚 Support

- **ggen**: https://github.com/seanchatmangpt/ggen

---

**Version**: v6.0.0 | **Last Updated**: 2026-02-08
