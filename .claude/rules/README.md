---
version: 26.7.2
last_updated: 2026-07-03
---

# Claude Code Rules - ggen

Modular rules for spec-driven Rust codegen. `_core/` auto-loads; `rust/` lazy-loads;
`andon/signals.md`; root: `architecture.md`, `coding-agent-mistakes.md`, `otel-validation.md`.

**Golden rule**: `just <task>` is the single entry point — never `cargo make`/bare `cargo`.

**Critical**: stop the line on Andon signals; RDF is source of truth, not generated `.md`;
OTEL spans are proof for LLM/external features; Chicago TDD only.

**DoD**: `just pre-commit` (gate list lives in `justfile`, not restated here). `just doctor` is
a separate optional check. For LLM/external features also:
`RUST_LOG=trace,ggen_ai=trace cargo test <test_name> 2>&1 | grep -E "llm\.|mcp\."`.

**Stack**: Rust nightly | Tokio | Oxigraph | Tera | Clap | 14-crate workspace
([architecture.md](architecture.md)) | Chicago TDD only.

**Support**: https://github.com/seanchatmangpt/ggen
