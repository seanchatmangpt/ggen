# ggen-cli — Crate Audit

**Path:** `crates/ggen-cli/`
**Lines:** 30+ command files, ~1,550 test markers
**Role:** CLI via clap-noun-verb auto-discovery, thin routing layer over domain crates

---

## STUBS (commands returning "not implemented")

| File:Line | Command | Returns | Should Do |
|-----------|---------|---------|-----------|
| `cmds/construct.rs:286` | `ggen construct create` | `"not_implemented"` | Blocked: `ggen_ai::llm_construct` module commented out (dspy dependency) |
| `cmds/construct.rs:327` | `ggen construct validate` | `"not_implemented"` | Blocked on construct creation |
| `cmds/yawl.rs:268` | `ggen watch yawl` | Error "Watch mode is not yet fully implemented" | File-watching with auto-regeneration |
| `cmds/mcp.rs:820` | MCP background mode | "Background mode not yet implemented. Use foreground mode." | Daemon mode with pid tracking |
| `commands/paas/handlers/deploy.rs:47` | `ggen paas deploy` | "Deployment not yet implemented" | Deploy to PaaS provider |
| `commands/paas/handlers/logs.rs:60` | `ggen paas logs --follow` | "Log following not yet implemented" | Real-time log tailing |
| `commands/paas/handlers/explain.rs:66` | `ggen paas explain --spec` | "Specification tracing not yet implemented" | Show RDF triples that generated a file |
| `commands/paas/handlers/explain.rs:78` | `ggen paas explain --pipeline` | "Pipeline details not yet implemented" | Show pipeline stages with timing |

---

## ERROR TYPE CHAOS

**File:** `src/prelude.rs`

Three incompatible `Result` types:
```rust
pub use anyhow::{Context, Result as AnyhowResult};
pub use clap_noun_verb::Result;
pub use ggen_utils::error::{Context as ErrorContext, Error, Result as UtilsResult};
```

Two `Context` types in scope simultaneously. Every CLI command must choose which Result/Context to use.

---

## IGNORED TESTS

| File | Count | Reason |
|------|:---:|--------|
| `tests/marketplace/install_tests.rs` | 26 | "TODO: Enable in Phase 2" — permanently dead |
| `tests/marketplace_stress_suite.rs` | 3 | "Run with --ignored" |
| `tests/paas_e2e_tests.rs` | 1 | Requires network + git operations |
| `tests/otel_verification_test.rs` | 1 | Requires GROQ_API_KEY |
| `tests/conventions/e2e_tests.rs` | 1 | Process management / timing-sensitive |
| `tests/llm_e2e_test.rs` | 1 | Requires explicit permission |

---

## DEAD CODE

| File:Line | Item | Status |
|-----------|------|--------|
| `cmds/utils.rs:38` | Unknown field | `#[allow(dead_code)]` |
| `cmds/construct.rs:357` | Unknown field | `#[allow(dead_code)]` |
| `cmds/packs_old.rs` | Legacy pack commands | Entire file is old code |
| `cmds/packs_receipt.rs` | Receipt code | Likely superseded |

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **FIX** | Construct command: re-enable llm_construct module | P1 |
| **FIX** | YAWL watch mode: implement file watching | P1 |
| **FIX** | MCP background mode: implement daemon | P1 |
| **FIX** | PaaS commands: implement deploy, logs follow, explain | P1 |
| **FIX** | Prelude triple-Result: standardize on one error type | P0 |
| **DELETE** | 26 "Phase 2" ignored install tests — dead since creation | P3 |
| **DELETE** | `cmds/packs_old.rs` — legacy, no #[verb] annotation | P3 |
| **DELETE** | `cmds/packs_receipt.rs` if superseded by ggen-receipt crate | P3 |
