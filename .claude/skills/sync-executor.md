---
name: Sync Executor
description: Execute ggen sync (Resolve→Enrich→Extract→Render→Write pipeline) via ggen-engine
paths: [".specify/**/*.ttl", "crates/ggen-engine/**"]
type: skill
---

# Skill: sync-executor

> **Superseded pipeline notice (v26.7.16 CLI-routing flip):** the `ggen sync` command no
> longer runs `ggen-core`'s `codegen::executor::SyncExecutor`. `crates/ggen-cli/src/cmds/sync.rs`
> (the old flat `sync` verb that called `SyncExecutor`) is archived — commented out in
> `crates/ggen-cli/src/cmds/mod.rs` (`// pub mod sync;`) because it collided with ggen-engine's
> own `sync` noun. `ggen sync` now routes through `crates/ggen-cli/src/lib.rs`'s
> `inject_default_verbs()` (bare `ggen sync` → `ggen sync run`) into ggen-engine's
> `sync` noun/`run` verb (`crates/ggen-engine/src/verbs/sync.rs::sync_run`) →
> `crates/ggen-engine/src/verbs/handlers.rs::handle_sync_run` →
> `crates/ggen-engine/src/sync.rs::sync`. `ggen-core::codegen::executor::SyncExecutor` itself
> is not deleted (fix-forward/non-deletion doctrine) and is still called directly by the
> experimental, default-off `ggen wizard` command's initial-sync step
> (`crates/ggen-cli/src/cmds/wizard.rs::run_initial_sync`, gated behind
> `#[cfg(feature = "experimental")]`) — but that is no longer the same code path as `ggen sync`,
> despite a stale comment in `wizard.rs` still claiming otherwise. This skill now describes the
> current `ggen sync` pipeline (ggen-engine). The old feature set below (CLI `--audit`/`--force`/
> `--watch`/`--validate-only` flags, `<<<<<<< GENERATED` merge markers, `audit.json`) does not
> map one-to-one onto the new pipeline — see the "What changed" table.

## Purpose
Execute ggen's five-stage sync pipeline (Resolve → Enrich → Extract → Render → Write) that
`ggen sync` drives, reading `.specify/**/*.ttl` (or a `ggen.toml`-declared ontology) and
producing generated files plus a chained cryptographic receipt.

## What changed (old SyncExecutor vs. current pipeline)

| Old (`ggen-core::codegen::executor::SyncExecutor`) | Current (`ggen-engine::sync::sync`) |
|---|---|
| `--audit` CLI flag → `audit.json` with `rules_executed` | Every non-dry-run sync always writes a chained receipt: `.ggen-v2/receipt.json` (`SyncReceipt { record: ReceiptRecord, payload: ReceiptPayload }`) plus an append-only `.ggen-v2/receipt-log.jsonl`. No opt-out flag. |
| `--force` CLI flag (global override) | Per-template `force: true` frontmatter field (`crates/ggen-engine/src/write.rs`) — scoped to the template, not a global CLI flag. |
| `--watch` CLI flag on the same command | `watch: bool` verb argument (`sync run --watch`) → `crates/ggen-engine/src/watch.rs::watch`; runs one sync, then re-runs on debounced filesystem changes (500ms window), ignoring `.ggen-v2`/`.git` to avoid self-triggering. |
| `--validate-only` CLI flag | No direct equivalent on `sync run`; static frontmatter/SPARQL lint now lives in the separate `graph validate` noun (`crates/ggen-engine/src/verbs/handlers.rs::handle_graph_validate`, backed by `crate::lint`). |
| Merge mode: `<<<<<<< GENERATED` conflict markers | Hygen-style `inject: true` frontmatter with `before`/`after`/`at_line` markers (`crates/ggen-engine/src/write.rs::inject_into`) — no conflict-marker mechanism. |
| Conditional execution: SPARQL ASK rule-skipping | `when:` SPARQL ASK guard per template (`crates/ggen-engine/src/template.rs` `Frontmatter::when`, evaluated in `crates/ggen-engine/src/sync.rs` around the Extract stage) — same ASK-guard idea, different frontmatter key and engine. |
| μ₁-μ₅ naming (CONSTRUCT/SELECT/Tera/Canonicalize/Receipt) | Five stages named Resolve → Enrich → Extract → Render → Write (see `crates/ggen-engine/src/sync.rs` module doc comment). Enrich runs each template's `construct:` query once and inserts the produced triples — **single pass**, not iterated to a fixed point; constructs depending on other constructs' output need a second `sync` run. |

## Triggers (WHEN)
- `ggen sync` command context
- `sync run` verb / `ggen-engine::sync::sync` reference
- `when:` ASK guard frontmatter context
- `inject:` / `force:` frontmatter fields
- `--watch` flag mentioned
- Receipt/receipt-chain context (`.ggen-v2/receipt.json`, `.ggen-v2/receipt-log.jsonl`)

## Don't Trigger (WHEN NOT)
- `spec-writer` or specification context
- Documentation generation
- Architectural discussion
- `ggen wizard`'s internal initial-sync step (still `ggen-core::codegen::executor::SyncExecutor` — see notice above; out of scope for this skill)

## Responsibilities

### Core Execution
- Modify `crates/ggen-engine/src/sync.rs` (the `sync()` five-stage pipeline)
- Modify `crates/ggen-engine/src/write.rs` (write/inject/force/backup semantics)
- Modify `crates/ggen-engine/src/watch.rs` (filesystem watch + debounce + re-sync)
- Modify `crates/ggen-engine/src/template.rs` (frontmatter fields incl. `when:`, Tera rendering)
- Wire new verb arguments through `crates/ggen-engine/src/verbs/sync.rs` (GENERATED — routes are a
  projection of `schema/praxis.ttl`; hand-written logic belongs in `verbs::handlers`, not here)
- Implement handler logic in `crates/ggen-engine/src/verbs/handlers.rs::handle_sync_run`

### CLI Integration
- `sync run` verb currently exposes `dry_run: bool` and `watch: bool` only
  (`crates/ggen-engine/src/verbs/sync.rs::sync_run`)
- Bare `ggen sync` (no verb) is rewritten to `ggen sync run` by
  `crates/ggen-cli/src/lib.rs::inject_default_verbs`

### File Operations
- Verify `force`/`inject`/`backup` frontmatter handling (`crates/ggen-engine/src/write.rs`)
- Confirm receipt write (`.ggen-v2/receipt.json`) and log append (`.ggen-v2/receipt-log.jsonl`)
  only occur on non-dry-run sync
- Confirm watch mode ignores `.ggen-v2`/`.git` to prevent retrigger loops

## Test Focus (Chicago TDD)

### Observable State Changes
- Verify files are written to disk (real filesystem, `TempDir`)
- Confirm `.ggen-v2/receipt.json` is created with the expected `SyncReceipt` shape and that
  `.ggen-v2/receipt-log.jsonl` gains one line per non-dry-run sync
- Validate `force`/`inject` frontmatter fields are honored per-template
- Check `when:` ASK guards correctly skip/include template rendering
- Verify watch mode re-runs sync on a debounced filesystem change and does not loop on its own
  receipt writes

### Test Pattern
```rust
#[test]
fn test_sync_writes_receipt_on_non_dry_run() {
    // Arrange: real project root with ggen.toml + ontology + one template
    let root = TempDir::new().unwrap();
    write_fixture_project(root.path());

    // Act: run the real five-stage pipeline
    let report = ggen_engine::sync::sync(root.path(), SyncOptions::default()).unwrap();

    // Assert: verify observable state
    assert!(root.path().join(".ggen-v2/receipt.json").exists());
    assert!(!report.written.is_empty());
}
```

## Related Tasks
- Receipt chain: `.ggen-v2/receipt.json` + `.ggen-v2/receipt-log.jsonl` writing
- `force`/`inject` frontmatter handling in `write.rs`
- `watch.rs` debounced re-sync
- `when:` ASK-guard evaluation in `sync.rs`

## Code Quality Standards
- ✅ `Result<T, E>` error handling (no unwrap in production) — errors are typed `FM-*` codes via
  `crates/ggen-engine/src/error.rs::AppError`
- ✅ Chicago TDD with observable state verification
- ✅ No panics in production code

## Related Skills
- `chicago-tdd-implementer` - Tests for all features
- `rust-executor` - Verification via `just check`/`just test`

## SLOs
- Sync execution: ≤5s (100 rules, 90th percentile) — see `.claude/rules/rust/performance.md`
- File write operations: <1s

## Files to Modify
- `crates/ggen-engine/src/sync.rs`
- `crates/ggen-engine/src/write.rs`
- `crates/ggen-engine/src/watch.rs`
- `crates/ggen-engine/src/template.rs`
- `crates/ggen-engine/src/verbs/handlers.rs`

## Existing Test Coverage (ggen-engine)
- `crates/ggen-engine/tests/sync_e2e.rs`
- `crates/ggen-engine/tests/write_behaviors_cli_e2e.rs`
- `crates/ggen-engine/tests/receipt_chain_e2e.rs`
- `crates/ggen-engine/tests/generation_rules_e2e.rs`
- `crates/ggen-engine/tests/frontmatter_fields_e2e.rs`
- `crates/ggen-engine/tests/graphlaw_e2e.rs`

## Architecture Compliance
- ✅ `verbs::sync` layer: GENERATED clap-noun-verb routing only, no logic (see file header comment)
- ✅ `verbs::handlers` layer: thin dispatch into `crate::sync::sync`
- ✅ `sync.rs`/`write.rs`/`watch.rs`/`template.rs`: pure pipeline + I/O logic

## Constitution Alignment
- Type-safe: All operations return `Result<T, E>` (`AppError` with `FM-*` diagnostic codes)
- Deterministic: Receipt chain (BLAKE3 over `{graph_hash, outputs}`) ensures reproducibility
- TDD: Observable state verification (Chicago School)
