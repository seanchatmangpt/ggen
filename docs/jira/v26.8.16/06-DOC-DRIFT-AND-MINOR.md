# 06 — Doc Drift and Minor Residue

Part of [00-OVERVIEW](00-OVERVIEW.md). Batch ticket: each item is under an hour, none
blocks the others.

## 1. Stale "landed inert" comment — `/Users/sac/ggen/Cargo.toml` (~line 66)

Says of `ggen-engine`/`praxis-core`/`praxis-graphlaw`: "Landed inert in this pass — no
consumer references them yet." False since 2026-07/08: three live dependency edges exist
(`ggen-mcp/Cargo.toml:22`, `ggen-lsp/Cargo.toml:48`, `ggen-cli`) with real call sites.
This drift actively misled the 2026-08-16 sweep into almost wiring a "first" consumer
that already existed — an agent caught it only by checking before acting. Rewrite the
comment to name the current consumers and the date verified.

## 2. Ignored `[profile]` block — `/Users/sac/ggen/crates/ggen-engine/Cargo.toml`

Every cargo invocation in the workspace opens with: "warning: profiles for the non root
package will be ignored, specify profiles at the workspace root". Cargo silently discards
the block. Either move the settings to the root `Cargo.toml` `[profile.*]` (if they are
still wanted) or delete the block from ggen-engine (it is vendored config residue).
Acceptance: `cargo check --workspace 2>&1 | head -1` no longer shows the warning.

## 3. Inconsistent env-file tuple — ostar settings

The sweep changed `env_file=(".env", ".env.local")` in
`src/ostar/config/settings.py:218` and `src/ostar/lm/configure.py:16`, but the round-1
survey flagged the edit as "applied inconsistently across the two settings classes it
should cover." Audit every `BaseSettings` subclass in the repo
(`grep -rn "SettingsConfigDict" src/ostar/`) and make the `env_file` tuple uniform.
Acceptance: one grep, all hits identical.

## 4. Orphaned Rust file — `/Users/sac/chatmangpt/ostar/src/gstar_bootstrap_test.rs`

A `fn main()` Rust file sitting in the Python package tree with no `Cargo.toml` at that
level; byte-identical in intent to the crate in item 5. Fold it into
`tests/rust-bootstrap/` (or delete-by-archive if the crate supersedes it) — it currently
compiles under nothing.

## 5. Unwired crate — `/Users/sac/chatmangpt/ostar/tests/rust-bootstrap/`

Real standalone Cargo crate (oxigraph 0.4 + anyhow, own `Cargo.lock`) that loads every
`ontology/core/*.nt` and asserts the store answers SPARQL — a genuine G* bootstrap proof,
runnable by nothing. Wire it: a `poe` task (e.g. `poe test-rust-bootstrap` running
`cargo run --manifest-path tests/rust-bootstrap/Cargo.toml`) and a line in `CLAUDE.md`'s
command table. Acceptance: the task runs green from a clean checkout with rust installed,
real output pasted.

## 6. `GAPS_SUMMARY.md` staleness — ostar root

Claims "ALL GAPS CLOSED" dated 2026-05-04, modified in the working tree, predating the
2026-08 sweep entirely. Either update it to reference this ticket set as the current gap
ledger or mark it superseded per the markdown-standards deprecation convention.

## Acceptance (batch)

All six items closed with their per-item acceptance checks; one commit per repo is fine.

## See Also

- [01-COMMIT-BOUNDARY](01-COMMIT-BOUNDARY.md) — items 3 and 6 touch files already in its
  commit groups; sequence accordingly
