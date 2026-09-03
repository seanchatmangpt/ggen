# 04 — ggen-cli Marketplace Test Suite: Seven Disabled Modules

Part of [00-OVERVIEW](00-OVERVIEW.md).

## Finding (verified live 2026-08-16)

`/Users/sac/ggen/crates/ggen-cli/tests/marketplace/mod.rs:98-107`:

```rust
// Existing Chicago TDD tests (v1 - DISABLED)
// mod install_tests;
// mod registry_tests;

// New comprehensive test suite (v1 - DISABLED)
// pub mod fixtures;
// pub mod integration;
// pub mod performance;
// pub mod security;
// pub mod unit;
```

Seven modules of marketplace test coverage are parked behind comments. This is the exact
dead-code-gate pattern `docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md` catalogued as
category (2) of the ~150 stale `ggen_core::` references: source that cannot compile
because its `use ggen_core::...` imports have no dependency edge to satisfy them, kept
invisible by commenting out the `pub mod` line. `cargo check --workspace` green is
therefore *not* evidence these tests are healthy — they are simply not compiled.

## Why it matters

`ggen-marketplace` is a live workspace member and the `pack` CLI surface is real
(`ggen pack list` returns 2 packs). Its test suite being dark means marketplace
regressions are invisible — an Oracle Gap: in the tree but not in the build.

## Fix — two-outcome rule, no third state

For each of the seven modules, exactly one of:

1. **Repair and re-enable**: port the `ggen_core::` imports to their live equivalents
   (`ggen-engine` / `ggen-marketplace` / `ggen-config` per the v26.7.16 migration map),
   uncomment the `mod` line, make it compile and pass for real. Chicago style: real
   registry/filesystem fixtures, state assertions — no mocks of crates this workspace
   owns.
2. **Formally archive**: move the file under `crates/ggen-cli/tests/archive/marketplace/`
   with a dated header stating why it was not ported (non-deletion doctrine — preserve,
   do not delete), and remove the commented line from `mod.rs`.

Recommended split: start with `install_tests` and `registry_tests` (the "Existing Chicago
TDD tests" — most likely to map 1:1 onto live APIs); triage the five "comprehensive"
modules after, where `performance` and `security` are the likeliest archive candidates if
their harnesses assumed ggen-core internals.

## Acceptance

- `grep -n "^// *\(pub \)\?mod " crates/ggen-cli/tests/marketplace/mod.rs` → zero hits.
- `cargo test -p ggen-cli-lib --test <marketplace harness>` — real output pasted; every
  re-enabled module compiles and its tests pass.
- Archived files carry the dated rationale header.

## See Also

- `/Users/sac/ggen/docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md` — the dead-code
  gate taxonomy this ticket closes one instance of
- [05-MARKETPLACE-BRIDGE](05-MARKETPLACE-BRIDGE.md) — a live test suite here is a
  prerequisite for trusting the bridge
