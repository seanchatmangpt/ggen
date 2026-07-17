# Marketplace / Pack-Registry Merge

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 2, depends on
[01-PUBLISH-SAFETY-AND-CRATE-RENAME](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md) and the receipt
design in [04-RECEIPT-SIGNING-AND-OTEL](04-RECEIPT-SIGNING-AND-OTEL.md).

## File reference table

### `domain/packs/` subtree (source, to be merged into ggen-marketplace)

All paths under `/Users/sac/ggen/crates/ggen-core/src/domain/packs/`:

| File | LOC |
|---|---:|
| `advanced_resolver.rs` | 561 |
| `capability_registry.rs` | 221 |
| `cloud_distribution.rs` | 266 |
| `compose.rs` | 307 |
| `composer.rs` | 467 |
| `dependency_graph.rs` | 391 |
| `external_fetcher.rs` | 570 |
| `generator.rs` | 103 |
| `installer.rs` | 510 |
| `metadata.rs` | 228 |
| `mod.rs` | 113 |
| `registry.rs` | 269 |
| `repository.rs` | 385 |
| `score.rs` | 186 |
| `sparql_executor.rs` | 502 |
| `template_generator.rs` | 562 |
| `types.rs` | 105 |
| `validate.rs` | 153 |
| `install.rs` | 280 |
| **Total (all 19 files, including `install.rs` and `mod.rs`)** | **6,179** |
| **Total excluding `install.rs` (dead stub — see below) but including `mod.rs`** | **5,899** |

**Correction to an earlier draft of this ticket**: "~6,179 lines excluding `install.rs`" was
arithmetically wrong — 6,179 is the sum of **all 19 files including** `install.rs` (280) and
`mod.rs` (113, previously omitted from the file list). The correct scope to merge into
`ggen-marketplace` is the 18 files excluding the dead `install.rs` stub: **5,899 lines**
(17 named files, 5,786, plus `mod.rs`, 113).

### Other ggen-core source files

| Path | LOC | Relevant lines |
|---|---:|---|
| `/Users/sac/ggen/crates/ggen-core/src/packs/lockfile.rs` | 788 | `PackLockfile`, `LockedPack`, `PackSource` |
| `/Users/sac/ggen/crates/ggen-core/src/packs/install.rs` | 39 | dead stub, `bail!(...)` at lines 33-39 (doc comment 29-32, fn signature line 33) |
| `/Users/sac/ggen/crates/ggen-core/src/lib.rs` | 381 | `config_lib` re-export 250-251; **marketplace re-export 253-255** (named-import list `pub use ggen_marketplace::{...}`, not a wildcard); receipt re-export 257-262 |
| `/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs` | 300 | `emit_install_receipt`, `PackInstallClosure`, `verify_install_receipt` — see [04](04-RECEIPT-SIGNING-AND-OTEL.md) |

### Destination

| Path | LOC | Notes |
|---|---:|---|
| `/Users/sac/ggen/crates/ggen-marketplace/src/lib.rs` | 17 | add `pub mod packs_registry;` (or similar) for the merged subtree |
| `/Users/sac/ggen/crates/ggen-marketplace/src/marketplace/install.rs` | 1,811 | `pub struct Installer<R: AsyncRepository>` at line 31; caching/signature-verification/rollback confirmed present (`TransactionSnapshot`-style rollback struct at ~line 42) |
| `/Users/sac/ggen/crates/ggen-marketplace/src/packs/lockfile.rs` | new file | ported from `ggen-core/src/packs/lockfile.rs` (788 lines) |

## The gap

`ggen-marketplace` already has a *more* mature `Installer`
(`/Users/sac/ggen/crates/ggen-marketplace/src/marketplace/install.rs:31`, 1,811 lines total
— caching, signature verification, atomic/rollback semantics) than ggen-core's own
`domain::packs::install` (`/Users/sac/ggen/crates/ggen-core/src/domain/packs/install.rs`,
280 lines) — but ggen-core's `domain::packs::*` subtree (5,899 lines excluding the dead
`install.rs` stub — see table above) and `packs::lockfile`
(`/Users/sac/ggen/crates/ggen-core/src/packs/lockfile.rs`, 788 lines, `.ggen/packs.lock`)
have no equivalent in `ggen-marketplace` today.

This is a **merge**, not a port: fold ggen-core's registry/search/capability breadth into
ggen-marketplace's already-better installer, not the reverse.
`ggen_core::packs::install::install_pack`
(`/Users/sac/ggen/crates/ggen-core/src/packs/install.rs:33-39`) itself is a dead stub
(`bail!("...use Installer instead")`) — delete, don't move it.

## What moves where

| Source (today) | Destination | Notes |
|---|---|---|
| `/Users/sac/ggen/crates/ggen-core/src/domain/packs/install.rs` (280 lines: `install_pack`, `InstallInput`, `InstallOutput`) | `/Users/sac/ggen/crates/ggen-marketplace/src/marketplace/install.rs` | Merge into (not duplicate) `Installer` (line 31), which already has caching/signature-verification/rollback. |
| The other 18 files in `/Users/sac/ggen/crates/ggen-core/src/domain/packs/` (5,899 lines total — see full table above) | `/Users/sac/ggen/crates/ggen-marketplace/src/packs_registry/` (new module) | Whole subtree; imported directly by `ggen-cli`'s `cmds/packs.rs`, `cmds/pack.rs`, `cmds/capability.rs`. |
| `/Users/sac/ggen/crates/ggen-core/src/packs/lockfile.rs` (788 lines: `PackLockfile`, `LockedPack`, `PackSource`) | `/Users/sac/ggen/crates/ggen-marketplace/src/packs/lockfile.rs` | `.ggen/packs.lock` format. `ggen-marketplace` has no lockfile concept today — net-new capability for it, not a merge. |
| `/Users/sac/ggen/crates/ggen-core/src/packs/install.rs:33-39` (`install_pack` dead stub) | Delete | `bail!("...use Installer instead")`. Not called by any ggen-cli command file. |
| `/Users/sac/ggen/crates/ggen-core/src/lib.rs:253-255` (`pub use ggen_marketplace::{policy, profile, metadata, models}`) | Already in `ggen-marketplace` | Not a move — cut the re-export at `lib.rs:253-255`, update `ggen-cli`'s `cmds/policy.rs` to import `ggen_marketplace::marketplace::*` directly. |
| `/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs` (`emit_install_receipt`, `PackInstallClosure`, `verify_install_receipt`) | `/Users/sac/ggen/crates/ggen-marketplace/src/` (new module, e.g. `receipt_emission.rs`) | Follows the install logic it witnesses. Must be pointed at `ggen_config::receipt::Receipt` (`/Users/sac/ggen/crates/ggen-config/src/receipt/receipt_impl.rs`, 352 lines) explicitly — see [04-RECEIPT-SIGNING-AND-OTEL](04-RECEIPT-SIGNING-AND-OTEL.md) — not the local ggen-core copy at `ggen-core/src/receipt/receipt_impl.rs` (362 lines). |

## Dependency-direction sanity check

Current Cargo dependency edges: `ggen-config` and `ggen-graph` are leaves (no
`ggen-core`/`ggen-marketplace` deps in either's `Cargo.toml`); `ggen-marketplace →
ggen-config`; `ggen-core → ggen-config, ggen-graph, ggen-marketplace`; `ggen-cli → ggen-core,
ggen-graph` (`/Users/sac/ggen/crates/ggen-cli/Cargo.toml:166,170` — no direct `ggen-config`
or `ggen-marketplace` dependency yet). No cycle exists today, and the proposed moves don't
create one — the engine crate (today's `ggen-core`) *loses* its edge to `ggen-marketplace`
once marketplace logic moves out, a net reduction in coupling.

### Risk: a real new edge is required

`GenerationRule.query: QuerySource` (defined in
`/Users/sac/ggen/crates/ggen-core/src/manifest/types.rs`, ported to
`/Users/sac/ggen/crates/ggen-config/src/manifest/types.rs` per
[05-MANIFEST-CONFIG-PORT](05-MANIFEST-CONFIG-PORT.md)) has a `Pack { pack, output, file }`
variant that resolves a SPARQL query file from a pack's *installed* output directory —
resolving it requires asking the registry/lockfile "where is this pack installed." That
knowledge moves to `ggen-marketplace` under this plan, so the engine crate (which owns
template render/generation) needs a **new**, deliberate dependency on `ggen-marketplace` to
resolve `QuerySource::Pack` at sync time — directionally safe (no cycle), but must be added
explicitly, not as an undocumented re-export the way `ggen-core/src/lib.rs:253-255` was.

## Definition of done for this ticket

- `ggen_marketplace::marketplace::install::Installer`
  (`/Users/sac/ggen/crates/ggen-marketplace/src/marketplace/install.rs:31`) absorbs
  ggen-core's `domain::packs::install` logic (merged, not duplicated).
- `ggen-marketplace` gains a `packs::lockfile` module (`.ggen/packs.lock` format preserved
  from `/Users/sac/ggen/crates/ggen-core/src/packs/lockfile.rs`, 788 lines).
- `ggen_core::marketplace::*` re-export (`ggen-core/src/lib.rs:253-255`) cut; `ggen-cli`'s
  `cmds/policy.rs` re-pointed.
- `agent::emit_install_receipt` moved and pointed at
  `/Users/sac/ggen/crates/ggen-config/src/receipt/receipt_impl.rs`'s `Receipt`.
- The new `ggen-marketplace` dependency for `QuerySource::Pack` resolution added explicitly
  and documented (not an undocumented re-export).
- Dead stub `/Users/sac/ggen/crates/ggen-core/src/packs/install.rs:33-39` deleted, not
  moved.
