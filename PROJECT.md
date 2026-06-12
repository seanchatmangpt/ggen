# Project: Ggen Release v26.6.9

## Architecture
- `ggen` is a deterministic, language-agnostic code generation framework.
- Core packages are in `crates/` including `ggen-core`, `ggen-cli`, `ggen-graph`, `ggen-marketplace`, etc.
- Dependencies between workspace crates are configured in the root `Cargo.toml` or individual crate `Cargo.toml` files.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Exploration & Survey | Discover version upgrade targets & marketplace structure | none | DONE |
| 2 | Version & Dependency Upgrades | Upgrade Cargo.tomls to v26.6.9 & integrate wasm4pm-compat | M1 | DONE |
| 3 | Verification (Build & Test) | Verify workspace builds, tests, clippy checks | M2 | DONE |
| 4 | Marketplace Verification | Validate ggen-marketplace taxonomy & metadata | M3 | DONE |

## Interface Contracts
### `ggen-graph` ↔ `wasm4pm-compat`
- `ggen-graph` integrates `wasm4pm-compat` as an active dependency, importing types and trait implementations.
- Path to `wasm4pm-compat` must be `/Users/sac/wasm4pm-compat`.
- Version of `wasm4pm-compat` must be `26.6.9`.

## Code Layout
- Root: `Cargo.toml`
- Crates: `crates/*`
- Coordinate Metadata: `.agents/teamwork_preview_orchestrator_release_v26_6_9/`
