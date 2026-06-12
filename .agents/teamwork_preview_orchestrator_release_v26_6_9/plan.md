# Release v26.6.9 Project Plan

This plan outlines the milestones, strategies, and verification procedures required to release version `26.6.9` of the `ggen` repository.

## Milestones

### Milestone 1: Exploration & Planning
- **Goal**: Survey the repository, find all `Cargo.toml` files that need version updates (both `version` field and local workspace/dependency references), examine how `crates/ggen-graph/Cargo.toml` is structured, and locate `crates/ggen-marketplace/src/marketplace/atomic.rs` and the relevant metadata.
- **Agent**: `teamwork_preview_explorer` (3 parallel instances)
- **Output**: Detailed survey report mapping all version upgrade targets, existing `wasm4pm-compat` integration status, and a description of the marketplace atomic pack taxonomy structure.
- **Status**: IN_PROGRESS

### Milestone 2: Version Upgrade & Dependency Integration
- **Goal**: Upgrade workspace package version and workspace dependency versions to `26.6.9` in all `Cargo.toml` files. Integrate `wasm4pm-compat = { version = "26.6.9", path = "/Users/sac/wasm4pm-compat" }` as an active dependency in `crates/ggen-graph/Cargo.toml`.
- **Agent**: `teamwork_preview_worker`
- **Output**: File changes across all target `Cargo.toml` files.
- **Verification**: `teamwork_preview_reviewer` and `teamwork_preview_challenger`
- **Status**: PLANNED

### Milestone 3: Workspace Compilation & Quality Verification
- **Goal**: Verify compilation, tests, and clippy warnings.
  - Compile the entire workspace (`cargo build --all-targets`)
  - Run tests (`cargo test --all-targets`)
  - Run clippy (`cargo clippy --all-targets --all-features -- -D warnings`)
- **Agent**: `teamwork_preview_worker` and `teamwork_preview_reviewer`
- **Verification**: `teamwork_preview_auditor` (Forensic integrity audit)
- **Status**: PLANNED

### Milestone 4: Marketplace Taxonomy and Metadata Validation
- **Goal**: Verify that the `ggen-marketplace` package, its taxonomy structures (specifically atomic pack classifications in `crates/ggen-marketplace/src/marketplace/atomic.rs`), and metadata are correct and structurally ready for release.
- **Agent**: `teamwork_preview_worker` / `teamwork_preview_explorer`
- **Verification**: `teamwork_preview_reviewer` and `teamwork_preview_auditor`
- **Status**: PLANNED

---

## Code Layout (Target Release v26.6.9)
- Root `Cargo.toml`: sets `workspace.package.version = "26.6.9"`, and other workspace dependency versions to `26.6.9`.
- Sub-crates `Cargo.toml` (in `crates/*`): package version inherits workspace version (`version.workspace = true`) or is explicitly `26.6.9`. Dependencies to other workspace crates upgraded to `26.6.9` (or `version.workspace = true`).
- `crates/ggen-graph/Cargo.toml`: contains `wasm4pm-compat = { version = "26.6.9", path = "/Users/sac/wasm4pm-compat" }`.
- `crates/ggen-marketplace/src/marketplace/atomic.rs`: contains correct taxonomy classifications.

## Interface Contracts & Integrity Rules
- No mocks, stubs, or placeholder values.
- All dependencies must compile cleanly.
- Forensic Auditor must pass cleanly with ZERO integrity violations.
