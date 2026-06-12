# Handoff Report: Survey and Integration Recommendation for `wasm4pm-compat`

## 1. Observation

1. **`crates/ggen-graph/Cargo.toml` Current Structure**:
   Lines 22-25 contain the commented-out dependency on `wasm4pm-compat`:
   ```toml
   # GALL-CONFORM-001 Stage 0: pin wasm4pm-compat as the canonical type authority.
   # ggen is an OCEL *producer*; wasm4pm is the PM authority (per GGEN-NEEDS.md §0).
   # This dependency gates Stages 1-4 of the PM-retirement migration.
   # wasm4pm-compat = { version = "26.6.9", path = "/Users/sac/wasm4pm-compat" }
   ```

2. **`/Users/sac/wasm4pm-compat/Cargo.toml` Structure**:
   Declares package metadata and version:
   ```toml
   [package]
   name = "wasm4pm-compat"
   version = "26.6.9"
   ```
   And the following key dependencies:
   ```toml
   [dependencies]
   quick-xml = "0.36.0"
   blake3 = "1.8.5"
   chrono = { version = "0.4.45", features = ["serde"] }
   serde = { version = "1.0.228", features = ["derive"] }
   serde_json = "1.0"
   uuid = { version = "1.23.2", features = ["v4", "serde", "js"] }
   hashbrown = "0.17.1"
   rustc-hash = "2"
   ```

3. **`wasm4pm-compat` Nightly Requirement**:
   `/Users/sac/wasm4pm-compat/rust-toolchain.toml` defines the toolchain:
   ```toml
   [toolchain]
   channel = "nightly-2026-04-15"
   ```
   `/Users/sac/wasm4pm-compat/src/lib.rs` (lines 8-20 and 147-155) forces nightly features unconditionally at the root:
   ```rust
   //! ## Nightly requirement
   //!
   //! This crate **requires nightly Rust** unconditionally. The `rust-toolchain.toml`
   //! pins the toolchain to nightly. The following features are declared at the
   //! crate root with no cfg gate:
   //!
   //! - `generic_const_exprs` — law machinery and `WfNetConst<SOUNDNESS>`
   //! - `adt_const_params` — `ConditionCell<BITS>`, `Between01<NUM,DEN>`, and
   //!   `Metric<KIND,NUM,DEN>`
   //! - `const_trait_impl` — compile-time trait dispatch in law surfaces
   //! - `min_specialization` — type-law narrowing in `nightly_foundry`
   //! - `portable_simd` — SIMD-width type-law surface in `nightly_foundry`
   ```
   ```rust
   // ── Nightly features — unconditional (nightly toolchain required) ────────────
   #![feature(generic_const_exprs)]
   #![feature(adt_const_params)]
   #![feature(unsized_const_params)]
   #![feature(const_trait_impl)]
   #![feature(min_specialization)]
   #![feature(portable_simd)]
   ```

4. **`ggen` Workspace Default Toolchain**:
   Running `rustc --version` in `/Users/sac/ggen` returns:
   ```
   rustc 1.95.0 (59807616e 2026-04-14)
   ```
   Rustup shows the default active toolchain for the `ggen` workspace is `stable-aarch64-apple-darwin` (since there is no local toolchain configuration file). However, `nightly-2026-04-15-aarch64-apple-darwin` is already installed on the system.

## 2. Logic Chain

1. **Nightly Compiler Requirement**: Because `wasm4pm-compat` unconditionally uses unstable features that are not allowed by the stable compiler (e.g. `generic_const_exprs`), `wasm4pm-compat` can only be compiled using a nightly Rust compiler (specifically, `nightly-2026-04-15` or newer).
2. **Workspace Toolchain Cascade**: In a Cargo workspace, compiling a workspace member (`ggen-graph`) that has a path dependency on an external crate (`wasm4pm-compat`) forces that external crate to be compiled as part of the workspace's build graph.
3. **Toolchain Alignment**: If `ggen` is compiled using its current default stable toolchain (`rustc 1.95.0`), compilation will fail during the building of `wasm4pm-compat`. Therefore, the entire `ggen` workspace must be configured to compile using the same nightly toolchain channel (`nightly-2026-04-15`).
4. **Integration Best Practices**: Rather than adding the absolute path dependency inline in `crates/ggen-graph/Cargo.toml`, registering `wasm4pm-compat` under `[workspace.dependencies]` in the root `Cargo.toml` enforces consistency across all workspace members and follows the workspace's existing conventions.
5. **Transitive Dependency Version Alignment**: Several dependencies of `wasm4pm-compat` conflict in version with `ggen` workspace dependencies:
   - `blake3`: `1.5` vs `1.8.5`
   - `hashbrown`: `0.15` vs `0.17.1` (major version conflict)
   - `chrono`: `0.4` vs `0.4.45`
   - `serde`: `1.0` vs `1.0.228`
   - `uuid`: `1.18` vs `1.23.2`
   To prevent duplicate dependency compilations, these version declarations should ideally be unified at the workspace level.

## 3. Caveats

- We did not verify if the other crates in the `ggen` workspace (such as `ggen-core`, `ggen-cli`, etc.) compile cleanly under `nightly-2026-04-15`. It is assumed that they do, or that any minor nightly compiler issues (e.g., stricter warnings or borrow-check changes) can be resolved.
- We have not performed any code modifications (as per the read-only exploration constraints).

## 4. Conclusion

To successfully integrate `wasm4pm-compat` version 26.6.9:
1. **Toolchain configuration**: Create a root `rust-toolchain.toml` at `/Users/sac/ggen/rust-toolchain.toml` pinning the toolchain to the same nightly channel as `wasm4pm-compat`:
   ```toml
   [toolchain]
   channel = "nightly-2026-04-15"
   ```
2. **Workspace Registration**: Add the dependency inside `[workspace.dependencies]` in `/Users/sac/ggen/Cargo.toml`:
   ```toml
   wasm4pm-compat = { version = "26.6.9", path = "/Users/sac/wasm4pm-compat" }
   ```
3. **Crate Activation**: In `/Users/sac/ggen/crates/ggen-graph/Cargo.toml`, activate the dependency using the workspace inheritance syntax:
   ```toml
   wasm4pm-compat = { workspace = true }
   ```
4. **Dependency Deduplication**: Align the versions of `blake3`, `hashbrown`, `chrono`, `serde`, and `uuid` in `ggen`'s root `Cargo.toml` to match those of `wasm4pm-compat` to ensure build speed optimization and avoid duplicate crates in the build tree.

## 5. Verification Method

To verify the integration independently:
1. Make the changes outlined in the Conclusion.
2. In `/Users/sac/ggen`, run:
   ```bash
   cargo check --workspace --all-targets
   ```
   This will verify that the workspace compiles successfully under `nightly-2026-04-15` and resolves the new path dependency correctly.
