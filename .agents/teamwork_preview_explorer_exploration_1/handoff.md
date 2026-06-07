# Handoff Report: Projection Core and Pack LSPs Exploration

## 1. Observation
- **Gpack Manifest & conventions**: Located in `crates/ggen-core/src/gpack.rs`.
  - Line 123: `pub struct GpackManifest`
  - Line 96: `pub struct PackConventions`
  - Glob conventions define template patterns: `&["templates/**/*.tmpl", "templates/**/*.tera"]`
- **Domain pack structures**: Located in `crates/ggen-core/src/domain/packs/types.rs`.
  - Line 9: `pub struct Pack`
  - Line 103: `pub struct PackFile`
- **Resolver**: Located in `crates/ggen-core/src/pack_resolver.rs`.
  - Line 171: `pub struct PackResolver`
  - Line 232: `pub fn resolve(&self) -> Result<ResolvedPacks>`
  - Lockfile is loaded from `.ggen/packs.lock` and parsed via `packs::lockfile::PackLockfile`.
- **LSP Diagnostics**:
  - `crates/ggen-lsp/src/server.rs`: Line 34: `async fn refresh_analyzer(&self, uri: &Url, content: &str)` calls `ServerState::analyze_and_observe`.
  - `crates/ggen-lsp/src/state.rs`: Line 484: `pub async fn analyze_and_observe(&self, uri: &Url, content: &str) -> Vec<(Url, Vec<Diagnostic>)>` runs the pipeline:
    - Line 505: `build_analyzer` constructs the single-file analyzer.
    - Line 520: `detect_tpl_001_for` (GGEN-TPL-001)
    - Line 529: `detect_harness_001_for` (GGEN-HARNESS-001)
    - Line 540: `detect_out_001_for` (GGEN-OUT-001)
    - Line 551: `detect_rule_001_for` (GGEN-RULE-001)
  - `crates/ggen-lsp/src/check.rs`: Line 321: `pub fn check_files_in_root` performs headless diagnostics and folds cross-surface results (`fold_tpl_001`, `fold_harness_001`, `fold_out_001`, `fold_rule_001`).
- **tower-lsp-max Crate Layout**:
  - `crates/tower-lsp-max-base/src/abstractions.rs` (traits `SourceObservation`, `ParseIngress`, `RelationAdmitter`, `StaticIndexEmitter`).
  - `tower-lsp-max-protocol/src/diagnostics.rs`: Line 52: `pub struct MaxDiagnostic` implements custom LSP attributes.
  - `tower-lsp-max-runtime/src/control_plane/views/update.rs`: Line 22: `pub fn update_views` updates definitions, references, hovers, and diagnostics.
  - `tower-lsp-max-runtime/src/control_plane/views/populate_hover_diag.rs`: Line 101: `pub(super) fn populate_diagnostics` populates LSP and live control-plane diagnostics.
  - `tower-lsp-max-runtime/src/control_plane/views/types.rs`: Line 61: `pub struct MaterializedViewStore` defines the storage maps.
  - `/Users/sac/ggen/PROJECT.md` details the layout and milestones for implementing ggen Projection Intelligence.

## 2. Logic Chain
- Since `GpackManifest` handles manifest parsing and `PackResolver` resolves packs topologically from `.ggen/packs.lock`, the introduction of `PackPlan` and `PackDescriptor` should be integrated directly into the `codegen::executor` and `pack_resolver` pipeline (Stage μ₀) to enforce and validate pack configurations before constructing code (F1, F2).
- Since `ServerState::analyze_and_observe` in `ggen-lsp` and `check_files_in_root` in `check.rs` orchestrate diagnostics, we can implement the new projection-specific diagnostics (`GGEN-PROJECTED-001`, `GGEN-DRIFT-001`, `GGEN-EVIDENCE-001`, `GGEN-CUSTOMIZE-001`, `GGEN-OVERRIDE-001`) by registering a new `Species` in the consolidation loop of Phase A/C and folding them in `check.rs` (F4).
- Since `tower-lsp-max` delegates view updates to Oxigraph and maintains diagnostics in a `MaterializedViewStore` DashMap, `tower-lsp-max` can compose `ggen-lsp` diagnostics by invoking `ggen-lsp`'s headless check or querying the generated JSON maps, then translating them to `MaxDiagnostic` with `source_id = "ggen-lsp"` and pushing them to the store (F3).

## 3. Caveats
- No actual code changes have been executed as this is a read-only investigation.
- We assume that `tower-lsp-max` and `ggen` use a unified version/snapshot tracking scheme, otherwise version mismatch checks (e.g. stale response dropping) will need explicit mapping.

## 4. Conclusion
- The pack and resolver architectures are ready for Projection Intelligence integration.
- The new projection structures (`PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, `ReceiptIndex`) should be placed in `crates/ggen-projection/` or integrated as a new submodule in `ggen-core`.
- Integrating `ggen-lsp` diagnostics into `tower-lsp-max` is fully supported by injecting them into the `MaterializedViewStore`.

## 5. Verification Method
- Independent verification can be performed by running the test suites of both projects:
  - In `~/ggen`: Run `cargo test` to ensure existing gpack parsing and LSP checks remain intact.
  - In `~/tower-lsp-max`: Run `cargo test` to verify the mesh runtime and views updater.
