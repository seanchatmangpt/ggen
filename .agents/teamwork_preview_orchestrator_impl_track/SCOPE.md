# Scope: Implementation Track

## Architecture
- `ggen-projection` provides the core library for parsing `pack.toml`, resolving pack dependencies, managing projection maps, tracking customization points, signing and writing cryptographic receipts, and implementing the `sync` command staging gate.
- `ggen-lsp` implements the observer language server (LSP 3.17) that monitors the output directory of projected files (e.g., `examples/clap-noun-verb-lsp/`), watches filesystem changes, publishes `GGEN-*` diagnostics, and scans manual files for projection opportunities.
- `tower-lsp-max` composite LSP acts as the editor front-end, routing JSON-RPC queries to `ggen-lsp` and downstream LSPs (like `rust-analyzer`), combining diagnostics and inlay hints, and enforcing strict source attribution (`source_id: "ggen-lsp"`).
- Durable Packs (`ggen-pack-clap-noun-verb` and `ggen-pack-tower-lsp-max`) contain the templates, ontologies, and `pack.toml` configurations.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|---|---|---|---|
| 1 | Pack & Projection Core Model | Implement `PackDescriptor`, `PackPlan`, maps, receipts, wasm4pm process-evidence export, and ggen sync stage μ₀. | None | DONE |
| 2 | Durable Packs | Implement `ggen-pack-clap-noun-verb` and `ggen-pack-tower-lsp-max` and target file generation sync inside `examples/clap-noun-verb-lsp/`. | M1 | DONE |
| 3 | LSP Meta-Observer | Implement standard LSP 3.17 observer server in `ggen-lsp` publishing diagnostics (GGEN-PROJECTED-001 through GGEN-OVERRIDE-001 and GGEN-PROJECT-OPPORTUNITY-001). | M2 | DONE |
| 4 | Composite LSP & Routing | Implement composite LSP routing and inlay hints composition with source attribution (`source_id = "ggen-lsp"`) in `tower-lsp-max`. | M3 | DONE |

## Interface Contracts
### `ggen-projection` ↔ `ggen-lsp`
- `ggen-projection` writes `projection-map.json`, `customization-map.json`, and `receipts.jsonl` into target directories.
- `ggen-lsp` reads these files to monitor conformance and compute diagnostics.

### `tower-lsp-max` ↔ `ggen-lsp`
- `tower-lsp-max` routes requests to `ggen-lsp`.
- Diagnostic payloads returned from `ggen-lsp` must be merged/composed with others and must contain `source_id` attribute.

### `ggen-projection` ↔ `wasm4pm`
- Process evidence export formatting: Blake3 template and output hashes, causal version ID, cryptographic signatures. No placeholders.
