# Project: 80/20 Projection Core and Pack LSPs (ggen Projection Intelligence)

## Mission
Establish a deterministic, language-agnostic code generation framework where software artifacts are treated as projections of knowledge graphs, complete with durable versioned packs, a meta-observer LSP server (`ggen-lsp`), and a composite routing LSP server (`tower-lsp-max`).

---

## Architecture

The project consists of three core components, durable template packs, and output targets:

1. **`ggen-projection` (Core Library)**
   - Implements the foundational data structures for projection management:
     - `PackDescriptor`: Repesentation of the `pack.toml` file containing metadata, dependencies, projection signatures, and customization points.
     - `PackPlan`: The resolved dependency and synchronization execution path for applying packs.
     - `ProjectionMap`: Explicit map tracking generated files, code blocks, source templates, and target ranges.
     - `CustomizationMap`: Tracks incomplete, completed, and user-provided customization slots.
     - `ReceiptIndex`: Cryptographically signs and indexes boundary-crossing events (materialization, validation).
   - Coordinates the staging/sync write gate inside the `ggen` pipeline to ensure no files are overwritten without verification or explicit override.

2. **`ggen-lsp` (Language Server Observer)**
   - Acts as an LSP 3.17 meta-observer server that monitors the output directory of projected files.
   - Computes and publishes diagnostics to track boundary compliance:
     - `GGEN-PROJECTED-001`: Inlay hints or diagnostics marking a file or range as projected and managed by a pack.
     - `GGEN-DRIFT-001`: Fired when a projected file/range differs from the template's expected output.
     - `GGEN-EVIDENCE-001`: Fired when a projected file lacks a valid cryptographic projection receipt.
     - `GGEN-CUSTOMIZE-001`: Fired when a required customization point is missing or incomplete in the code.
     - `GGEN-OVERRIDE-001`: Fired when a manual override is detected without a corresponding signed receipt.
   - Detects projection opportunities (`GGEN-PROJECT-OPPORTUNITY-001` / `CouldBeProjected` / `ShouldBeProjected`) by analyzing manual code signatures against known pack patterns.

3. **`tower-lsp-max` (Composite LSP)**
   - Acts as the outer JSON-RPC proxy/multiplexer for editor integration.
   - Exposes dynamic Pack LSP surfaces (declarative or typed providers) for both packs.
   - Composes diagnostics, code actions, and inlay hints from multiple LSP sources (such as `ggen-lsp`, pack-specific LSPs, and downstream language servers like `rust-analyzer`).
   - Enforces strict source attribution, embedding the `source_id` field within diagnostics to prevent anonymous merges.

4. **Durable Packs**
   - `ggen-pack-clap-noun-verb`: A template pack defining declarative CLI structures.
   - `ggen-pack-tower-lsp-max`: A template pack representing the language server structure, inheriting from `ggen-pack-clap-noun-verb`.
   - Both packs contain `pack.toml` files, SPARQL query files, and Tera templates.

---

## Milestones

| Milestone | Name | Scope | Dependencies | Status |
| :---: | --- | --- | :---: | :---: |
| **M1** | Scaffolding & Setup | Activate `ggen-projection` in workspace, configure Cargo dependencies, and set up the global test framework. | None | PLANNED |
| **M2** | Core Projection Model | Implement `PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, `ReceiptIndex`, and sync logic in `ggen-projection`. | M1 | PLANNED |
| **M3** | Durable Pack Proving | Define `ggen-pack-clap-noun-verb` and `ggen-pack-tower-lsp-max` metadata and verify projection sync into `examples/clap-noun-verb-lsp/`. | M2 | PLANNED |
| **M4** | LSP Meta-Observer | Implement standard LSP 3.17 handlers and publish diagnostics (`GGEN-PROJECTED-001` through `GGEN-OVERRIDE-001` and opportunity scans) in `ggen-lsp`. | M3 | PLANNED |
| **M5** | Composition Layer | Extend `tower-lsp-max` as a multi-surface router that merges and attributes diagnostics and hints from `ggen-lsp` and `rust-analyzer`. | M4 | PLANNED |
| **M6** | E2E Testing & Hardening | Complete Tier 1-4 tests, execute bypass-kill validation, run Tier 5 adversarial tests, and obtain Forensic Auditor approval. | M5 | PLANNED |

---

## Interface Contracts

### 1. `ggen-projection` ↔ `ggen-lsp` (Data Exchange Contract)
- `ggen-lsp` does not query the in-memory state of `ggen-projection` directly.
- Instead, `ggen-projection` materializes `projection-map.json`, `customization-map.json`, and `receipts.jsonl` into the target project output directory during the `sync` command execution.
- `ggen-lsp` parses these files to reconstruct the boundary model and compare the live filesystem state against the expected projection shape.

### 2. `tower-lsp-max` ↔ `ggen-lsp` (Composition Proxy Contract)
- `tower-lsp-max` acts as a composite language server. When a client requests diagnostics or hints, it forwards the request to `ggen-lsp` and `rust-analyzer`.
- Diagnostic payloads returned from upstream components must be attributed by appending source identifiers:
  ```json
  {
    "source": "ggen-lsp",
    "data": {
      "source_id": "ggen_lsp_observer"
    }
  }
  ```
- If `source_id` is missing or stripped, the composition layer must reject the merge and fail the boundary contract.

### 3. `ggen-projection` ↔ `wasm4pm` (Process Evidence Export Contract)
- Receipts generated in `receipts.jsonl` must follow the process-evidence shape.
- Every receipt must bind:
  1. The Blake3 hash of the input templates and ontologies.
  2. The Blake3 hash of the generated code.
  3. The causal version ID of the generator.
  4. The cryptographic signature of the generator subagent to prove origin.
- Placeholders, empty hashes, or mock strings (e.g. `"hash_placeholder"`) violate the contract and will be rejected.

---

## Code Layout

The source code and artifacts are structured as follows across the workspaces:

```
/Users/sac/ggen/
├── crates/
│   ├── ggen-projection/
│   │   ├── src/
│   │   │   ├── lib.rs            # Entrypoint
│   │   │   ├── descriptor.rs     # pack.toml schema and parser (PackDescriptor)
│   │   │   ├── plan.rs           # Dependency graph and sync planner (PackPlan)
│   │   │   ├── mapping.rs        # ProjectionMap and CustomizationMap serialization
│   │   │   ├── receipt.rs        # ReceiptIndex and cryptographic signing
│   │   │   └── pipeline.rs       # Staging/sync write gate
│   │   └── tests/                # Integration tests (Tier 1 & 2)
│   │
│   ├── ggen-lsp/
│   │   ├── src/
│   │   │   ├── lib.rs            # Entrypoint
│   │   │   ├── observer.rs       # Boundary observer and directory watcher
│   │   │   ├── diagnostics.rs    # Publication of GGEN-* diagnostic codes
│   │   │   └── opportunity.rs    # Signature pattern scanner for CouldBeProjected
│   │   └── tests/                # Integration tests (Tier 1 & 2)
│   │
│   ├── ggen-pack-clap-noun-verb/ # Durable metadata, queries, and templates
│   │   ├── pack.toml             # Signatures and customization points
│   │   └── templates/            # CLI noun-verb templates
│   │
│   └── ggen-pack-tower-lsp-max/  # Inherited pack metadata and templates
│       ├── pack.toml             # Depends on ggen-pack-clap-noun-verb
│       └── templates/            # LSP runtime templates
│
└── examples/
    └── clap-noun-verb-lsp/       # Generated target project output
        ├── projection-map.json   # Output mappings
        ├── customization-map.json# Output customization points
        └── receipts.jsonl        # Cryptographic process-evidence receipts
```
