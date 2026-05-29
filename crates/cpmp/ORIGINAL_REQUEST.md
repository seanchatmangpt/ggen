# Original User Request

## Initial Request — 2026-05-27T12:09:08-07:00

Build the core layers of the `capability-map` Rust-based Code Capability Catalog CLI tool. It must scan directories, extract symbols and capabilities, store them in SQLite, compute cryptographic hashes/receipts for non-deletion verification, and project markdown/toml inventories.

Working directory: /Users/sac/capability-map
Integrity mode: development

## Requirements

### R1. DB & Projection Layers
- Implement SQLite DB integration using `rusqlite` creating the schema under `.capability-map/workspace.sqlite` containing: `repositories`, `files`, `symbols`, `dependencies`, `tests`, `docs`, `capabilities`, `patterns`, `classifications`, `receipts`, `scan_runs`.
- Implement projection generation for the following markdown and TOML reports:
  - `CAPABILITY_INVENTORY.md`
  - `PATTERN_ATLAS.md`
  - `LEGACY_NAME_MAP.md`
  - `DORMANT_CODE_REGISTER.md`
  - `BROKEN_BUT_REAL_REGISTER.md`
  - `TEST_EVIDENCE_MAP.md`
  - `DOC_CLAIM_MAP.md`
  - `NON_DELETION_RECEIPT.toml`

### R2. Symbol & Capability Detection Layer
- Implement `extract_symbols` and `detect_capabilities` using token/regex matching for the initial vocabularies: `Genesis`, `ggen`, `Truex`, `Receipt`, `Replay`, `Refusal`, `Construct8`, `Pair2`, `RelationPage`, `Need9`, `Need257`, `Shard`, `Segment`, `Corpus`, `O*`, `mu`, `AtomVM`, `Erlang`, `WASM`, `POWL`, `OCEL`, `PROV`, `SHACL`, `DCAT`, `Field8`, `Instinct8`, `Doctor`, `Wizard`, `Telco`.
- Map files/symbols to classifications: `LIVE`, `PARTIAL`, `CAPABILITY_SEED`, `LEGACY_NAME`, `DORMANT`, `BROKEN_BUT_REAL`, `DOC_ONLY`, `TEST_ONLY`, `AMBIGUOUS`.

### R3. CLI Interface and Verifiers
- Support commands:
  - `scan <paths...> --out <dir>`
  - `summary --db <sqlite>`
  - `capability <name> --db <sqlite>`
  - `patterns --db <sqlite>`
  - `symbols <query> --db <sqlite>`
  - `tests <query> --db <sqlite>`
  - `receipts --db <sqlite>`
  - `verify-no-deletion --before <receipt-a> --after <receipt-b>`

## Acceptance Criteria

### Execution & Build Check
- [ ] Code builds without errors or warnings via `cargo build`.
- [ ] Database is successfully initialized with all required tables.
- [ ] Command-line runs successfully with the `scan` and projection commands.
- [ ] Receipts compute BLAKE3 hashes correctly and verification command refuses if files are deleted.

## Follow-up — 2026-05-27T19:22:37Z

# CRITICAL ARCHITECTURAL PIVOT: PIVOT TO OPEN ONTOLOGIES DATASTORE

Please instruct the Project Orchestrator and Remediation Workers to immediately pivot the `capability-map` / `cpmp` implementation.

## Architectural Changes:
1. **Source Datastore Pivot:** `cpmp` must use `open-ontologies` (RDF graph store via Oxigraph, Turtle/N-Quads) as the primary catalog datastore, NOT `workspace.sqlite`.
2. **Role of SQLite:** SQLite must only be used as a local cache/acceleration layer, not the source of truth.
3. **Required outputs under `~/.cpmp/`:**
   - `~/.cpmp/catalog/cpmp-catalog.ttl`
   - `~/.cpmp/catalog/cpmp-shapes.ttl`
   - `~/.cpmp/reports/CAPABILITY_INVENTORY.md`
   - `~/.cpmp/reports/PROJECT_ATLAS.md`
   - `~/.cpmp/reports/PATTERN_ATLAS.md`
   - `~/.cpmp/receipts/scan-<timestamp>.receipt.toml`

## Required Open Ontologies Pipeline:
Scan → Project RDF graph → Validate/Load/SHACL via `open-ontologies` CLI/MCP (`onto_*` tools) → Query SPARQL → Generate reports and receipts.

Please apply the "Required agent prompt patch" to the current coding task context, update planning artifacts/checklists, and continue iteration.

## Follow-up — 2026-05-27T19:33:49Z

# CRITICAL UPDATE: IMPLEMENT GGEN ENTERPRISE MEMBRANE & CPMP CONTROL PLANE

Please instruct the Project Orchestrator and Remediation Workers to integrate the **cpmp Enterprise Graph Control Plane** specifications.

## Key Directives:
1. **Pivoted Architecture:**
   - `cpmp` discovers workstation features and project facts.
   - `ggen` acts as the enterprise membrane (normalizes, projects to public RDF, SHACL, receipts).
   - `open-ontologies` acts as the query/validation graph engine.
   - The wrapper adds enterprise control (auth, tenancy, policy packs, audit, backup, approval gates).
2. **Required Modules (Implement stubs & boundaries):**
   - `cpmp-enterprise-auth`, `cpmp-enterprise-tenancy`, `cpmp-enterprise-policy`, `cpmp-enterprise-audit`
   - `cpmp-open-ontologies-adapter`, `cpmp-ggen-projection`, `cpmp-public-vocabulary-firewall`
3. **Required Documentation:**
   - Create all files under `docs/enterprise/` as requested in the prompt, especially `docs/enterprise/GAP_CLOSURE_MATRIX.md` (covering namespace laundering, stale reports, illegal URIs, SHACL reports, etc.) and `docs/enterprise/ENTERPRISE_DEFINITION_OF_DONE.md`.
4. **CLI expansion:** Integrate the new noun-verb commands: `discover`, `graph project`, `graph validate`, `graph load`, `graph query`, `graph version`, `graph drift`, `policy check`, `policy enforce`, `tenant create/list`, `audit lineage`, `receipt emit/verify-no-deletion`, and `enterprise doctor`.

Please update planning artifacts/checklists and execute immediately.
