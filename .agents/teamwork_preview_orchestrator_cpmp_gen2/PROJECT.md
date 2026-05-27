# Project: capability-map (cpmp)

## Architecture
`cpmp` is an enterprise capability surveyor that discovers projects, files, capabilities, tests, docs, and runtimes, normalizes/projects/validates them using public vocabulary RDF representations, and integrates with `open-ontologies` for audit, query, versioning, and policy enforcement.

```
[Filesystem Scan]
       │
       ▼ (read-only)
[cpmp scanner] ──(computes BLAKE3)──► [scan-<timestamp>.receipt.toml]
       │
       ▼ (materialize RDF)
[cpmp RDF projection] ──► cpmp-catalog.ttl / cpmp-catalog.nq / cpmp-shapes.ttl
       │
       ▼ (integrate & query)
[open-ontologies Store] ──► Validation Reports, Versioning, and Drift Detection
       │
       ▼ (query results)
[SPARQL Reports] ──► CAPABILITY_INVENTORY.md, PROJECT_ATLAS.md, PATTERN_ATLAS.md
```

## Code Layout
- `src/main.rs`: CLI Entrypoint and commands implementation
- `src/lib.rs`: Library module declarations
- `src/scanner.rs`: Filesystem scanning logic
- `src/rdf.rs`: Turtle/N-Quads RDF generation using public vocabularies
- `src/receipt.rs`: Receipt generation and no-deletion verification
- `src/gates.rs`: Scanner pipeline gates and open-ontologies verification
- `src/policy.rs`: Policy enforcement and validation reporting
- `src/projection.rs`: Projection membrane
- `src/models.rs`: Data structures and serialization
- `docs/enterprise/`: Enterprise documentation path
- `tests/`: Integration tests

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Codebase & Test Audit | Run tests, audit files, verify Open Ontologies toolchain | None | IN_PROGRESS |
| 2 | Enterprise Documentation | Write the required enterprise docs under docs/enterprise/ | M1 | PLANNED |
| 3 | CLI Command Complete | Implement / finish all CLI nouns & commands | M1 | PLANNED |
| 4 | Enterprise Module Stubs | Create stubs for the 12 cpmp-enterprise modules | M1 | PLANNED |
| 5 | Scan Pipeline Gates | Implement/enforce the 8 scan refusal gates and Open Ontologies checks | M3, M4 | PLANNED |
| 6 | Verification & Adjudication | Run full validation, compile cleanly, pass tests | M2, M5 | PLANNED |

## Interface Contracts
- `scanner::scan(paths: &[PathBuf], out_dir: &Path) -> Result<ScanReceipt>`
- `rdf::build_and_emit(entries: &[FileEntry], caps: &[Capability], receipt: &ScanReceipt, out_dir: &Path) -> Result<()>`
- `receipt::generate_receipt(root: &Path, entries: &[FileEntry]) -> Result<ScanReceipt>`
- `receipt::verify_no_deletion(before: &ScanReceipt, after: &ScanReceipt) -> NoDeletionReport`
- `gates::run_admission_gates(catalog_dir: &Path, receipt: &ScanReceipt) -> Result<String>`
