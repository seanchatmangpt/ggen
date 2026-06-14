# Project: ggen Pack and Marketplace Refactoring

## Architecture
The `ggen` codebase contains a marketplace package `crates/ggen-marketplace` that implements installer, cache, composition receipt, policy, trust, security, and RDF/SPARQL mapping logic. It interacts with the `marketplace/` catalog containing JSON/RDF schemas, validation scripts, templates, and the registry indexer.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | R1: Resolve Critical Bugs & Vulnerabilities | Fix cache verification, path traversal Zip Slip, nested TOML metadata indexing | none | DONE |
| 2 | R2: Refactor Code Quality & Typestates | HashMap->BTreeMap, trust logic, dynamic registry class mapping, case-insensitive SPARQL check, check README file existence | M1 | IN_PROGRESS (explorers: eaafecc2-96b9-44f0-b37a-37c9c5580566, 67e83b7b-1428-489b-bcdc-2f7f2dffe787, 1680722b-547e-4f5d-869e-5d46599a60df) |
| 3 | R3: Metadata & Build Configuration | Ontology OWL repair, remove duplicates from index, check existing files in validate-docs, Makefile.toml tasks, enable unit tests | M2 | PLANNED |
| 4 | R4: Verification and Compiling | Clean compilation under --all-targets, verify all tests pass | M3 | PLANNED |

## Code Layout
- `crates/ggen-marketplace/src/marketplace/cache.rs` - Cache verification logic
- `crates/ggen-marketplace/src/marketplace/install.rs` - Zip extraction and installer
- `crates/ggen-marketplace/src/marketplace/composition_receipt.rs` - Composition receipts
- `crates/ggen-marketplace/src/marketplace/trust.rs` - Trust tiers
- `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs` - RDF mapper
- `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs` - SPARQL checks
- `crates/ggen-marketplace/src/marketplace/validation.rs` - README validation
- `marketplace/scripts/generate_registry_index.py` - Registry indexer
- `marketplace/ontology.ttl` - Ontology definition
- `marketplace/index.json` - Marketplace catalog index
- `marketplace/validate-docs.sh` - Docs validation script
- `Makefile.toml` - Cargo make targets
- `crates/ggen-marketplace/tests/` & `marketplace/packages/agent-editor/tests/` - Tests
