# Project: rustlang-ontology

## Architecture
The `rustlang-ontology` is a semantic specification for representing Rust codebases as manufacturable RDF structures. The project provides:
1. **Schema Definitions (schema/):** RDF/TTL ontology mapping Rust workspaces, crates, modules, items, traits, generics, visibility, attributes, verification, and documentation.
2. **SHACL Shapes (hooks/ / schema/):** SHACL shapes to validate instance data, ensuring all necessary metadata (names, paths, types) are provided.
3. **SPARQL Projections (queries/):** SPARQL queries to extract facts from the ontology and project them into parameters for templates.
4. **Tera Templates (templates/):** Tera templates rendering Rust source files (Cargo.toml, lib.rs, modules, items, tests, receipts).
5. **E2E Test Runner (tests/):** A python-based or cargo-based test harness that validates the schema using SHACL, executes SPARQL queries over sample graphs, renders the templates, and verifies the generated code compiles and tests cleanly.

All files are created under the project directory `~/teamwork_projects/rustlang_ontology`.

## Code Layout
```text
~/teamwork_projects/rustlang_ontology/
├── schema/
│   ├── rustlang-ontology.ttl         # Core classes, properties, and definitions
│   └── rustlang-shapes.ttl           # SHACL shapes for code elements
├── queries/
│   ├── workspace.rq                  # Projects workspace configurations
│   ├── crate.rq                      # Projects crate and targets configuration
│   ├── module.rq                     # Maps module structure
│   └── items.rq                      # Maps functions, structs, traits, impls
├── templates/
│   ├── Cargo.toml.tera               # Crate configuration template
│   ├── lib.rs.tera                   # Crate entrypoint template
│   ├── module.rs.tera                # General module template
│   ├── items.rs.tera                 # Items rendering template (structs, traits)
│   └── receipt.tera                  # Lineage proof receipt template
├── tests/
│   ├── fixtures/
│   │   └── sample-workspace.ttl      # Sample ontology instance of a Rust system
│   ├── validate.py                   # Python validation runner
│   └── test_runner.sh                # Executable launcher for tests
└── docs/
    └── DESIGN.md                     # Documentation of the ontology design
```

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|---|---|---|---|
| M1 | E2E Test Suite | Create validation scripts, sample workspace TTL fixture, and verification tests | None | PLANNED |
| M2 | Ontology Design (TTL) | Complete schema definitions for all Rust construct layers in `rustlang-ontology.ttl` | M1 | PLANNED |
| M3 | SHACL Validation Shapes | Define and verify SHACL validation constraints in `rustlang-shapes.ttl` | M2 | PLANNED |
| M4 | SPARQL Projections | Write SPARQL queries mapping ontology facts to template parameters | M3 | PLANNED |
| M5 | Tera Templates | Implement Cargo.toml, lib.rs, module, item, and receipt templates | M4 | PLANNED |
| M6 | End-to-End Validation | Run full pipeline over sample workspace, verify compilation and tests | M5 | PLANNED |

## Interface Contracts
### Ontology Instance ↔ SPARQL Queries ↔ Tera Templates
- **Ontology Namespace:** Only standard public vocabularies (`rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`) are permitted in instance URIs/properties. Banned custom namespaces.
- **SPARQL Output:** SPARQL queries must return variables matching the exact Tera template parameter bindings (e.g. `?name`, `?path`, `?dependencies`).
- **Tera Input:** Templates consume JSON structure containing bindings returned by the SPARQL queries.
- **Receipt Schema:** Every generated file must include a receipt comment with source TTL entity, template path, query hash, output file hash, timestamp, and verification command.
