<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [External Projection and Validation Specification](#external-projection-and-validation-specification)
  - [Core Rule](#core-rule)
  - [Interop Boundaries](#interop-boundaries)
  - [Required Projection Table](#required-projection-table)
  - [Validation Classes](#validation-classes)
    - [1. RDF/N-Quads Expansion Validation](#1-rdfn-quads-expansion-validation)
    - [2. Public Vocabulary Survivability](#2-public-vocabulary-survivability)
    - [3. SHACL Validation](#3-shacl-validation)
    - [4. Relational Equivalence Validation](#4-relational-equivalence-validation)
    - [5. Graph Query Validation](#5-graph-query-validation)
    - [6. OCEL/Process Validation](#6-ocelprocess-validation)
    - [7. Audit/Provenance Validation](#7-auditprovenance-validation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# External Projection and Validation Specification

## Core Rule
**External validation does not create authority. It produces evidence.** External systems (like QLever, DuckDB, or SHACL tools) consume projections from `ggen` for validation, querying, and reporting. They never substitute for the Genesis pure foundation. `ggen` is the foundry and membrane that adapts and projects Genesis-bearing parts, while external engines act strictly as validators and consumers.

## Interop Boundaries

| Boundary | Owner | Input | Output | Proof | Replay | Refusal | Validation Path | Status |
|---|---|---|---|---|---|---|---|---|
| Graph Query Validation | `ggen` Projection Membrane | Genesis RelationPages | RDF/N-Quads, SPARQL Queries | Graph query results | Re-export N-Quads and re-run queries | Invalid graph topology or query failure | Validate via QLever/Oxigraph | [DOC_ONLY] |
| Relational Equivalence | `ggen` Projection Membrane | Genesis RelationPages | SQL DDL/DML, CSV | Query consistency checks | Re-export to DB and re-query | Schema mismatch or invariant violation | Validate via DuckDB/SQLite | [DOC_ONLY] |
| Structural Soundness | `ggen` Projection Membrane | Genesis Ontology Matter | Turtle/RDF | SHACL Validation Reports | Re-export Turtle and re-validate | SHACL constraint violations | Validate via SHACL tools | [DOC_ONLY] |
| Process & Lineage | `ggen` Projection Membrane | Genesis Audit Trail | OCEL 2.0 SQLite/JSON | Process Mining Conformance | Replay OCEL log through validators | Conformance deviation | Validate via wasm4pm/pictl | [DOC_ONLY] |
| Audit & Provenance | `ggen` Projection Membrane | Truex Receipt Chain | PROV/DCAT Reports | PROV compliance | Re-generate PROV documents | Corrupted receipt chain | Validate via PROV tools | [DOC_ONLY] |

## Required Projection Table

| Projection | Source from Genesis | ggen role | External format | Validator | Receipt binding | Replay path |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **RDF Expansion** | RelationPage Pair2 Tuples | Materialize IRIs and literals from dictionary | RDF / N-Quads / Turtle | RDF Tooling (Jena, RDF4J) | BLAKE3 of projected N-Quads | Re-read dictionary and Pair2 tuples |
| **Vocabulary** | Ontology / Metadata | Map internal dict to public URIs (`open-ontologies`) | RDF / OWL | Semantic Web Tools | BLAKE3 of mapping config | Re-run projection with versioned mapping |
| **SHACL Shapes** | Structural Constraints | Generate SHACL shapes from Genesis rules | SHACL / Turtle | SHACL engines (pyshacl, pyoxigraph) | BLAKE3 of SHACL file | Re-generate from Genesis constraints |
| **Relational Views**| RelationPage Pair2 Tuples | Flatten tuples to relational tables / CSV | SQL / CSV | DuckDB / SQLite | BLAKE3 of projected CSV/DB file | Re-flatten RelationPages |
| **Graph Query** | RelationPage Pair2 Tuples | Bulk export to triple format | N-Quads | QLever / Oxigraph | BLAKE3 of query results/export | Re-export and execute deterministic queries |
| **Process Mining** | Execution Trace / Audit | Extract object-centric events | OCEL 2.0 (SQLite/JSON) | `wasm4pm` / `pictl` / OCPQ | BLAKE3 of OCEL database | Re-extract from Genesis audit log |
| **Provenance** | Lifecycle Evidence | Format provenance and catalog records | PROV-O / DCAT | Audit / Compliance Tools | BLAKE3 of PROV document | Re-format from receipt chain |

## Validation Classes

### 1. RDF/N-Quads Expansion Validation
- **Description:** Ensures that the highly compressed, 2-byte Pair2 tuples within Genesis RelationPages are correctly expanded into standard RDF/N-Quads using the Genesis dictionary. Do not treat Pair2 as a compressed triple; it is a tuple in a predicate-fixed RelationPage.
- **Evidence:** Generated `.nq` files.
- **Status:** [DOC_ONLY] - Missing formal test suites for complete dictionary expansion validation.
- **Files/Modules:** `src/ostar/projection/rdf_exporter.rs` [MISSING]
- **Definition of Done:** 100% of valid Pair2 tuples can be exported to N-Quads and parsed by standard RDF tooling without error.

### 2. Public Vocabulary Survivability
- **Description:** Verifies that internal Genesis identifiers map correctly to public vocabularies via the `open-ontologies` checkpoint, ensuring long-term survivability and interoperability.
- **Evidence:** Mapped vocabulary files.
- **Status:** [DOC_ONLY]
- **Files/Modules:** `src/ostar/projection/vocabulary.rs` [MISSING]
- **Definition of Done:** All exposed predicates and entity types resolve to verified `open-ontologies` URIs.

### 3. SHACL Validation
- **Description:** Validates projected RDF graphs against structural constraints defined in SHACL, proving that Genesis data complies with expected schema shapes.
- **Evidence:** SHACL validation reports.
- **Status:** [DOC_ONLY]
- **Files/Modules:** `scripts/shacl_validation.py` [MISSING]
- **Definition of Done:** Automated SHACL validation script executes successfully on projected graphs with zero violations.

### 4. Relational Equivalence Validation
- **Description:** Proves that the Genesis data model can be faithfully represented and queried as a relational model.
- **Evidence:** DuckDB or SQLite database files and query execution results.
- **Status:** [DOC_ONLY]
- **Files/Modules:** `src/ostar/projection/relational_exporter.rs` [MISSING]
- **Definition of Done:** A defined set of standard queries returns identical logical results when executed natively vs. through DuckDB/SQLite on the projected data.

### 5. Graph Query Validation
- **Description:** Uses external, high-performance graph engines (QLever, Oxigraph) to validate the queryability and correctness of the projected triple data.
- **Evidence:** Query execution logs and result sets.
- **Status:** [DOC_ONLY]
- **Files/Modules:** `tests/integration/qlever_validation.sh` [MISSING]
- **Definition of Done:** Standard SPARQL benchmark suite passes on QLever/Oxigraph using the projected Genesis data.

### 6. OCEL/Process Validation
- **Description:** Projects Genesis execution and lifecycle receipts into OCEL 2.0 format to validate process conformance using `wasm4pm` and `pictl`.
- **Evidence:** OCEL 2.0 SQLite/JSON files, process mining conformance reports.
- **Status:** [DOC_ONLY]
- **Files/Modules:** `src/ostar/projection/ocel_exporter.rs` [MISSING], `tests/integration/wasm4pm_check.sh` [MISSING]
- **Definition of Done:** OCEL logs are fully parsable by `wasm4pm`/`pictl` and demonstrate 100% conformance to the expected lifecycle model.

### 7. Audit/Provenance Validation
- **Description:** Projects Truex receipt chains and execution histories into PROV and DCAT reports for external auditing.
- **Evidence:** PROV-O JSON-LD, DCAT catalogs.
- **Status:** [DOC_ONLY]
- **Files/Modules:** `src/ostar/projection/audit_reporter.rs` [MISSING]
- **Definition of Done:** Generated PROV and DCAT reports pass standard compliance checkers and accurately reflect the unbroken Truex receipt chain.
