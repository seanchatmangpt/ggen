# E2E Test Infra: rustlang-ontology

## Test Philosophy
- Opaque-box, requirement-driven.
- Enforces public-vocabulary-only constraints (no laundering, no private RDF namespaces).
- Proves correctness by loading code specifications from RDF/TTL, validating with SHACL, executing SPARQL queries to project facts, rendering via Tera templates, and running cargo check/test on the output.

## Feature Inventory
| # | Feature | Source (requirement) | Tier 1 | Tier 2 | Tier 3 |
|---|---------|---------------------|:------:|:------:|:------:|
| 1 | Public Vocabulary Schema | R1 | 5      | 5      | ✓      |
| 2 | SHACL Schema Constraints | R2 | 5      | 5      | ✓      |
| 3 | SPARQL Fact Projections | R3 | 5      | 5      | ✓      |
| 4 | Tera Code Generation     | R4 | 5      | 5      | ✓      |

## Test Architecture
- **Test Runner:** `tests/validate.py` (Python script using `rdflib` and `pyshacl` or open-ontologies tools).
- **Test Entry:** `tests/test_runner.sh` which executes the Python validator.
- **Fixtures:** `tests/fixtures/sample-workspace.ttl` containing a complete Turtle graph describing a multi-crate Rust workspace with dependencies, lib/bin targets, enums, structs, traits, functions, tests, and documentation.
- **Output Directory:** `tests/output/` (directory where the workspace is rendered and checked by cargo).

## Real-World Application Scenarios (Tier 4)
- **Scenario 1: Greenfield Multi-Crate Workspace:** Generation of a workspace with two crates: `core-lib` (library target, containing a trait and structs) and `cli-app` (binary target, depending on `core-lib` and implementing command line interface).
- **Scenario 2: Robust Error Handling & Refusal:** Sabotaged TTL graphs (e.g. function without a name, template without a projection query) must fail SHACL validation, producing the expected `sh:ValidationReport` with `sh:conforms false`.

## Coverage Thresholds
- Tier 1: >= 5 validation tests covering ontology entities (workspace, package, module, item, function).
- Tier 2: >= 5 boundary cases (empty workspace, missing dependencies, type parameter bounds, default generics).
- Tier 3: Pairwise combination of feature gates, cfg targets, and generics.
- Tier 4: At least 2 full end-to-end workspaces generated, compiled, and tested.
