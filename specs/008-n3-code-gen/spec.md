# Feature Specification: ggen v5 - Unified Sync Command

**Feature Branch**: `008-n3-code-gen`
**Created**: 2024-12-14
**Status**: Complete
**Input**: User description: "ggen sync as the single command for ggen v5. Remove all other commands. Fresh start with ontology → CONSTRUCT → code generation."

---

## Executive Summary

ggen v5 is a **fresh start** with a single command: `ggen sync`. This replaces ALL existing commands (generate, template, project, graph, ontology, marketplace, ai, test, utils, ci, workflow) with one unified code synchronization pipeline.

**The Vision**: `ggen sync` reads `ggen.toml`, loads ontology, executes inference rules (CONSTRUCT), generates code via templates, and writes files. One command. One flow. Complete determinism.

**Breaking Change**: All existing commands are removed. v5 is `ggen sync` only.

---

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Domain Model to Rust Code (Priority: P1)

A developer creates a domain ontology in Turtle format describing their entities (User, Order, Product). They run `ggen sync` and receive complete, production-ready Rust structs with derives, traits, and impl blocks - all inferred from semantic annotations in the ontology.

**Why this priority**: This is the core value proposition. A single ontology file produces thousands of lines of idiomatic Rust code. Everything depends on this working.

**Independent Test**: Create a `domain.ttl` with 3 entities, run `ggen sync`, verify output contains correct Rust structs with `#[derive(...)]`, field types, and documentation.

**Acceptance Scenarios**:

1. **Given** a Turtle file with `:User a rdfs:Class ; :codegen-as "struct"`, **When** `ggen sync` is executed, **Then** output contains `pub struct User { ... }` with correct derive macros.

2. **Given** an entity with `:auditable true` annotation, **When** sync runs, **Then** `created_at` and `updated_at` fields are automatically added via CONSTRUCT inference.

3. **Given** a struct with a `Uuid` field type, **When** sync runs, **Then** `Serialize` and `Deserialize` derives are automatically added via CONSTRUCT rule inference.

---

### User Story 2 - Relationship-Driven Impl Generation (Priority: P1)

A developer defines relationships between entities (`:User :has_many :Order`) in the ontology. CONSTRUCT queries automatically generate impl blocks with accessor methods (`get_orders()`) and relationship-aware code patterns.

**Why this priority**: Relationships are the heart of domain modeling. Auto-generating relationship code eliminates boilerplate and ensures consistency.

**Independent Test**: Define `:User :has_many :Order` and `:Order :belongs_to :User`, run `ggen sync`, verify impl blocks with `get_orders(&self) -> Vec<Order>` and `get_user(&self) -> User`.

**Acceptance Scenarios**:

1. **Given** `:Entity1 :has_many :Entity2`, **When** CONSTRUCT executes via `ggen sync`, **Then** output includes `impl Entity1 { pub fn get_entity2s(&self) -> Vec<Entity2> }`.

2. **Given** `:Entity :soft_delete true`, **When** sync runs, **Then** `deleted_at: Option<DateTime<Utc>>` field is inferred and added.

---

### User Story 3 - ggen.toml Manifest-Driven Sync (Priority: P1)

A developer creates a `ggen.toml` manifest specifying ontology sources, SPARQL queries, templates, and output paths. Running `ggen sync` executes the entire pipeline from manifest.

**Why this priority**: The manifest is the single source of truth for code generation. Without it, the system is unusable.

**Independent Test**: Create minimal ggen.toml with `[ontology]` and `[[generation.rules]]`, run `ggen sync`, verify output files exist.

**Acceptance Scenarios**:

1. **Given** `ggen.toml` with `source = "domain/model.ttl"`, **When** `ggen sync` runs, **Then** ontology is loaded from specified path.

2. **Given** `[[generation.rules]]` with `query`, `template`, `output_file`, **When** sync runs, **Then** SPARQL executes, template renders, file is written.

3. **Given** `ggen sync --dry-run`, **When** executed, **Then** shows what would be generated without writing files.

---

### User Story 4 - CONSTRUCT-Based Code Inference (Priority: P2)

A developer writes CONSTRUCT rules in ggen.toml that encode project conventions (e.g., "if trait has async methods, require async_trait macro"). The rules apply before code generation, ensuring all inferred patterns are included.

**Why this priority**: CONSTRUCT rules enable "convention over configuration" at the semantic level. This differentiates ggen from template-based generators.

**Independent Test**: Create CONSTRUCT rule for auto-deriving `PartialEq` on structs with more than 5 fields, verify derive appears in generated struct.

**Acceptance Scenarios**:

1. **Given** an inference rule for auto-deriving `PartialEq` on large structs, **When** a 7-field struct is defined, **Then** `PartialEq` derive is added without explicit annotation.

2. **Given** custom CONSTRUCT rules in `[[inference.rules]]`, **When** `ggen sync` runs, **Then** all rules are applied before generation rules execute.

---

### User Story 5 - CONSTRUCT Query Composition (Priority: P2)

A developer chains multiple CONSTRUCT queries to progressively build the code graph. First CONSTRUCT creates struct definitions, second adds impl blocks, third generates tests - all operating on the same semantic graph.

**Why this priority**: Composable transformations enable complex generation without monolithic queries. Each CONSTRUCT is testable independently.

**Independent Test**: Define 3 CONSTRUCT queries in ggen.toml, verify each produces expected graph additions, verify final code includes all generated elements.

**Acceptance Scenarios**:

1. **Given** CONSTRUCT queries for structs, impls, and tests in `[[inference.rules]]`, **When** `ggen sync` executes, **Then** all three code graph types are generated in dependency order.

2. **Given** a CONSTRUCT query with `BIND(IRI(CONCAT(...)))`, **When** executed, **Then** new IRIs are correctly generated without collision.

---

### User Story 6 - Poka-Yoke Safety Validation (Priority: P2)

Before any code is written to disk, CONSTRUCT queries validate the code graph for semantic errors (missing types, invalid relationships, unsafe patterns). Generation fails fast with semantic error messages.

**Why this priority**: Agent-driven code generation requires deterministic safety. SHACL + CONSTRUCT validation prevents invalid code from being written.

**Independent Test**: Create ontology with missing `:fieldType`, run `ggen sync`, verify exit code 1 (ValidationError) with clear error message.

**Acceptance Scenarios**:

1. **Given** a struct field without `:fieldType`, **When** validation runs via `ggen sync`, **Then** sync fails with "Field missing type annotation" error.

2. **Given** `no_unsafe = true` in ggen.toml, **When** generated code contains `unsafe`, **Then** output validation fails with exit code 4.

---

### User Story 7 - Audit Trail for Agent Verification (Priority: P3)

After sync completes, an `audit.json` file is created containing: input ontology hash, SPARQL queries executed, templates rendered, and validation status. AI agents can verify generation determinism.

**Why this priority**: Agent-safe code generation requires verifiable reproducibility. The audit trail enables automated verification.

**Independent Test**: Run `ggen sync --audit` twice with same inputs, verify both `audit.json` files have identical content except timestamps.

**Acceptance Scenarios**:

1. **Given** `require_audit_trail = true` in ggen.toml, **When** sync completes, **Then** `audit.json` is written with all pipeline steps.

2. **Given** same ontology and rules, **When** sync runs on different machines, **Then** output code is byte-for-byte identical.

---

### Edge Cases

- What happens when SPARQL query returns empty results? (Generate empty file or skip based on `skip_empty` config)
- What happens when ontology file does not exist? (Exit code 2 with "Ontology not found: path" message)
- How does system handle malformed Turtle syntax? (Parse error with line number, exit code 2)
- What happens when template references non-existent SPARQL variable? (Template error with variable name, exit code 4)
- How does system handle CONSTRUCT queries that produce duplicate triples? (Deduplicate automatically via RDF semantics)
- What happens when ggen.toml is missing required fields? (Validation error listing missing fields, exit code 1)

---

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System SHALL provide `ggen sync` as the ONLY top-level command
- **FR-002**: System MUST load RDF ontologies from Turtle (.ttl), N-Triples (.nt), and RDF/XML (.rdf) formats
- **FR-003**: System MUST execute SPARQL SELECT queries against loaded ontology graphs
- **FR-004**: System MUST execute SPARQL CONSTRUCT queries for graph enrichment (inference rules)
- **FR-005**: System MUST serialize code graphs to Rust source files via Tera templates
- **FR-006**: System MUST provide semantic exit codes (0=success, 1=manifest, 2=ontology, 3=sparql, 4=template, 5=io, 6=timeout)
- **FR-007**: System MUST generate deterministic output (same input produces same output, byte-for-byte)
- **FR-008**: System MUST parse ggen.toml manifests with `[project]`, `[ontology]`, `[inference]`, `[generation]`, `[validation]` sections
- **FR-009**: System MUST support `--dry-run` flag to preview changes without writing
- **FR-010**: System MUST support `--watch` flag for continuous regeneration on file changes
- **FR-011**: System MUST support `--audit` flag to generate audit.json
- **FR-012**: System MUST support `--validate-only` flag to validate without generating
- **FR-013**: System MUST timeout SPARQL queries exceeding configured `max_sparql_timeout_ms`
- **FR-014**: System MUST support frontmatter `sparql:` queries in templates for graph-driven context
- **FR-015**: System MUST validate generated Rust code syntax when `validate_syntax = true`
- **FR-016**: System MUST reject generated code containing `unsafe` when `no_unsafe = true`
- **FR-017**: System MUST remove ALL legacy commands (generate, template, project, graph, ontology, marketplace, ai, test, utils, ci, workflow)

### Key Entities

- **ggen.toml Manifest**: TOML configuration specifying the complete sync pipeline
- **Ontology Graph**: In-memory RDF triple store containing domain model, loaded via oxigraph
- **Code Graph**: Intermediate RDF representation of generated code using code ontology vocabulary
- **CONSTRUCT Query**: SPARQL query that builds new RDF graphs from existing data (inference rules)
- **Tera Template**: Text template that serializes code graph to Rust source files
- **Audit Trail**: JSON record of sync execution for verification

---

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can sync complete Rust crate from ontology in under 5 seconds for typical domain models (10-50 entities)
- **SC-002**: System produces identical output across runs (100% determinism verified by comparing output hashes)
- **SC-003**: CONSTRUCT inference reduces explicit annotations by 60% compared to annotation-only approach
- **SC-004**: Generated code compiles successfully on first attempt (0 syntax errors)
- **SC-005**: Audit trail enables automated verification of sync reproducibility
- **SC-006**: Users can add new code patterns by writing CONSTRUCT rules without modifying ggen source
- **SC-007**: CONSTRUCT query composition enables incremental pipeline development (each query testable independently)
- **SC-008**: System handles enterprise-scale ontologies (500+ entities) in under 30 seconds
- **SC-009**: Error messages identify exact ontology location (IRI, property) for all validation failures
- **SC-010**: `ggen sync` is the ONLY command available (no legacy commands)

---

## Innovation Summary: What Makes ggen v5 Different

### 1. One Command to Rule Them All
ggen v5 eliminates command sprawl. No more `generate`, `template`, `project`, `graph`. Just `ggen sync`.

### 2. Code as RDF Data (Novel Approach)
Traditional generators: `template + data → code`
ggen v5: `ontology → reasoning → code graph → code`

The code graph is queryable, validatable, and transformable before any text is written.

### 3. Inference-Driven Generation (First of Its Kind)
CONSTRUCT rules discover what to generate. Developers declare intent (`:auditable true`), reasoner infers implementation (`created_at`, `updated_at` fields).

### 4. Graph-Driven Frontmatter
Templates can query the enriched graph for additional context. Generation rules provide primary data, frontmatter `sparql:` queries provide supplementary data.

### 5. Agent-Safe Determinism (Critical for AI-Driven Development)
Same ontology + same rules = same code, always. Audit trails enable automated verification. AI agents can confidently sync code.

---

## 80/20 Implementation Leverage

### Existing Infrastructure (80% Built)
- oxigraph 0.5.1: Full SPARQL engine with CONSTRUCT support
- Tera 1.20: Template engine with custom filters
- ggen-core Graph: Cached SPARQL execution, thread-safe
- Template system: Frontmatter parsing, SPARQL integration, process_graph()
- Code ontology: RDF vocabulary for Rust structures
- Manifest parser: GgenManifest, InferenceRule, GenerationRule types
- Pipeline scaffolding: GenerationPipeline struct

### Remaining 20% (Maximum Impact)
1. Create `ggen sync` CLI command (sync.rs)
2. Complete pipeline execution (CONSTRUCT → SELECT → Template → File)
3. Remove all legacy command modules
4. Implement --dry-run, --watch, --audit flags
5. Integration tests for full sync flow

This 20% unlocks the unified sync paradigm.
