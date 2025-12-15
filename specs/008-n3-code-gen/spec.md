# Feature Specification: N3/CONSTRUCT Semantic Code Generator

**Feature Branch**: `008-n3-code-gen`
**Created**: 2024-12-14
**Status**: Draft
**Input**: User description: "Focus on ggen as the N3 code generator (CONSTRUCT, SPARQL, etc). What is innovative about N3 usage? What is possible that nobody has ever considered. Big bang 80/20 implementation based on the current graph/tera/frontmatter/ggen.toml"

---

## Executive Summary

ggen v5 transforms code generation from a template-centric process into **semantic graph transformation**. The innovation: treat code as RDF data that can be reasoned about, transformed via CONSTRUCT queries, and inferred via N3 rules. This is fundamentally different from every existing code generator.

**The Big Insight**: Code generation becomes **graph to graph to text** instead of **template to text**. The intermediate code graph enables:
1. **Semantic validation** before code is written
2. **Inference-driven generation** (N3 rules discover what to generate)
3. **Composable transformations** (chain CONSTRUCT queries)
4. **Agent-safe determinism** (same graph = same code, always)

---

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Domain Model to Rust Code (Priority: P1)

A developer creates a domain ontology in Turtle format describing their entities (User, Order, Product). They run `ggen generate` and receive complete, production-ready Rust structs with derives, traits, and impl blocks - all inferred from semantic annotations in the ontology.

**Why this priority**: This is the core value proposition. A single ontology file produces thousands of lines of idiomatic Rust code. Every other feature depends on this working.

**Independent Test**: Create a `domain.ttl` with 3 entities, run `ggen generate`, verify output contains correct Rust structs with `#[derive(...)]`, field types, and documentation.

**Acceptance Scenarios**:

1. **Given** a Turtle file with `:User a rdfs:Class ; :codegen-as "struct"`, **When** `ggen generate` is executed, **Then** output contains `pub struct User { ... }` with correct derive macros.

2. **Given** an entity with `:auditable true` annotation, **When** generation runs, **Then** `created_at` and `updated_at` fields are automatically added via N3 inference.

3. **Given** a struct with a `Uuid` field type, **When** generation runs, **Then** `Serialize` and `Deserialize` derives are automatically added via N3 rule inference.

---

### User Story 2 - Relationship-Driven Impl Generation (Priority: P1)

A developer defines relationships between entities (`:User :has_many :Order`) in the ontology. CONSTRUCT queries automatically generate impl blocks with accessor methods (`get_orders()`) and relationship-aware code patterns.

**Why this priority**: Relationships are the heart of domain modeling. Auto-generating relationship code eliminates boilerplate and ensures consistency.

**Independent Test**: Define `:User :has_many :Order` and `:Order :belongs_to :User`, run generation, verify impl blocks with `get_orders(&self) -> Vec<Order>` and `get_user(&self) -> User`.

**Acceptance Scenarios**:

1. **Given** `:Entity1 :has_many :Entity2`, **When** CONSTRUCT executes, **Then** output includes `impl Entity1 { pub fn get_entity2s(&self) -> Vec<Entity2> }`.

2. **Given** `:Entity :soft_delete true`, **When** N3 reasoning runs, **Then** `deleted_at: Option<DateTime<Utc>>` field is inferred and added.

---

### User Story 3 - ggen.toml Manifest-Driven Generation (Priority: P1)

A developer creates a `ggen.toml` manifest specifying ontology sources, SPARQL queries, templates, and output paths. Running `ggen generate` executes the entire pipeline from manifest.

**Why this priority**: The manifest is the single source of truth for code generation. Without it, the system is unusable.

**Independent Test**: Create minimal ggen.toml with `[ontology]` and `[[generation.rules]]`, run `ggen generate`, verify output files exist.

**Acceptance Scenarios**:

1. **Given** `ggen.toml` with `source = "domain/model.ttl"`, **When** `ggen generate` runs, **Then** ontology is loaded from specified path.

2. **Given** `[[generation.rules]]` with `query`, `template`, `output_file`, **When** generation runs, **Then** SPARQL executes, template renders, file is written.

---

### User Story 4 - N3 Rule-Based Code Inference (Priority: P2)

A developer writes N3 rules that encode project conventions (e.g., "if trait has async methods, require async_trait macro"). The N3 reasoner applies these rules before code generation, ensuring all inferred patterns are included.

**Why this priority**: N3 rules enable "convention over configuration" at the semantic level. This differentiates ggen from template-based generators.

**Independent Test**: Create N3 rule `{ ?trait :methods [ :methodAsync true ] } => { ?trait :requires_macro "async_trait" }`, verify macro appears in generated trait.

**Acceptance Scenarios**:

1. **Given** an N3 rule for auto-deriving `PartialEq` on structs with more than 5 fields, **When** a 7-field struct is defined, **Then** `PartialEq` derive is added without explicit annotation.

2. **Given** custom N3 rules in `rules.n3`, **When** `ggen generate` runs, **Then** all rules are applied before CONSTRUCT queries execute.

---

### User Story 5 - CONSTRUCT Query Composition (Priority: P2)

A developer chains multiple CONSTRUCT queries to progressively build the code graph. First CONSTRUCT creates struct definitions, second adds impl blocks, third generates tests - all operating on the same semantic graph.

**Why this priority**: Composable transformations enable complex generation without monolithic queries. Each CONSTRUCT is testable independently.

**Independent Test**: Define 3 CONSTRUCT queries in ggen.toml, verify each produces expected graph additions, verify final code includes all generated elements.

**Acceptance Scenarios**:

1. **Given** CONSTRUCT queries for structs, impls, and tests in `[[generation.rules]]`, **When** pipeline executes, **Then** all three code graph types are generated in dependency order.

2. **Given** a CONSTRUCT query with `BIND(IRI(CONCAT(...)))`, **When** executed, **Then** new IRIs are correctly generated without collision.

---

### User Story 6 - Poka-Yoke Safety Validation (Priority: P2)

Before any code is written to disk, CONSTRUCT queries validate the code graph for semantic errors (missing types, invalid relationships, unsafe patterns). Generation fails fast with semantic error messages.

**Why this priority**: Agent-driven code generation requires deterministic safety. SHACL + CONSTRUCT validation prevents invalid code from being written.

**Independent Test**: Create ontology with missing `:fieldType`, run generation, verify exit code 1 (ValidationError) with clear error message.

**Acceptance Scenarios**:

1. **Given** a struct field without `:fieldType`, **When** validation CONSTRUCT runs, **Then** generation fails with "Field missing type annotation" error.

2. **Given** SHACL constraint `sh:minCount 1` on a property, **When** property is missing, **Then** SHACL validation fails before code generation.

3. **Given** `no_unsafe = true` in ggen.toml, **When** generated code contains `unsafe`, **Then** output validation fails with exit code 4.

---

### User Story 7 - Audit Trail for Agent Verification (Priority: P3)

After generation completes, an `audit.json` file is created containing: input ontology hash, SPARQL queries executed, templates rendered, and validation status. AI agents can verify generation determinism.

**Why this priority**: Agent-safe code generation requires verifiable reproducibility. The audit trail enables automated verification.

**Independent Test**: Run generation twice with same inputs, verify both `audit.json` files have identical content except timestamps.

**Acceptance Scenarios**:

1. **Given** `require_audit_trail = true` in ggen.toml, **When** generation completes, **Then** `audit.json` is written with all pipeline steps.

2. **Given** same ontology and rules, **When** generation runs on different machines, **Then** output code is byte-for-byte identical.

---

### Edge Cases

- What happens when SPARQL query returns empty results? (Generate empty file or skip based on `skip_empty` config)
- How does system handle circular N3 rule dependencies? (Detect cycles during rule loading, fail with clear error)
- What happens when ontology file does not exist? (Exit code 1 with "Ontology not found: path" message)
- How does system handle malformed Turtle syntax? (Parse error with line number, exit code 1)
- What happens when template references non-existent SPARQL variable? (Template error with variable name, exit code 3)
- How does system handle CONSTRUCT queries that produce duplicate triples? (Deduplicate automatically via RDF semantics)
- What happens when N3 reasoning exceeds timeout? (Exit code 5 with "Reasoning timeout after Nms" message)
- What happens when ggen.toml is missing required fields? (Validation error listing missing fields, exit code 1)

---

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST load RDF ontologies from Turtle (.ttl), N-Triples (.nt), N3 (.n3), and RDF/XML (.rdf) formats
- **FR-002**: System MUST execute SPARQL SELECT queries against loaded ontology graphs
- **FR-003**: System MUST execute SPARQL CONSTRUCT queries to build intermediate code graphs
- **FR-004**: System MUST apply N3 rules for forward-chaining inference before CONSTRUCT execution
- **FR-005**: System MUST serialize code graphs to Rust source files via Tera templates
- **FR-006**: System MUST validate ontologies against SHACL shapes before code generation
- **FR-007**: System MUST provide semantic exit codes (0=success, 1=validation, 2=sparql, 3=template, 4=output, 5=timeout)
- **FR-008**: System MUST generate deterministic output (same input produces same output, byte-for-byte)
- **FR-009**: System MUST parse ggen.toml manifests with `[project]`, `[ontology]`, `[generation]`, `[validation]` sections
- **FR-010**: System MUST support inline RDF in templates via `rdf_inline:` frontmatter field
- **FR-011**: System MUST support SPARQL queries in templates via `query:` frontmatter field
- **FR-012**: System MUST timeout SPARQL queries exceeding configured `max_sparql_timeout_ms`
- **FR-013**: System MUST timeout N3 reasoning exceeding configured `max_reasoning_timeout_ms`
- **FR-014**: System MUST generate audit.json when `require_audit_trail = true`
- **FR-015**: System MUST validate generated Rust code syntax when `validate_syntax = true`
- **FR-016**: System MUST reject generated code containing `unsafe` when `no_unsafe = true`
- **FR-017**: System MUST support code ontology vocabulary (code:Struct, code:Trait, code:Method, code:Field, code:Impl)
- **FR-018**: System MUST infer derives from field types via N3 rules (Uuid implies Serialize/Deserialize)
- **FR-019**: System MUST infer audit fields from `:auditable true` annotation via N3 rules
- **FR-020**: System MUST generate impl blocks from relationship annotations via CONSTRUCT queries

### Key Entities

- **Ontology Graph**: In-memory RDF triple store containing domain model, loaded via oxigraph
- **Code Graph**: Intermediate RDF representation of generated code using code ontology vocabulary
- **N3 Rule Set**: Forward-chaining inference rules for automatic pattern discovery
- **CONSTRUCT Query**: SPARQL query that builds new RDF graphs from existing data
- **ggen.toml Manifest**: TOML configuration specifying the complete generation pipeline
- **Tera Template**: Text template that serializes code graph to Rust source files
- **Audit Trail**: JSON record of generation pipeline execution for verification
- **SHACL Shapes**: Constraint definitions for ontology validation before generation

---

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can generate complete Rust crate from ontology in under 5 seconds for typical domain models (10-50 entities)
- **SC-002**: System produces identical output across runs (100% determinism verified by comparing output hashes)
- **SC-003**: N3 inference reduces explicit annotations by 60% compared to annotation-only approach
- **SC-004**: SHACL validation catches 95% of semantic errors before code generation begins
- **SC-005**: Generated code compiles successfully on first attempt (0 syntax errors)
- **SC-006**: Audit trail enables automated verification of generation reproducibility
- **SC-007**: Users can add new code patterns by writing N3 rules without modifying ggen source
- **SC-008**: CONSTRUCT query composition enables incremental pipeline development (each query testable independently)
- **SC-009**: System handles enterprise-scale ontologies (500+ entities) in under 30 seconds
- **SC-010**: Error messages identify exact ontology location (IRI, property) for all validation failures

---

## Innovation Summary: What Makes This Different

### 1. Code as RDF Data (Novel Approach)
Traditional generators: `template + data to code`
ggen v5: `ontology to reasoning to code graph to code`

The code graph is queryable, validatable, and transformable before any text is written.

### 2. Inference-Driven Generation (First of Its Kind)
N3 rules discover what to generate. Developers declare intent (`:auditable true`), reasoner infers implementation (`created_at`, `updated_at` fields).

### 3. CONSTRUCT as Code Builder (Novel Application)
CONSTRUCT queries build new semantic structures, not just retrieve data. Chaining CONSTRUCTs enables progressive code graph construction.

### 4. Semantic Safety (Poka-Yoke for Code Generation)
SHACL + CONSTRUCT validation ensures semantic correctness before code exists. Invalid patterns are impossible, not just caught.

### 5. Agent-Safe Determinism (Critical for AI-Driven Development)
Same ontology + same rules = same code, always. Audit trails enable automated verification. AI agents can confidently generate code.

---

## 80/20 Implementation Leverage

### Existing Infrastructure (80% Built)
- oxigraph 0.5.1: Full SPARQL engine with CONSTRUCT support
- Tera 1.20: Template engine with custom filters
- ggen-core Graph: Cached SPARQL execution, thread-safe
- Frontmatter system: YAML parsing, SPARQL integration
- Code ontology: RDF vocabulary for Rust structures
- Poka-yoke guards: Timeout, validation, dry-run

### Remaining 20% (Maximum Impact)
1. N3 rule loading and execution in oxigraph
2. ggen.toml parser with strong typing
3. CONSTRUCT to Code Graph to Template pipeline
4. SHACL validation integration
5. Audit trail generation
6. CLI thin wrapper (`ggen generate`, `ggen validate`)

This 20% unlocks the full semantic code generation paradigm.
