# Feature Specification: ggen v6 - 3T (TOML, Tera, Turtle) Only Implementation

**Feature Branch**: `001-v6-3t-implementation`
**Created**: 2025-12-19
**Status**: Draft
**Input**: User description: "create v6 using 3T (TOML, Tera, Turtle) only"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Project Initialization with Pure 3T (Priority: P1)

A developer creates a new software project using ggen v6 where the entire system specification exists only as TOML configuration, Turtle ontologies, and Tera templates - with zero hand-written code in the generated output.

**Why this priority**: This is the foundational capability that enables all other v6 features. Without the ability to initialize and configure a pure 3T project, the entire v6 paradigm cannot function.

**Independent Test**: Can be fully tested by initializing a new v6 project with `ggen init --v6 project-name`, writing a minimal ontology (defining one type), and verifying that `ggen sync` generates complete, compilable code without any hand-written source files in the output directory.

**Acceptance Scenarios**:

1. **Given** a developer has ggen v6 installed, **When** they run `ggen init --v6 my-project`, **Then** a project structure is created containing only `ggen.toml`, `ontology/` directory with schema files, and `templates/` directory with rendering templates
2. **Given** a new v6 project, **When** the developer defines a simple type in `ontology/domain.ttl`, **Then** running `ggen sync` generates complete source code in `generated/` without requiring any hand-written code
3. **Given** a v6 project with ontology definitions, **When** the developer regenerates with `ggen sync`, **Then** the output is bit-for-bit identical to the previous generation (deterministic)
4. **Given** a v6 project, **When** the developer inspects the repository, **Then** the `generated/` directory is gitignored and only 3T files (TOML, Tera, Turtle) are version-controlled

---

### User Story 2 - Staged Pipeline Compilation (μ₁-μ₅) (Priority: P1)

A developer leverages the five-stage compilation pipeline to transform ontology substrate through normalization, extraction, emission, canonicalization, and receipt generation - ensuring deterministic, provenance-tracked artifact projection.

**Why this priority**: The staged pipeline is the core mechanism of v6's A = μ(O) constitutional equation. Without this, v6 is just another template-based code generator rather than an ontology-first system.

**Independent Test**: Can be tested by creating an ontology with complex types, running `ggen sync --verbose` to observe all five passes executing sequentially, and verifying that each pass produces expected intermediate outputs (normalized ontology, extracted bindings, emitted files, canonicalized output, and cryptographic receipt).

**Acceptance Scenarios**:

1. **Given** an ontology with type definitions, **When** μ₁ (Normalization Pass) executes, **Then** CONSTRUCT queries rewrite the ontology into canonical form with SHACL validation passing
2. **Given** a normalized ontology, **When** μ₂ (Extraction Pass) executes, **Then** SELECT queries extract RDF bindings that map to template variables
3. **Given** extracted bindings, **When** μ₃ (Emission Pass) executes, **Then** Tera templates render bindings into target artifacts (Rust source, JSON schemas, documentation)
4. **Given** emitted files, **When** μ₄ (Canonicalization Pass) executes, **Then** files are formatted deterministically (line endings normalized, whitespace trimmed, sorted)
5. **Given** canonicalized artifacts, **When** μ₅ (Receipt Pass) executes, **Then** a cryptographic receipt is generated binding output hashes to input ontology hashes
6. **Given** a completed pipeline run, **When** the developer inspects the receipt file, **Then** they can verify that artifacts are provably derived from the ontology substrate

---

### User Story 3 - Constitutional Invariants Enforcement (Priority: P1)

The system automatically enforces constitutional laws ensuring idempotence (μ∘μ = μ), determinism (same O → same A), provenance (hash verification), the no-edit law (artifacts never modified), and substrate primacy (ontology as sole truth source).

**Why this priority**: Constitutional invariants are what distinguish v6 from previous versions and traditional build systems. They guarantee zero-drift by construction rather than by discipline.

**Independent Test**: Can be tested by running `ggen sync` multiple times and verifying bit-for-bit identical output (idempotence), attempting to modify files in `generated/` and having the system reject them or warn on next sync (no-edit law), and verifying that hash chains in receipts prove output provenance from ontology.

**Acceptance Scenarios**:

1. **Given** a v6 project, **When** `ggen sync` is run twice without ontology changes, **Then** the second run produces no file changes (idempotence: μ∘μ = μ)
2. **Given** artifacts in `generated/`, **When** a developer manually edits a generated file, **Then** the next `ggen sync` either overwrites the change or fails with a warning about violated no-edit law
3. **Given** a project on different platforms (Linux, macOS, Windows), **When** `ggen sync` runs on each, **Then** artifacts are bit-for-bit identical (determinism)
4. **Given** a generated receipt file, **When** the developer verifies hashes, **Then** they can cryptographically prove that output hash = hash(μ(ontology hash))
5. **Given** an ontology change, **When** `ggen sync` runs, **Then** all affected artifacts regenerate and the receipt updates to reflect new provenance chain

---

### User Story 4 - Vocabulary Governance and Validation (Priority: P2)

Developers define allowed RDF namespaces in `ggen.toml` and the system enforces vocabulary constraints via SHACL shapes, preventing namespace pollution and ensuring architectural coherence at compile-time.

**Why this priority**: Vocabulary governance is critical for team projects and maintaining clean ontology design, but a single developer can work without it initially. It becomes essential at scale.

**Independent Test**: Can be tested by configuring allowed vocabularies in `ggen.toml`, adding triples from unauthorized namespaces to the ontology, and verifying that `ggen sync` fails validation during μ₁ (Normalization Pass) with clear error messages identifying the violation.

**Acceptance Scenarios**:

1. **Given** a `ggen.toml` with `allowed_vocabularies = ["http://schema.org/", "http://ggen.dev/"]`, **When** an ontology uses triples from `http://example.org/`, **Then** `ggen sync` fails during μ₁ with a vocabulary violation error
2. **Given** SHACL shape definitions in the ontology, **When** an ontology instance violates a shape constraint, **Then** validation fails during μ₁ with detailed constraint violation messages
3. **Given** a vocabulary governance configuration, **When** a developer adds a new namespace to `allowed_vocabularies`, **Then** previously-failing ontologies with that namespace now pass validation
4. **Given** a large ontology, **When** vocabulary validation runs, **Then** validation completes in under 2 seconds for ontologies with 10,000+ triples

---

### User Story 5 - Guards for Output Constraints (Priority: P2)

The system enforces guards (forbidden output patterns) to prevent accidental secret leaks, enforce path constraints, and maintain security policies defined in `ggen.toml`.

**Why this priority**: Guards are a security feature that becomes critical in production but isn't required for basic v6 functionality. Can be added after core pipeline works.

**Independent Test**: Can be tested by defining a secret guard pattern in `ggen.toml`, creating an ontology that would generate code containing that pattern, and verifying that `ggen sync` fails during μ₅ (Receipt Pass) with a guard violation error.

**Acceptance Scenarios**:

1. **Given** a guard configured as `secret = { pattern = "(?i)(password|secret|api.?key)", action = "reject" }`, **When** an ontology would generate code containing "API_KEY = 'xyz'", **Then** `ggen sync` fails with a secret guard violation before writing files
2. **Given** a path guard `path = { pattern = "generated/**", action = "allow" }`, **When** an ontology attempts to generate files outside `generated/`, **Then** the system rejects the operation
3. **Given** multiple guards defined, **When** a generation violates any guard, **Then** all violations are reported together rather than failing on the first

---

### User Story 6 - Meta-Circular Self-Generation (Priority: P3)

The v6 system includes its own type definitions, compilation rules, and projection logic in its ontology, enabling it to regenerate itself by running `ggen sync` on the ggen-core ontology.

**Why this priority**: This is the ultimate validation of v6's power and completeness, but it's a demonstration feature rather than essential functionality. Core v6 can work without self-generation capability initially.

**Independent Test**: Can be tested by loading the ggen-core ontology (which defines the v6 compiler itself), running `ggen sync` on it, and verifying that the generated Rust code compiles into a working ggen binary that is functionally equivalent to the original.

**Acceptance Scenarios**:

1. **Given** the ggen-core ontology defining the v6 compiler, **When** `ggen sync` runs on it, **Then** complete Rust source code for ggen is generated in `generated/`
2. **Given** generated ggen source, **When** it's compiled with `cargo build`, **Then** it produces a binary that can successfully compile other v6 projects
3. **Given** a self-generated ggen binary, **When** it runs `ggen sync` on its own ontology, **Then** it produces identical output to the original (self-rendering closure)

---

### Edge Cases

- **What happens when an ontology is syntactically invalid Turtle?** System fails fast during μ₁ with a parse error pointing to the exact line and character in the `.ttl` file
- **What happens when SPARQL queries in `ggen.toml` return no results?** μ₂ produces empty bindings, μ₃ renders templates with empty data (templates must handle empty cases gracefully), and the system completes successfully with a warning
- **What happens when two templates try to write to the same output file?** System detects the conflict during μ₃ and fails with an error listing both templates and the conflicting path
- **What happens when generated code doesn't compile?** This is outside v6's responsibility - the ontology author must ensure their ontology produces valid code for the target language (v6 guarantees deterministic projection, not correctness)
- **What happens when the receipt file is manually deleted?** Next `ggen sync` regenerates it from scratch based on current artifact hashes
- **What happens when a user modifies `generated/` files and runs `ggen sync`?** System either overwrites changes (default) or fails with a diff showing violations of the no-edit law (configurable via `ggen.toml`)
- **What happens when vocabulary validation is disabled?** System skips μ₁ SHACL checks but still validates Turtle syntax - useful for rapid prototyping but discouraged for production

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST parse TOML configuration files conforming to the ggen v6 schema, validating all required sections (`[project]`, `[v6]`, `[v6.passes]`, `[v6.invariants]`)
- **FR-002**: System MUST load Turtle (`.ttl`) ontology files using an RDF parser supporting the Turtle 1.1 specification
- **FR-003**: System MUST execute SPARQL 1.1 CONSTRUCT queries for ontology normalization (μ₁ pass)
- **FR-004**: System MUST execute SPARQL 1.1 SELECT queries for data extraction (μ₂ pass)
- **FR-005**: System MUST render Tera templates using bindings from SELECT query results (μ₃ pass)
- **FR-006**: System MUST canonicalize generated files through deterministic formatting (μ₄ pass): normalize line endings to LF, trim trailing whitespace, ensure final newline, sort when applicable
- **FR-007**: System MUST generate cryptographic receipts (μ₅ pass) containing SHA-256 hashes of input ontologies and output artifacts
- **FR-008**: System MUST validate ontologies against SHACL shapes during μ₁, failing if any constraint violations are detected
- **FR-009**: System MUST enforce vocabulary governance by rejecting triples from namespaces not in the `allowed_vocabularies` list
- **FR-010**: System MUST enforce guards by scanning generated content for forbidden patterns before writing files
- **FR-011**: System MUST detect and reject attempts to generate files outside the configured `output_dir`
- **FR-012**: System MUST be idempotent: running `ggen sync` twice without ontology changes produces no file modifications
- **FR-013**: System MUST be deterministic: identical ontology input produces bit-for-bit identical output across all platforms and runs
- **FR-014**: System MUST gitignore the `generated/` directory by default, preventing generated artifacts from being committed
- **FR-015**: System MUST fail fast with actionable error messages when ontology syntax is invalid, SPARQL queries fail, or SHACL validation fails
- **FR-016**: System MUST preserve TOML, Tera, and Turtle files as the only version-controlled files in a v6 project
- **FR-017**: System MUST execute the five passes in strict order (μ₁ → μ₂ → μ₃ → μ₄ → μ₅) with no parallelization within a single `ggen sync` invocation
- **FR-018**: System MUST support `ggen init --v6 <project-name>` to scaffold a new v6 project with minimal ontology, templates, and configuration
- **FR-019**: System MUST generate receipts in JSON format containing: timestamp, ontology file hashes (SHA-256), artifact file hashes (SHA-256), pipeline configuration hash
- **FR-020**: System MUST provide a `ggen verify` command that validates receipt integrity by recomputing hashes and comparing against receipt

### Key Entities

- **Ontology (O)**: The semantic substrate consisting of RDF triples in Turtle format. Represents the complete system specification including types, properties, constraints, and domain logic. The sole source of truth in v6.

- **Projection Function (μ)**: The deterministic transformation pipeline consisting of five passes (μ₁-μ₅). Takes ontology as input, produces artifacts as output. Implemented by the ggen v6 compiler.

- **Artifact (A)**: Any generated file in the `output_dir` (e.g., Rust source code, JSON schemas, markdown documentation). Artifacts are ephemeral, reproducible, and never hand-edited. Related to Ontology via the equation A = μ(O).

- **Template**: A Tera template file (`.tera`) that defines how to render RDF bindings into target format. Contains Jinja2-style syntax with filters, loops, and conditionals. Related to Projection Function as the formatting specification for μ₃.

- **Configuration (ggen.toml)**: The TOML file defining pipeline behavior, allowed vocabularies, guards, generation rules (SPARQL queries + template mappings), and constitutional invariant settings.

- **Receipt**: A JSON file containing cryptographic proof of provenance. Binds output artifact hashes to input ontology hashes with timestamp and configuration hash. Enables verification that A = μ(O).

- **SHACL Shape**: An RDF constraint definition validating ontology structure during μ₁. Ensures type safety and architectural coherence before projection begins.

- **Guard**: A constraint pattern (regex) with an action (allow/reject) enforcing security policies on generated output. Prevents secret leaks and path violations.

- **Pass**: One of the five stages in the projection pipeline (μ₁: Normalization, μ₂: Extraction, μ₃: Emission, μ₄: Canonicalization, μ₅: Receipt). Each pass is a pure function with defined inputs and outputs.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can initialize a new v6 project and generate working code from a minimal ontology (defining one type with properties) in under 3 commands and 2 minutes
- **SC-002**: The system compiles ontologies with 10,000+ triples in under 5 seconds on standard development hardware (4-core CPU, 8GB RAM)
- **SC-003**: Running `ggen sync` twice without ontology changes produces zero file modifications (100% idempotence validation)
- **SC-004**: Identical ontologies generate bit-for-bit identical artifacts across Linux, macOS, and Windows platforms (100% determinism validation)
- **SC-005**: SHACL validation catches 95%+ of common ontology modeling errors (missing required properties, type mismatches, cardinality violations) during μ₁
- **SC-006**: Guard enforcement prevents 100% of configured secret patterns from appearing in generated artifacts
- **SC-007**: The ggen-core ontology (defining the v6 compiler itself) successfully generates a compilable Rust codebase in under 10 seconds
- **SC-008**: Self-generated ggen binaries produce identical output to the original compiler when run on the same ontology (meta-circular closure)
- **SC-009**: Error messages during ontology validation include file name, line number, and specific constraint violated in 100% of SHACL/vocabulary failures
- **SC-010**: Developers can verify artifact provenance using `ggen verify` in under 1 second for projects with 100+ generated files

## Assumptions *(optional - only if relevant)*

- **A-001**: Developers have basic knowledge of RDF, SPARQL, and semantic web concepts (or are willing to learn from documentation)
- **A-002**: Target output languages (Rust, TypeScript, etc.) have stable syntax that doesn't change frequently enough to invalidate templates
- **A-003**: Ontologies authored by developers are semantically correct - v6 guarantees deterministic projection, not logical correctness of the specification
- **A-004**: Build systems for target languages (cargo, npm, etc.) are available to compile generated code - v6 only generates source files
- **A-005**: Developers accept that debugging generated code requires editing the ontology + templates, not the artifacts directly
- **A-006**: The Rust ecosystem's Oxigraph RDF store and Tera template engine remain actively maintained and stable
- **A-007**: Projects using v6 can tolerate a paradigm shift from imperative programming to declarative ontology authoring

## Dependencies *(optional - only if relevant)*

- **D-001**: Rust 1.75+ toolchain for building the ggen v6 compiler itself
- **D-002**: Oxigraph crate for RDF triple store and SPARQL query execution
- **D-003**: Tera crate for template rendering engine
- **D-004**: SHACL validation library (or custom implementation) for μ₁ constraint checking
- **D-005**: Git for version control of 3T files (TOML, Tera, Turtle)
- **D-006**: SHA-256 hashing library for cryptographic receipt generation
- **D-007**: Cross-platform file system access (std::fs) for reading/writing project files
- **D-008**: TOML parser for loading `ggen.toml` configuration

## Non-Goals / Out of Scope *(optional)*

- **NG-001**: Runtime code execution or interpretation - v6 only generates static artifacts
- **NG-002**: Graphical ontology editors or visual programming - v6 assumes text-based Turtle editing
- **NG-003**: Language-specific syntax validation - if generated code doesn't compile, that's an ontology/template authoring error
- **NG-004**: Automatic ontology migration from v5 to v6 - migration requires manual rewriting
- **NG-005**: Support for RDF serializations other than Turtle (N-Triples, JSON-LD, etc.) - Turtle is the canonical format
- **NG-006**: Incremental compilation or caching of intermediate results - each `ggen sync` runs the full μ₁-μ₅ pipeline
- **NG-007**: IDE integrations or language servers for Turtle/Tera editing - developers use standard text editors
- **NG-008**: Package management or distribution of ontologies - v6 projects are standalone Git repositories

## Risks & Mitigations *(optional)*

- **R-001**: **Risk**: Ontology authoring has a steep learning curve, limiting adoption. **Mitigation**: Provide comprehensive examples, starter templates, and migration guides from traditional code.
- **R-002**: **Risk**: Templates become complex and hard to maintain for large systems. **Mitigation**: Encourage modular template design, template inheritance, and reusable template libraries.
- **R-003**: **Risk**: SPARQL query performance degrades on large ontologies. **Mitigation**: Benchmark with 100K+ triple ontologies, optimize queries, and provide query complexity guidelines.
- **R-004**: **Risk**: Determinism breaks due to platform-specific file system behavior. **Mitigation**: Extensive cross-platform testing (Linux, macOS, Windows) in CI/CD pipeline.
- **R-005**: **Risk**: Meta-circular self-generation creates bootstrapping complexity. **Mitigation**: Maintain a hand-written v6 compiler as the bootstrap until self-generation is proven stable.
