# Feature Specification: ggen v5.0.0 Release

**Feature Branch**: `001-v5-release`
**Created**: 2025-12-17
**Completed**: 2025-12-17
**Status**: MVP Complete
**Input**: User description: "v5 release"

**MVP Scope**: Core `ggen sync` command implemented with 27 tests passing. Comprehensive testing suite (47 tasks) deferred to v5.1.0. See `IMPLEMENTATION_STATUS.md` for details.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Unified Sync Command (Priority: P1)

Users need a single, consistent command to synchronize RDF ontologies with generated code, replacing the fragmented multi-command workflow of v4.0.0. The `ggen sync` command serves as the sole interface for all code generation operations, eliminating the need to learn and use 9 separate CLI modules (ai, template, graph, ontology, project, paper, ci, workflow, utils).

**Why this priority**: This is the core architectural shift of v5. Without this unified command, the release has no value. It addresses the primary user pain point of command fragmentation and provides the foundation for all other v5 features.

**Independent Test**: Can be fully tested by running `ggen sync --from ontology.ttl --to generated/` and verifying that code is generated from RDF ontologies without requiring any other commands. Delivers immediate value by simplifying the workflow from ~5-10 commands to 1 command.

**Acceptance Scenarios**:

1. **Given** a valid RDF ontology file (Turtle format), **When** user runs `ggen sync --from schema.ttl --to src/generated.rs`, **Then** the system generates Rust code with proper type definitions and trait implementations
2. **Given** an existing generated codebase, **When** user runs `ggen sync --mode incremental --from schema.ttl`, **Then** the system updates only changed elements while preserving manual modifications marked with `// MANUAL` comments
3. **Given** a workspace with multiple crates, **When** user runs `ggen sync --from . --mode full`, **Then** the system synchronizes all workspace members according to their `ggen.toml` configurations
4. **Given** a CI/CD pipeline, **When** user runs `ggen sync --mode verify`, **Then** the system validates consistency without modifying files and exits with appropriate status codes
5. **Given** an invalid ontology file, **When** user runs `ggen sync --from broken.ttl`, **Then** the system reports clear error messages with line numbers and suggestions for fixing syntax errors

---

### User Story 2 - SPARQL-Driven Code Generation Pipeline (Priority: P1)

Users need a complete ontology-to-code pipeline that executes SPARQL CONSTRUCT queries for inference, SELECT queries for data extraction, and Tera templates for code generation, all orchestrated through the sync command. This replaces the implicit generation logic scattered across v4 modules.

**Why this priority**: This is the technical foundation enabling the unified sync command. Without this pipeline, users cannot generate code from ontologies. It's P1 because it must ship with User Story 1.

**Independent Test**: Can be tested independently by providing an ontology with SPARQL queries embedded (via `ggen:construct` and `ggen:select` predicates) and verifying that the pipeline executes queries and renders templates correctly. Delivers value even if other features aren't implemented yet.

**Acceptance Scenarios**:

1. **Given** an ontology with `ggen:construct` SPARQL queries, **When** sync executes, **Then** the system materializes inferred triples and adds them to the RDF graph before code generation
2. **Given** an ontology with `ggen:select` queries, **When** sync executes, **Then** the system extracts query results as JSON/YAML variables for template rendering
3. **Given** a Tera template file referenced in `ggen.toml`, **When** sync executes, **Then** the system renders the template with SPARQL query results and writes output to the configured path
4. **Given** multiple pipeline stages (CONSTRUCT → SELECT → Template), **When** sync executes, **Then** the system executes stages in correct order and passes data between stages
5. **Given** a pipeline error (invalid SPARQL syntax), **When** sync executes, **Then** the system halts execution, reports the failing stage and query, and exits with non-zero status

---

### User Story 3 - Migration Path from v4 to v5 (Priority: P2)

Users upgrading from v4.0.0 need clear documentation and tooling to migrate from the 47-verb multi-module CLI to the single sync command, including how to translate their existing workflows and configuration files.

**Why this priority**: P2 because it's critical for adoption but can be delivered after the core sync functionality. Users can manually adapt if necessary, but automated migration significantly improves user experience.

**Independent Test**: Can be tested by taking example v4 command sequences (e.g., `ggen template generate`, `ggen ontology validate`) and verifying that the migration guide provides equivalent v5 sync commands. Delivers value by reducing upgrade friction.

**Acceptance Scenarios**:

1. **Given** a v4 command `ggen template generate --template foo.tera --output bar.rs`, **When** user consults migration guide, **Then** the guide provides the equivalent v5 command `ggen sync --mode full --from foo.tera --to bar.rs`
2. **Given** a project using v4 `ggen.toml` format, **When** user runs v5 sync, **Then** the system either auto-migrates the config or provides clear instructions for manual migration
3. **Given** a CI/CD pipeline using v4 commands, **When** user follows migration guide, **Then** the pipeline successfully executes with v5 sync commands and produces identical outputs
4. **Given** v4 custom scripts relying on `--capabilities` and `--introspect` flags, **When** user migrates to v5, **Then** equivalent introspection mechanisms are documented for AI agent workflows

---

### User Story 4 - Comprehensive Error Handling and Diagnostics (Priority: P2)

Users need clear, actionable error messages when sync operations fail, including specific details about which stage failed (ontology parsing, SPARQL execution, template rendering), what went wrong, and how to fix it.

**Why this priority**: P2 because it significantly improves developer experience but isn't required for basic functionality. Users can work around poor errors by inspecting logs, but good error handling reduces debugging time by 80%.

**Independent Test**: Can be tested by introducing deliberate errors (malformed RDF, invalid SPARQL, missing template variables) and verifying that error messages are specific and actionable. Delivers value by reducing time-to-resolution for common issues.

**Acceptance Scenarios**:

1. **Given** an RDF file with syntax errors, **When** sync executes, **Then** the system reports the exact line number, character position, and nature of the syntax error with suggested fixes
2. **Given** a SPARQL query referencing undefined prefixes, **When** sync executes, **Then** the system identifies the missing prefix and suggests adding it to the ontology or `ggen.toml`
3. **Given** a Tera template using undefined variables, **When** sync executes, **Then** the system lists all undefined variables and shows which SPARQL query should provide them
4. **Given** file permission errors during output writing, **When** sync executes, **Then** the system reports the specific file path and suggests checking permissions or using `--dry-run` to preview changes
5. **Given** a timeout during SPARQL query execution, **When** sync executes, **Then** the system reports which query timed out, its complexity, and suggests optimizations or increasing the timeout setting

---

### User Story 5 - Performance Optimization and Incremental Sync (Priority: P3)

Users working with large ontologies (10,000+ triples) need incremental sync to avoid regenerating all code on every change, with proper change detection and selective update mechanisms.

**Why this priority**: P3 because it's an optimization for specific use cases (large projects). Basic full sync works for most users. However, it becomes critical for projects exceeding 5,000 triples where full sync takes >30 seconds.

**Independent Test**: Can be tested by modifying a single triple in a large ontology and verifying that incremental sync regenerates only affected code files, completing in <5 seconds versus >30 seconds for full sync. Delivers value for large-scale users.

**Acceptance Scenarios**:

1. **Given** a large ontology with 10,000 triples, **When** user modifies a single class definition and runs `ggen sync --mode incremental`, **Then** the system regenerates only the affected generated files (not the entire codebase)
2. **Given** a generated file with manual modifications marked by `// MANUAL` comments, **When** user runs incremental sync, **Then** the system preserves manual code and updates only generated sections
3. **Given** an ontology change affecting transitive dependencies, **When** incremental sync executes, **Then** the system correctly identifies all affected downstream artifacts and regenerates them
4. **Given** a workspace with 10 crates, **When** user modifies an ontology affecting only 2 crates, **Then** incremental sync skips the other 8 crates and completes in proportional time
5. **Given** a sync operation in progress, **When** user interrupts it (Ctrl+C), **Then** the system gracefully stops, reports which files were modified, and allows resuming via incremental sync

---

### Edge Cases

- **What happens when the ontology file is locked by another process?** System should detect the lock, report which process holds it (if possible), and suggest waiting or terminating the other process.
- **How does the system handle circular CONSTRUCT query dependencies?** System must detect cycles before execution and report the dependency chain with suggestions to break the cycle.
- **What happens when template rendering produces invalid Rust syntax?** System should optionally run `rustfmt` or `cargo check` on outputs and report syntax errors with file/line references.
- **How does the system handle concurrent sync operations on the same workspace?** System should use file locking to prevent race conditions and report if another sync is in progress.
- **What happens when SPARQL queries reference external RDF graphs not in the workspace?** System should validate all graph URIs, report missing graphs, and suggest either including them in the workspace or documenting the dependency.
- **How does the system handle platform-specific paths in templates (Windows vs Unix)?** System should normalize path separators in templates or provide platform-specific template variants.
- **What happens when a sync operation exhausts disk space during file generation?** System should detect disk space before writing, estimate required space, and fail early with clear error if insufficient.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a single `ggen sync` command that replaces all 47 verbs from v4.0.0 (ai, template, graph, ontology, project, paper, ci, workflow, utils modules)
- **FR-002**: System MUST support three sync modes: `full` (regenerate all), `incremental` (update changed only), `verify` (validate without writing)
- **FR-003**: System MUST parse RDF ontologies in Turtle, RDF/XML, and N-Triples formats
- **FR-004**: System MUST execute SPARQL CONSTRUCT queries to materialize inferred triples before code generation
- **FR-005**: System MUST execute SPARQL SELECT queries to extract data as JSON/YAML variables for templates
- **FR-006**: System MUST render Tera templates using SPARQL query results to generate code artifacts
- **FR-007**: System MUST preserve manual code modifications marked with `// MANUAL` comments during incremental sync
- **FR-008**: System MUST validate all SPARQL queries for syntax errors before execution
- **FR-009**: System MUST report file paths, line numbers, and specific error descriptions for all failures
- **FR-010**: System MUST exit with status code 0 on success, non-zero on failure (1=conflicts, 2=config error, 3=invalid ontology, 4=permission denied)
- **FR-011**: System MUST support workspace-wide sync across multiple crates using `ggen.toml` configuration
- **FR-012**: System MUST implement change detection for incremental sync by comparing ontology timestamps and content hashes
- **FR-013**: System MUST provide `--dry-run` flag to preview changes without modifying files
- **FR-014**: System MUST provide `--verbose` flag to output detailed operation logs for debugging
- **FR-015**: System MUST detect circular CONSTRUCT query dependencies and report the cycle before execution
- **FR-016**: System MUST support custom SPARQL query timeout configuration via `ggen.toml`
- **FR-017**: System MUST generate migration guide documenting v4-to-v5 command equivalents
- **FR-018**: System MUST validate output file paths and report permission errors before attempting writes
- **FR-019**: System MUST support both file-level and workspace-level sync granularity
- **FR-020**: System MUST log all sync operations to `.ggen/sync.log` for audit trail

### Key Entities *(include if feature involves data)*

- **RDF Ontology**: Source of truth defining domain concepts, relationships, and code generation rules. Contains triples in subject-predicate-object format. Key attributes: format (Turtle/RDF-XML/N-Triples), size (triple count), namespace prefixes, SPARQL queries (embedded via `ggen:construct` and `ggen:select` predicates).

- **SPARQL Query**: Declarative query executed against RDF graph to either materialize new triples (CONSTRUCT) or extract data for templates (SELECT). Key attributes: query type (CONSTRUCT/SELECT), timeout, variables, graph patterns, result format (JSON/YAML for SELECT).

- **Tera Template**: Code generation template with variable placeholders filled by SPARQL query results. Key attributes: template path, output path, required variables, optional variables, syntax (Rust/TOML/Markdown/LaTeX/etc).

- **Generated Artifact**: Output file produced by template rendering. Key attributes: file path, generation timestamp, source template, source ontology hash, manual modification markers (`// MANUAL` regions).

- **Sync Configuration** (`ggen.toml`): Workspace or package-level configuration defining sync behavior. Key attributes: sync mode (full/incremental/verify), source ontology paths, template mappings, SPARQL query timeout, output directories, excluded paths.

- **Workspace Member**: Individual crate in a Rust workspace that participates in sync operations. Key attributes: crate name, `ggen.toml` path, ontology path, generated artifact paths, dependencies on other members.

- **Sync Operation**: An execution of the sync command with specific parameters. Key attributes: mode (full/incremental/verify), start time, end time, modified files, status (success/failure), error messages, execution log.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can migrate from v4's 47-verb CLI to v5's single `ggen sync` command and generate identical code outputs using the migration guide
- **SC-002**: Full sync on a workspace with 1,000 RDF triples completes in under 5 seconds on standard hardware (4-core CPU, SSD)
- **SC-003**: Incremental sync on a large ontology (10,000 triples) with a single triple modification completes in under 3 seconds (90% faster than full sync)
- **SC-004**: 95% of sync failures provide actionable error messages that include file paths, line numbers, and specific fix suggestions
- **SC-005**: Zero data loss during interrupted sync operations—all file modifications are atomic and resumable via incremental sync
- **SC-006**: Workspace-wide sync across 10 crates completes in time proportional to modified crates only (not all crates)
- **SC-007**: Verify mode completes in under 2 seconds for a 1,000-triple ontology and reports all inconsistencies without false positives
- **SC-008**: Generated code compiles successfully with `cargo check` immediately after sync (no manual fixes required for valid ontologies)
- **SC-009**: Manual code modifications (marked with `// MANUAL`) survive 100% of incremental sync operations without corruption
- **SC-010**: SPARQL query execution handles ontologies up to 50,000 triples without timeouts (configurable timeout via `ggen.toml`)
- **SC-011**: Error messages for malformed RDF include exact line/column numbers and link to relevant RDF specification sections
- **SC-012**: Users can understand the complete sync workflow (ontology → CONSTRUCT → SELECT → Template → Code) from README documentation alone
- **SC-013**: CI/CD pipelines using v5 sync complete with zero exit code for valid ontologies and non-zero for invalid inputs (100% reliability)
- **SC-014**: Zero breaking changes for users who don't use the removed v4 commands (backward compatibility for core `ggen.toml` schema)
- **SC-015**: Performance benchmarks demonstrate v5 sync is at least as fast as v4 template generation for equivalent workloads (no regression)
