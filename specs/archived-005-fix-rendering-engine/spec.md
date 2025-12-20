# Feature Specification: Fix Template Rendering Engine

**Feature Branch**: `005-fix-rendering-engine`
**Created**: 2025-12-13
**Status**: Complete
**Input**: User description: "identify how the rendering engine works and fix it if is broken start with the frontmatter etc"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Template with SPARQL Frontmatter Generates Code (Priority: P1)

A developer creates a template with YAML frontmatter containing RDF file references, SPARQL prefixes, and SPARQL queries. When they run `ggen template generate` with this template, the system loads the RDF data, executes all SPARQL queries, and makes the results available as `sparql_results.*` variables in the Tera template body.

**Why this priority**: This is the core functionality - without SPARQL execution, templates cannot access RDF data at all. All RDF-driven code generation depends on this.

**Independent Test**: Can be fully tested by creating a minimal template with one SPARQL query against a simple TTL file and verifying the query results appear in output.

**Acceptance Scenarios**:

1. **Given** a template with `rdf: "data.ttl"` and `sparql: { items: "SELECT ?x WHERE {...}" }` in frontmatter, **When** user runs `ggen template generate --template <path>`, **Then** the RDF file is loaded into Oxigraph and SPARQL queries execute.

2. **Given** SPARQL queries executed successfully, **When** the template body references `{{ sparql_results.items }}`, **Then** the Tera renderer can iterate over query results.

3. **Given** a template with multiple SPARQL queries, **When** rendering completes, **Then** output JSON shows `sparql_queries_executed: N` where N matches query count.

---

### User Story 2 - Multi-File Output with FILE Markers (Priority: P2)

A developer creates a master template with `{# FILE: path/to/file.rs #}` markers to generate multiple files from a single template. When they run generation, each marked section is written to its respective file path relative to the output directory.

**Why this priority**: Multi-file output enables single-template packages (like clap-noun-verb) that generate complete projects. This is essential for DRY template authoring.

**Independent Test**: Can be tested by creating a template with two FILE markers and verifying both output files are created with correct content.

**Acceptance Scenarios**:

1. **Given** a template with `{# FILE: Cargo.toml #}` and `{# FILE: src/main.rs #}` markers, **When** user runs `ggen template generate --output ./out/`, **Then** files `./out/Cargo.toml` and `./out/src/main.rs` are created.

2. **Given** output directory does not exist, **When** generation runs, **Then** directories are created automatically.

3. **Given** `--force` flag is provided, **When** output files already exist, **Then** they are overwritten without error.

---

### User Story 3 - Project Generate with Convention-Based Discovery (Priority: P3)

A developer has a project with a `templates/` subdirectory containing `.tmpl` files and corresponding data files. When they run `ggen project generate --path <project>`, the system discovers all templates and processes them.

**Why this priority**: Convention-based generation reduces friction for users who don't want to specify individual templates.

**Independent Test**: Can be tested by creating a project directory with `templates/` containing one template+data pair, running `ggen project generate`, and checking output.

**Acceptance Scenarios**:

1. **Given** a project with `templates/*.tmpl` files, **When** user runs `ggen project generate --path ./project`, **Then** all templates are processed.

2. **Given** `--output` flag specifies a directory, **When** generation completes, **Then** output files are written relative to that directory.

3. **Given** `--force` flag is provided, **When** output directory exists, **Then** generation proceeds without "file exists" errors.

---

### Edge Cases

- What happens when referenced RDF file does not exist? (Clear error message with file path)
- How does system handle malformed SPARQL queries? (Error with query text and parse error)
- What happens when SPARQL query returns no results? (Empty array, not error)
- How does system handle relative vs absolute RDF paths? (Resolved relative to template location)
- What happens when template body has Tera syntax errors? (Error with line number and context)
- How does system handle `{# FILE: #}` markers with invalid paths? (Error before writing any files)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST parse YAML frontmatter from templates to extract `rdf`, `prefixes`, and `sparql` sections
- **FR-002**: System MUST load RDF files specified in frontmatter `rdf` field into Oxigraph store
- **FR-003**: System MUST resolve RDF file paths relative to the template file location
- **FR-004**: System MUST execute all SPARQL queries defined in frontmatter `sparql` section
- **FR-005**: System MUST inject SPARQL query results into Tera context as `sparql_results.<query_name>`
- **FR-006**: System MUST provide helper functions: `sparql_first()`, `sparql_count()`, `sparql_values()`, `sparql_empty()`
- **FR-007**: System MUST detect `{# FILE: path #}` markers and write content to separate files
- **FR-008**: System MUST create parent directories for output files automatically
- **FR-009**: System MUST report generation metrics: `rdf_files_loaded`, `sparql_queries_executed`, `files_created`
- **FR-010**: `--force` flag MUST override existing file checks across all generation commands
- **FR-011**: System MUST provide clear error messages with file paths and line numbers for template errors

### Key Entities

- **Template**: A file with YAML frontmatter (metadata) and Tera template body (content)
- **Frontmatter**: YAML section containing `to`, `rdf`, `prefixes`, `sparql` configuration
- **SPARQL Query**: Named query in frontmatter that extracts data from RDF
- **Template Context**: Tera context object containing `sparql_results` and user-provided variables
- **FILE Marker**: Comment syntax `{# FILE: path #}` that splits output into multiple files

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Templates with SPARQL frontmatter report `sparql_queries_executed > 0` in output metrics
- **SC-002**: All marketplace packages using RDF templates (clap-noun-verb, etc.) generate successfully
- **SC-003**: Multi-file templates with FILE markers produce the correct number of output files
- **SC-004**: `--force` flag prevents all "file exists" errors across template and project generate commands
- **SC-005**: Error messages include file path and specific failure reason (not generic "render failed")

## Assumptions

- Oxigraph is the RDF store used for SPARQL execution
- Tera is the template engine used for rendering
- YAML frontmatter is delimited by `---` markers
- RDF files are in Turtle (.ttl) format (other formats may be supported but TTL is primary)
- The `template generate` and `project generate` commands share core rendering logic
