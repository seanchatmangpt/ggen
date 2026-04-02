# Feature Specification: TTL Validation Command for ggen CLI

**Branch**: `001-ttl-validation-cli`
**Created**: 2025-12-20
**Status**: Draft

## User Input

Add TTL validation command to ggen CLI that validates RDF files against SHACL shapes

---

## User Stories

### User Story 1 - Validate Single TTL File Against SHACL Shapes (Priority: P1)

As a Rust developer using ggen, I want to validate a single TTL file against SHACL shapes so that I can ensure my RDF ontologies conform to their schema before code generation

**Why this priority**: Core functionality - validation is the foundation of the entire feature. Without this, users cannot verify RDF quality

**Independent Test**: Run 'ggen validate ontology.ttl --schema schema.ttl' and verify it returns validation status and violations (if any)

**Acceptance Scenarios**:

1. **Given** A valid TTL file conforming to SHACL shapes, **When** User runs 'ggen validate ontology.ttl --schema schema.ttl', **Then** Command exits with code 0 and displays 'Validation passed: 0 violations'
2. **Given** A TTL file with SHACL violations (missing required property), **When** User runs 'ggen validate ontology.ttl --schema schema.ttl', **Then** Command exits with code 1 and displays detailed violation report with line numbers and constraint descriptions
3. **Given** A malformed TTL file (syntax errors), **When** User runs 'ggen validate bad.ttl --schema schema.ttl', **Then** Command exits with code 2 and displays parse error with file location and syntax issue

### User Story 2 - View Detailed Violation Reports (Priority: P1)

As a developer fixing RDF violations, I want to see clear, actionable error messages with file locations and constraint descriptions so that I can quickly identify and fix issues

**Why this priority**: Critical for usability - unclear errors waste developer time and reduce tool adoption

**Independent Test**: Introduce intentional SHACL violations and verify error messages include: violation type, affected node, constraint path, expected vs actual values

**Acceptance Scenarios**:

1. **Given** A TTL file violating minCount constraint (missing required property), **When** User runs validation, **Then** Error message shows: 'Violation: Node :Feature1 missing required property sk:featureName (minCount: 1)'
2. **Given** Multiple violations in same file, **When** User runs validation, **Then** All violations listed with counts by severity, grouped by constraint type for easy scanning

### User Story 3 - Validate Multiple TTL Files (Priority: P2)

As a project maintainer with multiple ontology files, I want to validate all TTL files in a directory with a single command so that I can ensure consistency across my entire RDF dataset

**Why this priority**: Important for productivity - large projects have dozens of TTL files that need validation

**Independent Test**: Create directory with 10 TTL files (8 valid, 2 invalid), run 'ggen validate --schema schema.ttl specs/**/*.ttl' and verify summary shows 8 passed, 2 failed with file-by-file breakdown

**Acceptance Scenarios**:

1. **Given** Directory with 5 TTL files, 3 valid and 2 with violations, **When** User runs 'ggen validate --schema schema.ttl specs/**/*.ttl', **Then** Command validates all files, shows summary (3 passed, 2 failed), lists each file's status, exits with code 1 (since some failed)
2. **Given** Glob pattern matching no files, **When** User runs 'ggen validate --schema schema.ttl nonexistent/**/*.ttl', **Then** Command exits with code 2 and displays 'Error: No files matched pattern nonexistent/**/*.ttl'

### User Story 4 - Integrate with CI/CD Pipelines (Priority: P3)

As a DevOps engineer, I want deterministic exit codes and machine-readable output formats so that I can integrate validation into automated build pipelines and fail builds on invalid RDF

**Why this priority**: Nice-to-have for automation - enables quality gates in CI/CD but not required for initial release

**Independent Test**: Run validation in CI script, check exit code is 0 for valid RDF and 1 for violations, verify JSON output with --format=json can be parsed by jq

**Acceptance Scenarios**:

1. **Given** CI pipeline script running validation step, **When** Validation detects violations, **Then** Command exits with code 1, pipeline fails, violation summary shown in build logs
2. **Given** CI pipeline requiring JSON output for dashboard, **When** User runs 'ggen validate --format=json ontology.ttl --schema schema.ttl', **Then** Command outputs structured JSON with file paths, violation counts, and detailed error objects parseable by CI tools

---

## Functional Requirements

- **FR-001** (CLI Integration): System MUST provide a 'ggen validate' subcommand accessible from the existing ggen CLI
- **FR-002** (SHACL Engine): System MUST validate RDF graphs against SHACL shapes using a standard-compliant SHACL validation engine
- **FR-003** (File Input): System MUST accept TTL files as positional arguments and support glob patterns for batch validation (e.g., 'specs/**/*.ttl')
- **FR-004** (Schema Specification): System MUST accept SHACL shape files via --schema flag and support auto-discovery of schema.ttl in current directory
- **FR-005** (Violation Reporting): System MUST report violations with: affected node URI, constraint type, constraint path, severity level, and human-readable message
- **FR-006** (Exit Codes): System MUST return exit code 0 for successful validation (no violations), 1 for validation failures, 2 for errors (file not found, parse errors)
- **FR-007** (Batch Processing): System MUST validate multiple files in sequence and provide summary statistics (total files, passed, failed, total violations)
- **FR-008** (Output Formatting): System MUST support --format flag with options: 'human' (default, colored terminal output), 'json' (machine-readable), 'compact' (one-line per file)
- **FR-009** (Error Handling): System MUST gracefully handle errors: file not found, permission denied, malformed TTL, malformed SHACL, and display actionable error messages
- **FR-010** (Performance): System MUST validate files with <2 second overhead for graphs under 1000 triples and stream results for large files to avoid memory exhaustion

---

## Success Criteria

- **SC-001** (Validation performance for 1000-triple RDF graph: < 5 seconds): Users can validate 1000-triple RDF files in under 5 seconds on standard development machines
- **SC-002** (SHACL compliance detection accuracy: 100%): System detects 100% of SHACL violations present in test suite (no false negatives or false positives)
- **SC-003** (Developer time to fix violations: < 2 minutes per violation): Developers can locate and fix 90% of violations in under 2 minutes using error messages (measured via usability testing)
- **SC-004**: Validation command integrates seamlessly with existing ggen CLI (same argument patterns, help system, error conventions)
- **SC-005** (CI/CD pipeline integration success rate: >= 95%): 95% of users successfully integrate validation into CI/CD pipelines without custom scripting (measured via adoption survey)

---

## Key Entities

### ValidationReport

Complete validation result for one or more RDF files against SHACL shapes

**Key attributes**: file paths, total files validated, passed count, failed count, list of violations, execution time

### SHACLShape

SHACL constraint definition specifying structural requirements for RDF graphs

**Key attributes**: shape URI, target class, properties with constraints (minCount, maxCount, datatype, pattern), severity level

### RDFGraph

Parsed RDF data structure loaded from TTL file for validation

**Key attributes**: triples (subject-predicate-object), namespaces, base URI, source file path

### Violation

Single SHACL constraint violation detected during validation

**Key attributes**: focus node, result path, constraint component, severity, message, value, source shape

---

## Edge Cases

- **Empty TTL file (zero triples)**: Validation succeeds with warning: 'File contains no triples, validation skipped'
- **TTL file larger than available memory (>4GB)**: System uses streaming validation or fails gracefully with error: 'File too large for in-memory validation, consider splitting'
- **SHACL shape file references undefined namespaces**: Validation fails with error: 'SHACL shape file contains undefined namespace prefix: xyz'
- **Circular references in RDF graph**: Validation handles circular references without infinite loops, completes successfully
- **File path with special characters (spaces, unicode)**: System correctly parses file paths with spaces (quoted or escaped) and unicode characters

---

## Assumptions

- Existing ggen CLI codebase is structured to easily add new subcommands via Rust clap command architecture
- Rust ecosystem has production-ready SHACL validation libraries (e.g., oxigraph with SHACL support or similar)
- Target users have basic familiarity with RDF/Turtle syntax and SHACL constraint language (no tutorial mode needed in v1)

---

**Generated with**: [ggen v6](https://github.com/seanchatmangpt/ggen) ontology-driven specification system
**Constitutional Equation**: `spec.md = μ(ontology)`
