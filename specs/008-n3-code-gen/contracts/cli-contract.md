# CLI Contract: N3/CONSTRUCT Semantic Code Generator

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14 | **Version**: 1.0.0

---

## Overview

This document specifies the CLI interface for the N3/CONSTRUCT semantic code generator. All commands follow the existing ggen CLI patterns and exit code semantics.

---

## Commands

### 1. `ggen generate`

Execute the code generation pipeline from a ggen.toml manifest.

#### Synopsis

```bash
ggen generate [OPTIONS] [MANIFEST]
```

#### Arguments

| Argument | Type | Default | Description |
|----------|------|---------|-------------|
| `MANIFEST` | Path | `./ggen.toml` | Path to manifest file |

#### Options

| Option | Short | Type | Default | Description |
|--------|-------|------|---------|-------------|
| `--output-dir` | `-o` | Path | from manifest | Override output directory |
| `--dry-run` | `-n` | Flag | false | Show what would be generated without writing |
| `--force` | `-f` | Flag | false | Overwrite existing files |
| `--audit` | `-a` | Flag | from manifest | Generate audit.json |
| `--no-inference` | | Flag | false | Skip inference rules |
| `--rule` | `-r` | String | all | Execute specific generation rule(s) |
| `--verbose` | `-v` | Flag | false | Show detailed progress |
| `--quiet` | `-q` | Flag | false | Suppress non-error output |
| `--timeout` | `-t` | u64 | 30000 | Overall timeout (ms) |

#### Exit Codes

| Code | Meaning | Example Scenario |
|------|---------|------------------|
| 0 | Success | Generation completed successfully |
| 1 | ValidationError | Ontology or manifest validation failed |
| 2 | SparqlError | SPARQL query syntax or execution error |
| 3 | TemplateError | Tera template rendering failed |
| 4 | OutputInvalid | Generated Rust code failed syntax check |
| 5 | Timeout | Operation exceeded time limit |
| 127 | Internal | Unexpected error |

#### Output (stdout)

**Normal mode**:
```
Generated 5 files in 1.234s
  src/generated/models/user.rs (2.1KB)
  src/generated/models/order.rs (1.8KB)
  src/generated/traits/repository.rs (3.2KB)
  src/generated/impls/user_impl.rs (4.1KB)
  src/generated/impls/order_impl.rs (3.9KB)
```

**Verbose mode** (`-v`):
```
Loading manifest: ./ggen.toml
Loading ontology: domain/model.ttl (1,234 triples)
  Imported: domain/base.ttl (456 triples)
Executing inference rules:
  [1/3] auditable_fields: +12 triples (5ms)
  [2/3] uuid_derives: +8 triples (3ms)
  [3/3] relationship_methods: +24 triples (8ms)
Building code graph:
  [1/5] structs: 5 entities (15ms)
  [2/5] traits: 2 entities (8ms)
  [3/5] impls: 7 entities (12ms)
  ...
Writing output:
  src/generated/models/user.rs (2.1KB)
  ...
Validation: PASSED
Audit trail: ./audit.json
Generated 5 files in 1.234s
```

**Dry-run mode** (`-n`):
```
[DRY RUN] Would generate 5 files:
  src/generated/models/user.rs (would create)
  src/generated/models/order.rs (would create)
  src/generated/traits/repository.rs (would overwrite)
  ...
```

**Quiet mode** (`-q`): No output on success, errors only.

#### Output (stderr)

**Validation error**:
```
error[E0001]: Field missing type annotation
  --> domain/model.ttl
  |
  | :User :hasField :userName .
  |                 ^^^^^^^^^ no code:fieldType property
  |
  = help: Add `:userName code:fieldType "String" .`

error: aborting due to validation error
```

**SPARQL error**:
```
error[E0002]: SPARQL syntax error
  --> queries/structs.sparql:15:20
  |
15| SELECT ?name WHER { ?s rdfs:label ?name }
   |                    ^^^^ expected WHERE
```

**Template error**:
```
error[E0003]: Template variable not found
  --> templates/struct.tera:8
  |
8 | pub struct {{ struct_name }} {
  |               ^^^^^^^^^^^ variable 'struct_name' not in context
  |
  = note: Available variables: name, fields, derives
```

#### Examples

```bash
# Basic generation
ggen generate

# Generate from specific manifest
ggen generate project/ggen.toml

# Dry-run to preview
ggen generate --dry-run

# Generate specific rule only
ggen generate --rule structs

# Force overwrite with audit
ggen generate --force --audit

# Verbose output with custom timeout
ggen generate -v --timeout 60000
```

---

### 2. `ggen validate`

Validate ontology and manifest without generating code.

#### Synopsis

```bash
ggen validate [OPTIONS] [MANIFEST]
```

#### Arguments

| Argument | Type | Default | Description |
|----------|------|---------|-------------|
| `MANIFEST` | Path | `./ggen.toml` | Path to manifest file |

#### Options

| Option | Short | Type | Default | Description |
|--------|-------|------|---------|-------------|
| `--ontology` | `-o` | Flag | true | Validate ontology syntax |
| `--shacl` | `-s` | Flag | true | Run SHACL validation |
| `--manifest` | `-m` | Flag | true | Validate manifest schema |
| `--queries` | `-q` | Flag | true | Validate SPARQL syntax |
| `--all` | `-a` | Flag | false | Run all validations (default) |
| `--verbose` | `-v` | Flag | false | Show detailed results |
| `--format` | `-f` | String | text | Output format: text, json |

#### Exit Codes

| Code | Meaning | When |
|------|---------|------|
| 0 | Valid | All validations passed |
| 1 | Invalid | One or more validations failed |
| 127 | Internal | Unexpected error |

#### Output (stdout)

**Text format** (default):
```
Validating ggen.toml...

Manifest schema:     PASS
Ontology syntax:     PASS (domain/model.ttl: 1,234 triples)
SHACL constraints:   PASS (shapes/domain.ttl: 12 shapes)
SPARQL queries:      PASS (5 queries validated)

All validations passed.
```

**Text format with failures**:
```
Validating ggen.toml...

Manifest schema:     PASS
Ontology syntax:     PASS (1,234 triples)
SHACL constraints:   FAIL
  - :User: Missing required property :email (sh:minCount 1)
  - :Order: Invalid pattern for :orderId (expected UUID format)
SPARQL queries:      PASS

Validation failed with 2 errors.
```

**JSON format** (`--format json`):
```json
{
  "manifest": { "valid": true },
  "ontology": { "valid": true, "triples": 1234 },
  "shacl": {
    "valid": false,
    "errors": [
      {
        "focus": ":User",
        "path": ":email",
        "constraint": "sh:minCount",
        "message": "Missing required property :email"
      }
    ]
  },
  "queries": { "valid": true, "count": 5 }
}
```

#### Examples

```bash
# Full validation
ggen validate

# Validate specific manifest
ggen validate project/ggen.toml

# Only SHACL validation
ggen validate --shacl

# JSON output for CI
ggen validate --format json

# Verbose validation
ggen validate -v
```

---

### 3. `ggen init` (Extension)

Initialize a new ggen project with template ggen.toml.

#### Synopsis

```bash
ggen init [OPTIONS] [DIRECTORY]
```

#### Arguments

| Argument | Type | Default | Description |
|----------|------|---------|-------------|
| `DIRECTORY` | Path | `.` | Project directory |

#### Options

| Option | Short | Type | Default | Description |
|--------|-------|------|---------|-------------|
| `--name` | `-n` | String | dir name | Project name |
| `--template` | `-t` | String | basic | Template: basic, full, domain |
| `--force` | `-f` | Flag | false | Overwrite existing files |

#### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Directory exists with files |
| 127 | Internal error |

#### Examples

```bash
# Initialize in current directory
ggen init

# Initialize new project
ggen init my-project

# Full template with all features
ggen init --template full my-project
```

---

## Configuration Reference

### ggen.toml Schema

```toml
# Required: Project metadata
[project]
name = "my-domain"           # Project name (alphanumeric, hyphens)
version = "1.0.0"            # Semantic version

# Required: Ontology configuration
[ontology]
source = "domain/model.ttl"  # Primary ontology file
imports = [                  # Optional additional files
  "domain/base.ttl",
  "domain/types.ttl"
]
base_iri = "http://example.org/"  # Base IRI for relative URIs

[ontology.prefixes]          # Prefix mappings
code = "http://ggen.dev/code#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"

# Optional: Inference rules (CONSTRUCT-based)
[[inference.rules]]
name = "auditable_fields"
description = "Add created_at/updated_at to auditable entities"
construct = """
PREFIX code: <http://ggen.dev/code#>
CONSTRUCT { ... }
WHERE { ... }
"""
order = 1                    # Execution order (lower = earlier)

# Required: Generation rules
[[generation.rules]]
name = "structs"
query = { file = "queries/structs.sparql" }  # or { inline = "SELECT ..." }
template = { file = "templates/struct.tera" }
output_file = "src/generated/models/{{name}}.rs"
skip_empty = true            # Skip if query returns empty

[[generation.rules]]
name = "traits"
query = { inline = "SELECT ?name WHERE { ?t a code:Trait }" }
template = { file = "templates/trait.tera" }
output_file = "src/generated/traits/{{name}}.rs"

[generation]
max_sparql_timeout_ms = 5000
require_audit_trail = true
determinism_salt = "stable-v1"
output_dir = "src/generated"

# Optional: Validation
[validation]
shacl = ["shapes/domain.ttl"]
validate_syntax = true       # Check generated Rust syntax
no_unsafe = true             # Reject unsafe code
```

---

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GGEN_MANIFEST` | `./ggen.toml` | Default manifest path |
| `GGEN_OUTPUT_DIR` | from manifest | Override output directory |
| `GGEN_VERBOSE` | `false` | Enable verbose output |
| `GGEN_NO_COLOR` | `false` | Disable colored output |
| `GGEN_TIMEOUT_MS` | `30000` | Default timeout |

---

## Integration Examples

### CI/CD Pipeline

```yaml
# GitHub Actions
jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Validate ontology
        run: ggen validate --format json > validation.json
      - name: Generate code
        run: ggen generate --audit
      - name: Verify determinism
        run: |
          ggen generate --output-dir /tmp/gen2
          diff -r src/generated /tmp/gen2
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Validate before commit
ggen validate --quiet || exit 1

# Regenerate and check for changes
ggen generate --quiet
if ! git diff --quiet src/generated/; then
  echo "error: Generated code out of sync. Run 'ggen generate' and commit."
  exit 1
fi
```

### Makefile Integration

```makefile
.PHONY: generate validate clean

generate:
	ggen generate --verbose --audit

validate:
	ggen validate

clean:
	rm -rf src/generated/ audit.json

watch:
	watchexec -w domain/ -w templates/ -- ggen generate
```

---

## Error Message Format

All errors follow this structure:

```
error[ECODE]: Brief error description
  --> file:line:column
  |
N | Source line with problem
  |   ^^^^^ Specific problem location
  |
  = help: Actionable suggestion
  = note: Additional context
```

Error codes:
- `E0001`: Validation error
- `E0002`: SPARQL error
- `E0003`: Template error
- `E0004`: Output validation error
- `E0005`: Timeout error
