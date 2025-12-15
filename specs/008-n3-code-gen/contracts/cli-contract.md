# CLI Contract: ggen v5 - Unified Sync Command

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14 | **Version**: 5.0.0

---

## Overview

ggen v5 provides a single command: `ggen sync`. All other commands have been removed for a fresh start. This document specifies the complete CLI interface for the unified sync command.

**Breaking Change**: v5.0 removes ALL legacy commands. Only `ggen sync` is available.

---

## The Only Command: `ggen sync`

Execute the complete code synchronization pipeline from a ggen.toml manifest.

### Synopsis

```bash
ggen sync [OPTIONS]
```

### Options

| Option | Short | Type | Default | Description |
|--------|-------|------|---------|-------------|
| `--manifest` | `-m` | Path | `./ggen.toml` | Path to manifest file |
| `--output-dir` | `-o` | Path | from manifest | Override output directory |
| `--dry-run` | `-n` | Flag | false | Show what would be generated without writing |
| `--force` | `-f` | Flag | false | Overwrite existing files |
| `--audit` | `-a` | Flag | from manifest | Generate audit.json |
| `--rule` | `-r` | String | all | Execute specific generation rule(s) |
| `--verbose` | `-v` | Flag | false | Show detailed progress |
| `--watch` | `-w` | Flag | false | Watch for changes and regenerate |
| `--validate-only` | | Flag | false | Validate manifest and ontology only |
| `--format` | | String | text | Output format: text, json |
| `--timeout` | `-t` | u64 | 30000 | Overall timeout (ms) |

### Exit Codes

| Code | Name | Meaning | Example Scenario |
|------|------|---------|------------------|
| 0 | Success | Sync completed successfully | All files generated |
| 1 | ManifestError | Manifest validation failed | Missing required fields, invalid TOML |
| 2 | OntologyError | Ontology load/parse error | File not found, invalid Turtle syntax |
| 3 | SparqlError | SPARQL query syntax or execution error | Invalid query, timeout |
| 4 | TemplateError | Tera template rendering failed | Missing variable, syntax error |
| 5 | IoError | File I/O error | Permission denied, disk full |
| 6 | Timeout | Operation exceeded time limit | SPARQL query too slow |

### Output (stdout)

**Normal mode**:
```
Synced 5 files in 1.234s
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
Executing generation rules:
  [1/5] structs: 5 entities (15ms)
  [2/5] traits: 2 entities (8ms)
  [3/5] impls: 7 entities (12ms)
  ...
Writing output:
  src/generated/models/user.rs (2.1KB)
  ...
Validation: PASSED
Audit trail: ./audit.json
Synced 5 files in 1.234s
```

**Dry-run mode** (`-n`):
```
[DRY RUN] Would sync 5 files:
  src/generated/models/user.rs (would create)
  src/generated/models/order.rs (would create)
  src/generated/traits/repository.rs (would overwrite)
  ...
```

**JSON format** (`--format json`):
```json
{
  "status": "success",
  "files_synced": 5,
  "duration_ms": 1234,
  "files": [
    {"path": "src/generated/models/user.rs", "size_bytes": 2150, "action": "created"},
    {"path": "src/generated/models/order.rs", "size_bytes": 1843, "action": "created"}
  ],
  "inference_rules_executed": 3,
  "generation_rules_executed": 5,
  "audit_trail": "./audit.json"
}
```

**Validate-only mode** (`--validate-only`):
```
Validating ggen.toml...

Manifest schema:     PASS
Ontology syntax:     PASS (domain/model.ttl: 1,234 triples)
SPARQL queries:      PASS (5 queries validated)
Templates:           PASS (3 templates validated)

All validations passed.
```

### Output (stderr)

**Manifest error**:
```
error[E0001]: Manifest validation failed
  --> ggen.toml
  |
  | [ontology]
  | source = "missing.ttl"
  |          ^^^^^^^^^^^^^ file not found
  |
  = help: Create the ontology file or update the path
```

**Ontology error**:
```
error[E0002]: Ontology parse error
  --> domain/model.ttl:15:20
  |
15| :User a rdfs:Clas .
   |              ^^^^ undefined prefix 'rdfs'
  |
  = help: Add prefix declaration: @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
```

**SPARQL error**:
```
error[E0003]: SPARQL syntax error
  --> queries/structs.sparql:15:20
  |
15| SELECT ?name WHER { ?s rdfs:label ?name }
   |                    ^^^^ expected WHERE
```

**Template error**:
```
error[E0004]: Template variable not found
  --> templates/struct.tera:8
  |
8 | pub struct {{ struct_name }} {
  |               ^^^^^^^^^^^ variable 'struct_name' not in context
  |
  = note: Available variables: name, fields, derives
```

### Examples

```bash
# Basic sync (the primary workflow)
ggen sync

# Sync from specific manifest
ggen sync --manifest project/ggen.toml

# Dry-run to preview changes
ggen sync --dry-run

# Sync specific rule only
ggen sync --rule structs

# Force overwrite with audit trail
ggen sync --force --audit

# Watch mode for development
ggen sync --watch --verbose

# Validate without generating
ggen sync --validate-only

# JSON output for CI/CD
ggen sync --format json

# Verbose output with custom timeout
ggen sync -v --timeout 60000
```

---

## Removed Commands

The following commands have been removed in ggen v5.0. They may be added back incrementally in future versions.

| Command | v4.x | v5.0 | Notes |
|---------|------|------|-------|
| `ggen generate` | ✓ | ✗ | Replaced by `ggen sync` |
| `ggen validate` | ✓ | ✗ | Use `ggen sync --validate-only` |
| `ggen init` | ✓ | ✗ | Use `ggen sync --help` for ggen.toml examples |
| `ggen template *` | ✓ | ✗ | Replaced by `ggen sync` |
| `ggen project *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen graph *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen ontology *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen marketplace *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen ai *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen test *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen utils *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen ci *` | ✓ | ✗ | Add back in v5.1+ |
| `ggen workflow *` | ✓ | ✗ | Add back in v5.1+ |

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
  sync:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Validate manifest and ontology
        run: ggen sync --validate-only --format json > validation.json
      - name: Sync code
        run: ggen sync --audit
      - name: Verify determinism
        run: |
          ggen sync --output-dir /tmp/gen2
          diff -r src/generated /tmp/gen2
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Validate before commit
ggen sync --validate-only || exit 1

# Regenerate and check for changes
ggen sync
if ! git diff --quiet src/generated/; then
  echo "error: Generated code out of sync. Run 'ggen sync' and commit."
  exit 1
fi
```

### Makefile Integration

```makefile
.PHONY: sync validate clean watch

sync:
	ggen sync --verbose --audit

validate:
	ggen sync --validate-only

clean:
	rm -rf src/generated/ audit.json

watch:
	ggen sync --watch --verbose
```

### cargo make Integration

```toml
# Makefile.toml
[tasks.sync]
command = "ggen"
args = ["sync", "--verbose", "--audit"]

[tasks.sync-validate]
command = "ggen"
args = ["sync", "--validate-only"]

[tasks.sync-watch]
command = "ggen"
args = ["sync", "--watch", "--verbose"]
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
- `E0001`: Manifest validation error
- `E0002`: Ontology error
- `E0003`: SPARQL error
- `E0004`: Template error
- `E0005`: I/O error
- `E0006`: Timeout error

---

## Migration from v4.x

### Quick Migration Guide

| v4.x Command | v5.0 Equivalent |
|--------------|-----------------|
| `ggen generate` | `ggen sync` |
| `ggen generate --dry-run` | `ggen sync --dry-run` |
| `ggen generate --audit` | `ggen sync --audit` |
| `ggen validate` | `ggen sync --validate-only` |

### Breaking Changes

1. **Command removal**: All commands except `ggen sync` are removed
2. **CLI structure**: No more noun-verb pattern, just `ggen sync`
3. **Exit codes**: Updated to semantic codes (0-6)

### Rationale

ggen v5 is a fresh start focused on one thing: synchronizing code from ontologies. The simplified CLI reduces cognitive load and makes the tool more predictable. Utility commands will be added back incrementally based on user needs.
