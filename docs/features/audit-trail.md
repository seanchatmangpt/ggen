# Audit Trail Feature

## Overview

The `--audit` flag enables comprehensive change tracking for all code generation operations. When enabled, ggen creates a detailed JSON audit trail documenting every file operation, transformation decision, and generation step.

## Purpose

- **Regulatory Compliance**: Track all code changes for audit purposes
- **Debugging**: Understand why specific code was generated
- **Rollback**: Restore previous states with confidence
- **Analytics**: Analyze generation patterns over time
- **Safety**: Review changes before applying `--force`

## Usage

### Basic Usage

```bash
# Enable audit trail for current sync
ggen sync --audit
```

### Combined with Other Flags

```bash
# Audit + dry-run (preview what WOULD be logged)
ggen sync --audit --dry-run

# Audit + force (CRITICAL: track destructive changes)
ggen sync --audit --force

# Audit + watch mode (continuous tracking)
ggen sync --audit --watch

# Audit + specific rule
ggen sync --audit --rule structs
```

## Audit Trail Structure

### Output Location

```
.ggen/audit/
├── 2025-12-20T14-30-45.json    # Timestamped audit files
├── 2025-12-20T14-31-12.json
└── latest.json                  # Symlink to most recent
```

### JSON Schema

```json
{
  "timestamp": "2025-12-20T14:30:45Z",
  "manifest": "ggen.toml",
  "execution_id": "uuid-v4",
  "options": {
    "dry_run": false,
    "force": true,
    "watch": false,
    "validate_only": false
  },
  "pipeline_stages": [
    {
      "stage": "ontology_load",
      "duration_ms": 45,
      "triples_loaded": 1247,
      "source_files": ["domain.ttl", "inference.ttl"]
    },
    {
      "stage": "inference",
      "duration_ms": 123,
      "rules_executed": 12,
      "triples_inferred": 348,
      "construct_queries": ["infer_types", "infer_relationships"]
    },
    {
      "stage": "generation",
      "duration_ms": 267,
      "rules_executed": 5,
      "files_generated": 23
    }
  ],
  "files": [
    {
      "path": "src/models/user.rs",
      "action": "updated",
      "size_bytes": 2048,
      "hash_before": "sha256:abc123...",
      "hash_after": "sha256:def456...",
      "template": "rust_struct.tera",
      "sparql_query": "SELECT ?struct WHERE { ... }",
      "data_snapshot": {
        "struct_name": "User",
        "fields": [...]
      }
    }
  ],
  "validation": {
    "shacl_constraints": 12,
    "shacl_violations": 0,
    "sparql_asks": 3,
    "ask_results": [true, true, false]
  },
  "summary": {
    "files_created": 15,
    "files_updated": 8,
    "files_unchanged": 42,
    "total_duration_ms": 435,
    "errors": []
  }
}
```

## Key Fields

### execution_id
Unique identifier (UUID v4) for this sync operation. Used for:
- Correlating logs across distributed systems
- Linking audit trails to OTEL traces
- Debugging specific runs

### pipeline_stages
Detailed breakdown of each pipeline stage:
- `ontology_load`: RDF parsing and graph construction
- `inference`: CONSTRUCT query execution
- `generation`: Template rendering and file writes
- `validation`: SHACL/SPARQL checks

### files.data_snapshot
Complete data sent to templates. Enables:
- Exact reproduction of generated code
- Understanding template input context
- Debugging template errors

### validation
Results from all validation rules:
- SHACL shape violations (structural constraints)
- SPARQL ASK results (semantic constraints)
- Custom validation hooks

## Workflow Examples

### Example 1: Safe Force Overwrite

```bash
# Step 1: Audit what would be overwritten
ggen sync --audit --dry-run

# Step 2: Review audit trail
cat .ggen/audit/latest.json | jq '.files[] | select(.action == "updated")'

# Step 3: Apply changes with audit enabled
ggen sync --audit --force

# Step 4: Verify changes match expectations
diff <(git show HEAD:src/models/user.rs) src/models/user.rs
```

### Example 2: Continuous Integration

```bash
# CI pipeline script
#!/bin/bash
set -e

# Always enable audit in CI
ggen sync --audit --format json > sync_result.json

# Archive audit trail as artifact
cp .ggen/audit/latest.json "artifacts/audit_${CI_BUILD_ID}.json"

# Verify no unexpected changes
if jq -e '.files[] | select(.action == "updated" and .path | startswith("vendor/"))' sync_result.json; then
  echo "ERROR: Unexpected vendor file modification detected"
  exit 1
fi
```

### Example 3: Debugging Generation Issues

```bash
# Generate with audit
ggen sync --audit --rule structs --verbose

# Extract template inputs for specific file
cat .ggen/audit/latest.json | \
  jq '.files[] | select(.path == "src/models/user.rs") | .data_snapshot' \
  > debug_user_data.json

# Manually test template with this data
tera render templates/rust_struct.tera debug_user_data.json
```

### Example 4: Rollback Analysis

```bash
# List recent audits
ls -lt .ggen/audit/*.json | head -5

# Compare two audit trails
diff <(jq '.files[].path' .ggen/audit/2025-12-20T14-30-45.json) \
     <(jq '.files[].path' .ggen/audit/2025-12-20T14-31-12.json)

# Identify what changed between runs
jq -s '.[0].files - .[1].files' \
  .ggen/audit/2025-12-20T14-31-12.json \
  .ggen/audit/2025-12-20T14-30-45.json
```

## Performance Impact

- **Memory**: ~2-5MB per 1000 files generated
- **Disk**: ~50KB per audit trail (compressed JSON)
- **Speed**: <5% overhead (primarily JSON serialization)

## Best Practices

1. **Always use `--audit` with `--force`**
   - Track destructive changes
   - Enable rollback if needed

2. **Archive audit trails in CI**
   - Keep 30 days of history
   - Correlate with deployments

3. **Use `execution_id` for tracing**
   - Link to OTEL spans
   - Search logs across systems

4. **Review `data_snapshot` for debugging**
   - Understand template inputs
   - Reproduce issues locally

5. **Check validation results**
   - `shacl_violations > 0`: Fix ontology constraints
   - `ask_results` with `false`: Conditional execution skipped

## Integration with Other Features

### With `--force`
```bash
ggen sync --audit --force
# audit.json will show hash_before/hash_after for overwritten files
```

### With `--watch`
```bash
ggen sync --audit --watch
# Creates new audit trail on each trigger
# Files: audit/2025-12-20T14-30-45.json, audit/2025-12-20T14-31-12.json, ...
```

### With `--validate-only`
```bash
ggen sync --audit --validate-only
# Audit trail contains validation results ONLY (no file operations)
```

### With `--dry-run`
```bash
ggen sync --audit --dry-run
# Audit trail shows what WOULD happen (action = "would_create", "would_update")
```

## Troubleshooting

### Issue: Audit trail not created

**Cause**: `.ggen/audit/` directory doesn't exist or lacks write permissions

**Fix**:
```bash
mkdir -p .ggen/audit
chmod 755 .ggen/audit
```

### Issue: Audit files too large

**Cause**: Large `data_snapshot` fields for complex templates

**Fix**: Limit data snapshot size (env var):
```bash
GGEN_AUDIT_MAX_SNAPSHOT_SIZE=10000 ggen sync --audit
# Default: unlimited, recommended: 10KB per file
```

### Issue: Missing execution_id in logs

**Cause**: Logging framework not configured to extract from context

**Fix**: Use structured logging:
```rust
tracing::info!(
    execution_id = %ctx.execution_id,
    "Generation complete"
);
```

## Security Considerations

- **Audit trails may contain sensitive data** (ontology content, file paths)
- Store audit trails in secure locations (`.ggen/` is gitignored by default)
- Sanitize audit trails before sharing externally
- Use encryption for audit archives in compliance environments

## Related Documentation

- [Force Flag](force-flag.md) - Overwrite protection and audit trail
- [Validation](validation.md) - SHACL and SPARQL constraint checking
- [Watch Mode](watch-mode.md) - Continuous audit trail generation
