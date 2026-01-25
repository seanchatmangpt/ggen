<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Audit Trail Feature](#audit-trail-feature)
  - [Overview](#overview)
  - [Purpose](#purpose)
  - [Usage](#usage)
    - [Basic Usage](#basic-usage)
    - [Combined with Other Flags](#combined-with-other-flags)
  - [Audit Trail Structure](#audit-trail-structure)
    - [Output Location](#output-location)
    - [JSON Schema](#json-schema)
  - [Key Fields](#key-fields)
    - [execution_id](#execution_id)
    - [pipeline_stages](#pipeline_stages)
    - [files.data_snapshot](#filesdata_snapshot)
    - [validation](#validation)
  - [Workflow Examples](#workflow-examples)
    - [Example 1: Safe Force Overwrite](#example-1-safe-force-overwrite)
    - [Example 2: Continuous Integration](#example-2-continuous-integration)
    - [Example 3: Debugging Generation Issues](#example-3-debugging-generation-issues)
    - [Example 4: Rollback Analysis](#example-4-rollback-analysis)
  - [Performance Impact](#performance-impact)
  - [Best Practices](#best-practices)
  - [Integration with Other Features](#integration-with-other-features)
    - [With `--force`](#with---force)
    - [With `--watch`](#with---watch)
    - [With `--validate-only`](#with---validate-only)
    - [With `--dry-run`](#with---dry-run)
  - [Troubleshooting](#troubleshooting)
    - [Issue: Audit trail not created](#issue-audit-trail-not-created)
    - [Issue: Audit files too large](#issue-audit-files-too-large)
    - [Issue: Missing execution_id in logs](#issue-missing-execution_id-in-logs)
  - [Security Considerations](#security-considerations)
  - [Audit Trail Recovery & Verification](#audit-trail-recovery--verification)
    - [File Integrity Verification](#file-integrity-verification)
      - [Verifying File Integrity](#verifying-file-integrity)
      - [Manual Verification (Using jq)](#manual-verification-using-jq)
    - [Detection of Unexpected File Changes](#detection-of-unexpected-file-changes)
      - [Change Detection Methods](#change-detection-methods)
    - [Restoration from Backup Procedure](#restoration-from-backup-procedure)
      - [Prerequisites](#prerequisites)
      - [Restoration Workflow](#restoration-workflow)
    - [Complete Recovery Example](#complete-recovery-example)
    - [Comparison with OTEL Span Traces](#comparison-with-otel-span-traces)
      - [Correlation Example](#correlation-example)
      - [Cross-Validation Workflow](#cross-validation-workflow)
  - [Troubleshooting](#troubleshooting-1)
    - [Issue: Missing audit.json Recovery](#issue-missing-auditjson-recovery)
    - [Issue: Corrupted Audit Entries Handling](#issue-corrupted-audit-entries-handling)
    - [Issue: Entropy-Based File Change Detection](#issue-entropy-based-file-change-detection)
    - [Issue: Audit Trail Too Large](#issue-audit-trail-too-large)
    - [Issue: Execution ID Not Found in OTEL Traces](#issue-execution-id-not-found-in-otel-traces)
  - [CLI Help for Audit Trail Commands](#cli-help-for-audit-trail-commands)
    - [Audit Verification Command](#audit-verification-command)
    - [Audit Recovery Command](#audit-recovery-command)
    - [Audit Diff Command](#audit-diff-command)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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

## Audit Trail Recovery & Verification

### File Integrity Verification

The audit trail provides comprehensive recovery capabilities through SHA256 hashing of all generated files. This enables detection of unexpected changes and verification of file integrity.

#### Verifying File Integrity

```bash
# Verify all files against audit trail
ggen audit-verify --audit-file .ggen/audit/latest.json

# Verify specific file
ggen audit-verify --audit-file .ggen/audit/latest.json --file src/models/user.rs

# Continuous monitoring for unexpected changes
ggen audit-verify --audit-file .ggen/audit/latest.json --watch
```

#### Manual Verification (Using jq)

```bash
# Extract expected hash for a file
expected_hash=$(jq -r '.files[] | select(.path == "src/models/user.rs") | .content_hash' .ggen/audit/latest.json)

# Calculate current hash
current_hash=$(sha256sum src/models/user.rs | cut -d' ' -f1)

# Compare
if [ "$expected_hash" != "$current_hash" ]; then
  echo "WARNING: File has been modified outside of ggen!"
  echo "Expected: $expected_hash"
  echo "Current:  $current_hash"
fi
```

### Detection of Unexpected File Changes

The audit trail enables detection of files modified outside the ggen pipeline through hash comparison and entropy analysis.

#### Change Detection Methods

**1. SHA256 Hash Comparison (Deterministic)**

```bash
# Detect ALL changed files
jq -r '.files[] | "\(.path)|\(.content_hash)"' .ggen/audit/latest.json | while IFS='|' read -r path hash; do
  current=$(sha256sum "$path" 2>/dev/null | cut -d' ' -f1)
  if [ "$current" != "$hash" ]; then
    echo "MODIFIED: $path"
  fi
done
```

**2. Entropy-Based Change Detection (Statistical)**

For large codebases, entropy analysis can identify files with unusual modification patterns:

```bash
# Calculate file entropy (requires Python)
python3 <<'EOF'
import json
import math
from collections import Counter

def calculate_entropy(data):
    """Calculate Shannon entropy of byte sequence"""
    if not data:
        return 0.0
    counter = Counter(data)
    length = len(data)
    return -sum((count/length) * math.log2(count/length) for count in counter.values())

# Load audit trail
with open('.ggen/audit/latest.json') as f:
    audit = json.load(f)

# Check files for unusual entropy (may indicate manual modification)
for file_info in audit['files']:
    path = file_info['path']
    try:
        with open(path, 'rb') as f:
            data = f.read()
        entropy = calculate_entropy(data)

        # Generated code typically has entropy 4.5-6.0
        # Manual edits often increase entropy > 6.5
        if entropy > 6.5:
            print(f"HIGH ENTROPY ({entropy:.2f}): {path} (possible manual edit)")
    except FileNotFoundError:
        print(f"MISSING: {path}")
EOF
```

**3. Timestamp-Based Detection**

```bash
# Find files modified after last sync
audit_time=$(jq -r '.timestamp' .ggen/audit/latest.json)
audit_epoch=$(date -d "$audit_time" +%s 2>/dev/null || date -j -f "%Y-%m-%dT%H:%M:%S" "$audit_time" +%s)

find src -type f -name "*.rs" -newer <(touch -d "@$audit_epoch" /tmp/audit_marker) | while read -r file; do
  echo "MODIFIED AFTER SYNC: $file"
done
```

### Restoration from Backup Procedure

#### Prerequisites

1. **Audit trail exists**: `.ggen/audit/latest.json` or timestamped audit file
2. **Git history available**: Files can be restored from git commits
3. **Known good state**: Reference audit trail from successful generation

#### Restoration Workflow

**Step 1: Identify Changed Files**

```bash
# Compare current state to audit trail
cat .ggen/audit/latest.json | jq -r '.files[] | .path' > /tmp/expected_files.txt

# Find files with mismatched hashes
while read -r path; do
  expected=$(jq -r ".files[] | select(.path == \"$path\") | .content_hash" .ggen/audit/latest.json)
  current=$(sha256sum "$path" 2>/dev/null | cut -d' ' -f1)

  if [ "$current" != "$expected" ]; then
    echo "$path" >> /tmp/changed_files.txt
  fi
done < /tmp/expected_files.txt

echo "Changed files:"
cat /tmp/changed_files.txt
```

**Step 2: Backup Current State**

```bash
# Create backup before restoration
backup_dir=".ggen/backups/$(date +%Y%m%d-%H%M%S)"
mkdir -p "$backup_dir"

while read -r file; do
  mkdir -p "$backup_dir/$(dirname "$file")"
  cp "$file" "$backup_dir/$file"
done < /tmp/changed_files.txt

echo "Backed up changed files to: $backup_dir"
```

**Step 3: Restore from Git (If Available)**

```bash
# Find commit that matches audit trail timestamp
audit_time=$(jq -r '.timestamp' .ggen/audit/latest.json)
git_commit=$(git log --until="$audit_time" --format="%H" -n 1)

echo "Restoring from commit: $git_commit"

# Restore changed files
while read -r file; do
  git checkout "$git_commit" -- "$file"
  echo "Restored: $file"
done < /tmp/changed_files.txt
```

**Step 4: Regenerate from Ontology (Recommended)**

```bash
# The BEST recovery method: regenerate from source of truth
ggen sync --audit --force

# Verify restoration
ggen audit-verify --audit-file .ggen/audit/latest.json
```

### Complete Recovery Example

```bash
#!/bin/bash
# audit-recovery.sh - Restore files to audit trail state

set -e

AUDIT_FILE="${1:-.ggen/audit/latest.json}"

if [ ! -f "$AUDIT_FILE" ]; then
  echo "ERROR: Audit file not found: $AUDIT_FILE"
  exit 1
fi

echo "=== AUDIT TRAIL RECOVERY ==="
echo "Audit file: $AUDIT_FILE"
echo "Timestamp: $(jq -r '.timestamp' "$AUDIT_FILE")"
echo ""

# Step 1: Detect changes
echo "[1/4] Detecting file changes..."
changed_files=()
while IFS= read -r path; do
  expected=$(jq -r ".files[] | select(.path == \"$path\") | .content_hash" "$AUDIT_FILE")

  if [ ! -f "$path" ]; then
    echo "  MISSING: $path"
    changed_files+=("$path")
    continue
  fi

  current=$(sha256sum "$path" | cut -d' ' -f1)
  if [ "$current" != "$expected" ]; then
    echo "  MODIFIED: $path"
    changed_files+=("$path")
  fi
done < <(jq -r '.files[] | .path' "$AUDIT_FILE")

if [ ${#changed_files[@]} -eq 0 ]; then
  echo "✓ No changes detected. All files match audit trail."
  exit 0
fi

echo "Found ${#changed_files[@]} changed file(s)"
echo ""

# Step 2: Backup
echo "[2/4] Backing up current state..."
backup_dir=".ggen/backups/$(date +%Y%m%d-%H%M%S)"
mkdir -p "$backup_dir"

for file in "${changed_files[@]}"; do
  if [ -f "$file" ]; then
    mkdir -p "$backup_dir/$(dirname "$file")"
    cp "$file" "$backup_dir/$file"
  fi
done

echo "✓ Backed up to: $backup_dir"
echo ""

# Step 3: Regenerate
echo "[3/4] Regenerating from ontology..."
ggen sync --audit --force

echo "✓ Regeneration complete"
echo ""

# Step 4: Verify
echo "[4/4] Verifying restoration..."
mismatches=0
for file in "${changed_files[@]}"; do
  expected=$(jq -r ".files[] | select(.path == \"$file\") | .content_hash" .ggen/audit/latest.json)
  current=$(sha256sum "$file" | cut -d' ' -f1)

  if [ "$current" != "$expected" ]; then
    echo "  MISMATCH: $file"
    ((mismatches++))
  fi
done

if [ $mismatches -eq 0 ]; then
  echo "✓ All files restored successfully"
  exit 0
else
  echo "⚠ WARNING: $mismatches file(s) still mismatched"
  exit 1
fi
```

### Comparison with OTEL Span Traces

Audit trails complement OpenTelemetry spans by providing file-level verification, while OTEL provides execution-level observability.

#### Correlation Example

```bash
# Extract execution_id from audit trail
execution_id=$(jq -r '.execution_id' .ggen/audit/latest.json)

# Query OTEL collector for matching spans
curl -X POST http://localhost:4318/v1/traces \
  -H "Content-Type: application/json" \
  -d "{
    \"resource_spans\": [{
      \"scope_spans\": [{
        \"spans\": [{
          \"attributes\": [{
            \"key\": \"execution_id\",
            \"value\": {\"stringValue\": \"$execution_id\"}
          }]
        }]
      }]
    }]
  }" | jq '.resource_spans[].scope_spans[].spans[] | {name, duration, status}'
```

#### Cross-Validation Workflow

```bash
# Verify audit trail matches OTEL trace
audit_duration=$(jq -r '.total_duration_ms' .ggen/audit/latest.json)
otel_duration=$(query_otel_span "$execution_id" | jq -r '.duration_ms')

if [ "$audit_duration" != "$otel_duration" ]; then
  echo "WARNING: Audit and OTEL durations mismatch"
  echo "Audit: ${audit_duration}ms"
  echo "OTEL:  ${otel_duration}ms"
fi

# Verify file count matches
audit_files=$(jq -r '.files | length' .ggen/audit/latest.json)
otel_files=$(query_otel_span "$execution_id" | jq -r '.attributes.files_generated')

if [ "$audit_files" != "$otel_files" ]; then
  echo "WARNING: File count mismatch"
  echo "Audit: $audit_files files"
  echo "OTEL:  $otel_files files"
fi
```

## Troubleshooting

### Issue: Missing audit.json Recovery

**Symptoms**: `.ggen/audit/audit.json` or `.ggen/audit/latest.json` does not exist

**Causes**:
1. First-time execution without `--audit` flag
2. `.ggen/audit/` directory was deleted
3. File system permissions issue
4. Concurrent execution conflict

**Fix**:

```bash
# Method 1: Regenerate with audit enabled
ggen sync --audit

# Method 2: Restore from git history (if committed)
git checkout HEAD -- .ggen/audit/

# Method 3: Recreate from timestamped audit files
if ls .ggen/audit/*.json 2>/dev/null | grep -v latest.json; then
  latest=$(ls -t .ggen/audit/*.json | grep -v latest.json | head -1)
  ln -sf "$(basename "$latest")" .ggen/audit/latest.json
  echo "Restored latest.json from: $latest"
fi

# Method 4: Accept current state as baseline
echo "WARNING: Creating new baseline audit trail"
ggen sync --audit --force
```

### Issue: Corrupted Audit Entries Handling

**Symptoms**: `jq` fails to parse audit.json, or fields are missing/malformed

**Diagnosis**:

```bash
# Check JSON validity
jq empty .ggen/audit/latest.json
if [ $? -ne 0 ]; then
  echo "ERROR: audit.json is not valid JSON"
fi

# Check required fields
required_fields=("timestamp" "rules_executed" "files_changed" "file_hashes" "metadata")
for field in "${required_fields[@]}"; do
  if ! jq -e ".$field" .ggen/audit/latest.json >/dev/null 2>&1; then
    echo "MISSING FIELD: $field"
  fi
done
```

**Fix**:

```bash
# Attempt to repair JSON (if truncated)
python3 <<'EOF'
import json
import sys

try:
    with open('.ggen/audit/latest.json', 'r') as f:
        content = f.read()

    # Try to parse
    data = json.loads(content)
    print("✓ JSON is valid")

except json.JSONDecodeError as e:
    print(f"ERROR at line {e.lineno}, col {e.colno}: {e.msg}")

    # Attempt recovery by finding last valid JSON closing
    content = content[:e.pos] + '}'
    try:
        data = json.loads(content)
        print("✓ Repaired JSON by truncating")

        # Write repaired version
        with open('.ggen/audit/latest.json.repaired', 'w') as f:
            json.dump(data, f, indent=2)
        print("Wrote repaired version to: latest.json.repaired")
    except:
        print("✗ Cannot repair - regeneration required")
        sys.exit(1)
EOF

# If repair succeeded, validate and replace
if [ -f .ggen/audit/latest.json.repaired ]; then
  mv .ggen/audit/latest.json .ggen/audit/latest.json.backup
  mv .ggen/audit/latest.json.repaired .ggen/audit/latest.json
  echo "Restored repaired audit trail"
fi
```

### Issue: Entropy-Based File Change Detection

**Use Case**: Detect files with manual edits that may have bypassed ggen pipeline

**Implementation**:

```python
#!/usr/bin/env python3
# entropy-check.py - Detect anomalous file modifications

import json
import math
import sys
from collections import Counter
from pathlib import Path

def calculate_entropy(data: bytes) -> float:
    """Calculate Shannon entropy of byte sequence"""
    if not data:
        return 0.0
    counter = Counter(data)
    length = len(data)
    probs = [count/length for count in counter.values()]
    return -sum(p * math.log2(p) for p in probs if p > 0)

def analyze_files(audit_path: str, threshold: float = 6.5):
    """Analyze files for unusual entropy patterns"""
    with open(audit_path) as f:
        audit = json.load(f)

    anomalies = []
    missing = []

    for file_info in audit['files']:
        path = file_info['path']
        expected_hash = file_info['content_hash']

        if not Path(path).exists():
            missing.append(path)
            continue

        with open(path, 'rb') as f:
            data = f.read()

        entropy = calculate_entropy(data)

        # Generated code typically 4.5-6.0, manual edits often > 6.5
        if entropy > threshold:
            anomalies.append({
                'path': path,
                'entropy': entropy,
                'threshold': threshold
            })

    # Report
    if anomalies:
        print("⚠ HIGH ENTROPY FILES (possible manual edits):")
        for item in anomalies:
            print(f"  {item['entropy']:.2f} > {item['threshold']}: {item['path']}")

    if missing:
        print("\n⚠ MISSING FILES:")
        for path in missing:
            print(f"  {path}")

    if not anomalies and not missing:
        print("✓ All files have normal entropy patterns")

    return len(anomalies) + len(missing)

if __name__ == '__main__':
    audit_file = sys.argv[1] if len(sys.argv) > 1 else '.ggen/audit/latest.json'
    threshold = float(sys.argv[2]) if len(sys.argv) > 2 else 6.5

    exit_code = analyze_files(audit_file, threshold)
    sys.exit(min(exit_code, 1))
```

**Usage**:

```bash
# Check default threshold (6.5)
python3 entropy-check.py .ggen/audit/latest.json

# Custom threshold
python3 entropy-check.py .ggen/audit/latest.json 7.0

# Integrate into CI/CD
if python3 entropy-check.py .ggen/audit/latest.json; then
  echo "✓ Entropy check passed"
else
  echo "⚠ High entropy detected - manual review required"
  exit 1
fi
```

### Issue: Audit Trail Too Large

**Symptoms**: `.ggen/audit/latest.json` exceeds 100MB, causing performance issues

**Cause**: Large `data_snapshot` fields for complex templates with deep object graphs

**Fix**:

```bash
# Method 1: Disable data snapshots (environment variable)
export GGEN_AUDIT_MAX_SNAPSHOT_SIZE=0  # Disable snapshots entirely
ggen sync --audit

# Method 2: Limit snapshot size (10KB per file)
export GGEN_AUDIT_MAX_SNAPSHOT_SIZE=10000
ggen sync --audit

# Method 3: Archive and compress old audits
find .ggen/audit -name "*.json" -mtime +30 -type f | while read -r file; do
  gzip "$file"
  echo "Compressed: $file"
done

# Method 4: Prune old audit trails (keep last 30 days)
find .ggen/audit -name "*.json.gz" -mtime +30 -type f -delete
echo "Pruned audit trails older than 30 days"
```

### Issue: Execution ID Not Found in OTEL Traces

**Symptoms**: `execution_id` from audit.json does not appear in OTEL spans

**Cause**: Logging framework not configured to extract execution context

**Fix**:

```rust
// Configure tracing to include execution_id
use tracing::{info, span, Level};
use tracing_subscriber::layer::SubscriberExt;

// In pipeline executor
let execution_id = uuid::Uuid::new_v4();
let span = span!(Level::INFO, "ggen_sync", execution_id = %execution_id);
let _guard = span.enter();

info!(execution_id = %execution_id, "Starting sync pipeline");

// Emit OTEL span with execution_id attribute
#[cfg(feature = "otel")]
{
    use opentelemetry::trace::TraceContextExt;
    let cx = tracing::Span::current().context();
    cx.span().set_attribute(
        opentelemetry::KeyValue::new("execution_id", execution_id.to_string())
    );
}
```

## CLI Help for Audit Trail Commands

### Audit Verification Command

```bash
ggen audit-verify --help
```

**Output**:
```
Verify file integrity against audit trail

USAGE:
    ggen audit-verify [OPTIONS]

OPTIONS:
    --audit-file <PATH>     Path to audit.json file [default: .ggen/audit/latest.json]
    --file <PATH>           Verify specific file (can be repeated)
    --watch                 Continuously monitor for changes
    --format <FORMAT>       Output format: text, json [default: text]
    --fix                   Automatically regenerate mismatched files
    --entropy-threshold <F> Entropy threshold for anomaly detection [default: 6.5]

EXAMPLES:
    # Verify all files
    ggen audit-verify

    # Verify specific file
    ggen audit-verify --file src/models/user.rs

    # Continuous monitoring
    ggen audit-verify --watch

    # Auto-fix mismatches
    ggen audit-verify --fix

    # JSON output for CI/CD
    ggen audit-verify --format json

EXIT CODES:
    0    All files match audit trail
    1    Mismatches detected
    2    Audit file not found or corrupted
```

### Audit Recovery Command

```bash
ggen audit-recover --help
```

**Output**:
```
Restore files to audit trail state

USAGE:
    ggen audit-recover [OPTIONS]

OPTIONS:
    --audit-file <PATH>     Path to audit.json file [default: .ggen/audit/latest.json]
    --backup-dir <PATH>     Backup directory [default: .ggen/backups/TIMESTAMP]
    --from-git              Restore from git commit matching audit timestamp
    --regenerate            Regenerate from ontology (recommended)
    --dry-run               Show what would be restored without making changes

EXAMPLES:
    # Backup and regenerate
    ggen audit-recover --regenerate

    # Restore from git
    ggen audit-recover --from-git

    # Preview changes
    ggen audit-recover --dry-run

WORKFLOW:
    1. Detect changed files (SHA256 comparison)
    2. Backup current state to .ggen/backups/
    3. Restore using selected method
    4. Verify restoration against audit trail

EXIT CODES:
    0    Successfully restored
    1    Partial restoration (some files failed)
    2    Audit file not found
    3    No changes detected
```

### Audit Diff Command

```bash
ggen audit-diff --help
```

**Output**:
```
Compare two audit trails

USAGE:
    ggen audit-diff <AUDIT1> <AUDIT2> [OPTIONS]

ARGUMENTS:
    <AUDIT1>    First audit trail (older)
    <AUDIT2>    Second audit trail (newer)

OPTIONS:
    --format <FORMAT>       Output format: text, json, diff [default: text]
    --files-only            Show only file changes (no metadata)
    --metadata-only         Show only metadata changes

EXAMPLES:
    # Compare two audit trails
    ggen audit-diff .ggen/audit/2025-12-20T14-30-45.json .ggen/audit/latest.json

    # Show only file changes
    ggen audit-diff old.json new.json --files-only

    # Machine-readable JSON
    ggen audit-diff old.json new.json --format json

OUTPUT:
    - Files added
    - Files removed
    - Files modified (with hash diff)
    - Metadata changes (duration, rules executed)

EXIT CODES:
    0    Trails compared successfully
    1    Differences found
    2    Audit file error
```

## Related Documentation

- [Force Flag](force-flag.md) - Overwrite protection and audit trail
- [Validation](validation.md) - SHACL and SPARQL constraint checking
- [Watch Mode](watch-mode.md) - Continuous audit trail generation
