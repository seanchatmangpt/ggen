# Force Flag Feature

## Overview

The `--force` flag overrides ggen's default safety behavior of refusing to overwrite existing files. This is a **destructive operation** that should be used with extreme caution.

## Default Behavior (Without `--force`)

By default, ggen sync:
- **Creates** new files if they don't exist
- **Skips** existing files without modification
- **Reports** skipped files in output
- **Exits 0** even if files were skipped

This "create-only" mode ensures ggen never accidentally destroys manual code changes.

## Force Behavior (With `--force`)

When `--force` is enabled, ggen sync:
- **Overwrites** all existing files unconditionally
- **Destroys** any manual changes in those files
- **Cannot be undone** (unless `--audit` or git tracking enabled)
- **Logs** each overwrite operation

## Usage

### Basic Usage

```bash
# DANGER: Overwrites all generated files
ggen sync --force
```

### Safe Usage Patterns

```bash
# RECOMMENDED: Always combine with --audit
ggen sync --force --audit

# SAFE: Preview what would be overwritten
ggen sync --force --dry-run

# SAFER: Force only specific rule
ggen sync --force --rule structs

# SAFEST: Git commit before force
git add -A && git commit -m "Before ggen sync --force"
ggen sync --force --audit
```

## When to Use `--force`

### Legitimate Use Cases

1. **Initial Project Setup**
   - First-time generation into empty directories
   - No risk of overwriting manual code

2. **Pure Code Generation Projects**
   - 100% generated code (no manual edits allowed)
   - Version-controlled ggen.toml + ontologies only
   - Generated code is artifact (like compiled binaries)

3. **Ontology-Driven Refactoring**
   - Major schema changes require regenerating all code
   - Manual code changes already migrated to ontology
   - Controlled cutover scenario

4. **Disaster Recovery**
   - Regenerating code from known-good ontology state
   - Reverting manual corruption
   - Restoring from backup

5. **CI/CD Pipelines**
   - Automated generation in ephemeral environments
   - No risk of overwriting human work
   - Generated code is built/deployed, not committed

### Inappropriate Use Cases

1. **Exploratory Development**
   - Trying different ontology changes
   - **Use `--dry-run` instead**

2. **Partial Manual Code**
   - Some files are hand-edited
   - **Use merge mode or git markers instead**

3. **Production Hotfixes**
   - Emergency manual patches in generated code
   - **Fix in ontology, then force regenerate**

4. **Unsure What Will Change**
   - Don't know impact of ontology changes
   - **Use `--dry-run --audit` to preview**

## Safety Mechanisms

### 1. Combine with `--audit` (MANDATORY)

```bash
# WRONG: No audit trail
ggen sync --force

# RIGHT: Audit trail enables rollback
ggen sync --force --audit
```

The audit trail records:
- SHA256 hash of file before overwrite
- SHA256 hash of file after overwrite
- Full template input data snapshot
- Timestamp and execution ID

### 2. Use `--dry-run` First

```bash
# Step 1: Preview changes
ggen sync --force --dry-run --audit

# Step 2: Review audit trail
cat .ggen/audit/latest.json | jq '.files[] | select(.action == "would_update")'

# Step 3: Apply if safe
ggen sync --force --audit
```

### 3. Scope with `--rule`

```bash
# Instead of regenerating everything:
ggen sync --force

# Regenerate only specific rules:
ggen sync --force --rule structs
ggen sync --force --rule api_endpoints
```

### 4. Git Integration

```bash
# Commit before forcing
git add -A
git commit -m "Before ggen sync --force (ontology v2.3.1)"
ggen sync --force --audit

# Review changes
git diff

# Rollback if needed
git reset --hard HEAD^
```

## Workflow Examples

### Example 1: Safe Refactoring

```bash
#!/bin/bash
# safe_regenerate.sh - Safe force regeneration workflow

set -e

# Ensure clean working tree
if ! git diff-index --quiet HEAD --; then
  echo "ERROR: Uncommitted changes detected. Commit or stash first."
  exit 1
fi

# Preview changes
echo "=== Dry-run Preview ==="
ggen sync --force --dry-run --format json > preview.json

# Show what would change
jq -r '.files[] | select(.action | startswith("would_")) | "\(.action): \(.path)"' preview.json

# Confirm with user
read -p "Proceed with force regeneration? (yes/no): " confirm
if [ "$confirm" != "yes" ]; then
  echo "Aborted."
  exit 0
fi

# Create safety commit
git add -A
git commit -m "Pre-regeneration snapshot (ggen v5.1.0)"

# Execute with audit
ggen sync --force --audit --verbose

# Show diff
git diff --stat

# Offer rollback
read -p "Keep changes? (yes/no): " keep
if [ "$keep" != "yes" ]; then
  git reset --hard HEAD^
  echo "Rolled back."
  exit 0
fi

echo "Regeneration complete."
```

### Example 2: CI/CD Pipeline

```bash
# .github/workflows/generate.yml
name: Generate Code

on:
  push:
    paths:
      - 'ontology/**/*.ttl'
      - 'ggen.toml'

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install ggen
        run: cargo install ggen --version 5.1.0

      - name: Generate code
        run: |
          # Force is SAFE in CI (ephemeral environment)
          ggen sync --force --audit --format json | tee sync_result.json

      - name: Archive audit trail
        uses: actions/upload-artifact@v4
        with:
          name: audit-trail
          path: .ggen/audit/latest.json

      - name: Create PR with changes
        uses: peter-evans/create-pull-request@v5
        with:
          commit-message: "chore: Regenerate code from ontology"
          title: "Generated Code Update"
          body: |
            Automated code generation triggered by ontology changes.

            See audit trail artifact for details.
```

### Example 3: Pure Code Generation Project

```
# Project structure (NO manual code allowed)
project/
├── ontology/
│   ├── domain.ttl         # Source of truth
│   ├── inference.ttl
│   └── validation.ttl
├── templates/
│   └── rust_struct.tera   # Code templates
├── ggen.toml               # Generation rules
├── .gitignore
│   └── src/                # Generated code (NEVER commit)
└── README.md

# .gitignore
src/
target/
.ggen/

# Workflow: Always force regenerate
ggen sync --force
cargo build
cargo test
```

This pattern is valid because:
- `src/` is never committed to git
- Developers CANNOT make manual edits
- Source of truth is ontology + templates
- Generated code is treated like compiled artifacts

## Flag Precedence

When multiple flags conflict:

```bash
# --force overrides safety checks
ggen sync --force --dry-run
# Result: Dry-run (shows what WOULD be overwritten)

# --validate-only overrides --force
ggen sync --force --validate-only
# Result: Validation only (no files written)

# --watch + --force = continuous overwriting
ggen sync --force --watch
# Result: Every file change triggers full regeneration
```

## Error Handling

### Permission Denied

```bash
$ ggen sync --force
ERROR: Permission denied: src/models/user.rs (read-only)
```

**Fix**: Make file writable
```bash
chmod +w src/models/user.rs
ggen sync --force
```

### File Lock Conflicts

```bash
$ ggen sync --force
ERROR: File locked by another process: src/api/server.rs
```

**Fix**: Close editor/IDE, retry

### Audit Trail Failure

```bash
$ ggen sync --force --audit
ERROR: Cannot write audit trail: .ggen/audit/ (disk full)
```

**Fix**: Free disk space or disable audit (UNSAFE):
```bash
ggen sync --force  # Proceeds without audit
```

## Best Practices

1. **ALWAYS use `--audit` with `--force`**
   - Enables rollback via audit trail
   - Documents what was changed and why

2. **ALWAYS use `--dry-run` before real `--force`**
   - Preview impact of changes
   - Validate assumptions about file operations

3. **ALWAYS commit to git before `--force`**
   - Native rollback mechanism
   - Diff review with `git diff`

4. **Scope `--force` with `--rule` when possible**
   - Minimize blast radius
   - Easier to reason about changes

5. **Document why you used `--force`**
   ```bash
   git add -A
   git commit -m "Regenerate models after schema migration (BREAKING: User.email now required)"
   ```

6. **Never use `--force` in development without safety net**
   - Use merge mode for iterative development
   - Reserve `--force` for known-safe scenarios

## Security Considerations

- **`--force` can destroy security patches** applied manually to generated code
- **Always regenerate** from ontology if manual security fix needed
- **Audit trail** documents destruction of manual code
- **Git tracking** provides forensic evidence of changes

## Related Documentation

- [Audit Trail](audit-trail.md) - Track all file overwrites
- [Merge Mode](merge-mode.md) - Alternative to `--force` for preserving manual code
- [Validation](validation.md) - Pre-flight checks before `--force`
- [Watch Mode](watch-mode.md) - Continuous `--force` regeneration
