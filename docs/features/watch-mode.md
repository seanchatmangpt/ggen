# Watch Mode Feature

## Overview

The `--watch` flag enables continuous file monitoring and automatic code regeneration. When ontology files, templates, or `ggen.toml` change, ggen automatically re-runs the sync pipeline without manual intervention.

## Purpose

- **Rapid Iteration**: See code changes instantly as ontology evolves
- **Development Workflow**: TDD-style ontology development
- **Template Development**: Test template changes in real-time
- **Error Feedback**: Immediate validation errors on save

## Usage

### Basic Usage

```bash
# Watch ontology and templates, regenerate on change
ggen sync --watch
```

### Combined with Other Flags

```bash
# Watch with verbose logging
ggen sync --watch --verbose

# Watch with audit trail
ggen sync --watch --audit

# Watch with force (DANGER: auto-overwrites on every change)
ggen sync --watch --force --audit

# Watch specific rule only
ggen sync --watch --rule structs

# Watch with validation only (no generation)
ggen sync --watch --validate-only
```

## Watched Files

Watch mode monitors these file patterns (configurable in `ggen.toml`):

### Default Patterns

```
ggen.toml                    # Manifest changes
ontology/**/*.ttl            # Ontology files
templates/**/*.tera          # Tera templates
query/**/*.rq                # SPARQL queries
validation/**/*.ttl          # SHACL shapes
```

### Custom Patterns

```toml
# ggen.toml
[watch]
patterns = [
  "ggen.toml",
  "ontology/**/*.ttl",
  "templates/**/*.{tera,j2}",
  "query/**/*.{rq,sparql}",
  "validation/**/*.{ttl,shacl}"
]

# Ignore patterns
ignore = [
  "**/.git/**",
  "**/target/**",
  "**/*.swp",
  "**/*~"
]
```

## Debounce Behavior

Watch mode uses debouncing to prevent excessive regeneration:

### Default Debounce

```
File saved → Wait 300ms → No more changes? → Execute sync
```

### Customizable Debounce

```toml
# ggen.toml
[watch]
debounce_ms = 500  # Wait 500ms after last change
```

### Burst Protection

```toml
# ggen.toml
[watch]
debounce_ms = 300
max_burst_events = 10  # Ignore events beyond 10 in 1 second
```

Example: Editor auto-saves every 100ms while typing
- Events 1-10: Queued
- Events 11+: Ignored (burst protection)
- After 300ms quiet: Execute once

## Workflow Examples

### Example 1: Ontology-Driven Development

```bash
# Terminal 1: Watch mode
ggen sync --watch --verbose --audit

# Terminal 2: Rust tests
cargo watch -x test

# Terminal 3: Edit ontology
vim ontology/domain.ttl
# Save → Auto-regenerates → Tests re-run → Immediate feedback
```

### Example 2: Template Development

```bash
# Watch with dry-run (preview changes without writing)
ggen sync --watch --dry-run --format json | jq '.files[].path'

# Edit template
vim templates/rust_struct.tera
# Save → See JSON output → Iterate
```

### Example 3: SHACL Validation Iteration

```bash
# Watch with validate-only
ggen sync --watch --validate-only

# Terminal 2: Edit SHACL shapes
vim validation/domain_constraints.ttl
# Save → Validation runs → See violations → Fix → Repeat
```

### Example 4: Multi-Rule Development

```bash
# Watch all rules
ggen sync --watch --verbose

# ggen.toml
[[generation_rule]]
name = "structs"
query = "query/structs.rq"
template = "templates/rust_struct.tera"

[[generation_rule]]
name = "traits"
query = "query/traits.rq"
template = "templates/rust_trait.tera"

# Edit domain.ttl → Both rules regenerate
# Edit query/structs.rq → Only "structs" rule regenerates
```

## Intelligent Rule Filtering

Watch mode detects which files changed and only re-runs affected rules:

### Example: Query Change

```bash
# Edit query/structs.rq
vim query/structs.rq
# Only "structs" rule regenerates (uses structs.rq)
# "traits" rule skipped (uses traits.rq)
```

### Example: Ontology Change

```bash
# Edit ontology/domain.ttl
vim ontology/domain.ttl
# ALL rules regenerate (ontology affects all queries)
```

### Example: Template Change

```bash
# Edit templates/rust_struct.tera
vim templates/rust_struct.tera
# Only rules using rust_struct.tera regenerate
```

## Output Formats

### Terminal Output (Default)

```bash
$ ggen sync --watch
[14:30:45] Watching for changes...
[14:31:12] ontology/domain.ttl changed
[14:31:12] Running sync...
[14:31:13] ✓ Generated 5 files (320ms)
[14:31:13] Watching for changes...
```

### Verbose Output

```bash
$ ggen sync --watch --verbose
[14:30:45] Watch patterns:
  - ggen.toml
  - ontology/**/*.ttl
  - templates/**/*.tera
[14:30:45] Watching for changes...
[14:31:12] Event: Modify(ontology/domain.ttl)
[14:31:12] Debouncing... (300ms)
[14:31:12] Executing sync...
[14:31:12]   - Loading ontology... (45ms)
[14:31:12]   - Running inference... (123ms)
[14:31:12]   - Generating files... (152ms)
[14:31:13] ✓ Success (320ms total)
[14:31:13]   - Created: src/models/user.rs
[14:31:13]   - Updated: src/models/product.rs
[14:31:13]   - Unchanged: 42 files
[14:31:13] Watching for changes...
```

### JSON Output (for tools)

```bash
$ ggen sync --watch --format json
{"event":"start","timestamp":"2025-12-20T14:30:45Z"}
{"event":"watch_start","patterns":["ggen.toml","ontology/**/*.ttl"]}
{"event":"file_change","path":"ontology/domain.ttl","timestamp":"2025-12-20T14:31:12Z"}
{"event":"sync_start","execution_id":"uuid-1234"}
{"event":"sync_complete","status":"success","files_synced":5,"duration_ms":320}
{"event":"watch_resume"}
```

## Error Handling

### Transient Errors

Non-fatal errors don't stop watch mode:

```bash
[14:31:12] ontology/domain.ttl changed
[14:31:12] Running sync...
[14:31:12] ✗ ERROR: SPARQL syntax error (line 42)
[14:31:12] Watching for changes... (fix and save to retry)

[14:31:45] ontology/domain.ttl changed
[14:31:45] Running sync...
[14:31:46] ✓ Generated 5 files (310ms)
[14:31:46] Watching for changes...
```

### Fatal Errors

Some errors stop watch mode:

```bash
[14:31:12] FATAL: ggen.toml not found
[14:31:12] Watch mode exiting.
```

Fatal error types:
- `ggen.toml` deleted
- Working directory changed
- File system errors (inotify limit reached)
- User interrupt (Ctrl+C)

## Performance Optimization

### Incremental Compilation

Watch mode caches parsed ontology between runs:

```bash
# First sync: Parse ontology from scratch (300ms)
# Subsequent syncs: Reuse cached graph (50ms)

# Cache invalidated when:
# - Ontology files change
# - ggen.toml changes
# - Manual cache clear
```

### Parallel Execution

Multiple file changes trigger single sync (deduplicated):

```bash
# Edit 3 files in quick succession:
vim ontology/domain.ttl     # 14:31:10
vim ontology/inference.ttl  # 14:31:11
vim templates/struct.tera   # 14:31:12

# Debounce: Wait 300ms after last change (14:31:12 + 300ms = 14:31:12.3)
# Execute: Single sync with all changes (14:31:12.3)
```

### Selective Regeneration

Only regenerate files affected by changes:

```toml
# ggen.toml
[watch]
incremental = true  # Only regenerate affected files

# Example: Edit query/structs.rq
# Before (incremental=false): Regenerate all 100 files (2s)
# After (incremental=true): Regenerate only 5 struct files (200ms)
```

## Advanced Configuration

### Polling vs Native

```toml
# ggen.toml
[watch]
method = "native"  # Use inotify (Linux), FSEvents (macOS), ReadDirectoryChangesW (Windows)
# OR
method = "poll"    # Fallback: Poll filesystem every N ms

poll_interval_ms = 1000  # Only used if method = "poll"
```

### Resource Limits

```toml
# ggen.toml
[watch]
max_inotify_watches = 8192  # Linux only: increase inotify limit

# If watching fails with "too many open files":
# Linux: echo 'fs.inotify.max_user_watches=524288' | sudo tee -a /etc/sysctl.conf
# macOS: Increase ulimit -n
```

### Graceful Shutdown

```bash
# Ctrl+C triggers graceful shutdown
$ ggen sync --watch
[14:30:45] Watching for changes...
^C
[14:30:50] Interrupt received, finishing current sync...
[14:30:51] ✓ Sync complete, exiting cleanly.
```

## Integration Examples

### Example 1: Docker Development

```dockerfile
# Dockerfile.dev
FROM rust:1.75
WORKDIR /app

# Install ggen
RUN cargo install ggen --version 5.1.0

# Copy ontology and templates
COPY ontology/ /app/ontology/
COPY templates/ /app/templates/
COPY ggen.toml /app/

# Watch mode with volume mount
CMD ["ggen", "sync", "--watch", "--verbose"]
```

```bash
# docker-compose.yml
version: '3.8'
services:
  codegen:
    build:
      context: .
      dockerfile: Dockerfile.dev
    volumes:
      - ./ontology:/app/ontology
      - ./templates:/app/templates
      - ./src:/app/src
```

```bash
# Developer workflow
docker-compose up codegen
# Edit ontology files locally
# Changes trigger regeneration in container
# Generated code appears in ./src
```

### Example 2: VS Code Integration

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "ggen watch",
      "type": "shell",
      "command": "ggen sync --watch --verbose",
      "problemMatcher": {
        "owner": "ggen",
        "fileLocation": ["relative", "${workspaceFolder}"],
        "pattern": {
          "regexp": "^ERROR: (.*):(\\d+):(\\d+): (.*)$",
          "file": 1,
          "line": 2,
          "column": 3,
          "message": 4
        }
      },
      "isBackground": true,
      "presentation": {
        "reveal": "always",
        "panel": "dedicated"
      }
    }
  ]
}
```

### Example 3: GitHub Codespaces

```json
{
  "name": "ggen Development",
  "image": "mcr.microsoft.com/devcontainers/rust:1.75",
  "postCreateCommand": "cargo install ggen && ggen sync",
  "postStartCommand": "ggen sync --watch &",
  "customizations": {
    "vscode": {
      "extensions": ["rust-lang.rust-analyzer"]
    }
  }
}
```

## Troubleshooting

### Issue: Watch mode not triggering

**Cause**: Editor saves to temp file instead of direct write

**Fix**: Configure editor for direct writes
```bash
# Vim
set nobackup
set nowritebackup

# VS Code (settings.json)
"files.watcherExclude": {}
```

### Issue: Too many files watched

**Cause**: Watching `target/` or `.git/` directories

**Fix**: Add ignore patterns
```toml
[watch]
ignore = [
  "**/target/**",
  "**/.git/**",
  "**/node_modules/**"
]
```

### Issue: Debounce too short/long

**Cause**: Default 300ms doesn't match workflow

**Fix**: Tune debounce
```toml
[watch]
debounce_ms = 1000  # Slower editors/networks
# OR
debounce_ms = 100   # Fast local development
```

### Issue: Watch mode exits immediately

**Cause**: No files match watch patterns

**Fix**: Verify patterns
```bash
# Test watch patterns
find . -path './ontology/**/*.ttl'
# Should return ontology files
```

## Best Practices

1. **Use watch during development only**
   - Not for CI/CD (use explicit `ggen sync`)
   - Not for production deployments

2. **Combine with `--audit` for traceability**
   ```bash
   ggen sync --watch --audit
   # Each change creates audit trail entry
   ```

3. **Use `--verbose` for debugging**
   - See exactly which files trigger syncs
   - Understand debounce behavior

4. **Scope with `--rule` for large projects**
   ```bash
   # Instead of watching all rules:
   ggen sync --watch

   # Watch specific rule while developing:
   ggen sync --watch --rule structs
   ```

5. **Use `--dry-run` for safe template development**
   ```bash
   ggen sync --watch --dry-run
   # See what would generate without writing files
   ```

6. **Configure appropriate debounce**
   - Local SSD: 100-300ms
   - Network drive: 500-1000ms
   - Cloud IDE: 1000-2000ms

## Security Considerations

- **Watch mode runs with full file system access** (same as manual sync)
- **Malicious ontology changes auto-regenerate** (use `--validate-only` to review first)
- **Large file changes may cause resource exhaustion** (burst protection mitigates)

## Related Documentation

- [Force Flag](force-flag.md) - Auto-overwrite with watch (`--watch --force`)
- [Audit Trail](audit-trail.md) - Track all watch-triggered generations
- [Validation](validation.md) - Auto-validate on watch triggers
- [Conditional Execution](conditional-execution.md) - Skip generation based on SPARQL ASK
