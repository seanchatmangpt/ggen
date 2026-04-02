<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CLI Complete Reference (All Commands & Flags)](#cli-complete-reference-all-commands--flags)
  - [Single Command: `ggen sync`](#single-command-ggen-sync)
    - [Syntax](#syntax)
    - [Flags & Options](#flags--options)
    - [Exit Codes](#exit-codes)
  - [Examples](#examples)
    - [Basic](#basic)
    - [Development](#development)
    - [CI/CD](#cicd)
    - [Selective Execution](#selective-execution)
    - [Debugging](#debugging)
    - [Overrides (CI/CD)](#overrides-cicd)
  - [Output Formats](#output-formats)
    - [Text (Default)](#text-default)
    - [JSON (`--format json`)](#json---format-json)
  - [Watch Mode (`--watch`)](#watch-mode---watch)
  - [Audit Trail (`--audit FILE`)](#audit-trail---audit-file)
  - [Environment Variables](#environment-variables)
  - [Integration Patterns](#integration-patterns)
    - [GitHub Actions](#github-actions)
    - [CI/CD: Only Validate](#cicd-only-validate)
    - [Local Development](#local-development)
    - [Deployment](#deployment)
  - [Decision Matrix: When to Use Each Flag](#decision-matrix-when-to-use-each-flag)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CLI Complete Reference (All Commands & Flags)

> Every ggen CLI command and flag on 3 pages.

## Single Command: `ggen sync`

All ggen v5+ functionality flows through ONE command configured via `ggen.toml`.

### Syntax
```bash
ggen sync [OPTIONS]
```

### Flags & Options

| Flag | Type | Default | Purpose |
|------|------|---------|---------|
| `--config <FILE>` | path | ggen.toml | Use alternate config |
| `--ontology-override <DIR>` | path | from config | Override ontology_dir |
| `--template-override <DIR>` | path | from config | Override templates_dir |
| `--output-override <DIR>` | path | from config | Override output_dir |
| `--rule <NAME>` | string | all | Run specific inference rule only |
| `--timeout <MS>` | u64 | 30000 | Max execution time (milliseconds) |
| `--dry-run` | flag | false | Don't write files, show what would happen |
| `--validate-only` | flag | false | Validate ontology/config, don't generate |
| `--format <FMT>` | json\|text | text | Output format |
| `--audit <FILE>` | path | none | Write audit trail to file |
| `--watch` | flag | false | Live-reload on file changes |
| `--verbose` / `-v` | flag | false | Debug output (repeat for more: -vv, -vvv) |
| `--quiet` | flag | false | Suppress output |
| `--force` | flag | false | Overwrite even if protected |
| `--color` | always\|never\|auto | auto | Colored output |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Generic error |
| 2 | Config/ontology parse error |
| 3 | Generation error |
| 4 | Validation failed |
| 5 | Timeout exceeded |

---

## Examples

### Basic
```bash
ggen sync                        # Use ggen.toml in current dir
ggen sync --config prod.toml     # Use different config
```

### Development
```bash
ggen sync --watch                # Live-reload on changes
ggen sync --dry-run              # Show what would happen
ggen sync -vv                    # Debug output
```

### CI/CD
```bash
ggen sync --validate-only        # Just check validity
ggen sync --format json          # Machine-readable
ggen sync --audit audit.json     # Capture audit trail
```

### Selective Execution
```bash
ggen sync --rule inheritance_closure    # One rule only
ggen sync --timeout 120000               # 2 minute limit
```

### Debugging
```bash
ggen sync --dry-run -vvv         # Max verbosity, no writes
ggen sync --audit debug.json     # See exactly what happened
```

### Overrides (CI/CD)
```bash
ggen sync \
  --ontology-override "ontology/prod/" \
  --output-override "src/generated-prod/" \
  --validate-only
```

---

## Output Formats

### Text (Default)
```
✓ Loaded ontology: ontology/domain.ttl (142 triples)
✓ Running rule: inheritance_closure
✓ Loaded templates: 8 files
✓ Generated: src/generated/models.rs (187 lines)
✓ Generated: src/generated/api.rs (425 lines)
✓ Sync complete in 342ms
```

### JSON (`--format json`)
```json
{
  "success": true,
  "timestamp": "2025-12-23T10:30:45Z",
  "duration_ms": 342,
  "ontology": {
    "file": "ontology/domain.ttl",
    "triples": 142
  },
  "rules": [
    {"name": "inheritance_closure", "status": "success"}
  ],
  "generated": [
    {"file": "src/generated/models.rs", "lines": 187},
    {"file": "src/generated/api.rs", "lines": 425}
  ]
}
```

---

## Watch Mode (`--watch`)

Continuously monitors and regenerates:

```bash
$ ggen sync --watch

✓ Watching for changes...
[10:30:15] Modified: ontology/domain.ttl
[10:30:15] → Regenerating...
[10:30:16] ✓ Updated src/generated/models.rs
[10:30:16] ✓ Tests pass (42 tests in 0.8s)
[10:30:16] Ready for next change...

[10:35:22] Modified: templates/rust-api.tera
[10:35:22] → Regenerating...
[10:35:23] ✓ Updated src/generated/api.rs
[10:35:23] Tests pass
[10:35:23] Ready for next change...
```

Triggers on:
- Changes to any .ttl file in `ontology_dir`
- Changes to any .tera file in `templates_dir`
- Changes to `ggen.toml`

---

## Audit Trail (`--audit FILE`)

Captures everything that happened:

```json
{
  "version": "5.0.2",
  "timestamp": "2025-12-23T10:30:45Z",
  "command": "ggen sync --rule inheritance_closure",
  "config": "ggen.toml",
  "input_hash": "sha256:abc123...",
  "execution": {
    "phase": "Loading",
    "duration_ms": 42,
    "ontology_file": "ontology/domain.ttl",
    "ontology_size_bytes": 1024,
    "triples_loaded": 142
  },
  "rules_executed": [
    {
      "name": "inheritance_closure",
      "type": "CONSTRUCT",
      "duration_ms": 28,
      "triples_added": 87,
      "status": "success"
    }
  ],
  "templates_rendered": [
    {
      "template": "rust-api.tera",
      "output": "src/generated/api.rs",
      "lines": 425,
      "duration_ms": 102
    }
  ],
  "files_written": [
    {"path": "src/generated/models.rs", "lines": 187, "status": "created"},
    {"path": "src/generated/api.rs", "lines": 425, "status": "updated"}
  ],
  "output_hash": "sha256:def456...",
  "total_duration_ms": 342,
  "success": true
}
```

---

## Environment Variables

| Variable | Purpose | Example |
|----------|---------|---------|
| `GGEN_CONFIG` | Default config file | GGEN_CONFIG=prod.toml |
| `GGEN_LOGLEVEL` | Log verbosity | GGEN_LOGLEVEL=debug |
| `GGEN_HOME` | Cache directory | GGEN_HOME=~/.cache/ggen |
| `RUST_LOG` | Rust logger filter | RUST_LOG=ggen=debug |

Usage:
```bash
GGEN_CONFIG=custom.toml GGEN_LOGLEVEL=debug ggen sync -v
```

---

## Integration Patterns

### GitHub Actions
```yaml
- name: Validate specs
  run: ggen sync --validate-only --format json > /tmp/result.json

- name: Generate code
  run: ggen sync --audit /tmp/audit.json

- name: Check for changes
  run: |
    if git diff --exit-code src/generated/; then
      echo "No changes"
    else
      echo "Generated code changed - commit?"
      git add src/generated/
      git commit -m "chore: regenerate from specs"
    fi
```

### CI/CD: Only Validate
```bash
ggen sync --validate-only --format json
echo $?  # Exit 0 if valid, non-zero otherwise
```

### Local Development
```bash
# Terminal 1: Watch for changes
ggen sync --watch

# Terminal 2: Run tests
cargo test --watch
```

### Deployment
```bash
ggen sync \
  --config deployment.toml \
  --ontology-override "ontology/production/" \
  --audit deployment-audit.json
```

---

## Decision Matrix: When to Use Each Flag

| Scenario | Command |
|----------|---------|
| Development | `ggen sync --watch` |
| CI validation | `ggen sync --validate-only` |
| See output format | `ggen sync --format json` |
| Debug issues | `ggen sync -vvv --audit debug.json` |
| Run one rule | `ggen sync --rule inheritance_closure` |
| Don't modify files | `ggen sync --dry-run` |
| Prevent hangups | `ggen sync --timeout 60000` |
| Production deploy | `ggen sync --config prod.toml --audit audit.json` |

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| "Config not found" | Use `--config` or set `GGEN_CONFIG` env var |
| "No such rule" | Check rule name in `ggen.toml`, list with `--validate-only -v` |
| "Timeout exceeded" | Increase `--timeout` or simplify SPARQL queries |
| "Permission denied" | Use `--force` to override `protected_paths` |
| "Validation failed" | Run with `-vvv` to see details |

