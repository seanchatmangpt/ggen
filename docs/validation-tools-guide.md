# Validation Tools Guide

Comprehensive guide to ggen's 8 validation MCP tools — from individual checks to orchestrated project-wide validation.

## Overview

The ggen MCP server provides validation tools organized into three tiers:

| Tier | Tools | Purpose |
|------|-------|---------|
| **Individual** | `validate`, `validate_sparql`, `validate_templates` | Single-file checks |
| **Pipeline** | `validate_pipeline` | All 6 quality gates on a project |
| **Orchestration** | `validate_project`, `validate_incremental`, `validate_dependency_graph` | Multi-tool coordination |

## Individual Validation Tools

### `validate` — Turtle Validation

Validate Turtle (.ttl) content using the oxigraph RDF parser.

```
Tool call:
  ttl: "@prefix ex: <http://example.org/> . ex:Foo a ex:Class"
```

Returns:
- `is_valid`: boolean
- `error_count`: number of parse errors
- `errors[]`: array of error details

**When to use:** Quick inline TTL validation without file system access.

### `validate_sparql` — SPARQL Syntax Check
Parse and validate a SPARQL query file.

```
Tool call:
  query_path: "/path/to/queries/list.rq"
```

Returns:
- `is_valid`: boolean
- Syntax errors if invalid

**When to use:** Before running sync, after editing .rq files.

### `validate_templates` — Template Syntax Check
Validate Tera, Handlebars, or Jinja2 template syntax.

```
Tool call:
  template_path: "/path/to/templates/hello.tera"
```

Returns:
- `is_valid`: boolean
- Parse errors if invalid

**When to use:** After modifying template files, Supports `.tera`, `.tmpl`, `.hbs`, `.j2`.

## Pipeline Validation

### `validate_pipeline` — Quality Gates
Run all 6 quality gates against a ggen project directory.

```
Tool call:
  project_path: "/path/to/my-project"
```

**Quality gates executed:**

| Gate | What it checks |
|------|---------------|
| 1. Manifest exists | ggen.toml is present and parseable |
| 2. Ontology valid | TTL files parse correctly |
| 3. Queries valid | SPARQL files parse correctly |
| 4. Templates valid | Template files parse correctly |
| 5. No cycles | No circular ontology imports |
| 6. Output writable | Output directory is writable |

Returns per gate:
- `gate_name`: name
- `passed`: boolean
- `message`: details
- `duration_ms`: execution time

## Orchestration Tools

### `validate_project` — Full Project Validation

Orchestrates all validation tools in dependency order with early-exit on critical failures.

```
Tool call:
  project_root: "/path/to/my-project"
  validation_level: "all"  # syntax | semantics | security | all
```

**Validation layers execute in order:**

```
Layer 1: Manifest parse ── early exit if fails
Layer 2: Manifest dependencies ── early exit if fails
Layer 3: Quality gates ── report warnings
Layer 4: TTL syntax ── report per-file
Layer 5: SPARQL syntax ── report per-file
Layer 6: Template syntax ── report per-file
```

**Early exit:** If Layer 1 or 2 fails, no further layers execute. Returns immediately with `critical_errors` populated.

**Validation levels:**

| Level | Layers Executed |
|-------|----------------|
| `syntax` | 1, 4, 5, 6 |
| `semantics` | 1, 2, 3 |
| `security` | (reserved for future use) |
| `all` | 1, 2, 3, 4, 5, 6 |

**Example response:**
```json
{
  "is_valid": true,
  "validations_run": [
    { "tool": "validate_manifest_parse", "status": "pass", "duration_ms": 2 },
    { "tool": "validate_manifest_dependencies", "status": "pass", "duration_ms": 1 },
    { "tool": "validate_manifest_quality_gates", "status": "pass", "duration_ms": 0 },
    { "tool": "validate_ttl_syntax", "status": "pass", "files_checked": 3, "duration_ms": 15 },
    { "tool": "validate_sparql", "status": "pass", "files_checked": 2, "duration_ms": 3 },
    { "tool": "validate_templates", "status": "pass", "files_checked": 5, "duration_ms": 8 }
  ],
  "overall_status": "pass",
  "total_duration_ms": 29,
  "critical_errors": [],
  "warnings": []
}
```

### `validate_incremental` — Dev Workflow Validation
Validates only changed files, ideal for pre-commit hooks and CI.

```
Tool call:
  project_root: "/path/to/my-project"
  # Option A: Explicit file list
  changed_files: ["ontology/schema.ttl", "queries/list.rq"]
  # Option B: Auto-detect from git
  since_commit: "HEAD~3"
```

**Auto-detection:** If `changed_files` is not provided, runs `git diff --name-only` against `since_commit` (default: `HEAD~1`).

**File-to-tool mapping:**

| File changed | Tool called |
|---------------|-------------|
| `*.toml` | `validate_manifest_parse` |
| `*.ttl` | `validate_ttl_syntax` |
| `*.rq` | `validate_sparql` |
| `*.tera`, `*.tmpl`, `*.hbs`, `*.j2` | `validate_templates` |
| Other | Skipped |

**Dependency tracing:** Templates with `{% extends %}` or `{% include %}` are flagged in `dependencies_affected` so you can re-validate their parent templates.

**Example response:**
```json
{
  "is_valid": true,
  "files_validated": [
    { "path": "ontology/schema.ttl", "tool": "validate_ttl_syntax", "status": "pass" },
    { "path": "queries/list.rq", "tool": "validate_sparql", "status": "pass" }
  ],
  "dependencies_affected": ["templates/layout.tera"]
}
```

### `validate_dependency_graph` — Dependency Analysis

Builds a cross-input dependency graph and detects cycles, orphans, and critical paths.

```
Tool call:
  project_root: "/path/to/my-project"
```

**What it parses:**

| Input | Dependencies extracted |
|-------|----------------------|
| `ggen.toml` | File references from `ontology`, `queries`, `templates` fields |
| `.ttl` files | `IMPORT <...>`, `@import <...>` directives |
| `.rq` files | `FROM <...>`, `USING <...>` clauses |
| `.tera` files | `{% extends '...' %}`, `{% include '...' %}` directives |

**Cycle detection:** Uses DFS with recursion stack to find all circular dependencies.

**Orphan detection:** Files with no dependencies and not depended upon by any other file.

**Critical path:** Longest path through the dependency graph — shows the compilation hot path.

**Example response:**
```json
{
  "is_valid": true,
  "dependency_graph": {
    "ggen.toml": ["ontology/schema.ttl", "queries/"],
    "schema.ttl": ["imports/base.ttl"],
    "list.rq": ["schema.ttl"],
    "base.ttl": []
  },
  "circular_dependencies": [],
  "orphan_nodes": ["unused.ttl"],
  "critical_path": ["ggen.toml", "ontology/schema.ttl", "queries/list.rq"]
}
```

## Workflow Patterns

### Pre-commit Hook
```bash
# Validate only changed files before committing
validate_incremental(since_commit="HEAD")
```

### CI Pipeline
```bash
# Full project validation on every PR
validate_project(validation_level="all")
```

### Fast Syntax Check
```bash
# Quick syntax-only validation
validate_project(validation_level="syntax")
```

### Refactor Safety
```bash
# Check dependency graph before restructuring
validate_dependency_graph()
# Fix any cycles found
fix_cycles(strategy="create_interface", dry_run=true)
```

## Troubleshooting

| Symptom | Cause | Fix |
|---------|-------|-----|
| Early exit after Layer 1 | ggen.toml missing or malformed | Check manifest_path, verify TOML syntax |
| "No .ttl file found" | No TTL files in project | Create ontology directory with at least one .ttl |
| `dependencies_affected` is empty | No templates use extends/include | Expected if templates are self-contained |
| Circular dependencies detected | Ontology import cycles | Use `fix_cycles` tool with appropriate strategy |
| `validate_incremental` returns empty | Not in a git repo, or no changes | Provide explicit `changed_files` list |

## Performance

| Tool | Typical Time | Notes |
|------|-------------|-------|
| `validate` | < 5ms | In-memory TTL parsing |
| `validate_sparql` | < 2ms | Single file parse |
| `validate_templates` | < 2ms | Single file parse |
| `validate_pipeline` | < 100ms | 6 gates |
| `validate_project` | < 200ms | All layers |
| `validate_incremental` | < 50ms | Only changed files |
| `validate_dependency_graph` | < 500ms | Full graph + DFS (scales with project size) |
