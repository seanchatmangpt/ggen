# Orchestration Tools — Design Reference

Architecture and algorithms for the three orchestration validation tools in the ggen MCP server.

## Architecture

The three orchestration tools form a layered validation system:

```
validate_project          Full project validation (orchestrator)
    ├── validate_incremental  Changed-files validation (dev workflow)
    └── validate_dependency_graph  Dependency analysis (refactor safety)
```

All three live in `crates/ggen-a2a-mcp/src/ggen_server.rs` and share helper functions for file discovery.

## Parameter Structs

### `ValidateProjectParams`
```rust
pub struct ValidateProjectParams {
    pub project_root: String,                    // Required
    pub manifest_path: Option<String>,           // Optional: defaults to project_root/ggen.toml
    pub validation_level: Option<String>,        // Optional: "syntax"|"semantics"|"security"|"all" (default: "all")
}
```

### `ValidateIncrementalParams`
```rust
pub struct ValidateIncrementalParams {
    pub project_root: String,                    // Required
    pub changed_files: Option<Vec<String>>,      // Optional: explicit file list
    pub since_commit: Option<String>,            // Optional: git ref (default: "HEAD~1")
}
```

### `ValidateDependencyGraphParams`
```rust
pub struct ValidateDependencyGraphParams {
    pub project_root: String,                    // Required
    pub manifest_path: Option<String>,           // Optional: defaults to project_root/ggen.toml
}
```

## `validate_project` — Dependency-Ordered Validation

### Layer Ordering

Layers execute strictly in order. Layers 1-2 are critical — failure triggers early exit.

```
Layer 1: Manifest Parse
  ├── Reads ggen.toml from manifest_path
  ├── Checks for required fields (ontology, queries)
  └── On failure: push to critical_errors, set overall_status="fail", RETURN EARLY

Layer 2: Manifest Dependencies
  ├── Reports status (pass/fail)
  └── On failure: RETURN EARLY

Layer 3: Quality Gates
  └── Reports status (pass/warn)

Layer 4: TTL Syntax
  ├── Discovers all .ttl files via find_files_by_extension
  └── Reports per-file count

Layer 5: SPARQL Syntax
  ├── Discovers all .rq files via find_files_by_extension
  └── Reports per-file count

Layer 6: Template Syntax
  ├── Discovers all .tera/.tmpl/.hbs/.j2 files via find_files_by_extensions
  └── Reports per-file count
```

### Validation Level Filtering

| Level | Layer 1 | Layer 2 | Layer 3 | Layer 4 | Layer 5 | Layer 6 |
|-------|---------|---------|---------|---------|---------|---------|
| `syntax` | Yes | - | - | Yes | Yes | Yes |
| `semantics` | Yes | Yes | Yes | - | - | - |
| `security` | - | - | - | - | - | - |
| `all` | Yes | Yes | Yes | Yes | Yes | Yes |

### Return Schema

```json
{
  "is_valid": boolean,
  "validations_run": [
    {
      "tool": string,          // "validate_manifest_parse" etc.
      "status": string,        // "pass" | "fail"
      "duration_ms": number,   // Per-tool wall time
      "files_checked"?: number // For file-scanning tools
    }
  ],
  "overall_status": string,    // "pass" | "fail"
  "total_duration_ms": number, // Total wall time
  "critical_errors": string[], // Non-empty only on Layer 1-2 failure
  "warnings": string[]         // Future use
}
```

## `validate_incremental` — Git-Aware Validation

### File Detection Strategy

```
changed_files provided?
  ├── Yes → use explicit list
  └── No → git diff --name-only <since_commit>
            ├── since_commit provided? → use it
            └── No → default to "HEAD~1"
```

Uses `std::process::Command` to invoke `git`. Requires the project directory to be in a git repository for auto-detection.

### Extension-to-Tool Mapping

```
.toml  → validate_manifest_parse
.ttl   → validate_ttl_syntax
.rq    → validate_sparql
.tera  → validate_templates
.tmpl  → validate_templates
.hbs   → validate_templates
.j2    → validate_templates
other  → skip (no validation)
```

### Dependency Tracing

For `.tera` and `.tmpl` files, the tool scans for:

- `{% extends '...' %}` — template inheritance
- `{% include '...' %}` — template inclusion

Files containing these directives are added to `dependencies_affected` so the caller knows to re-validate the referenced parent templates.

### Return Schema

```json
{
  "is_valid": boolean,
  "files_validated": [
    {
      "path": string,    // Relative path from project_root
      "tool": string,    // Tool that would validate this file
      "status": string   // "pass" | "fail"
    }
  ],
  "dependencies_affected": string[]  // Templates with extends/include
}
```

## `validate_dependency_graph` — Graph Analysis

### Graph Construction

The tool builds a directed graph (`HashMap<String, Vec<String>>`) from four input sources:

| Source | Regex | Dependencies |
|--------|-------|-------------|
| `ggen.toml` | TOML field extraction | `ontology`, `queries`, `templates` field values |
| `.ttl` files | `(IMPORT\|@import)\s*<([^>]+)>` | Imported file paths |
| `.rq` files | `(FROM\|USING)\s*<([^>]+)>` | Referenced graph URIs |
| `.tera` files | `{%\s*extends\s+['"]([^'"]+)['"]` and `{%\s*include\s+['"]([^'"]+)['"]` | Parent/included templates |

### Cycle Detection — DFS Algorithm

```
function dfs(node, graph, visited, rec_stack, path, cycles):
    visited.add(node)
    rec_stack.add(node)
    path.push(node)

    for neighbor in graph[node]:
        if neighbor not in visited:
            dfs(neighbor, ...)
        elif neighbor in rec_stack:
            // Found cycle: extract from path
            cycle_start = path.index(neighbor)
            cycles.push(path[cycle_start:])

    path.pop()
    rec_stack.remove(node)
```

**Time complexity:** O(V + E) where V = nodes, E = edges.

### Orphan Detection

A node is orphan if:
- It has no outgoing edges (no dependencies)
- No other node depends on it (not in any adjacency list)

### Critical Path — Longest Path

For each node, performs DFS to find the longest reachable path. The overall critical path is the maximum across all starting nodes.

**Note:** Does not account for edge weights — all edges have unit weight.

### Return Schema

```json
{
  "is_valid": boolean,           // true iff no circular dependencies
  "dependency_graph": {          // Adjacency list
    "node_name": ["dep1", "dep2"]
  },
  "circular_dependencies": [     // Each cycle is a path
    ["A", "B", "C", "A"]
  ],
  "orphan_nodes": string[],      // Nodes with no connections
  "critical_path": string[]      // Longest dependency chain
}
```

## Helper Functions

### `find_files_by_extension(dir, ext) -> io::Result<Vec<PathBuf>>`
Recursively walks `dir` collecting all files matching extension `ext`. Returns empty vec if directory doesn't exist.

### `find_files_by_extensions(dir, extensions) -> io::Result<Vec<PathBuf>>`
Calls `find_files_by_extension` for each extension in `extensions`, combining results.

## OTEL Instrumentation

All three tools use `#[tracing::instrument]` with:

```rust
#[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(
    project_root = %params.project_root,
    service.name = "ggen-mcp-server",
    service.version = env!("CARGO_PKG_VERSION"),
))]
```

Additional attributes recorded during execution:

| Tool | Attributes |
|------|-----------|
| `validate_project` | `validation.level`, `validation.tools_executed_count`, `validation.total_duration_ms` |
| `validate_incremental` | `validation.files_changed_count`, `validation.dependencies_affected_count` |
| `validate_dependency_graph` | `graph.nodes_count`, `graph.edges_count`, `graph.cycles_count` |

Error spans set:
- `error = true`
- `error.type = "classification"` (e.g., `cycle_detection_error`)
