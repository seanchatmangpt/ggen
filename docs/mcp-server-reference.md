# MCP Server Reference — ggen v6.0.1

Complete reference for the ggen MCP server (Model Context Protocol), exposing 16 tools, 3 prompts, and 4 resource types.

## Server

| Field | Value |
|-------|-------|
| **Crate** | `ggen-a2a-mcp` |
| **Transport** | `rmcp` 1.3.0 |
| **Server struct** | `GgenMcpServer` |
| **Capabilities** | Tools, Resources, Prompts, Completions |

### Starting the Server

```bash
# stdio transport
ggen mcp start-server --transport stdio

```

## Tools (16)

### Core Generation

#### `generate`

Generate code from an RDF ontology file through the full mu1-mu5 pipeline.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `ontology_path` | string | Yes | Path to the RDF ontology file (.ttl) |
| `queries_dir` | string | No | Directory containing .rq SPARQL files (defaults to `queries/` beside ontology) |
| `output_dir` | string | No | Target output directory (defaults to `generated/` beside ontology) |
| `language` | string | No | Target language: `go`, `rust`, `python`, `typescript`, `elixir`, `auto` (default: `auto`) |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=generate`

**Returns:** JSON with generation summary, files written count, duration.

---

#### `validate`

Validate Turtle (.ttl) content with oxigraph.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `ttl` | string | Yes | Turtle content string to validate |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=validate`

**Returns:** JSON with `is_valid`, `error_count`, `errors[]`.

---

#### `sync`

Run the full ggen sync pipeline.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `ontology_path` | string | Yes | Path to the RDF ontology file (.ttl) |
| `queries_dir` | string | No | Directory containing .rq SPARQL files |
| `output_dir` | string | No | Target output directory |
| `language` | string | No | Target language |
| `dry_run` | bool | No | Dry-run mode — preview only, no files written |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=sync`

**Returns:** JSON with sync results, files generated/previewed.

### Ontology

#### `query_ontology`

Run a SPARQL SELECT query against inline Turtle content.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `ttl` | string | Yes | Turtle content to query |
| `sparql` | string | Yes | SPARQL SELECT query |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=query_ontology`

**Returns:** JSON with query results as array of objects.

---

#### `search`

Search marketplace packages by keyword and category.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `query` | string | Yes | Search query string |
| `category` | string | No | Filter by category |
| `limit` | number | No | Maximum results (default: 10) |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=search`

**Returns:** JSON with matching packages.

### Validation

#### `validate_pipeline`

Run all 6 quality gates on a ggen project.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `project_path` | string | Yes | Path to project directory containing ggen.toml |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=validate_pipeline`

**Returns:** JSON with gate results per gate: `pass`/`fail`, issues, suggestions.

---

#### `validate_sparql`

Validate SPARQL query syntax.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `query_path` | string | Yes | Path to SPARQL query file (.rq) |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=validate_sparql`

**Returns:** JSON with `is_valid`, syntax errors if any.

---

#### `validate_templates`

Validate template syntax (Tera, Handlebars, Jinja2).

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `template_path` | string | Yes | Path to template file (.tera, .hbs) |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=validate_templates`

**Returns:** JSON with `is_valid`, parse errors if any.

---

#### `fix_cycles`

Detect and fix circular dependencies in ontologies.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `project_path` | string | Yes | Path to project directory |
| `strategy` | string | Yes | Fix strategy: `remove_import`, `merge_files`, or `create_interface` |
| `dry_run` | bool | No | Dry-run mode — preview only |

**OTEL spans:** `ggen.mcp.tool_call`, `mcp.tool.name=fix_cycles`

**Returns:** JSON with `cycles_found`, `fixes_applied`, `files_modified`, `backup_path`.

### Orchestration

#### `validate_project`

Full project validation with dependency ordering. Orchestrates all validation tools in the correct sequence: manifest parse, manifest dependencies, quality gates, TTL syntax, SPARQL syntax, template syntax. Supports early exit on critical errors.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `project_root` | string | Yes | Root directory of the ggen project |
| `manifest_path` | string | No | Path to ggen.toml (defaults to `project_root/ggen.toml`) |
| `validation_level` | string | No | Level: `syntax`, `semantics`, `security`, `all` (default: `all`) |

**Validation layers (ordered):**

| Layer | Tools | Behavior on Failure |
|-------|-------|---------------------|
| 1 | Manifest parse | **Early exit** — critical |
| 2 | Manifest dependencies | **Early exit** — critical |
| 3 | Quality gates | Report warning |
| 4 | TTL syntax | Report per-file |
| 5 | SPARQL syntax | Report per-file |
| 6 | Template syntax | Report per-file |

**OTEL attributes:** `validation.level`, `validation.tools_executed_count`, `validation.total_duration_ms`

**Returns:**
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

---

#### `validate_incremental`

Validate only changed files,for development workflow. Auto-detects changed files via `git diff` if no explicit list provided.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `project_root` | string | Yes | Root directory of the ggen project |
| `changed_files` | string[] | No | Explicit list of changed files to validate |
| `since_commit` | string | No | Git commit to compare against (default: `HEAD~1`) |

**File extension mapping:**

| Extension | Tool Called |
|-----------|-------------|
| `.toml` | `validate_manifest_parse` |
| `.ttl` | `validate_ttl_syntax` |
| `.rq` | `validate_sparql` |
| `.tera`, `.tmpl`, `.hbs`, `.j2` | `validate_templates` |

**Dependency tracing:** Templates using `{% extends %}` or `{% include %}` are flagged as `dependencies_affected`.

**OTEL attributes:** `validation.files_changed_count`, `validation.dependencies_affected_count`

**Returns:**
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

---

#### `validate_dependency_graph`

Cross-input dependency analysis. Builds a directed graph from all project inputs and detects cycles via DFS, finds orphans, and computes the critical path.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `project_root` | string | Yes | Root directory of the ggen project |
| `manifest_path` | string | No | Path to ggen.toml (defaults to `project_root/ggen.toml`) |

**Graph sources:**

| Source | What's Parsed |
|--------|---------------|
| `ggen.toml` | File references (ontology, queries_dir, templates) |
| `.ttl` files | `@prefix`, `IMPORT <...>` directives |
| `.rq` files | `FROM <...>`, `USING <...>` clauses |
| `.tera` files | `{% extends '...' %}`, `{% include '...' %}` directives |

**OTEL attributes:** `graph.nodes_count`, `graph.edges_count`, `graph.cycles_count`

**Returns:**
```json
{
  "is_valid": true,
  "dependency_graph": {
    "ggen.toml": ["ontology/schema.ttl", "queries/"],
    "schema.ttl": ["imports/base.ttl"],
    "list.rq": ["schema.ttl"]
  },
  "circular_dependencies": [],
  "orphan_nodes": ["unused.ttl"],
  "critical_path": ["ggen.toml", "ontology/schema.ttl", "queries/list.rq"]
}
```

### Examples

#### `list_examples`

List bundled ggen example projects.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `category` | string | No | Filter by category (substring match) |
| `limit` | number | No | Maximum results (default: 50) |

**Returns:** JSON array of `{ name, description, category }`.

---

#### `get_example`

Retrieve example details (ggen.toml, TTL, README).

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `name` | string | Yes | Example directory name (use `list_examples` to discover) |

**Returns:** JSON with `name`, `description`, `category`, `config` (raw ggen.toml content).

---

#### `list_generators`

List available code generators.

**Parameters:** None

**Returns:** JSON array of supported generators: `go`, `python`, `rust`, `typescript`, `elixir`, `terraform`, `docker-kubernetes`.

---

#### `scaffold_from_example`

Copy an example project as a starting point. Excludes `target/`, `node_modules/`, `.git/`, `dist/`, `build/`, `cache/`, `.cache/`, `.ggen/cache/`.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `example_name` | string | Yes | Name of the source example |
| `target_dir` | string | Yes | Target directory path |

**Returns:** JSON with `files_copied` count and file list.

## Resources

| URI | Description |
|-----|-------------|
| `ggen://example/{name}` | Example summary (JSON with name, description, category, config) |
| `ggen://example/{name}/ttl` | Raw ontology TTL content |
| `ggen://example/{name}/readme` | README content |
| `ggen://example/{name}/config` | Raw ggen.toml content |

Resources are paginated (20 per page).

## Prompts

### `explain-rdf-schema`

Explain a Turtle RDF ontology in plain English.

**Arguments:**

| Name | Required | Description |
|------|----------|-------------|
| `ttl_content` | Yes | The Turtle (.ttl) content to explain |

**Output:** Structured explanation listing all classes, properties, data types, and relationships.

---

### `generate-from-example`

Adapt a ggen example project to a new domain.

**Arguments:**

| Name | Required | Description |
|------|----------|-------------|
| `example_name` | Yes | Source example name (use `list_examples` to discover) |
| `target_domain` | Yes | New domain to adapt for |

**Output:** Adapted TTL ontology and ggen.toml for the target domain.

---

### `scaffold-project`

Design a new ggen project from scratch.

**Arguments:**

| Name | Required | Description |
|------|----------|-------------|
| `domain` | Yes | Business domain for the project |
| `language` | No | Target code generation language (default: `auto`) |

**Output:** Complete ggen.toml, starter TTL ontology, and design explanation.

## Completions

| Argument | Values |
|----------|--------|
| `example_name` | Discovered example directory names |
| `generator` / `language` | `go`, `python`, `rust`, `typescript`, `elixir`, `terraform`, `docker-kubernetes` |

## OTEL Instrumentation

All 16 tools emit OpenTelemetry spans with the following attributes:

| Attribute | Description |
|-----------|-------------|
| `service.name` | `ggen-mcp-server` |
| `service.version` | Crate version from `CARGO_PKG_VERSION` |
| `mcp.tool.name` | Tool name (e.g., `validate_project`) |
| `mcp.project_path` | Project path when applicable |
| `error` | `true` on failure |
| `error.type` | Error classification on failure |

Orchestration tools add additional attributes:

| Attribute | Tools | Description |
|-----------|-------|-------------|
| `validation.level` | `validate_project` | Validation scope |
| `validation.tools_executed_count` | `validate_project` | Number of tools run |
| `validation.total_duration_ms` | `validate_project` | Total wall time |
| `validation.files_changed_count` | `validate_incremental` | Files examined |
| `validation.dependencies_affected_count` | `validate_incremental` | Dependencies to recheck |
| `graph.nodes_count` | `validate_dependency_graph` | Nodes in graph |
| `graph.edges_count` | `validate_dependency_graph` | Edges in graph |
| `graph.cycles_count` | `validate_dependency_graph` | Cycles detected |

## Enabling Trace Logging

```bash
export RUST_LOG=trace,ggen_a2a_mcp=trace
```
