<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [construct Tool](#construct-tool)
  - [Parameters](#parameters)
  - [Behavior](#behavior)
  - [Example Usage](#example-usage)
  - [Output](#output)
    - [Success Response](#success-response)
    - [Error Response](#error-response)
  - [Common Errors](#common-errors)
  - [Related Tools](#related-tools)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# construct Tool

MCP name: `ggen.construct`. Implementation: `crates/ggen-lsp/src/a2a_mcp/mcp_server.rs`
(`GgenMcpServer::construct`). Runs a full `ggen sync` against the ggen project found in the
server process's current working directory and returns the resulting file list.

This page documents the tool as it is actually implemented today, not an aspirational
interface — verify against the source above before relying on any field not listed here.

## Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `task_id` | string | Yes | Caller-supplied identifier, echoed back verbatim in the response. Not interpreted or validated by the tool. |
| `jtbd` | string | Yes | Free-text "job to be done" description, echoed into the response's text body. Not parsed or acted on. |
| `avatar` | string | Yes | Accepted by the tool's parameter schema but currently **unused** by the handler — it has no effect on generation. Documented honestly here rather than implying behavior that doesn't exist; may become load-bearing in a future revision. |

## Behavior

1. Resolves the server process's current working directory as the project root.
2. Confirms `ggen.toml` exists there — if not, returns an error (see below) rather than
   searching parent directories or accepting an explicit path parameter.
3. Runs `ggen_engine::sync::sync(base_path, SyncOptions::default())` — the same five-stage
   pipeline behind the `ggen sync run` CLI command, handling both the frontmatter-per-template
   and `[[generation.rules]]` declarative `ggen.toml` schemas automatically. There is no LLM
   injection point on this path (the old `ggen-core`-era handler had one; it had zero
   production callers and was not ported).
4. On success, returns a human-readable text summary (files written, one per line) plus a
   structured `meta.ggen_result` JSON payload (see below).

## Example Usage

```json
{
  "name": "ggen.construct",
  "arguments": {
    "task_id": "task-42",
    "jtbd": "Generate the REST handlers for the invoicing ontology",
    "avatar": "backend-agent"
  }
}
```

## Output

### Success Response

Text content — one line per generated file:

```
Successfully constructed artifact for task task-42.
JTBD: Generate the REST handlers for the invoicing ontology
Generated 3 files:
- src/handlers/invoice.rs
- src/handlers/mod.rs
- Cargo.toml
```

`meta.ggen_result` JSON:

```json
{
  "status": "success",
  "task_id": "task-42",
  "files_count": 3,
  "graph_hash_hex": "b3a1...",
  "duration_ms": 842
}
```

| Field | Type | Notes |
|-------|------|-------|
| `status` | string | Always `"success"` on this path — an error response is returned separately (see below), never a `status: "error"` variant of this payload. |
| `task_id` | string | Echoed from the request. |
| `files_count` | integer | `report.written.len()` — count of files the sync pipeline actually wrote. |
| `graph_hash_hex` | string | Content-identity hash of the RDF graph for this run (`SyncReport::graph_hash_hex`). Added when this tool was repointed from the retired `ggen-core` pipeline to `ggen-engine`; there is no equivalent per-file content hash at the top level. |
| `duration_ms` | integer | Wall-clock time for the `sync()` call, measured locally around the call site. This is a transport-layer metric only — it is not sourced from `SyncReport` (which carries no timing field) and is not part of the cryptographic receipt; `ggen-engine` deliberately treats wall-clock timing as non-deterministic receipt content. |

Note: the per-file text lines list only the output path — the pre-`ggen-engine` version of
this handler also included each file's size and content hash in this text. That detail was not
restored (would require a `fs::metadata` call per written file); `files_count` and
`graph_hash_hex` in the structured payload remain accurate substitutes for programmatic callers.

### Error Response

`ggen.toml` not found in the working directory:

```json
{
  "error": {
    "message": "ggen.toml not found in current directory. `ggen.construct` requires a ggen project context.",
    "data": { "path": "/path/to/cwd" }
  }
}
```

Sync pipeline failure (any error from `ggen_engine::sync::sync`, e.g. an invalid ontology or a
failed quality gate):

```json
{
  "error": {
    "message": "Generation pipeline failed: <underlying error text>"
  }
}
```

## Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `ggen.toml not found in current directory` | The MCP server's CWD is not a ggen project root | Start the server from (or `cd` the caller's context into) a directory containing `ggen.toml` |
| `Generation pipeline failed: ...` | Ontology/template/schema error surfaced by `ggen_engine::sync::sync` | Inspect the underlying message; the same failure is reproducible via `ggen sync run` from a terminal in the same directory |

## Related Tools

- `hello` — trivial demo tool in the same server, exercises the tool-router wiring only.
