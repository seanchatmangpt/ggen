# ggen-mcp

An MCP server exposing ggen's introspection surface as tool calls.

## Why this exists

ggen's entire CLI is ten whole-pipeline verbs:

```
doctor run | graph validate | law derive|explain|export|load|validate
receipt history|verify | sync run
```

None of them can run a SPARQL query. So the authoring loop was: write the
ontology, write the query, write the template, run the whole generator, look
at the output file, and guess what went wrong.

The concrete cost, from a real session: a SPARQL query had a mandatory
(non-`OPTIONAL`) triple on a predicate used **zero times** in the graph. It
silently returned 0 of 113 expected rows and generated 11 empty classes.
Nothing could answer "does my query return rows, and how many" before the
query was committed to a template. Roughly two hours were lost to it.

Related failures from the same session: 3 of the 25 legal frontmatter keys
were used because nothing enumerated them (`for_each:`, the fan-out
mechanism the redesign needed, was never discovered), and Jinja2 ternary
syntax was written into a Tera template — a construct Tera does not have,
discoverable only by running the full pipeline and reading a parse error.

Each tool below closes one of those.

## Tools

| Tool | Answers |
|---|---|
| `ggen_query_preview` | Does my SPARQL return rows? **How many** — truthfully, before truncation? |
| `ggen_config_classify` | Which of ggen.toml's two incompatible schemas applies here? |
| `ggen_frontmatter_schema` | What frontmatter keys exist, and does this template write one file or one per row? |
| `ggen_frontmatter_lint` | Will this Tera body parse? Does it consume a variable the SELECT never binds? |
| `ggen_sync_dry_run` | What would be written — and *why* was each skip skipped? |
| `ggen_check_project` | What do the cross-surface `GGEN-*`/`E00xx` diagnostics say? |
| `ggen_rule_graph` | What rules exist, and what does each read and write? |
| `ggen_capability_status` | Am I relying on a field that is accepted but not implemented? |
| `ggen_write_apply` | *(the only destructive tool)* Apply the sync. |

All are `readOnlyHint` except `ggen_write_apply`, which is `destructiveHint`
and requires `confirm: true`. Read and write are separate tools, never one
tool with a mode flag, so a client can gate on the annotation alone.

## Design notes

**Tools only.** `rmcp` 1.8 also supports Resources, Prompts, Sampling and
Tasks. None are used. Every friction point above is a question an agent asks
mid-session, not something it browses or subscribes to — and tool-call
support is the one part of the protocol every MCP client implements today.
Resources (for cacheable/subscribable graph state) and Sampling (for
"help me fix this query") are real future options, deliberately deferred
rather than rejected.

**A zero-row query is a success, not an error.** `ggen_query_preview`
returns `ok: true, row_count: 0`. A mandatory triple pattern matching
nothing is a *correct* query execution; the job is to make that fact loud,
not to fail on it. `row_count` is always the true count before truncation,
with `truncated` explicit — silent truncation would recreate the exact class
of bug this crate exists to prevent.

**Reuse, never reimplement.** Query execution goes through `GraphEngine`;
graph loading through `ggen_engine::project_graph::load_for_query` (which is
also what `sync` uses); path resolution through `write::resolve_target`, the
same traversal check `to:` already uses; var-diff through
`ggen_engine::lint`; diagnostics through `ggen_lsp::check_files_in_root`.
Row and byte caps are re-exported from `ggen-engine`, never re-literaled, so
they cannot drift.

## Running it

```bash
cargo run --release -p ggen-mcp    # stdio, no arguments needed
```

Wired into this repo's `.mcp.json` as `ggen-mcp`.

```bash
cargo test -p ggen-mcp             # 34 tests
```

Tests are Chicago TDD throughout: real `TempDir`, real `ggen.toml`, real
ontology, real templates, no mocks. `tests/mcp_protocol_test.rs` spawns the
actual binary and speaks JSON-RPC over its stdio — the only place tool
annotations are verifiable, since they exist only in the `tools/list`
payload a client receives.
