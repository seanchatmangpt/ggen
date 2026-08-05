# ggen-mcp

An MCP server exposing ggen's introspection surface as tool calls.

## Why this exists

None of the pipeline-facing nouns expose a way to run an ad-hoc SPARQL
query. For example (illustrative, not the full CLI surface — `doctor` alone
also has `inspect`/`domain`, and `law`/`receipt` have further verbs beyond
what's shown here):

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
| `ggen_check_project` | What do the cross-surface `GGEN-*`/`E00xx` diagnostics say? *(fast, incomplete-by-design first pass — escalate to `ggen_sync_dry_run`/`ggen_receipt_verify` for what it can't see)* |
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
graph loading through `ggen_engine::project_graph::load_for_query`, which
shares `sync`'s own `new_graph_engine`/`read_ontology_file` primitives but
resolves packs read-only (an uncached git pack is refused, not cloned — see
`project_graph.rs`'s module doc and `query_preview_test.rs`'s
`uncached_git_pack_never_triggers_clone_or_cache_write`); path resolution
through `write::resolve_target`, the same traversal check `to:` already
uses; var-diff through `ggen_engine::lint`; diagnostics through
`ggen_lsp::check_files_in_root`. The row cap is re-exported from
`ggen-engine`, never re-literaled; the two byte caps are ggen-mcp-local
literals chosen to match `ggen-lsp`'s `MAX_CONTENT_BYTES`/`MAX_PATH_BYTES`
convention by value, not by import (see `src/limits.rs`).

## Running it

```bash
cargo run --release -p ggen-mcp    # stdio, no arguments needed
```

Wired into this repo's `.mcp.json` as `ggen-mcp`.

```bash
cargo test -p ggen-mcp             # 65 tests (verified 2026-08-03; recount if this drifts)
```

Tests are Chicago TDD throughout: real `TempDir`, real `ggen.toml`, real
ontology, real templates, no mocks. `tests/mcp_protocol_test.rs` spawns the
actual binary and speaks JSON-RPC over its stdio — the only place tool
annotations are verifiable, since they exist only in the `tools/list`
payload a client receives.

### Self-play harness

Three `tests/self_play_*_test.rs` files replay every real pack under
`packs/` that ships an `ontology.ttl` (73 as of writing) through this
crate's own tools end to end — syntax gate, query, independent row
recount, write the template, dry run, apply, receipt verify, second apply
for idempotence — with a referee (`src/selfplay/referee.rs`) checking
invariants like "a successful write always produces a receipt" and "a
second sync of the same inputs changes nothing." `self_play_vacuity_test.rs`
is the meta-check that the sweep actually reaches the write path for most
of those packs, not just the read-only path. `tests/common/` holds shared
fixture-writing helpers; `tests/corpus/` holds fixed adversarial case files
(malformed SPARQL, path traversal, cartesian blowup, ...) replayed
deterministically, separate from the pack sweep.

`just self-play` (`cargo test -p ggen-mcp --test self_play_test --test
self_play_falsifier_test --test self_play_vacuity_test`) is a mandatory,
last-in-chain dependency of `just pre-commit` — not an optional extra
suite. A separate `just self-play-explore` recipe grows the corpus via a
local LLM (`ggen-selfplay-explore`, a dev-only binary); it is corpus
generation, never itself a test, and is not part of any gate.
