# ggen-lsp

`ggen-lsp` is the offline language server and law-diagnostic engine for ggen RDF, SPARQL, Tera, and TOML surfaces. It performs static analysis only. Editor features do not require an LLM, network service, hosted telemetry collector, or external process-mining service.

The editor, headless checker, MCP bridge, and A2A bridge share the same analyzer, diagnostic, route, and receipt semantics. Transport framing may differ; meaning must not.

## Delivered editor contract

The server provides:

- completion, hover, definitions, references, and rename;
- document and workspace symbols;
- diagnostics and repair-route code actions;
- document and range formatting;
- folding ranges, inlay hints, code lenses, and full semantic tokens;
- call hierarchy prepare, incoming-call, and outgoing-call requests;
- type hierarchy prepare, supertype, and subtype requests.

Call and type hierarchy capabilities are negotiated. Call hierarchy is returned statically when the client supports it without dynamic registration. When the client supports dynamic registration, call and type hierarchy are registered with `client/registerCapability` after initialization. A prepared symbol with no known edges returns an empty relation array rather than a JSON-RPC `method_not_found` error.

## Law surfaces

| Extension | Analyzer |
|---|---|
| `.ttl` | Turtle/RDF |
| `.nt` | N-Triples |
| `.nq` | N-Quads |
| `.rq`, `.sparql` | SPARQL |
| `.tera` | Tera |
| `.toml` | ggen/TOML |

Cross-surface diagnostics use the project and harness indexes. Open buffers override disk for indexed query and template content so diagnostics reflect the current editor state.

## Diagnostic lifecycle

Each refresh produces one coherent publication per URI. Registry replacement is document-scoped: stale entries for the refreshed document are removed without deleting another document's diagnostics. Diagnostic identity includes URI, range, code, and message.

The `.ggen/lambda_cd.gate` file is global to the active workspace:

- `1` means at least one open document has an active `GGEN-*` error;
- `0` means no open document has an active gating violation.

A clean document cannot open the gate while another document remains violated. Shutdown removes this server session's registry diagnostics and writes an open gate. The gate directory is created before the write.

Workspace identity is selected in this order for registry and gate state:

1. `workspaceFolders[0]`;
2. deprecated `rootUri`;
3. process current directory.

## Build and run

From the repository root:

```bash
cargo build -p ggen-lsp
./target/debug/ggen-lsp
```

The language-server transport is stdio. Stdout is reserved for LSP `Content-Length` frames.

The CLI also exposes headless and protocol surfaces:

```bash
ggen lsp start
ggen lsp serve --protocol lsp
ggen lsp serve --protocol mcp
ggen lsp check .
ggen lsp replay <case>
ggen lsp metrics
ggen lsp mine
ggen lsp emit_pack
ggen lsp verify_pack
```

## Verification

Use the pinned repository toolchain:

```bash
cargo fmt --check -p ggen-lsp
cargo check -p ggen-lsp --all-features
cargo test -p ggen-lsp --lib
cargo test -p ggen-lsp --test lsp_protocol_test
cargo test -p ggen-lsp --test lsp_contract_completion_test
cargo build -p ggen-lsp
python3 scripts/lsp-smoke.py
```

`lsp_contract_completion_test` spawns the real binary and verifies dynamic hierarchy registration, all four hierarchy follow-up methods, workspace-folder gate placement, gate-directory creation, and shutdown cleanup over actual LSP framing.

## Architecture

```text
src/server.rs          LSP capability negotiation and protocol lifecycle
src/state.rs           open documents, analyzers, cross-surface publication
src/analyzers/         RDF, SPARQL, Tera, TOML, harness, and source laws
src/handlers/          feature request handlers
src/check.rs           headless law gate
src/route.rs           shared repair-route selection
src/intel/             OCEL event capture and mining projections
```

The governing subtree contract is [`AGENTS.md`](AGENTS.md). The architecture specification is [`docs/architecture/LSP-ARD-PRD.md`](../../docs/architecture/LSP-ARD-PRD.md).
