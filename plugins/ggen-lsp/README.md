# ggen-lsp — Claude Code plugin

The ggen language server, packaged as a Claude Code LSP marketplace plugin. It
gives Claude Code (and any LSP editor) author-time intelligence over ggen's **law
surfaces** — the files the μ₁–μ₅ pipeline treats as source of truth:

| Surface | Extensions | What the server provides |
|---------|-----------|--------------------------|
| RDF | `.ttl` `.nt` `.nq` | located syntax diagnostics, prefix/IRI completion, hover, go-to-definition, references, symbols, semantic tokens, formatting |
| SPARQL | `.rq` `.sparql` | parse diagnostics (E0010 VALUES-in-external, E0011 CONSTRUCT-without-ORDER-BY), completion, hover, references, formatting (qlue-ls) |
| Tera | `.tera` | template syntax diagnostics (E0024), symbols |
| Config | `ggen.toml` / `.toml` | schema + enum-law diagnostics (E0001 parse, E0023 enum), symbols |

Beyond editor convenience, ggen-lsp is the **author-time admission gate**: every
diagnostic carries a canonical *repair route* (a `RouteEnvelope`) — the same route
the headless `ggen lsp check` gate and the MCP/A2A bridges produce, so an edit a
human makes in-editor and an edit an agent makes over MCP are judged by one law.

## How it integrates with the Claude Code lifecycle

This plugin follows the exact contract the official `rust-analyzer-lsp` and
`typescript-lsp` plugins use — a `lspServers` entry in the marketplace manifest:

```jsonc
// .claude-plugin/marketplace.json  →  plugins[].lspServers
"ggen": {
  "command": "ggen-lsp",                 // dedicated LSP binary, like `rust-analyzer`
  "extensionToLanguage": { ".ttl": "turtle", ".rq": "sparql", ".tera": "tera", ... }
}
```

When a project contains a mapped file type, Claude Code spawns `ggen-lsp`, speaks
**LSP JSON-RPC over stdio**, sends `initialize` → `textDocument/didOpen`/`didChange`
→ receives `publishDiagnostics`, and routes the LSP tool's
`documentSymbol`/`definition`/`hover`/`references` to the running server. A bare
`ggen-lsp` invocation defaults to stdio (matching the rust-analyzer convention);
`ggen-lsp stdio` is equivalent.

## Install

1. **Put the server on `$PATH`** (the plugin configures the connection; it does not
   bundle the binary — same as rust-analyzer-lsp requiring `rust-analyzer` on PATH):
   ```bash
   cargo install --path crates/ggen-lsp     # installs the `ggen-lsp` binary
   # or, for local dev: cargo build -p ggen-lsp && cp target/debug/ggen-lsp ~/.cargo/bin/
   ```
2. **Add this repo as a marketplace, install + enable the plugin:**
   ```bash
   /plugin marketplace add /Users/sac/ggen     # (or the published git repo)
   /plugin install ggen-lsp@ggen
   ```
   Enablement is recorded in `~/.claude/settings.json`:
   ```json
   { "enabledPlugins": { "ggen-lsp@ggen": true } }
   ```

## Verify it works

```bash
ggen-lsp stdio </dev/null            # starts, waits for JSON-RPC on stdin (Ctrl-C to exit)
ggen lsp check --root . --with_routes true   # the same engine, headless (needs --features lsp)
```

The server, the headless gate, and the MCP/A2A bridges are one route engine — see
`docs/reference/cli/delivery-plane-proof.md` for the cross-transport parity proof.
