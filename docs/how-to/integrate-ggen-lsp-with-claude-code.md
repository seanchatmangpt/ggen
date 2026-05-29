# How to integrate ggen-lsp with the Claude Code lifecycle

> How-to. Goal: get Claude Code to launch ggen's language server over its normal
> LSP lifecycle, so editing ggen law surfaces (`.ttl`/`.rq`/`.tera`/`ggen.toml`)
> surfaces live diagnostics + repair routes. Emulates the official
> `rust-analyzer-lsp` / `typescript-lsp` plugins.

## What am I trying to accomplish?

Make `ggen-lsp` a first-class Claude Code LSP plugin — spawned automatically when a
project has ggen law surfaces, driving diagnostics, navigation, and repair-route
code actions through Claude Code's standard JSON-RPC-over-stdio LSP integration.

## How Claude Code's LSP plugins work (the contract we emulate)

Official LSP plugins (e.g. `rust-analyzer-lsp@claude-plugins-official`) declare a
language server in the marketplace manifest's `lspServers` map — nothing more:

```jsonc
// rust-analyzer-lsp (verbatim from the official marketplace.json):
"lspServers": { "rust-analyzer": { "command": "rust-analyzer",
                                    "extensionToLanguage": { ".rs": "rust" } } }
// typescript-lsp:
"lspServers": { "typescript": { "command": "typescript-language-server",
                                "args": ["--stdio"], "extensionToLanguage": { ".ts": "typescript", ... } } }
```

The contract is just `command` (binary on `$PATH`), optional `args`, and
`extensionToLanguage` (which file types route to this server). Claude Code spawns
the binary, speaks LSP JSON-RPC over **stdio**, and maps its LSP tool operations
(`documentSymbol`/`definition`/`hover`/`references`/diagnostics) to it. The plugin
**configures the connection; it does not bundle the binary** — same as
rust-analyzer-lsp requiring `rust-analyzer` on PATH.

## What command do I run?

```bash
# 1. Put the ggen-lsp server binary on $PATH (it is a dedicated LSP binary, like rust-analyzer):
cargo install --path crates/ggen-lsp        # provides `ggen-lsp`
ggen-lsp stdio </dev/null                    # smoke test: starts, waits for JSON-RPC (Ctrl-C)

# 2. Register this repo as a marketplace and install + enable the plugin:
/plugin marketplace add /Users/sac/ggen      # (or the published git URL)
/plugin install ggen-lsp@ggen
```

## What file changes?

`~/.claude/settings.json` gains the enablement:
```json
{ "enabledPlugins": { "ggen-lsp@ggen": true } }
```
The plugin is defined in this repo at `.claude-plugin/marketplace.json` (the `ggen`
marketplace) → `plugins[ggen-lsp].lspServers.ggen`, with `command: "ggen-lsp"` and
the law-surface extension map (`.ttl`→turtle, `.nt`/`.nq`, `.rq`/`.sparql`→sparql,
`.tera`→tera, `.toml`→toml).

## What result proves it worked?

Open or edit a ggen law surface in a Claude Code project. A broken one shows the
diagnostic live (Ctrl-O), e.g. for `playground/proof/broken-construct.rq`:
```
E0011  CONSTRUCT query lacks ORDER BY — required when strict_mode is enabled
       → repair route: template.values-inline
```
The same engine, headless, confirms it: `ggen lsp check --root . --with_routes true`
(needs the `ggen` binary built `--features lsp`).

## What do I do next?

The LSP, the headless gate, and the MCP/A2A bridges are **one route engine** — the
editor surface you just wired is the author-time face of the same law the agent
transports use. See [delivery-plane-proof.md](../reference/cli/delivery-plane-proof.md)
for the cross-transport parity proof, and [genesis-run](../explanation/genesis-run.md)
for why author-time enforcement is the foundation.

## Notes

- `ggen-lsp` defaults to stdio when run bare (rust-analyzer convention); `ggen-lsp
  stdio` is equivalent. Stdio is the only transport.
- The legacy `[package.metadata.claude-code]` block in `crates/ggen-lsp/Cargo.toml`
  predates this and is NOT what Claude Code reads — the authoritative declaration is
  `.claude-plugin/marketplace.json`.
