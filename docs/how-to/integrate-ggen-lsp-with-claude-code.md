<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to integrate ggen-lsp with the Claude Code lifecycle](#how-to-integrate-ggen-lsp-with-the-claude-code-lifecycle)
  - [What am I trying to accomplish?](#what-am-i-trying-to-accomplish)
  - [How Claude Code's LSP plugins work (the contract we emulate)](#how-claude-codes-lsp-plugins-work-the-contract-we-emulate)
  - [What command do I run?](#what-command-do-i-run)
  - [What file changes?](#what-file-changes)
  - [What result proves it worked?](#what-result-proves-it-worked)
  - [What do I do next?](#what-do-i-do-next)
  - [The three-plane hosting model](#the-three-plane-hosting-model)
  - [Testing & smoke](#testing--smoke)
  - [Notes](#notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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

## The three-plane hosting model

The same route engine (the E00XX law set, e.g. `E0011`) is exposed to Claude Code
over three independent transports. Each plane is hosted by a different config file
and speaks a different framing, but the diagnostics and repair routes are identical.

| Plane | Declared in | Key | Command | Framing |
|-------|-------------|-----|---------|---------|
| **LSP** | `.claude-plugin/marketplace.json` | `plugins[ggen-lsp].lspServers.ggen` | `ggen-lsp` (on `$PATH`) | Content-Length-framed LSP JSON-RPC over stdio |
| **MCP** | project `.mcp.json` | `mcpServers.ggen-lsp-mcp` | `cargo run -q -p ggen-lsp-mcp` | newline-delimited JSON-RPC over stdio |
| **A2A** | `ggen-lsp-a2a` (bridge library) | — | (library; consumed by A2A hosts) | agent-to-agent bridge over the same route engine |

- **LSP plane** — Claude Code spawns the `ggen-lsp` binary (like `rust-analyzer`)
  and speaks Content-Length-framed LSP. This is the author-time editor face that
  surfaces diagnostics + repair routes on `.ttl`/`.rq`/`.tera`/`ggen.toml`.
- **MCP plane** — the project's `.mcp.json` already declares
  `mcpServers.ggen-lsp-mcp` → `cargo run -q -p ggen-lsp-mcp`. This exposes the same
  repair routes as an MCP tool, framed with newline-delimited JSON-RPC.
- **A2A plane** — `ggen-lsp-a2a` is a cycle-free **leaf crate** that bridges the
  `ggen-lsp-mcp` route engine to A2A (agent-to-agent) hosts. It is a library, not a
  standalone server, kept dependency-cycle-free by design.

`ggen-lsp-mcp` (and the A2A bridge on top of it) are deliberately leaf crates to
avoid the `ggen-core`↔`ggen-a2a-mcp` dependency cycle.

## Testing & smoke

The plugin test harness is first-class under `cargo make` (the repo is
**Cargo Make Only**). Build the binaries once, then run the harness:

```bash
cargo make build-lsp-bins   # build ggen-lsp + ggen-lsp-mcp (prereq for the rest)

# Integration tests (spawn the built binaries, drive the real protocols)
cargo make lsp-test         # ggen-lsp: manifest_contract + lsp_protocol + lsp_sabotage
cargo make mcp-test         # ggen-lsp-mcp: mcp_protocol

# Live smoke tests — exit 0 iff the live E0011 route is observed end-to-end
cargo make lsp-smoke        # python3 scripts/lsp-smoke.py  (Content-Length framed)
cargo make mcp-smoke        # python3 scripts/mcp-smoke.py  (newline framed)

# Oracle-Gap detector — fails on HIGH (real todo!/unimplemented! in live code)
cargo make find-fakes       # bash scripts/find-fakes.sh
cargo make find-fakes-clippy  # non-breaking WARN inventory of unwrap/expect/panic/...
```

- `scripts/lsp-smoke.py` spawns the built `ggen-lsp` binary, speaks
  **Content-Length-framed** LSP JSON-RPC, and exits 0 only if the live `E0011`
  route is observed.
- `scripts/mcp-smoke.py` spawns `ggen-lsp-mcp`, speaks **newline-framed**
  JSON-RPC, and likewise exits 0 only on the live `E0011` route.
- `cargo make find-fakes-clippy` runs clippy's fake-set lints
  (`unwrap_used`/`expect_used`/`panic`/`todo`/`unimplemented`/`dbg_macro`) as
  **warnings (`-W`)**, not denials. The `[workspace.lints.clippy]` block in
  `Cargo.toml` already *denies* this set, but it is **inert** — no member crate
  opts in via `[lints] workspace = true` — so the WARN inventory is non-breaking.

`lsp-test`, `mcp-test`, and `find-fakes` are wired into the `cargo make ci`
aggregate, so the full pipeline exercises all three.

## Notes

- `ggen-lsp` defaults to stdio when run bare (rust-analyzer convention); `ggen-lsp
  stdio` is equivalent. Stdio is the only transport.
- The legacy `[package.metadata.claude-code]` block in `crates/ggen-lsp/Cargo.toml`
  predates this and is NOT what Claude Code reads — the authoritative declaration is
  `.claude-plugin/marketplace.json`.
