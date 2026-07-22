# ggen-lsp — Language Server Protocol for ggen

A **pure code intelligence** language server for ggen RDF, Tera, and TOML files. No LLM integration — static analysis only.

> **Core value:** the process-intelligence route/OCEL engine — diagnostics become OCEL events, mined into ranked failure edges and POWL repair routes delivered as CodeActions, headless JSON, or MCP tools. The editor features below are the surface; the engine is the point.

## Features (delivered)

✨ **Completion** — Context-aware suggestions for RDF predicates, Tera filters, config keys  
✨ **Hover Documentation** — Show SHACL shapes, type info, config descriptions  
✨ **Definition Jumping** — Navigate to class definitions, template includes  
✨ **References** — Find all usages across files  
✨ **Rename** — Rename symbols with cross-file validation  
✨ **Document Symbol** — List all definitions in current file  
✨ **Code Folding** — Collapse RDF shapes, Tera blocks, TOML sections  
✨ **Code Actions** — Apply repair routes as `WorkspaceEdit` quickfixes  
✨ **Diagnostics** — Surface E00XX law violations live as you type  
✨ **Semantic Tokens** — Full-document tokenization (namespace/class/property/variable/…)  
✨ **Formatting** — Document and range formatting for TOML/Turtle/SPARQL  
✨ **Code Lenses** — Inline actionable annotations  
✨ **Workspace Symbol** — Search definitions across the workspace root  
✨ **Inlay Hints** — Inline type/value hints  

> Available via handler but NOT advertised in server capabilities: call hierarchy, type hierarchy — `prepare_call_hierarchy`/`prepare_type_hierarchy` are implemented in `server.rs` (and backed by the analyzers) but `initialize()` declares no `call_hierarchy_provider`/`type_hierarchy_provider`, so editors will not request them.

## Installation

Via Claude Code Marketplace (auto-registered when opening ggen files):

```bash
# Or manually: ensure ggen is in PATH
cargo install --path crates/ggen-cli
```

## Configuration

The LSP server is stdio-only. Editor-side settings live in `~/.claude/settings.json`:

```json
{
  "ggen-lsp": {
    "enabled": true
  }
}
```

> Note: `transport`, `auto_format_on_save`, and `show_hints` are editor-side hints only — they are NOT read or enforced by ggen.

## CLI Verbs

`ggen lsp` exposes 10 verbs:

| Verb | Purpose |
|------|---------|
| `start` | Run the language server (stdio only; editors launch this) |
| `serve` | Run a protocol server — `--protocol lsp` or `--protocol mcp` |
| `check` | Headless gate: scan all law surfaces, exit non-zero on ERROR |
| `init` | One-command setup: write editor configs + Agent Admissibility Pack |
| `replay` | Replay a recorded OCEL case |
| `metrics` | Report process-intelligence metrics |
| `field-status` | Show field/surface status |
| `mine` | Discover the project's 80/20 failure edges (OCEL → SPARQL DFG) |
| `emit_pack` | Regenerate the movable stewardship pack |
| `verify_pack` | Verify a stewardship pack |

## Usage

| Action | Result |
|--------|--------|
| Open `.ttl` file | LSP auto-starts, diagnostics enabled |
| Completion | Suggests `sh:property`, `sh:path`, etc. |
| Hover | Shows SHACL shape, documentation |
| Go to definition | Opens definition |
| Find references | Lists all usages |
| Rename | Rename across all files |
| Document symbol | List all definitions |
| Code action | Apply a repair-route quickfix |

## Architecture

```
ggen-lsp/
├── src/
│   ├── server.rs           # LanguageServer trait (15+ LSP methods)
│   ├── state.rs            # Document cache, analyzer dispatch
│   ├── analyzers/          # Per-file-type analyzers
│   │   ├── rdf_analyzer.rs      # .ttl parsing + RDF logic
│   │   ├── tera_analyzer.rs     # .tera parsing + template logic
│   │   └── toml_analyzer.rs     # ggen.toml schema validation
│   ├── handlers/           # Protocol message handlers (14 handlers)
│   ├── error.rs            # Error types
│   └── lib.rs              # Library entry points
├── marketplace/
│   ├── ggen-lsp.md         # Marketplace plugin manifest
│   ├── schema.json         # Configuration schema
│   ├── INSTALL.md          # Setup guide
│   └── icon.png            # Plugin icon (128x128)
└── Cargo.toml              # Package metadata + marketplace registration
```

## LSP Capabilities (lsp-max 26.7.1)

| Feature | Status | Use Case |
|---------|--------|----------|
| Completion | 🟢 Delivered | RDF predicates, Tera filters, config keys |
| Hover | 🟢 Delivered | SHACL shapes, documentation, type info |
| Definition | 🟢 Delivered | Jump to class/template definition |
| References | 🟢 Delivered | Find all usages across files |
| Rename | 🟢 Delivered | Refactor symbol names across files |
| Document Symbol | 🟢 Delivered | Outline: list all definitions |
| Folding Range | 🟢 Delivered | Collapse RDF/Tera/TOML blocks |
| Code Action | 🟢 Delivered | Repair-route quickfixes (`WorkspaceEdit`) |
| Diagnostics | 🟢 Delivered | Surface E00XX law violations live |
| Semantic Tokens (full) | 🟢 Delivered | Full-document tokenization (no `range`) |
| Formatting (document) | 🟢 Delivered | Format whole TOML/Turtle/SPARQL document |
| Formatting (range) | 🟢 Delivered | Format a selection |
| Inlay Hint | 🟢 Delivered | Inline type/value hints |
| Code Lens | 🟢 Delivered | Inline actionable annotations |
| Workspace Symbol | 🟢 Delivered | Search definitions across workspace root |
| Call Hierarchy | 🟡 Handler only | `prepare_call_hierarchy` implemented; not advertised in `initialize()` |
| Type Hierarchy | 🟡 Handler only | `prepare_type_hierarchy` implemented; not advertised in `initialize()` |

> The 🟢 rows above are exactly the capabilities advertised in `server.rs` `initialize()` (`semantic_tokens_provider` advertises `full` only — no `range`). The 🟡 rows have working handlers in `server.rs` but no matching provider in `initialize()`, so they are reachable only if a client requests them directly. Nothing is now wholly absent.

## Performance

| Operation | Target | Status |
|-----------|--------|--------|
| Server startup | <200ms | ✅ |
| Completion | <100ms | ✅ |
| Hover | <50ms | ✅ |
| Diagnostics (large files) | <500ms | ✅ |
| Memory usage | <100MB | ✅ |

## No LLM Integration

This LSP is **pure code intelligence**:
- ✅ Static analysis (RDF parsing, SPARQL validation, Tera AST)
- ✅ Local file I/O only
- ✅ No external API calls
- ✅ No Claude, Groq, OpenAI, or any LLM support

Perfect for fast, offline IDE support.

## Development

```bash
# Build
cargo build -p ggen-lsp

# Test (implemented by other agent)
cargo test -p ggen-lsp

# Run LSP server (stdio only — editors launch this)
ggen lsp start

# Run a protocol server (lsp or mcp)
ggen lsp serve --protocol mcp
```

## Testing

Chicago TDD style (no mocks):
- Real file I/O (TempDir)
- Real analyzer execution
- Real LSP protocol messages
- 80%+ code coverage target

See [LSP-ARD-PRD.md](../../docs/architecture/LSP-ARD-PRD.md) for test strategy.

## References

- **Architecture**: [LSP-ARD-PRD.md](../../docs/architecture/LSP-ARD-PRD.md) — full spec
- **lsp-max**: [https://github.com/seanchatmangpt/lsp-max](https://github.com/seanchatmangpt/lsp-max) — law-state LSP runtime
- **LSP Spec**: [https://microsoft.github.io/language-server-protocol/](https://microsoft.github.io/language-server-protocol/)

## License

Same as ggen project (see LICENSE file in root)

## Support

- Issues: [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
- Discussions: [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
