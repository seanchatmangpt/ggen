# ggen-lsp — Language Server Protocol for ggen

A **pure code intelligence** language server for ggen RDF, Tera, and TOML files. No LLM integration — static analysis only.

## Features

✨ **Syntax Highlighting** — Color-code RDF namespaces, Tera keywords, TOML keys  
✨ **Completion** — Context-aware suggestions for RDF predicates, Tera filters, config keys  
✨ **Hover Documentation** — Show SHACL shapes, type info, config descriptions  
✨ **Definition Jumping** — Navigate to class definitions, template includes  
✨ **Refactoring** — Rename symbols with cross-file validation  
✨ **Code Folding** — Collapse RDF shapes, Tera blocks, TOML sections  
✨ **Document Outline** — List all definitions in current file  
✨ **Code Lenses** — Inline commands ("Show SHACL shape", etc.)  
✨ **Auto-formatting** — Format RDF, Tera, TOML with consistent style  
✨ **Class Hierarchy** — Show SHACL parent/child classes  
✨ **Template Includes** — Trace template composition  

## Installation

Via Claude Code Marketplace (auto-registered when opening ggen files):

```bash
# Or manually: ensure ggen is in PATH
cargo install --path crates/ggen-cli
```

## Configuration

Edit `~/.claude/settings.json`:

```json
{
  "ggen-lsp": {
    "enabled": true,
    "transport": "stdio",
    "auto_format_on_save": true,
    "show_hints": true
  }
}
```

## Usage

| Action | Keys | Result |
|--------|------|--------|
| Open `.ttl` file | — | LSP auto-starts, syntax highlighting enabled |
| Completion | Type `sh:` | Suggests `sh:property`, `sh:path`, etc. |
| Hover | Hover over class | Shows SHACL shape, documentation |
| Jump to definition | Cmd+Click on symbol | Opens definition |
| Find references | Cmd+Shift+F | Lists all usages |
| Rename | Cmd+K Cmd+R | Rename across all files |
| Format document | Cmd+Shift+P → Format | Auto-format file |
| Show outline | Cmd+O | List all definitions |

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

## Advanced Features (tower-lsp 0.20)

| Feature | Status | Use Case |
|---------|--------|----------|
| Semantic Tokens | 🟢 Core | Syntax highlighting (namespace, class, property colors) |
| Completion | 🟢 Core | RDF predicates, Tera filters, config keys |
| Hover | 🟢 Core | SHACL shapes, documentation, type info |
| Document Symbols | 🟢 Core | Outline: list all definitions |
| Workspace Symbols | 🟢 Core | Project-wide search |
| Code Lenses | 🟢 Core | Inline "Show shape", "Show bindings" |
| Folding Range | 🟢 Core | Collapse RDF/Tera/TOML blocks |
| Formatting | 🟢 Core | Auto-format with consistent style |
| Rename | 🟡 Extended | Refactor symbol names across files |
| Inlay Hints | 🟡 Extended | Show type hints, defaults inline |
| Call Hierarchy | 🟡 Extended | Template include graph |
| Type Hierarchy | 🟡 Extended | SHACL class hierarchy |

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

# Run LSP server
ggen lsp start --transport stdio

# Debug HTTP transport
ggen lsp start --transport http --port 9999
```

## Testing

Chicago TDD style (no mocks):
- Real file I/O (TempDir)
- Real analyzer execution
- Real LSP protocol messages
- 80%+ code coverage target

See [LSP-ARD-PRD.md](../../LSP-ARD-PRD.md) for test strategy.

## References

- **Architecture**: [LSP-ARD-PRD.md](../../LSP-ARD-PRD.md) — full spec
- **tower-lsp**: [https://docs.rs/tower-lsp/](https://docs.rs/tower-lsp/)
- **LSP Spec**: [https://microsoft.github.io/language-server-protocol/](https://microsoft.github.io/language-server-protocol/)

## License

Same as ggen project (see LICENSE file in root)

## Support

- Issues: [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
- Discussions: [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
