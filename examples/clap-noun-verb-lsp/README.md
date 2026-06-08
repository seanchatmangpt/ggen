# clap-noun-verb-lsp

`clap-noun-verb-lsp` is a working LSP for `clap-noun-verb` projects. It turns a CLI command grammar into an inspectable, diagnosable, repairable, source-attributed language surface.

## Features
- **CLI validates**: Extracts noun/verb command graph.
- **Domain computes**: Differentiates domain logic from integration.
- **LSP observes**: Pull diagnostics, Code Actions, Semantic Tokens, Inlay Hints, Inline Values.
- **Dynamic Registration**: Registers and unregisters capabilities based on workspace changes.
- **Virtual Documents**: Read-only command graph documents.
- **Receipts**: Emits JSONL receipts for major LSP requests.

## Tests
To run tests locally:
```sh
cargo test -p clap-noun-verb-lsp --all-features
```
