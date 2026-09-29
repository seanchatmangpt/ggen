# Rust Code Intelligence (LSP)

Use LSP for all Rust symbol navigation — never grep for a symbol in `.rs`. LSP understands
semantics (30-crate workspace, heavy traits/re-exports/generics).

`workspaceSymbol`→`documentSymbol`→`goToDefinition`→`findReferences`→`goToImplementation`
(project traits)→`hover`→`prepareCallHierarchy`→`incomingCalls`/`outgoingCalls`. Params:
`filePath`, `line`/`character` (1-based).

Fallback if "No LSP server available": `rustup component add rust-analyzer`, verify
`rust-analyzer-lsp@claude-plugins-official` enabled, restart; still failing → grep, flag as
degraded.

Grep OK for: TODO/FIXME text, non-Rust files, string literals/error messages/test assertions.
