# lsp-max RulePackServer rule index

Generated from the `lsp-max-pack` ontology. Each rule ships as its own
`rules/lsp_max_*.toml` file and is loaded by the `RulePackServer` trait
implementation at server startup.

| Rule ID | Name | Severity | Message |
|---------|------|----------|---------|
| `LSPMAX-RAWJSON-002` | Raw protocol fallback | warning | Raw JSON fallback detected; use the generated metaModel.json protocol types. |
| `LSPMAX-UNWRAP-001` | Unwrap in production path | error | unwrap()/expect() outside #[cfg(test)] panics the language server; return Result instead. |
| `LSPMAX-WALLCLOCK-003` | Wall clock in server code | error | Wall-clock reads make receipts nondeterministic; thread time in from the transport boundary. |

