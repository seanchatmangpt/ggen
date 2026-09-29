# ggen v26.8.2 - Rust Code Generation CLI

Spec-driven codegen from RDF. A=μ(O), 5-stage pipeline. Rust nightly | Tokio | Oxigraph | Tera |
Clap | Chicago TDD only | 14-crate workspace. Crate map/packs/commands: GENERATED
`.claude/rules/architecture.md` — edit `.specify/repo-facts.ttl`, never the file.

**Process Intelligence Boundary**: ggen EMITS OCEL evidence, never ANALYSES it
(wasm4pm-compat/wasm4pm own that). `wasm4pm` can't be a direct dep. Analysis code in ggen =
residue, delete it.

`mode="Create"` silently skips existing files; never `mode="Overwrite"` on hand-written logic.

Never fabricate examples/OTEL/schemas. Forbidden: `mcp__desktop-commander__*`. LSP over Grep
for `.rs` ([[rust/lsp]]). Andon stops the line ([[andon/signals]]). `just <task>` only. Zero
`unwrap()/expect()` in production. DoD = `just pre-commit` green + OTEL for LLM/external
features.

**Receipts**: BLAKE3 chained `.ggen-v2/receipt.json`, sign key `GGEN_SIGNING_KEY` env else
`.ggen/keys/signing.key`. Verify: `ggen receipt verify` (zero args).

**Git hooks**: `pre-commit`/`pre-push` are `main`-only (run `just pre-commit` manually on
branches). `cargo build -p <crate>` isn't a health signal — use `--workspace`.

Push only to `origin`. OTEL → [[otel-validation]]. Chicago TDD → [[rust/testing]].

**Bounded Unattended-Write Dispatcher**: `ggen-mcp`'s `unattended_dispatch::
try_unattended_apply` writes with zero decision step, narrow frontmatter-opt-in eligibility,
working-tree only.

Repo: github.com/seanchatmangpt/ggen. `ggen-core` fully deleted, replaced by vendored
`ggen-engine`/`praxis-core`/`praxis-graphlaw`.
