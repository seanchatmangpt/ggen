### Crate Map (14 workspace crates, generated from `.specify/repo-facts.ttl`)

Verified against `Cargo.toml`'s `[workspace] members = [...]` array (13 entries) plus the root `ggen` package = 14 total (`grep -c '^  "crates/' Cargo.toml` → 13; corrected 2026-10-09, re-corrected 2026-10-10 after the praxis-core/praxis-graphlaw retirement dropped 2 members; the parity test caught both drifts). Trimmed from 17 packages / 24 disk dirs to 10 packages / 9 disk dirs by the 2026-07 crate-consolidation pass — see `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` for that pass's evidence base and history. The workspace then gained `ggen-engine` for the ggen-core-replacement migration (`docs/jira/v26.7.16/`), plus vendored `praxis-core`/`praxis-graphlaw` — the latter two retired 2026-10-10 (SJIRA-15), replaced by the sibling `graphlaw` crate. PR #255 (2026-07-17) added 4 more — `powl2-decompose`, `chicago-tdd-tools`, `bcinr-pddl`, `bcinr-mfw-ir` — vendored to eliminate every absolute `/Users/sac/...` Cargo path dependency in the workspace (they only resolved on one machine, breaking CI). `chicago-tdd-tools` was removed again on 2026-08-03 once chicago-tdd-tools 26.8.3 (including the `cli-proof` feature) was published to crates.io, dropping the vendor and pointing every consumer at the registry version instead — net member count 18, not 19. PR #257 (2026-07-17) added a 17th member, `ggen-cheat-scanner`; `ggen-mcp` (an MCP server exposing ggen's SPARQL/frontmatter/diagnostic introspection surface as tool calls) followed as an 18th member, present in `Cargo.toml` since before 2026-08-03 but only added to this facts file on that date once `crates/ggen-config/tests/system_crate_map_parity_test.rs` caught the divergence (see Agent & LSP surface table in `.claude/rules/architecture.md`). `crates/ggen-architecture/` is deliberately not one of these 19: it declares its own `[workspace]` table (and carries its own `Cargo.lock`), so Cargo treats it as an independent nested workspace, not a member of this one — it is listed in root `Cargo.toml`'s `[workspace] exclude` for that reason, alongside `examples/7-agent-validation`. For the fuller, actively-refreshed breakdown (Praxis-kernel split, per-crate detail, the license note on `wasm4pm-cognition`) see `.claude/rules/architecture.md`; this table is a lighter top-level summary.

`ggen-core` is **fully deleted, not merely disconnected** (PR #255 retired it from the default path; PR #259 deleted the crate outright, 2026-07-17). `crates/ggen-core/` does not exist on disk. No workspace member depends on it; `ggen sync`/`doctor`/`graph`/`receipt` route to `ggen-engine` instead (`crates/ggen-cli/src/lib.rs`'s `inject_default_verbs`). The experimental, default-off `ggen wizard`/`sigma`/`inverse_sync` commands, which used to import `ggen_core::` symbols, were deleted in the same pass rather than re-pointed — see `crates/ggen-cli/src/cmds/mod.rs`'s "REMOVED" comments and `docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md` (marked superseded/executed).

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-engine` | The live code-generation pipeline behind `ggen sync` (vendored, renamed from `~/praxis/crates/ggen`; `docs/jira/v26.7.16/`). Five stages in `src/sync.rs` (OTEL spans `pipeline.load`/`extract`/`validate`/`generate`/`emit`); GENERATED clap-noun-verb CLI routing in `src/verbs/` (hand-written logic only in `verbs::handlers`). `publish = true` as of v26.10.10 (praxis retirement); graph backend is the sibling `graphlaw` crate (26.10.5, path dep) |
| *(removed)* | `praxis-core`/`praxis-graphlaw` retired 2026-10-10 (SJIRA-15) — receipt-chain/epoch port landed in sibling `graphlaw` |
| `ggen-cli` | CLI interface for ggen (binary + `ggen-cli-lib`); routes `sync`/`doctor`/`graph`/`receipt` to `ggen-engine`'s nouns |
| `ggen-config` | Defines only ONE of `ggen.toml`'s two incompatible schemas (depends on the published `star-toml` crate, not an embedded copy) — see "ggen.toml has two schemas" in `.claude/rules/architecture.md` |
| `ggen-marketplace` | Marketplace / package management system for ggen |
| `ggen-graph` | Deterministic RDF graph module — Oxigraph wrapper with deterministic hashing, deltas, validation hooks, transition receipts |
| `ggen-lsp` | Language server for ggen surfaces (analyzers, check, intel, pack, route, repair); also exposes `check`/`init`/`mine` library APIs. Absorbed `ggen-lsp-mcp`, `ggen-a2a-mcp`, and `ggen-lsp-a2a` as feature-gated modules (`mcp`, `a2a`) in the 2026-07 consolidation |
| `ggen-cheat-scanner` | `syn`-based AST scanner (PR #257) detecting test-quality anti-patterns (vacuous asserts, tautological checks, no-assertion tests, mock imports) across the workspace; wired into `just pre-commit` via `guard-cheat-scan` |
| `powl2-decompose` | Vendored from `~/praxis/crates/powl2-decompose` (2026-07-17, PR #255): Kourani et al. Stage-1 WF-net → POWL 2.0 decomposition. `praxis-graphlaw`'s dependency (not published on crates.io). `publish = false` |
| `bcinr-pddl` | Vendored from `~/bcinr/crates/bcinr-pddl` (2026-07-17, PR #255) — **not** the crates.io release: the published 26.6.26 lacks `Pddl8Error::PlanningFailed`/`.into_result()` that `praxis-graphlaw`'s code needs (version-tag/content divergence, confirmed by a failed build against the registry version before vendoring). PDDL8 → POWL tape → Prolog8 admission → OCEL → BLAKE3 receipt. `publish = false` |
| `bcinr-mfw-ir` | Vendored alongside `bcinr-pddl` (its own hard dependency, same divergence reasoning) — shared IR types/trait contracts for the multifractal-workflow planner. `publish = false` |
| `ggen-mcp` | MCP server (`rmcp` over stdio) exposing ggen's SPARQL/frontmatter/diagnostic introspection surface as tool calls: nine tools, read-only except `ggen_write_apply` (destructive, requires explicit `confirm: true`). Present in `Cargo.toml` `[workspace] members` since before 2026-08-03 but absent from this facts file until that date, when `crates/ggen-config/tests/system_crate_map_parity_test.rs` caught the divergence — a real crate-map/repo-facts gap, not merely asserted here. Also ships `ggen-selfplay-explore`, a separate corpus-growth binary using a local LLM, never invoked from a test |
| `ggen` | Workspace root package |
| `pm4pytest-cli` | Thin `cargo` wrapper exposing the external `pm4pytest` process-conformance tool as a workspace command: resolves the binary from `PATH` or `PM4PYTEST_BIN`, never a direct Python/wasm dependency |
| `ggen-abb-sbb` | IO-free ABB/SBB admission kernel per RFC docs/rfc/v26.9.26/abb-sbb-implementation.md: `admit()`/`plan()`/`depgraph` (transitive closure, cycle + unbound-port refusals). Integrated into the root workspace in v26.10.9; wired as `ggen-engine`'s Stage 2c admission gate (crates/ggen-engine/src/sync.rs). `publish = true` as of v26.10.10 (crates.io release chain, SJIRA-25) |

### Commands (generated)

Use `just` as the entry point for all tasks (native cargo recipes; Makefile.toml is historical reference only).

| Command | Purpose | Timeout |
|---------|---------|---------|
| `just check` | `timeout 300s cargo check --workspace` | 300s |
| `just test` | Full test suite (unit + integration + property); escalates from a 30s hot-cache attempt to a 600s cold-compile retry | 30s→600s |
| `just test-lib` | `timeout 30s cargo test --lib --workspace` (fast dev loop) | 30s |
| `just lint` | `cargo clippy --all-targets -- -D warnings` — **root `ggen` package only**, not `--workspace`; confirmed 2026-07-17 (`Checking` output names exactly one package). Real, untriaged debt exists in other crates once `--workspace` is added — see the `lint:` recipe's own comment in `justfile` | 180s |
| `just pre-commit` | The justfile `pre-commit:` dependency line itself is the sole source of truth for the full gate list, order, and count — deliberately not restated here as an enumerated chain, since that enumeration already went stale at least once (a prior version of this fact omitted `guard-fail-open-subprocess`, `guard-gate-count`, and `self-play`, which had all landed in the live recipe without this doc being updated). `crates/ggen-config/tests/governance_precommit_gate_count_test.rs` parses the live recipe line and refuses if any governance doc restates a hardcoded gate *count* again, but it does not diff the full gate *name* list, which is how the prior staleness went undetected. Selected notes that don't drift with gate order: `guard-short-test-timeout` refuses load-sensitive sub-second test timeouts; `guard-pack-proofs` re-syncs + re-tests `examples/receiptctl`, making the generated pack proofs a repo-state-checkable gate; `guard-cheat-scan` currently fails on ~464 pre-existing test-quality findings (tracked as TECH-DEBT-001 in `docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`) — a real, tracked-not-fixed failure, not a regression; `self-play` (added for `ggen-mcp`) runs that crate's adversarial pack-lifecycle and anti-vacuity test suites | <2min |
| `just slo-check` | Performance SLOs — real wall-clock `date +%s` deltas around `cargo test -p ggen-engine --test receipt_chain_e2e` (180s threshold) plus a `cargo bench` startup check; see the `slo-check` recipe in `justfile` | - |
| `just audit` | Security vulnerabilities scan | - |
| `just doc` | Build HTML docs into `target/doc/` | - |
| `just test-doc` | Validate all `# Examples` blocks compile/run | - |
| `just bench` | `cargo bench` — **root `ggen` package only** (same scoping as `lint`; the root package does have 12 real `[[bench]]` entries, so this isn't a no-op, but it doesn't reach `ggen-engine`'s or other crates' benches). `just slo-check` separately targets the one bench that matters for the SLO gate (`cli_startup_performance`, which does live in the root package) | - |
| `just sync` | Runs `ggen sync --audit true` — **currently broken**: the live `sync run` verb (`crates/ggen-engine/src/verbs/sync.rs`) has no `--audit` flag (only `--dry-run`/`--watch`); confirmed 2026-07-17 by running the recipe (`error: unexpected argument '--audit' found`, exit 1) | - |
| `just sync-dry` | Runs `ggen sync --dry_run true` — **currently broken**: `--dry-run`/`--dry_run` is a bare switch on the live verb, it does not take a `true` value; confirmed 2026-07-17 by running the recipe (`error: unexpected argument 'true' found`, exit 1). Use `ggen sync run --dry-run` directly instead | - |
| `ggen graph validate` / `ggen law validate` | SHACL / law validation — there is no bare `ggen validate` noun (confirmed: `error: unrecognized subcommand 'validate'`); see `crates/ggen-engine/src/verbs/{graph.rs,law.rs}` | - |
| `ggen receipt verify` | Verify the BLAKE3 chain hash of the current sync receipt — **zero arguments**; always targets `.ggen-v2/receipt.json` under the resolved project root (`crates/ggen-engine/src/verbs/receipt.rs`) | - |

### Diagnostic Codes (Law Surfaces, generated)

Not all codes in the table below are implemented in `ggen-lsp`: the five
`GGEN-*` codes are author-time analyzer output (`crates/ggen-lsp/src/analyzers/tera_analyzer.rs`,
aggregated in `crates/ggen-lsp/src/check.rs`); `E0010`/`E0011`/`E0013`/`E0015` are SPARQL-analyzer
output (`crates/ggen-lsp/src/analyzers/sparql_analyzer.rs`). `E0010`/`E0011`/`E0013` are **also**
independently re-implemented as sync-time hard errors in `crates/ggen-config/src/manifest/validation.rs`
— two separate implementations, not one shared one (confirmed 2026-07-17: `crates/ggen-core`, which an
earlier version of this doc cited here, no longer exists — see Crate Map above). `E0014` (found
2026-08-03, red-team finding F4) is implemented ONLY in
`crates/ggen-config/src/manifest/validation.rs` as a sync-time hard error — it has no author-time
`ggen-lsp` analyzer equivalent. `ggen-engine`, the live pipeline, does not currently implement any
of these codes itself.

| Code | Law Surface | Severity | Meaning | Owner |
|------|-------------|----------|---------|-------|
| **GGEN-TPL-001** | SPARQL ↔ Tera | ERROR | Template consumes `{{ var }}` that SELECT does not produce. | `ggen-lsp/src/analyzers/tera_analyzer.rs` |
| **GGEN-OUT-001** | SPARQL ↔ ggen.toml | ERROR | `output_file` pattern consumes unbound variable. | `ggen-lsp/src/analyzers/tera_analyzer.rs` |
| **GGEN-YIELD-001** | ggen.toml ↔ OS | ERROR | `output_file` escapes the project root (Layer Violation). | `ggen-lsp/src/analyzers/tera_analyzer.rs` |
| **GGEN-RULE-001** | ggen.toml ↔ OS | ERROR | `{file = ...}` binding points at a missing file. | `ggen-lsp/src/analyzers/tera_analyzer.rs` |
| **GGEN-QUERY-002** | SPARQL | WARNING | `SELECT *` used (disables provision checks). | `ggen-lsp/src/analyzers/tera_analyzer.rs` |
| **E0011 / E0013** | SPARQL | WARNING* | `CONSTRUCT` / `SELECT` lacks `ORDER BY` (Strict Mode: ERROR). | `ggen-lsp/src/analyzers/sparql_analyzer.rs` (author-time) **and** `ggen-config/src/manifest/validation.rs` (sync-time hard error; two independent implementations, not shared) |
| **E0015** | SPARQL | WARNING | Identity `CONSTRUCT` detected (no-op mapping) — actively emitted, not reserved. | `ggen-lsp/src/analyzers/sparql_analyzer.rs` |
| **E0010** | SPARQL ↔ ggen.toml | ERROR | External `.rq` file contains a `VALUES` clause (data must be inline in `ggen.toml`). | `ggen-lsp/src/analyzers/sparql_analyzer.rs` (author-time) **and** `ggen-config/src/manifest/validation.rs` (sync-time hard error; two independent implementations, not shared) |
| **E0014** | ggen.toml | ERROR | Rule's `query`/`template` references a pack not declared in `[[packs]]`. | `ggen-config/src/manifest/validation.rs` (sync-time hard error only — no author-time `ggen-lsp` analyzer equivalent) |
