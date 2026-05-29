# Command-Proof Matrix (Reference)

> Reference + release artifact. This is the single table that answers, for every CLI
> noun: *does it work by executable proof, or is it removed from the release boundary?*
> ggen v26.5.28. Derived from `crates/ggen-cli/src/cmds/mod.rs`.

> Every command either works by executable proof or is removed from the release boundary
> before rest. — the rest condition for the command surface.

## Proof status legend

| Status | Meaning |
|--------|---------|
| **PROVEN** | A Chicago-TDD test exists and has **passed** in a verifying run; the command does real durable work. |
| **TEST-AUTHORED** | A proof test is written but the verifying run has not yet been captured green this session (build lane occupied). Promotes to PROVEN when `cargo make test` is green. |
| **PROOF-PENDING** | Requires an implementation fix before it can be proven (a known contract-drift gap). |
| **GATED** | Behind a default-off feature; proven within its own crate's tests when enabled. |
| **ARCHIVED** | Removed from the default release boundary (behind `experimental`, default-off). Code preserved as fossil evidence; advertisement ended. |
| **INTERNAL** | Not a user-facing noun (shared helpers / git hooks). |

## The matrix

| Noun | Boundary decision | Proof status | Backing evidence |
|------|-------------------|--------------|------------------|
| `init` | KEEP / PROVE | TEST-AUTHORED | core scaffolding; e2e coverage in `crates/ggen-cli/tests/` |
| `sync` | KEEP / PROVE | TEST-AUTHORED | the μ₁–μ₅ value path; sync sabotage tests (lockfile/receipt invariants) |
| `graph` | KEEP / PROVE | TEST-AUTHORED | `tests/proof_graph_test.rs` — 11 tests over real Oxigraph: `load`/`query`/`export`/`visualize`/`validate`, each asserting observable state (triple counts, written artifacts, real IRIs) |
| `pack` | KEEP / PROVE | **PROOF-PENDING** | `tests/proof_pack_test.rs` exists; `pack add` has a lockfile contract-drift gap (`install_pack` does not write `.ggen/packs.lock`) — must wire to `PackInstaller::update_lockfile` before promotion |
| `policy` | KEEP / PROVE | TEST-AUTHORED | `tests/proof_policy_doctor_utils_test.rs` |
| `doctor` | KEEP / PROVE | TEST-AUTHORED | `tests/proof_policy_doctor_utils_test.rs` |
| `utils` | KEEP / PROVE | TEST-AUTHORED | `tests/proof_policy_doctor_utils_test.rs` |
| `lsp` | KEEP (feature-gated) | GATED | `#[cfg(feature = "lsp")]`; proven by the `ggen-lsp` crate suite (103 tests) when enabled |
| `a2a` | **ARCHIVE** | ARCHIVED | ambiguous capability; gated behind `experimental` (default-off). A2A delivered as the `ggen-lsp-a2a` bridge library, not a CLI noun |
| `framework` | **ARCHIVE** | ARCHIVED | framework-bridge (LangChain etc.) not provable as finished in v26.5.28 |
| `mcp` | **ARCHIVE** | ARCHIVED | MCP delivered via `ggen lsp serve --protocol mcp` (under `lsp` feature) and the `ggen-lsp-mcp` binary, not this noun |
| `sigma` | **ARCHIVE** | ARCHIVED | ambiguous capability |
| `wizard` | **ARCHIVE** | ARCHIVED | ambiguous capability |
| `git_hooks` | — | INTERNAL | git hook installer; not a top-level noun |
| `helpers` | — | INTERNAL | shared command helpers |

## Promotion rule

A row moves to **PROVEN** only when its backing test has passed in a captured
`cargo make test` run (and, for any command touching an external service, the OTEL spans
required by `.claude/rules/otel-validation.md` are present). Until then it is honestly
TEST-AUTHORED. This matrix is updated, not narrated — the status reflects the last
verifying run, not intent.

## See also

- [Oracle Gaps](../../explanation/oracle-gaps.md) — why ARCHIVE is a lawful closure
- [Motion does not count](../../explanation/motion-does-not-count.md) — why TEST-AUTHORED ≠ PROVEN
- [v26.5.28 boundary](../release/v26-5-28-boundary.md) — the boundary these decisions define
