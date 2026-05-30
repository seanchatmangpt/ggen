<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Command-Proof Matrix (Reference)](#command-proof-matrix-reference)
  - [Proof status legend](#proof-status-legend)
  - [The matrix](#the-matrix)
  - [Promotion rule](#promotion-rule)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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
| `init` | KEEP / PROVE | **PROVEN** | `tests/proof_init_test.rs` — 4 tests; a behavioral GALL boundary-preparation pier: `init` scaffolds a real durable O* on disk, refuses to clobber an existing boundary without `--force`, `--force` re-scaffolds but preserves user files, and into-nonexistent-subdir creates the boundary there |
| `sync` | KEEP / PROVE | **PROVEN** | `tests/gall_sync_actuation.rs` — 5 tests; the GALL actuation triad (μ₁–μ₅ value path): refuses an incapable boundary (no phantom receipt/artifact), dry-run on a capable boundary previews without actuating, and a real sync on a capable boundary writes artifact + signed receipt |
| `graph` | KEEP / PROVE | **PROVEN** | `tests/proof_graph_test.rs` — 10 tests over real Oxigraph: `load`/`query`/`export`/`visualize`/`validate`, each asserting observable state (triple counts, written artifacts, real IRIs) |
| `pack` | KEEP / PROVE | **PROVEN** | `tests/proof_pack_test.rs` — 10 tests; `pack add` now writes a non-empty `.ggen/packs.lock` entry (lockfile invariant 4.1) with a `sha256-…` `integrity` digest (CRACK #57 fixed; `EXPECT_LOCKFILE = true`) and emits a signed receipt binding `pack:<id>@<version>:<digest>` in `input_hashes` |
| `policy` | KEEP / PROVE | **PROVEN** | `tests/proof_policy_doctor_utils_test.rs` — 7 policy tests: `policy list`/`show` resolve real profiles (unknown fails closed), `policy validate`/`check` run real enforcement against a real lockfile and fail-closed without one |
| `doctor` | KEEP / PROVE | **PROVEN** | `tests/proof_policy_doctor_utils_test.rs` — 2 doctor tests: `doctor check` passes with `ggen.toml` present, reports it missing otherwise |
| `utils` | KEEP / PROVE | **PROVEN** | `tests/proof_policy_doctor_utils_test.rs` — 3 utils tests: `utils env get`/`list`/`set` read/collect/parse the real environment |
| `lsp` | KEEP (feature-gated) | **PROVEN** (GATED) | `#[cfg(feature = "lsp")]`; binary built `--features lsp`; the noun + its MCP/A2A planes proven on the playground project — same E0011 + route across all three transports. See [delivery-plane-proof.md](delivery-plane-proof.md). Plus the `ggen-lsp` crate suite. |
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
