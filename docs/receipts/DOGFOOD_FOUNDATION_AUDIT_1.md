# Dogfood Foundation Audit #1

> "Eating our own dog" — turning ggen's own inspection discipline on ggen itself.
> Loop `9f9f2e72` (every 30 min), first iteration. READ-ONLY audits by 3 agents,
> all findings spot-verified before recording (anti-fabrication).

## SETTLEMENT-CHECK-1: HOLDS

`cargo check --workspace --all-targets` (default) exits 0 after two cracks resolved
(262c3cf8): the broken `examples/7-agent-validation` removed from `members` (an example
should never have been a member; `members` is now exactly the 15 crates), and the
`cmds::wizard` reference in the OTEL test gated behind `experimental`. The 17-commit
foundation pour carries the existing house.

## Verified cracks (recorded as backlog #55–#58)

| # | Crack | Severity | Evidence (spot-verified) |
|---|-------|----------|--------------------------|
| #55 | A2A `ggen_construct.rs` fake-success stub + hardcoded dummy receipt | HIGH (Oracle Gap) | line 247 `// simulate success`; `rcpt-simulated-1234567890.json`; `status:"success"`. Live `pub mod`. Distinct from the MCP construct gap already closed (3a7f1f7e). |
| #56 | `packs_receipt::generate_pack_install_receipt` under-binds closure + fail-open | HIGH | hashes only `pack_id`+`status`; emits unconditionally. LIVE path: `receipt_manager.rs:382,405`. |
| #57 | `pack add` does not write `.ggen/packs.lock` | HIGH (highest-value fixable) | `install_pack` (install.rs) never writes the lock; `proof_pack_test.rs:228` flags `EXPECT_LOCKFILE=false`. |
| #58 | `init` lacks a behavioral Gall pier | MEDIUM | only `project init` integration coverage; no proof_* for the top-level noun. |

## Command-foundation coverage (Agent 3)

PROVEN: `sync`, `graph`, `policy`, `doctor`, `utils`, `lsp` (gated). PROOF-PENDING: `pack`
(install/list/show/remove proven; `add` lockfile drift = #57). WEAK: `init` (#58).

## Agent over-flag (corrected)

Agent 1 reported 7 more "default-build hazards" (`cmds::market`/`cmds::project` refs). The
settlement re-check exiting 0 is the oracle: those refs live in `tests/integration/` and
`tests/cli/` subdirs **not compiled into the passing target set**, so they are not active
default-build hazards. Agent found the text but did not confirm compilation. Not acted on
(but worth a later cleanup — dangling refs to non-existent `cmds::market`/`cmds::project`).

## Receipt-closure honesty (Agent 2)

Gold standard: `sync.rs:emit_sync_receipt` (full O* closure + dry-run/refusal gating).
Honest: `pack/mod.rs:emit_pack_receipt`, `intel/mine.rs:emit_promotion_receipt` (pre→post
transition bound, success-only). Conditional: `pipeline.rs:μ₅` (closure via epoch
indirection). Defective: #55, #56.

## Next loop iterations

Fix in priority order: #57 (localized, test-flagged) → #56 (live receipt path) → #55
(Oracle Gap) → #58 (init pier). Then PUBLIC-ONTOLOGY-FOUNDATION-1 (no-`gall:` rewrite),
validated through `ggen sync`.
