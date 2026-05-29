# Foundation Audit — Master Synthesis (ggen v26.5.28 @ cf7a6004)

Read-only audit, 5 parallel auditors, no fabrication (every finding cites file:line or a real test/build run). Dimensions: rest-gate test state, Oracle Gaps, receipts/contract-drift, docs-vs-reality, boundary/dead-code.

## Verdict

| Layer | State |
|-------|-------|
| **Production code** | **Strong.** No HIGH live-path Oracle Gap. The session's fixes all hold. |
| **Receipts** | **Mostly honest.** 7 fixed paths verified; remaining drift is latent/dead or low. |
| **Boundary** | **Clean** (archived nouns verified absent by build), minus one dormant-crate leak. |
| **Tests** | **Lib green; doctests catastrophically broken** (~437, systemic `crate::`). |
| **Docs** | **Worst area.** ~528 broken links (41%), removed commands documented as live, phantom crate, stale versions, split-brain Diátaxis. |

The machine that BUILDS is in good shape. The machine the DOCS describe is not.

## 1. Rest-gate test state (per the ggen-core agent's quantification)

- ggen-core **lib: 1491 pass / 0 fail**. Whole-workspace `cargo check --all-targets`: green.
- Other crate lib/doctests: cli-lib (90 lib, 4 doc), config (71 lib, 10 doc), graph (14 lib + integration), marketplace (223 lib, 1 doc), genesis/cpmp/stpnt (all green, no doc fences) — **all green**.
- **ggen-core doctests: ~437 FAILING** — systemic: edition-2021 doctests use `crate::` paths, which don't resolve in rustdoc (need `ggen_core::`). This is THE biggest gap to a green `cargo make test`.
- **~24 mcp-template-render integration tests** diverged from the μ₅ template refactor (templates gutted to iterate `sparql_results`; tests still assert rich param output).
- **~7 tests** need a missing workspace-root `specify/` data dir (only `.specify/` exists).

## 2. Oracle Gaps — 5 session fixes CONFIRMED CLOSED; remaining:

**Live-path (MED — fix soon):**
- `crates/ggen-cli/src/cmds/wizard.rs:448-458` — prints "Running initial sync… Initial sync completed" and returns `status:"success"` **without running sync**. (wizard is archived/experimental, so off the default surface — but still a lie when enabled.)
- `crates/ggen-core/src/lifecycle/state.rs:210-228` `get_available_space` → `Ok(u64::MAX)` on all platforms, **defeating its own disk-space guard** (live lifecycle write path).

**Dead/library-only (MED — deceptive, become HIGH if wired; gate/mark/remove):**
- `ontology/delta_proposer.rs:329` `RealLLMProposer` — named "Real", body returns mock proposals, never calls the LLM. Name is a trap.
- `parts_foundry/part_signer.rs:34-76` — fake signature (`sig_<hash>`) + fail-open verify (any non-empty string passes).
- `stpnt/github.rs` + `crates/stpnt/src/membrane/github.rs` — fabricated GitHub issue URLs/ids, no API call.
- `marketplace/v3.rs:343` `V3OptimizedRegistry::batch_insert` — claims all-or-nothing txn, no-ops, returns empty Ok.
- `crates/ggen-cli/src/pack_install.rs` — entire mock `PackInstaller` (fake sig-verify, mock download); dead `pub mod`, zero callers (real path is ggen-core).
- `manufacturing/gates.rs` — proof gates hardcoded `passed:true` (dead; sync builds `gates: Vec::new()`).

## 3. Receipts / contract drift — 20 paths audited; 7 session fixes hold. Remaining:

- **MED** `crates/stpnt/src/proof/receipt.rs:34-40` StewardshipReceipt — `route_hash`/`obligation_hash` = zeros, `signature` = empty. Latent (never persisted/signed), but violates the receipt invariant by construction.
- **MED** `RepairReceipt.gate_pass` hardcoded `true` at 2 live LSP sites (`ggen-lsp/src/pack/mod.rs:320`, `intel/mine.rs:344`) — a failed gate would still mint `gate_pass:true`.
- **LOW** dead/under-binding: `receipt_manager.rs generate_composition_receipt` (hashes id-string, dead), `ReceiptGenerationPass` (dead, superseded by pipeline μ₅), `--queries` low-level sync path under-binds, lockfile non-empty-digest is caller- not type-enforced + non-atomic `fs::write`, LSP receipt persistence swallows write errors (`let _ =`) yet returns a sig, external-pack content not verified (identity digest only).

## 4. Docs-vs-reality — the heaviest backlog

- **528 broken markdown links (~41%)**; the two front doors are worst: `docs/README.md` (59), `docs/INDEX.md` (38). 25 malformed `file:///Users/sac/...` machine-local links.
- **Removed commands documented as live:** `ggen template *` in 14 docs; `wizard` in README + QUICK_REFERENCE; `validate`/`inspect`/`add`/`publish`/`info` in `reference/01-commands.md`; a2a/mcp reference pages.
- **Phantom crate `ggen-ai`** referenced in 37 docs (doesn't exist); `.claude/rules/README.md:88` says "30 crates" (real 15); `crates.md` still lists archived `7-agent-validation` as a member; phantom `crates/ggen-core/src/v26.5.19/` paths in GATE reports.
- **Stale versions:** 55 docs with `26.5.19`, 4 with `26.5.21`, `v0.2.0` refs.
- **Two Diátaxis trees** (`docs/diataxis/*` vs top-level `docs/*`) — split-brain; `docs/diataxis/how-to/template-rendering-rdf.md` teaches the removed `template` command the spine declares fiction.
- **CLEAN:** `docs/reference/cli/command-proof-matrix.md` — every PROVEN row's backing test verified to exist with the cited count. It is the model the rest should follow.

## 5. Boundary / dead-code

- **PASS** (verified by building both binaries): archived nouns (a2a/framework/mcp/sigma/wizard) absent from the default binary, present with `--features experimental`. lsp gated. Feature flags correctly wired. Dead market/project refs are inert (`#![cfg(any())]`).
- **MED — genesis-lockchain boundary leak:** `crates/stpnt/Cargo.toml:10` declares `genesis-lockchain` as a path dep but `stpnt/src/` never uses it → it's pulled into the build graph, contradicting CLAUDE.md/architecture.md which document it as excluded/non-compiled. Unused dep contaminating the boundary.
- **MED (latent) — `pub mod pack_install`** mock installer next to the real path (see §2).

## Prioritized fix backlog (each a BOUNDED task — not an unbounded agent)

1. **[HIGH-value, mechanical] ggen-core doctests `crate::` → `ggen_core::`** (~437) — the single biggest step to a green `cargo make test`. Scriptable.
2. **[MED, live] wizard "sync completed" lie** + **lifecycle `get_available_space` u64::MAX** — two real live-path gaps.
3. **[MED] gate/mark/remove the dead deceptive fakes** — `RealLLMProposer` (rename/gate), `PartSigner` (gate `#[cfg(experimental)]` or implement real Ed25519), github membranes, `V3 batch_insert`, `pack_install.rs` mock.
4. **[MED] genesis-lockchain** — drop the unused `stpnt` dep (restore boundary) or update the docs.
5. **[MED] receipts** — thread real `gate_pass` into the 2 LSP receipt sites; finish stpnt StewardshipReceipt or mark WIP.
6. **[HIGH-volume docs] DOCS-REST-1 completion** — fix the 528 broken links (start with README.md/INDEX.md), purge removed commands from live docs, collapse the two Diátaxis trees, sweep versions → 26.5.28, remove `ggen-ai` phantom. The command-proof matrix is the truthful template.
7. **[MED] mcp-template + specify/ tests** — reconcile templates with tests; provide or stub the `specify/` data.

## Bottom line

Production + receipts + boundary are release-grade (modulo the listed MED items, mostly dead-code hygiene). The two genuine deficits are **(a) the ggen-core doctest catastrophe** (mechanical, large) and **(b) the docs estate** (broken links + stale command/version claims). Neither is a correctness bug in the shipping binary; both block "Sabbath-grade done." Both are best tackled as bounded, dedicated efforts.
