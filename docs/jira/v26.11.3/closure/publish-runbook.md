# v26.11.3 Publish Runbook

Date: 2026-11-04. Scope: ggen (PR #824), cgap, ggen-marketplace. Sources:
`docs/jira/v26.11.3/closure/` per repo, landed receipts, live `git tag` /
`Cargo.toml` reads this session. Never rounded up.

## 1. Per-repo remaining blockers

### ggen — branch `v26-11-3-wave` @ 87d04ccea (pushed), workspace version `26.11.3`

1. **PR #824 is OPEN, not merged.** `gh pr view 824` (2026-11-04 08:53Z):
   MERGEABLE, but failing checks include kernel (ABB/SBB Manufacture
   Kernel), Cyberpunk TV Platform, Examples repair constructor, Fortune 5
   BBlock + Normalize, GALL-001 Replay Court, GBB Certification, Live
   examples, Receipt chain migration, TAI Enterprise, TCPS Generated
   Validation, ggen Root Dogfood, and CI `Deep/quality_deep` +
   `Deep/docs_deep`; several `Deep/*` jobs still IN_PROGRESS. Must go
   green, or each failure carries a receipted, disclosed waiver — no
   silent skips.
2. **Version-lattice laggard (PUB-6 B6 residue):**
   `crates/ggen-engine/Cargo.toml` pins `version = "26.10.12"` while the
   workspace is 26.11.3. ggen-engine is the only crate with explicit
   `publish = true`; it must be bumped to 26.11.3 before any crates.io
   publish. (XD-10 in flight.)
3. **Publish-chain version reqs in ggen-engine's manifest must resolve on
   crates.io:** `ggen-abb-sbb ^26.10.11`, `ggen-scip ^26.10.12`,
   **`ggen-graph =26.11.3` (exact pin — ggen-graph must publish at
   26.11.3 first)**, `ggen-marketplace >=26.10.10,<26.15`,
   `ggen-config >=26.10.10,<26.15`. ggen-graph has a bare path dep on
   ggen-engine handled by the include/exclude mechanism documented at
   `crates/ggen-graph/Cargo.toml:37-50`; ggen-abb-sbb's `include` list is
   load-bearing for publish (E0583 history).
4. **BLOCKED/carry residue (seal receipt + descope-ledger v2, unchanged
   or narrowed):** 4 pre-existing sync_surface FM-LAW-015 failures
   (XD-11 in flight); DS-7 mutation battery 4 registered-vacuous
   criteria; F2 ~100 declarative-fixture refusals; R80-11 receipt
   standing UNKNOWN; R80-12 `lib.rs` wiring one-liner; R80-6 SCIP/
   write-path wiring; `--skip-ea-admission` unwired; `sync.rs:431`
   hardcode; `diagnostic_relay_probe` CARRIED-OPEN. Named, cited
   residue — each re-verified before tag.
5. **Tag state:** no `v26.11.3` tag. On-lineage tags end `v26.11.0`;
   `v26.12.0`-`v26.15.1` are ancestors of HEAD, and **`v26.15.2`
   (10fcf3d6b, "Homebrew release") is on a divergent line, NOT an
   ancestor of the release branch — disclose, never cite or mint below
   it.**

### cgap — `main` @ f561b563 (pushed), workspace version `26.11.3`, latest tag `v26.11.2`

1. **Working-tree state from XD-1/5/6/GD-1 lanes and earlier** must be
   committed + pushed by the coordinator before tagging (lane law: lanes
   never run git).
2. All 12 DoD gates (`dod-checklist.md`, generated from dod-pack) are
   MEASURED with receipts (dod-001..dod-012). **Re-run the full blocking
   ladder at the exact tag SHA** before publish.
3. **Publish-surface hygiene:** 19 crates carry `publish = true`;
   **`cgap-mcp` and `wire-shm` have no explicit publish line** (cargo
   default = true) — fold into the ladder or set `publish = false`
   deliberately first.
4. Mint `v26.11.3` (none exists; latest is v26.11.2).

### ggen-marketplace — `main` @ pushed (9ba2496b4), clean, packs-only repo

1. Mint `v26.11.3` (no v26.11.x tags exist; latest is `v26.10.8-3`). Cut
   CHANGELOG's `[Unreleased]` (strata hardening wave) into `[v26.11.3]`.
2. Pack-side standing to re-verify at the tag SHA: aoq-pack canonical
   (9 gates + 18 witnesses + executable witness court), cgap-pack gates
   (19), tort-pack, cscb-pack, 344-pack census + 6 fixes, 313 baseline
   digests (counts refreshed by XD-7: 295/9/8/1), pin-gate marker
   self-satisfied by the in-tree ledger row from 7834fd045.

## 2. Tag-advance citation law

Law: **every commit SHA cited in the closure receipts must be reachable
(ancestor) from the minted release tag.** One tag per repo covers all
cited SHAs (ancestry verified this session). Total: 3 tags, all
`v26.11.3`.

| Repo | Mint | At |
|---|---|---|
| ggen | `v26.11.3` | the PR #824 **merge commit** (post-merge HEAD of `main`), not the branch tip |
| cgap | `v26.11.3` | post-commit HEAD of main (f561b563 descendant) |
| ggen-marketplace | `v26.11.3` | current main head |

Negative findings: do not mint anything at or beneath ggen's `v26.15.2`
Homebrew line (not on the release lineage); arcus main 3a6cffc is
LOCAL-ONLY terminal (no remote — recorded, not deferred); lsp-max
c977c1b6 pushed 2026-11-04 (resolved-with-root-cause, outside this
runbook's three-repo scope but cited by the seal receipt).

Falsifier per the seal receipt: a commit SHA recorded in the runbook
that does not exist in its repo voids the runbook. Re-verify each SHA
with `git cat-file -t` at execution time.

## 3. Publish order

**Phase 0 — land the SHAs (coordinator-only):**
1. Commit + push cgap working tree (lane landings).
2. Merge PR #824 (ggen) after check remediation/waivers.
3. Mint tags: ggen-marketplace, then cgap, then ggen (each after its
   landing).

**Phase 1 — ggen-marketplace packs BEFORE ggen.** ggen's sync/pack
resolution consumes marketplace packs via `ggen.lock`/baseline digests;
cgap's dod-011/012 (sync ×2 `written:[]`) and FM-LAW-015-zero gates run
against them. Re-run per-pack `verify.py` courts + pin-freshness gate at
`v26.11.3`, confirm the baseline digests and aoq-pack 9-gate court, then
cut the tag.

**Phase 2 — cgap crates.io ladder.** Re-run dod-001..dod-012 at the tag
SHA. Publish the 19 `publish = true` crates in dependency order,
`cargo publish --dry-run` per crate first. Resolve cgap-mcp / wire-shm
publish-line ambiguity first.

**Phase 3 — ggen crates.io ladder.** Only after Phases 1-2:
1. Bump `crates/ggen-engine` pinned version 26.10.12 → 26.11.3 (receipt
   as PUB-6 B6 completion).
2. Ladder: `ggen-graph` 26.11.3 (required by engine's exact `=26.11.3`
   pin) → `ggen-abb-sbb`, `ggen-scip`, `ggen-config`,
   `ggen-marketplace` → `ggen-engine` 26.11.3. `cargo publish --dry-run`
   before each live publish; ggen-abb-sbb's `include` list is
   load-bearing (E0583 history).
3. Root `ggen` package is not a publish target (ggen-cli-lib is
   `publish = false` by design).

Phases 0-3 are strictly ordered. Failure at any phase = Andon
stop-the-line per ggen rules.

## 4. CI/CD gate requirements

- **ggen:** PR #824 status rollup green or receipted-waived (§1.1 list).
  The deep CI matrix (`Deep/quality_deep`, `Deep/docs_deep`,
  `Deep/lsp_deep`) must complete — not skipped.
- **cgap:** all 12 dod gates MEASURED at the tag SHA; dod-011/012
  (`ggen sync run` ×2, `written: []`, zero FM-LAW-015) are the
  cross-repo gates binding Phases 1-3 together.
- **Seal falsifiers (apply to this runbook):** exit-gate re-run fails; a
  cited receipt's standing word disagrees with its body; a cited SHA
  does not exist; receipt count disagrees with `ls | wc -l` of its dir.
  Any one voids the seal.

## Critical files

- /Users/sac/ggen/docs/jira/v26.11.3/closure/v26.11.3-closure.md (seal
  receipt — blocker/falsifier authority)
- /Users/sac/ggen/docs/jira/v26.11.3/closure/milestone-final-sweep.md
- /Users/sac/ggen/crates/ggen-engine/Cargo.toml (version pin + publish
  chain)
- /Users/sac/cgap/docs/jira/v26.11.3/dod-checklist.md (12-gate publish
  bar)
