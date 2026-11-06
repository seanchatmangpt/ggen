# v26.11.3 Publish Runbook

Date: 2026-11-04 (refreshed 2026-11-04, see §5). Scope: ggen (PR #824),
cgap, ggen-marketplace. Sources: `docs/jira/v26.11.3/closure/` per repo,
landed receipts, live `git tag` / `Cargo.toml` reads this session. Never
rounded up. §5 (2026-11-04 refresh) supersedes any stale value in §1-§4
where they disagree.

## 1. Per-repo remaining blockers

### ggen — branch `v26-11-3-wave` @ 72b743d23 (branch tip; 87d04ccea is its verified ancestor), workspace version `26.11.3`

1. **PR #824 is OPEN, not merged.** `gh pr view 824` (2026-11-04
   refresh): state OPEN, mergeable MERGEABLE, mergeStateStatus UNSTABLE,
   head 72b743d23. Failing checks (13): kernel, Real-state gap closure,
   Manufacture/polyglot Chicago TDD/receipt/replay, Construct core-team
   example closure, Construct and verify repair candidate, Exact-head
   clean replay, TAI / 18 standards / 10 testing suites / 60 proof
   surfaces, migrate, Generate roadmap / close G0-G9 / execute scenarios
   / replay receipts, Sync/compile/execute/receipt/replay,
   exact-head-producer-and-receiver, `Deep / quality_deep`,
   `Deep / docs_deep`. Still IN_PROGRESS (3): Execute and persist receipt
   migration, `Deep / core_deep`, `Deep / integration_deep`. Must go
   green, or each failure carries a receipted, disclosed waiver — no
   silent skips.
2. ~~**Version-lattice laggard (PUB-6 B6 residue):**~~ **RESOLVED
   (2026-11-04 refresh):** `crates/ggen-engine/Cargo.toml:10` now reads
   `version.workspace = true  # CalVer YY.M.patch (workspace root,
   26.11.3)` — the literal 26.10.12 pin is gone; Phase 3 step 1 is a
   no-op.
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

### cgap — `main` @ 37721e2fa (local; f561b563 is its verified ancestor), workspace version `26.11.3` (`Cargo.toml:49`), latest tag `v26.11.2`

1. **Working tree still DIRTY at refresh: 23 entries (14 modified,
   9 untracked).** Must be committed + pushed by the coordinator before
   tagging (lane law: lanes never run git).
2. All 12 DoD gates (`dod-checklist.md`, generated from dod-pack) are
   MEASURED with receipts (dod-001..dod-012). **Re-run the full blocking
   ladder at the exact tag SHA** before publish.
3. ~~**Publish-surface hygiene**~~ **RESOLVED (2026-11-04 refresh):**
   `crates/cgap-mcp/Cargo.toml:9` and `crates/wire-shm/Cargo.toml:7` now
   carry explicit `publish = false`; the 19 `publish = true` crates are
   unchanged. No ambiguity remains for Phase 2.
4. **Licenses gate:** `cargo deny check licenses` is BLOCKING since
   YD-1 (2026-11-04), clarified via `LicenseRef-P28-SIL` in `deny.toml`
   (`.github/workflows/ci.yml:148,170,174-175`). Now part of the
   blocking ladder.
5. Mint `v26.11.3` (none exists; latest is v26.11.2).

### ggen-marketplace — `main` @ e7c58d5de (local, 1 commit ahead of origin/main d451e63e4; both verified commits), 1 dirty file, packs-only repo

1. **Push state at refresh:** origin/main = d451e63e4 ("repair ZD-12
   REFUSED packs"); local HEAD e7c58d5de ("ZD-26: lift 2 GGM-SRC-002
   drifts into aoq-pack source") is unpushed; working tree has 1 dirty
   file (`packs/industry-manufacturing-dflss/templates/
   shacl-manufacturing-shapes.ttl.tmpl`). Coordinator commits + pushes
   before tagging.
2. ~~Cut CHANGELOG's `[Unreleased]`~~ **DONE (2026-11-04 refresh):**
   `CHANGELOG.md` opens with `## [v26.11.3] — 2026-11-04` (aoq/cscb/
   tort/cgap-pack entries + baseline recompute + strata hardening);
   `[Unreleased]` section no longer exists.
3. ~~**313 baseline digests (295/9/8/1)**~~ **RE-PROJECTED (2026-11-04
   refresh):** `qualification/baseline.json` schema v1 now carries 347
   packs — counts ALIVE 327 / WARN 10 / REFUSED 9 / SKIPPED 1. Phase 1
   courts re-run against these counts.
4. Pack-side standing to re-verify at the tag SHA: aoq-pack canonical
   (9 gates + 18 witnesses + executable witness court), cgap-pack gates
   (19), tort-pack, cscb-pack, pin-gate marker self-satisfied by the
   in-tree ledger row from 7834fd045 (verified ancestor of HEAD).
5. Mint `v26.11.3` (no v26.11.x tags exist; latest is `v26.10.8-3`).

## 2. Tag-advance citation law

Law: **every commit SHA cited in the closure receipts must be reachable
(ancestor) from the minted release tag.** One tag per repo covers all
cited SHAs (ancestry verified this session). Total: 3 tags, all
`v26.11.3`.

| Repo | Mint | At |
|---|---|---|
| ggen | `v26.11.3` | the PR #824 **merge commit** (post-merge HEAD of `main`), not the branch tip |
| cgap | `v26.11.3` | post-commit HEAD of main (37721e2fa descendant) |
| ggen-marketplace | `v26.11.3` | current main head |

Negative findings: do not mint anything at or beneath ggen's `v26.15.2`
(10fcf3d6b, re-verified NOT an ancestor of the release branch at
refresh); arcus main 1c08fd1 ("YD-2 toolchain pin, vulncheck clean",
verified commit, 0 remotes — LOCAL-ONLY terminal, recorded, not
deferred); lsp-max resolved at c261f613 ("merge artifact-stripped master
onto origin/master lineage"), verified pushed — origin/master IS
c261f613 (outside this runbook's three-repo scope but cited by the seal
receipt).

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
`cargo publish --dry-run` per crate first. (cgap-mcp / wire-shm
ambiguity resolved: both now `publish = false` — see §1.)

**Phase 3 — ggen crates.io ladder.** Only after Phases 1-2:
1. Bump `crates/ggen-engine` pinned version 26.10.12 → 26.11.3 (receipt
   as PUB-6 B6 completion).
2. Ladder: `ggen-graph` 26.11.3 (required by engine's exact `=26.11.3`
   pin) → `ggen-abb-sbb`, `ggen-scip`, `ggen-config`,
   `ggen-marketplace` → `ggen-engine` 26.11.3. `cargo publish --dry-run`
   before each live publish; ggen-abb-sbb's `include` list is
   load-bearing (E0583 history).
1. ~~Bump `crates/ggen-engine` pinned version 26.10.12 → 26.11.3~~
   (resolved — engine now uses `version.workspace = true`, see §1).
2. Ladder: `ggen-graph` 26.11.3 (required by engine's exact `=26.11.3`

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

## 5. 2026-11-04 refresh (ZB-2)

ZB-2 found this runbook's SHAs stale (branch tip moved
87d04ccea → 72b743d23). Every value below is from a live command this
session (`git cat-file -t`, `merge-base --is-ancestor`, `gh pr view`,
`git status --porcelain`, `python3 json.load`); the runbook's own
falsifier law (§2) applied to each cited SHA — all exist, all
ancestry claims verified.

### ZB-2 per-repo readiness rows

| Repo | Readiness at refresh | Live evidence |
|---|---|---|
| ggen | **BLOCKERS-3 → recount**: tip 72b743d23; PR #824 OPEN/MERGEABLE/UNSTABLE, head 72b743d23, 13 failing + 3 pending checks; engine `version.workspace = true`; no `v26.11.3` tag; `v26.15.2` (10fcf3d6b) re-verified NOT an ancestor | `git rev-parse HEAD`; `gh pr view 824`; `grep version crates/ggen-engine/Cargo.toml`; `git tag -l` |
| cgap | **DIRTY**: main @ 37721e2fa (f561b563 verified ancestor), 23 dirty entries (14 M / 9 ??); cgap-mcp:9 + wire-shm:7 now explicit `publish = false`; licenses gate BLOCKING since YD-1; latest tag still v26.11.2 | `git status --porcelain \| wc -l`; grep on Cargo.tomls; `.github/workflows/ci.yml:148` |
| ggen-marketplace | **LOCAL-AHEAD + 1 dirty**: HEAD e7c58d5de, 1 commit ahead of origin/main d451e63e4; CHANGELOG `[v26.11.3]` cut; baseline re-projected 347 packs (ALIVE 327 / WARN 10 / REFUSED 9 / SKIPPED 1) | `git rev-parse origin/main`; head of CHANGELOG.md; `json.load(qualification/baseline.json)` |
| lsp-max | **RESOLVED**: c261f613 pushed — origin/master = c261f613 | `git rev-parse origin/master` in /Users/sac/lsp-max |
| arcus | **LOCAL-ONLY terminal**: 1c08fd1 ("YD-2 toolchain pin"), 0 remotes | `git remote -v \| wc -l` = 0 |

### Publish-hygiene items resolved since the original runbook

1. **ggen-engine version pin** — `crates/ggen-engine/Cargo.toml:10` is
   `version.workspace = true`; the 26.10.12 literal is gone. Phase 3
   step 1 (PUB-6 B6) is a no-op.
2. **cgap publish=false lines** — `crates/cgap-mcp/Cargo.toml:9` and
   `crates/wire-shm/Cargo.toml:7` now explicitly `publish = false`;
   19 `publish = true` crates unchanged.
3. **cgap licenses gate blocking** — `cargo deny check licenses` is
   BLOCKING since YD-1 (2026-11-04), clarified via `LicenseRef-P28-SIL`
   (`ci.yml:148,170,174-175`).
4. **Marketplace CHANGELOG cut** — `[v26.11.3] — 2026-11-04` section
   landed (aoq/cscb/tort/cgap-pack, baseline recompute, strata
   hardening); no `[Unreleased]` remains.
5. **Baseline re-projected** — `qualification/baseline.json` (schema
   v1) now 347 packs, counts ALIVE 327 / WARN 10 / REFUSED 9 /
   SKIPPED 1 (was 313 total, 295/9/8/1).

### Still open (unchanged class, re-verified at refresh)

- ggen PR #824: 13 failing + 3 IN_PROGRESS checks — merge gate unsatisfied.
- ggen tag state: no `v26.11.3`; `v26.15.2` off-lineage disclosure stands.
- ggen BLOCKED/carry residue (§1.4): unchanged, re-verified before tag
  required.
- cgap working tree dirty (23 entries) — coordinator commit+push before tag.
- ggen-marketplace: 1 unpushed commit (e7c58d5de) + 1 dirty file.
- cgap tag: `v26.11.3` unminted (latest v26.11.2); marketplace tag:
  `v26.11.3` unminted (latest `v26.10.8-3`).
- arcus 1c08fd1: local-only, terminal — recorded, not deferred.

### 2026-11-04 second as-of refresh (ZD-71, later same day)

Second refresh after the day's landings. Every SHA below re-verified this
session with `git cat-file -t` (all six: `commit`) and
`merge-base --is-ancestor` where lineage is claimed; PR state via `gh pr
view 824`; counts from live `git status --porcelain`, `git tag -l`,
`python3 json.load`. §1-§4 SHA rows above are stale as-of this
subsection and are superseded by the values here.

#### Live subject SHAs (as-of ZD-71)

| Repo | Tip / subject | Verification |
|---|---|---|
| ggen | `v26-11-3-wave` @ **58a3b4108** ("ZD-31 runbook as-of refresh..."; = PR #824 head, = origin tip) | `git cat-file -t` commit; `gh pr view 824` head 58a3b4108; `merge-base --is-ancestor HEAD origin/v26-11-3-wave` true |
| cgap | `main` @ **e3233191** ("v26.11.3 terminal sweep: W2-A/W2-C ... ZD-53 cgap-git court fixes, ZD-55 DoD command ruling, membrane close-out, index 70/70"); origin/main = e3233191 | cat-file commit; `merge-base --is-ancestor origin/main main` true |
| ggen-marketplace | `main` @ **b408361c1** ("ZD-49: gac:Registry individual ... baseline re-projected 327/9 -> 332/5"); origin/main = b408361c1 — the zd-49/zd-52 push has LANDED | cat-file commit; `merge-base --is-ancestor origin/main main` true |
| lsp-max | `master` @ **bd97e5c** ("ZD-54: max-protocol.ttl drift ... ZD-47 probe committed; gitignore finalverify scratch"); origin/master = bd97e5c — pushed | cat-file commit; `merge-base --is-ancestor origin/master master` true |
| arcus | `main` @ **fed6db3** ("W2-D: flight admission path ... 24 courts x2, goldens regen"); 0 remotes (`git remote -v` empty) — LOCAL-ONLY terminal, supersedes the 1c08fd1 row above | cat-file commit; no upstream to compare |
| claude-code-config-lsp | `main` @ **6a04124** ("ZD-25: receipts-dir index — 70 rows = 70 files, verified"); origin/main = 6a04124 | cat-file commit; `merge-base --is-ancestor origin/main main` true |

Negative findings re-verified at this refresh: no `v26.11.3` tag exists
in ggen, cgap, or ggen-marketplace (latest: v26.11.0 / v26.11.2 /
`v26.10.8-3`); ggen `v26.15.2` (10fcf3d6b, cat-file `commit`) re-verified
NOT an ancestor of `v26-11-3-wave`.

#### ZB-2 readiness rows (re-count at ZD-71)

| Repo | Readiness at this refresh |
|---|---|
| ggen | PR #824 OPEN / MERGEABLE / mergeStateStatus UNSTABLE, head 58a3b4108 — **14 failing checks, 0 pending** (failing list shifted vs ZB-2's 13+3: `Deep / integration_deep` and `observe-manufacture-falsify-replay` now in the fail set; `Deep / core_deep`, `Deep / quality_deep`, and the three IN_PROGRESS migrations have resolved or moved). Engine `version.workspace = true` (checked at ZB-2, no landing since touched it). Merge gate still unsatisfied. |
| cgap | main @ e3233191 — the 23-entry dirty tree LANDED in the terminal sweep; 2 untracked entries remain (`crates/cgap-ffi/src/bin/`, `crates/cgap-ffi/tests/aoq_sidecar_court.rs` — the sidecar lane's in-flight files, the ZD-43 dirty-worktree class for cgap-ffi). Latest tag still v26.11.2; `v26.11.3` unminted. |
| ggen-marketplace | main @ b408361c1 = origin/main (push done); 1 commit ahead resolved to 0 divergence (local == origin). Working tree 28 dirty entries (27 M/D + 1 ??): ZD-62/MKT-01 adoption edits to `scripts/qualify_packs.py` + re-projected baseline + zd-28/zd-62 receipts uncommitted. Baseline now **347 packs: ALIVE 326 / WARN 13 / REFUSED 7 / SKIPPED 1** (`json.load(qualification/baseline.json)` — counts moved again from ZB-2's 327/10/9/1 via the ZD-49 and ZD-62 re-projections). |

#### Hygiene resolved today (2026-11-04 landings, all witnessed by receipts on disk)

1. **lsp-max probe committed + pushed — bd97e5c**: the ZD-47
   `diagnostic_relay_probe` CARRIED-OPEN item is committed at the tip
   (commit message: "ZD-47 probe committed"), alongside the ZD-54
   max-protocol.ttl drift fix (2 aspirational `lspMax` individuals with
   zero Rust implementation removed; proof COUNT 3->5). origin/master =
   master. Resolves the lsp-max "RESOLVED at c261f613" row — the tip has
   advanced past it, ancestry re-verified.
2. **ZD-54 drift fix landed** (same commit): code authority =
   andon/lsp.rs consts, expectations transcribe ttl.
3. **cgap-git court fixes (ZD-53) landed** in cgap e3233191's terminal
   sweep, with ZD-55 DoD command ruling — cgap's
   `docs/jira/v26.11.3/dod-checklist.md` now shows **12/12 gates
   MEASURED** (grep: 12 MEASURED/VERIFIED rows).
4. **Wave-2 membrane LANDED**: `/Users/sac/cgap/docs/jira/v26.11.3/plans/
   wave-2-membrane-plan.md` flipped PLAN (UNVERIFIED) -> LANDED per
   zd-63-membrane-closeout.md (W2-A/W2-B/W2-C/W2-D all ALIVE per-lane).
5. **crates.io dry-run ladder 7/20 with typed, self-healing classes**
   (cgap zd-43-dryrun-ladder.md @ 37721e2fa): 7 exit 0 (all of L0:
   cgap-arrow/blake3/bosym/branchless/aoq/git/parquet); 11 fail
   registry-dep-missing on unpublished ^26.11.3 siblings — **self-healing
   by construction** (Stage 3 publishes L0 first, dependents resolve); 2
   BLOCKED dirty-worktree (cgap-audit file since committed; cgap-ffi
   still uncommitted, see the readiness row). The predicted
   license-file Stage-1 blocker never fired — packaging metadata passed
   on all 20. Census discrepancy recorded for Stage-3 adjudication:
   22 manifests `publish = true` vs the task's 20 (integration,
   wasm-footprint).
6. **Marketplace baseline hardened to 332+/5-class counts**: the
   zd-49/zd-52 push landed (origin/main = b408361c1, "baseline
   re-projected 327/9 -> 332/5"); live re-read now 326/13/7/1 after the
   uncommitted ZD-62/MKT-01 re-projection. MKT-01 design is **settled**:
   ambient ggen 26.11.3 as the qualification court identity with the
   disclosed `--skip-ea-admission` opt-out on both `sync run`
   invocations in `scripts/qualify_packs.py`
   (`zd-62-mkt01-adoption.md`), `ggen_version` field updated to
   `ggen 26.11.3`.
7. **DoD 12/12** (see item 3) — cgap's publish bar is fully measured;
   full ladder re-run still required at the exact tag SHA.

#### Still open (re-verified at ZD-71)

- **ggen PR #824**: 14 failing checks (0 pending) — merge gate
  unsatisfied. **ZD-66..70 in flight**: zd-66 (doc-clippy + EOF sweep)
  and zd-69 (ENOSPC x3 + Lean verify manifest) receipts are on disk at
  58a3b4108+; zd-67/68/70 lanes not yet receipted. Fails must go green or
  carry receipted, disclosed waivers — no silent skips.
- **cgap crates.io Stage 3**: not started. Entry conditions from zd-43:
  owning lanes commit the cgap-ffi in-flight files (still untracked),
  adjudicate the 22-vs-20 publish census, then run the real ladder in
  topo order with bounded polls. §5.1/§5.2 falsifiers re-test at
  Stage 3.
- **ggen Phase 3 ladder**: not started (blocked on Phases 1-2 and PR
  #824 green). Step 1 remains a no-op (workspace version).
- **Tag mints**: all three `v26.11.3` tags unminted (ggen latest
  v26.11.0 on-lineage; cgap v26.11.2; marketplace `v26.10.8-3`);
  ggen tag must go on the PR #824 merge commit, cgap on an e3233191
  descendant, marketplace on a b408361c1 descendant (after the ZD-62
  dirty tree lands).
- **cgap working tree**: 2 untracked cgap-ffi entries — coordinator
  commit before tag.
- **ggen-marketplace working tree**: 28 dirty entries (ZD-62/MKT-01 +
  zd-28/zd-62 receipts) — coordinator commit + push before tag.
- **arcus fed6db3**: local-only (0 remotes), terminal — recorded, not
  deferred (supersedes the 1c08fd1 row).

## 6. Merge mechanics for PR #824 (green-or-waived gate)

Source: ZP-7 plan + ZB-18 preflight facts, verified live 2026-11-04.

**Branch protection reality**: `main` carries NO branch protection
(`GET /branches/main/protection` → 404). No required checks, no required
reviews — the green-or-waived gate is procedural, ours to enforce; GitHub
will accept a red merge.

**Merge method — merge commit (`gh pr merge 824 --merge`)**:
- All three methods enabled; `delete_branch_on_merge: false`; no CODEOWNERS.
- Repo history is merge-commit convention (#820/#819/#808/#807 all merge
  commits; squash used only for release-bump commits).
- Decisive: §2 requires the merge COMMIT as tag anchor. Squash would mint
  a new SHA severing the wave's replayed commits from main's lineage;
  rebase leaves no merge commit. Merge commit satisfies the anchor law.
- No `--auto` (allow_auto_merge false; the gate must be checked, not
  deferred).

**Merge sequence (post green-or-waived)**:
1. Gate: every rollup check SUCCESS, or a waiver row in
   `closure/waivers.md` (schema below). Zero unwaivered red.
2. Record pre-merge branch head (`gh pr view 824 --json headRefOid`).
3. `gh pr merge 824 --merge` (no --delete; branch tip is the replay
   anchor until tags+receipts confirm).
4. `git fetch origin && git checkout main && git merge --ff-only
   origin/main`.
5. Read the merge SHA off disk (`git log -1 --format=%H origin/main`);
   verify two-parent (`git cat-file -p <sha>`).
6. Tag: `git tag -a v26.11.3 <merge-sha>` then `git push origin
   v26.11.3 main`. cgap + marketplace tags likewise at their mains.
7. Re-run the seal falsifier sweep at the merge SHA; exact-head claims
   re-witnessed at the anchor.
8. Optional post-step-7: `git push origin --delete v26-11-3-wave`.

**Waiver schema (one row per red check at merge time)**:
`{waiver: W-<n>, check, workflow, run_url, root_cause: env-missing |
flake | pre-existing-on-main | out-of-scope, not_blocking_because,
observed_on_main_too (must cite the matching red run on origin/main if
true), recorded_by, date}`. flake waivers cite the passing re-run;
anything else is fix-forward.
