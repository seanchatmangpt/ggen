# Residual Branches — ggen v26.10.8 (Lane R16 closure)

Typed branch-closure state, 2026-10-09. Gate: each branch either 0/0 vs
`origin` or a receipted PARKED with reason. No force-push, no rebase, no
`--no-verify` was used.

## lane/cdt-revocation — PUSHED (closed)

- Was local-only at `a82fd95ea` ("spec: CDT revocation for the receipt
  fabric"), contained in no other branch.
- Content: 1 commit, docs-only (`docs/specs/cdt-revocation/README.md` +
  `test-vector.rs`, 521 insertions, 0 deletions). Coherent, no code.
- `git push -u origin lane/cdt-revocation` — exit 0.
- Now 0/0 vs origin.

## main — PARKED (BLOCKED by pre-push gate)

- State: 38/0 ahead of `origin/main` (`d593a7f30` → tip `fcfd6349d`), lawful
  descendant (merge-base = `origin/main`), no force needed. Receipt-clean:
  the single `wip` commit `2ccaceb29` (self-declared "unverified in-flight")
  is completed by the later commit `339ba4e9b` ("complete consumer_mode
  field...") — the wip+fix pair is the receipt.
- Two push attempts, both refused by the installed pre-push hook
  (`.git/hooks/pre-push`: `just check` / `just lint` / `just fmt-check` /
  `just test-lib`):
  1. Attempt 1: `just check` exceeded the hook's 300s timeout on a cold
     build (transient — `just check` passes warm in 3.35s, exit 0).
  2. Attempt 2: real `just lint` (clippy `-D warnings`) failures —
     `crates/ggen-engine/src/graph.rs:483` `Some('?') | Some('$')`
     (`clippy::unnested_or_patterns`) and `:461` (`collapsible_match`),
     present identically in main's own content; plus clippy errors in
     `crates/praxis-graphlaw/tests/arw1_wire_spec_test.rs` /
     `ggen_law_pack_audit.rs` that exist only on the checked-out
     spec-integration tree, not in main.
- Typed reason: `BLOCKED[HOOK_VALIDATES_CHECKED_OUT_TREE]` — the hook gates
  the working tree (`spec-integration` = main+15, another lane's in-flight
  state), not the pushed ref's content. Closing it requires either (a)
  fixing spec-integration's clippy debt (not lane R16's files) or (b)
  `--no-verify` (refused: bypassing a gate is a force-class transition). A
  checkout-global branch switch on the shared canonical checkout is
  likewise refused by same-checkout fan-out law.
- Push commands + exits: `git push origin main` x2, exits 1 and 1. Origin
  `main` unchanged at `d593a7f30`.

## feat/v26.10.5-release-cut — PUSHED (closed)

- Was 1 commit ahead of origin (`905d8af33` → `2ccaceb29`, "wip(os-13):
  consumer-mode fixtureOnly emission filter (WP-5 consumer half)"). The tip
  is fully contained in main (ancestor of `fcfd6349d`), so the push carries
  no content not already covered by main's eventual landing.
- `git push origin feat/v26.10.5-release-cut` — exit 0; origin moved
  `905d8af33..2ccaceb29`. Now 0/0 vs origin.
- The tip is a self-declared wip whose completion (`339ba4e9b`) lives on
  main; no separate PARKED needed — branch is 0/0 and its content is
  superseded on main.

## Receipt summary

| branch | from → to | command | exit | standing |
|---|---|---|---|---|
| lane/cdt-revocation | local-only → `a82fd95ea` | `git push -u origin lane/cdt-revocation` | 0 | PUSHED, 0/0 |
| main | `d593a7f30` → `fcfd6349d` (38 commits, fast-forward, NOT pushed) | `git push origin main` (x2) | 1, 1 | PARKED: `BLOCKED[HOOK_VALIDATES_CHECKED_OUT_TREE]` |
| feat/v26.10.5-release-cut | `905d8af33` → `2ccaceb29` | `git push origin feat/v26.10.5-release-cut` | 0 | PUSHED, 0/0 |

Fix-forward path for main (one lane, ~3 lines): nest the or-pattern at
`crates/ggen-engine/src/graph.rs:483` (`Some('?' | '$')`), collapse the
`collapsible_match` at `:461`, and land spec-integration or fix its
praxis-graphlaw test clippy debt so the hook's `just lint` passes on the
checked-out tree; then re-push main (still a fast-forward).

## main — R19 re-type (2026-10-09, lane R19)

`BLOCKED[HOOK_VALIDATES_CHECKED_OUT_TREE]` is CONFIRMED but the fix-forward
path above is UNDERSTATED. Findings, from running the hook's exact lint gate
(`timeout 300s cargo clippy --workspace --all-targets --keep-going -- -D
warnings -A unexpected_cfgs`) against main's exact bytes via
`git archive main` into a scratch dir (no worktree, no branch switch):

1. **Main's own lib debt is 6 lints, not 2.** graph.rs:461
   (`collapsible_match`), :483 (`unnested_or_patterns`), :515
   (`map_unwrap_or`); sync.rs:673 (`uninlined_format_args`), :1571
   (`default_trait_access`), :1577 (`doc_markdown`). All six were fixed in
   the working tree (clippy --fix on `-p ggen-engine` + one hand edit), the
   lint gate then passed for the ggen-engine lib, and the fixes were
   reverted per compile-freeze-SLA disclosure rules. Receipted diff:
   `docs/sjira/v26.10.8/r19-main-clippy-unblock.patch` (applies cleanly to
   the current tree; `git apply --check` exit 0). Owner lane lands it.
2. **Main cannot pass its own hook even in isolation.** With the 6 lib
   fixes applied to main's bytes, the same lint gate still fails with 25
   errors in 4 main-content test files (identical to main on disk):
   `crates/ggen-engine/tests/sparql_refusals_e2e.rs` (15: expect_used/
   expect_err_used, needless raw-string hashes, uninlined_format_args),
   `generation_rules_e2e.rs` (2: unused variable, doc_markdown),
   `composed_packs_e2e.rs` (1: too-many-lines 109/100),
   `consumer_mode_fixture_only_e2e.rs` (1: doc_markdown). These lints only
   surface once the lib compiles, which is why R16's sweep stopped at 2.
3. **spec-integration debt on the shared tree** (not fixed, not lane R19's):
   `crates/praxis-graphlaw/tests/arw1_wire_spec_test.rs:33` (empty line
   after doc comment), `ggen_law_pack_audit.rs:81` (needless_borrows),
   `crates/ggen-engine/tests/abb_sbb_datalog_admission_e2e.rs` (2, new file
   not on main).
4. **Working tree returned to pre-lane state.** `git status` = only
   `?? docs/sjira/`; `git diff` empty; graph.rs/sync.rs verified
   byte-identical to HEAD via `git show HEAD:` diff (exit 0).

Re-typed standing:
`BLOCKED[HOOK_VALIDATES_CHECKED_OUT_TREE: main-self-debt=6-lib+25-test-lints;
spec-integration-debt=3-files]`. No push attempted (hook would refuse).
Push of main remains a lawful fast-forward d593a7f30 → fcfd6349d once the
shared tree's lint gate passes; unblock order: (a) land
`r19-main-clippy-unblock.patch` on main, (b) fix the 4 main test files'
lint debt on main, (c) land/fix spec-integration's 3 files, (d) re-push.

Commands + exits (this lane): `just check` exit 0 (warm, 2.93s); `just
lint` exit 101 pre-fix; `cargo clippy -p ggen-engine --fix` applied 5 of 6
lints (default_trait_access applied by hand); `just lint` post-fix: lib
clean, 3 spec-integration files + 4 main test files failing; scratch
main-content lint runs as above; `git checkout -- graph.rs sync.rs` +
byte-identical restoration proof exit 0.


## main — R35 hook fix + patch landing (2026-10-09, lane R35)

Closes R19 unblock order (a); (b) measured and re-typed as operator policy;
(c)/(d) no longer gate main's push (hook now validates the ref).

1. **Hook root-caused and fixed** (`scripts/hooks/pre-push.sh`, symlinked as
   `.git/hooks/pre-push`): the hook ran `just check/lint/fmt-check/test-lib`
   in the shared working tree, so main was gated on the checkout's branch
   content. Rewritten to `git archive <local_sha>` the pushed ref into a
   scratch dir and run the same four gates there — identical clippy flags,
   only WHAT is validated changed. Cargo target cache is per-sha under
   `~/.cache/ggen-pre-push/<sha12>`, so the shared tree's `target/` and
   working files are untouched. Committed on spec-integration:
   `b58f7cb9a`.
2. **R19 lib patch landed on main via plumbing** (no branch switch): tree
   `a6eb4f6a5d76850e46d8b7d90875955033017d59` (diff vs `fcfd6349d` = exactly
   graph.rs+sync.rs, ±11 lines), commit `ecb36850e`, parent `fcfd6349d` —
   main fast-forward, history append-only. Verified on the patched scratch
   tree (/tmp/ggen-r35-main.51083, warm target reuse):
   `just check` PASS; tree diff `--stat` = 2 files only.
3. **Push re-tested through the new hook** (`git push --dry-run origin
   main`, exit 1): hook header shows `ref ecb36850e532` + scratch export
   (validates the REF, demonstrated); `[1/4] Check PASS`, `[2/4] Lint FAIL`
   — `BLOCKED: pushed ref ecb36850e532 failed gates. Push refused.`
4. **Re-typed standing (ref-validation removed as a cause):**
   `BLOCKED[MAIN_SELF_TEST_LINT_POLICY: 19 clippy::expect_used-class errors
   in 4 e2e test files on main's own content —
   crates/ggen-engine/tests/sparql_refusals_e2e.rs (15),
   generation_rules_e2e.rs (2), composed_packs_e2e.rs (1),
   consumer_mode_fixture_only_e2e.rs (1)]`. This is the operator policy
   decision R19 deferred: `expect_used`/`expect_err` in e2e tests vs
   `-D warnings`. Until decided, main is NOT pushed; local main
   `ecb36850e` remains a lawful FF of origin `d593a7f30`.
   Note: measured 19 errors vs R19's "25" — R19's count included
   spec-integration tree noise; the ref-scoped run is authoritative.
5. **spec-integration content untouched**: shared tree still on
   spec-integration at `b58f7cb9a` (hook-fix commit + R19 docs only; no
   branch switch, no rebase, no --no-verify).

## main — R51 expect_used policy adjudication (2026-10-09, lane R51)

Adjudicates R35's `BLOCKED[MAIN_SELF_TEST_LINT_POLICY]` via operator doctrine:
annotations are explicit decisions, not silent relaxations; `expect!`-style
assertions are idiomatic in e2e tests; lib gate untouched.

1. **Policy**: `#![expect(clippy::expect_used)]` (or item-level `#[expect]`)
   with a one-line justification comment is the documented mechanism for
   expect/unwrap in e2e test assertions. `expect!`/unwrap forbidden in lib
   code — the lib gate is unchanged and separately enforced.
2. **Measured correction to R35's count**: of the 4 flagged files, 3
   (composed_packs_e2e.rs, consumer_mode_fixture_only_e2e.rs,
   generation_rules_e2e.rs) already carry the house
   `#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]` at
   module scope, so expect_used never fired there; only
   `sparql_refusals_e2e.rs` (5 expect sites, no allow) needed the annotation.
   A crate-level `#![expect]` on the 3 allow-carrying files is REFUSED by
   `-D warnings` as unfulfilled_lint_expectations — expect, not allow,
   enforced.
3. **Candidate commit** `c24af1b24158` (parent ecb36850e; delta vs ecb36850e
   = exactly the 4-line annotation block in sparql_refusals_e2e.rs, minted
   via temp-index plumbing, main CAS-updated ecb36850e→c24af1b24158).
   `git push --dry-run origin main` through the ref-validating hook:
   `[1/4] Check PASS`, `[3/4] Format PASS`, `[4/4] Unit tests PASS`,
   `[2/4] Lint FAIL` — `BLOCKED: pushed ref c24af1b24158 failed gates. Push
   refused.` Local main = c24af1b24158; origin main = d593a7f30 (FF chain
   d593a7f30→fcfd6349d→ecb36850e→c24af1b24158 intact, NOT pushed).
4. **Re-typed standing**: expect_used class is now **0** (annotation
   fulfilled in sparql_refusals_e2e.rs:1-4). `BLOCKED[MAIN_TEST_LINT_RESIDUE:
   8 non-expect clippy errors on main's own content, 4 files]`:
   - sparql_refusals_e2e.rs: needless_raw_string_hashes ×3 (:24, :60, :77)
     + uninlined_format_args (:114)
   - consumer_mode_fixture_only_e2e.rs: doc_markdown (:4)
   - composed_packs_e2e.rs: too_many_lines (:257, 109/100)
   - generation_rules_e2e.rs: doc_markdown (:992) + unused_variables (:1005)
   Next lane: fix the 8 mechanical lints (only too_many_lines needs a
   function split); then main FF-pushes clean through all 4 gates.
5. **spec-integration carries the annotation** (file identical on both refs,
   so the policy landing is ref-agnostic): annotation committed alongside
   this doc update; hook does not gate spec-integration pushes.

## main — PUSHED (closed, lane R68, 2026-10-09)

- All 8 residual clippy errors fixed on top of R51's candidate
  `c24af1b24158`; new main candidate `16b99731a703` (tree `787bd6e48a`,
  parent `c24af1b24158`, minted via temp-index plumbing, main CAS-updated
  c24af1b24158→16b99731a). Per-site fixes:
  - sparql_refusals_e2e.rs: needless_raw_string_hashes ×3 (hash-less raw
    strings) + uninlined_format_args (shared `msg` binding, inlined).
  - consumer_mode_fixture_only_e2e.rs: doc_markdown (`GraphLaw` backticked).
  - generation_rules_e2e.rs: doc_markdown (`output_file` backticked) +
    unused_variables (`report` → `_report`).
  - composed_packs_e2e.rs: too_many_lines — mechanical extraction of
    assert_bootstrap_evidence / assert_lock_covers_composed_packs /
    assert_sabotage_refusal_is_fm_pack_013 from
    thirty_one_packs_compose_with_verify_gates_active (no behavior change).
- Local clippy on the 4 targets (`-D warnings`): 0 errors.
- Gate: `git push --dry-run origin main` → all 4 gates PASS on ref
  16b99731a703; real push `d593a7f30..16b99731a main -> main` exit 0.
  main is 0/0 vs origin.
- **Hook root-cause fix (lane R68)**: the pre-push hook's per-push
  `mktemp` scratch + `rm -rf`, combined with its per-sha cargo cache,
  baked a deleted scratch path into `env!("CARGO_MANIFEST_DIR")` at
  compile time; every cache-reuse push re-ran binaries whose baked paths
  no longer existed (praxis-core
  committed_tcps_generated_head_recomputes_to_its_stored_chain_hash
  failed NotFound through 3 refused pushes even as the same tests passed
  from a live tree). Fix in `scripts/hooks/pre-push.sh`: persistent
  per-sha scratch `~/.cache/ggen-pre-push/<sha12>-src`, refreshed with
  `find -delete` + re-extract per push, scratch no longer deleted after
  gates. Gate commands and strength unchanged.
- spec-integration itself remains 0/0 vs origin after this doc commit.

## closure — PR #795 merged + same-checkout violation record (2026-10-09, lane R109)

1. **PR #795 merged** (`gh pr view 795 --repo seanchatmangpt/ggen --json
   state,mergeCommit`): `state=MERGED`, mergeCommit
   `16b99731a703fa13c1ec0c310c4137e85186af9d`. `git fetch origin` +
   `git rev-parse origin/main` → `16b99731a703f...` — origin/main matches
   R68's landed candidate exactly. main is 0/0 and closed; the R68
   "PUSHED" standing above is now MERGED-via-PR.
2. **Recovered stash delta — already landed, no residual diff.** The
   violating actor's stash (`2896f0588`, "WIP on spec-integration",
   parented on `4619246cc`) contained exactly one path delta:
   `scripts/hooks/pre-push.sh` (+10/−2) — the persistent per-sha scratch
   path that R68 receipts above. Verification:
   `git diff 2896f0588 HEAD -- scripts/hooks/pre-push.sh` is empty; the
   fix is byte-identical in R68's commit `583d01573` (attribution: hook
   fix found applied mid-flight by an unknown lane, landed and disclosed
   by R68). `docs/sjira/v26.10.8/RESIDUAL-BRANCHES.md` carried no delta in
   the stash — this section is the only doc change. The stash ref
   `2896f0588` is preserved, not dropped.
3. **Same-checkout violation recorded** (for operator audit): at
   2026-10-09 11:44:49−0700 an unknown actor ran `git reset` + a
   stash-creating WIP commit (`2896f0588`) on spec-integration, then
   `git checkout feat/integrate-graphlaw-engine` at 11:44:58 (reflog:
   `checkout: moving from spec-integration to
   feat/integrate-graphlaw-engine`, landing at `5d209290c`), then
   `checkout: moving from feat/integrate-graphlaw-engine to
   spec-integration` at 11:50:39 (back at `749c7441f`). This is a
   checkout-global branch switch + stash on the shared canonical checkout
   — both banned by same-checkout fan-out law rule 1. In-flight edits were
   swept into the stash mid-flight. No worktree was created; the stash ref
   is retained as evidence.
