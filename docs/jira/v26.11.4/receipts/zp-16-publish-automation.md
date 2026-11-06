# ZP-16 — Publish Automation Gap-Close Receipt

Standing: ALIVE

Date: 2026-11-04. Lane: GAP-CLOSE. Status: ALIVE for workflow files and
structure (read + line-cited this session); the PRE-FIX version bump is
NOT landed, so the ggen gate as written is currently BLOCKED-by-design.

## ggen publish.yml — dep-edge DAG + PRE-FIX-gate BLOCKED state

`/Users/sac/ggen/.github/workflows/publish.yml` (189 lines, 5 jobs),
tag-push keyed on `v*` (`:25-28`), OIDC trusted publishing, zero registry
token secrets (`:30-32`), concurrency group
`publish-ggen-${{ github.ref }}` with `cancel-in-progress: false` (`:34-36`,
"never cancel a publish mid-flight").

- PRE-FIX gate (`:40-73`): "ggen-engine's version reqs on ggen-abb-sbb and
  ggen-scip must be at 26.11.3 before any publish is lawful ...
  Fail-closed." BLOCKED output at `:66` (`BLOCKED(PRE-FIX): crates/
  ggen-engine/Cargo.toml ggen-abb-sbb req is $abb, not 26.11.3`) and `:70`
  (same for ggen-scip).
- Dep-edge DAG (`:76-90`, edges cited to
  `crates/ggen-marketplace/Cargo.toml:65` and
  `crates/ggen-engine/Cargo.toml`): wave 1 = ggen-abb-sbb, ggen-scip,
  ggen-config (no intra-workspace deps); wave 2 = ggen-marketplace (dep:
  ggen-config >= 26.10.8, < 26.15); wave 3 = ggen-engine. ggen-cli and
  ggen-lsp are `publish = false`; ggen-graph is not in the publish order.
- Jobs: `pre-fix-gate` (:44) -> `publish-wave-1` (:91) -> `publish-wave-2`
  (:117) -> `publish-wave-3` (:142) -> `publish-verify` (:165, "poll law:
  versions[0], never max_version"). Publish jobs run behind
  `environment: publish-ggen` (:96, :122, :147).
- Poll law (`:13-21`): crates.io polls MUST read `versions[0]`, never
  `max_version` — the July-CalVer anomaly; verify loop `:175-189` polls
  `jq -r '.versions[0].num'`, exits 1 on MISMATCH (`:185`).
- BLOCKED today, by the gate's own design: `crates/ggen-engine/
  Cargo.toml:118` still pins `ggen-abb-sbb ... version = "26.10.11"` and
  `:121` `ggen-scip ... version = "26.10.12"` — the PRE-FIX bump to
  26.11.3 (PLANS.md:51-52) has NOT landed, so a tag push right now
  refusals at pre-fix-gate. That is the gate working, not a defect of the
  workflow.

## cgap publish.yml — 18-job tier DAG

`/Users/sac/cgap/.github/workflows/publish.yml` (279 lines, 18 jobs —
count verified this session). Header law (`:3-7`): ONE attempt per crate
per release; human admission edges stay human — each tier job runs behind
a GitHub environment protection rule (`publish-tier0/1/2`) whose required
reviewers the operator wires in the GitHub UI. DAG encoded from actual
Cargo.toml [dependencies] edges, dev-dependencies excluded (`:10-14`);
cgap-storage included though the runbook list omits it (`:16-18`).

- Tier 0 (4 jobs, `publish-tier0`): arrow :39, blake3 :51, git :63,
  parquet :75 — "Actual [dependencies] verified empty of cgap crates"
  (:36-37).
- Tier 1 (9 jobs, `publish-tier1`): branchless :93, gate :105, core :117,
  aoq :129 (no needs); crypto :141 (needs [blake3]), bont :154, bn3 :167,
  storage :180, p28 :193 (needs [blake3, crypto]). Gate's cgap-core edge
  is dev-only, so no branchless/gate/core needs edge exists (:88-91).
- Tier 2 (5 jobs, `publish-tier2`): ffi :214 (needs 9 crates), gpu :227
  (needs [arrow]), audit :240 (needs [arrow, parquet]), umbrella :253
  (needs 11 crates), wasm-footprint :266 (needs [arrow, parquet];
  wasm32-wasip1 target :275; wasm-footprint pins arrow/parquet at exactly
  26.11.3, `:212`).
- Every job: `cargo publish -p X --dry-run` gate step, then the real
  publish step.

## Marketplace NO-OP-NOT-CREATED rationale

Honest correction to the session-memory claim: the string
"NO-OP-NOT-CREATED" does NOT exist anywhere in /Users/sac/ggen or
/Users/sac/cgap (exhaustive case-insensitive grep this session). The
nearest real artifact is `marketplace-test.yml:31`, which warns a dead
test invocation "would silently report '0 tests run' and exit 0 ... a
decorative no-op, not a real check" — that is the anti-vacuity rationale
in the marketplace workflow family (a job that has nothing to do must say
so loudly, never fake a pass), but no create-job no-op-instead-of-fail
mechanism is on disk. The claimed behavior is UNVERIFIED against current
artifacts; do not cite it as landed without re-derivation.

## ZB-65 status

ZB-65 (portability) per `/Users/sac/ggen/docs/jira/v26.11.4/PRD.md:31` and
`PLANS.md:135-136`: one live ubuntu failure + one macOS-side break.

- Half 1 (ubuntu, BSD `sed -i ''` in verify_v26.10.31.sh): LANDED in cgap
  — receipt `/Users/sac/cgap/docs/jira/v26.11.3/receipts/
  zd-141-sed-portability.md` (`:3` "Status: LANDED"; `:17-19` portable
  non-`-i` rewrite `sed ... "$TABLE" > "$TABLE.tmp" && mv`). Cross-referenced
  at `cgap/docs/jira/v26.11.3/README.md:193` (GNU-side proof deferred to
  next ubuntu run, disclosed). Cited PARTIAL (not re-run this session).
- Half 2 (lean4 script GNU-only): NOT LANDED. Verified this session:
  `grep -n mapfile /Users/sac/ggen/scripts/verify-lean4-rust-pipeline.sh`
  returns `:13 mapfile -t root_files < <(find ... -printf ...)` and
  `:23 mapfile -t authored < <(...)` — still GNU-only bash `mapfile` +
  `find -printf`. No lane receipt, commit, or doc claims a fix (no "GG1"
  label exists anywhere in ggen docs or git history — grep-verified).
  PLANS.md:136 still lists it as an open break. Standing: OPEN, not
  closed.

  > CORRECTION 2026-11-04: Half 2 has now LANDED (errata to the "NOT
  > LANDED" above). `grep -nE 'mapfile|-printf|readlink -f|echo -e'
  > scripts/verify-lean4-rust-pipeline.sh` returns zero hits; bash-3.2
  > syntax-check passes; the portable tmp-file `while IFS= read -r`
  > block was executed under system bash 3.2 (file-list checks +
  > ggen sync + receipt verify green). LANDED 2026-11-04: lean4 script
  > portable (mapfile/-printf removed). Full end-to-end run still stops
  > at a downstream, pre-existing lake build failure
  > (`Lean4RustPipeline.lean:1:0: unknown module prefix
  > 'Fortune5Policy'` — lakefile.lean declares only `lean_lib
  > Lean4RustPipeline`), byte-identical on the committed tree, not
  > introduced by the portability edit.

## Operator-gated list (human admission edges, not automatable by lanes)

1. GitHub environments with required reviewers: ggen `publish-ggen`
   (publish.yml:96,122,147); cgap `publish-tier0` (:42,:54,:66,:78),
   `publish-tier1` (:96,:108,:120,:132,:145,:158,:171,:184,:197),
   `publish-tier2` (:218,:231,:244,:257,:270) — reviewers wired in the
   GitHub UI (cgap publish.yml:6-7).
2. crates.io trusted-publisher registration per crate: repo + workflow
   filename + environment tuple registered on crates.io (ggen
   publish.yml:7-8; PLANS.md:40-41).
3. PRE-FIX version requirements: engine's `ggen-scip "26.10.12"` +
   `ggen-abb-sbb "26.10.11"` reqs must move to 26.11.3 before any tag
   publish is lawful (PLANS.md:51-52) — currently unmet in-tree, so this
   is an operator/lane action outstanding.
4. The publish actuation decision itself stays human (cgap
   publish.yml:3-7: the workflow "automates the workflow, never the
   actuation decision").

## Standing summary

- Workflow structures, DAGs, gate lines, job counts: ALIVE (files read
  and line-cited this session).
- zd-141 sed fix: PARTIAL (landed per receipt, not re-run).
- lean4 mapfile fix: **VERIFIED-LANDED** (2026-11-04 recount).
  Re-confirmed on disk this session: `grep -nE 'mapfile|-printf|readlink
  -f|echo -e' scripts/verify-lean4-rust-pipeline.sh` returns zero hits;
  `bash -n scripts/verify-lean4-rust-pipeline.sh` passes under system
  bash 3.2; the fix is committed in fa7c47faa ("ci: publish workflow
  (ZP-16) + lean4 portability (ZB-65) + disk-reclaim + lock-untrack
  (ZD-124)"), verified an ancestor of HEAD via `git merge-base --is-ancestor`.
- marketplace NO-OP-NOT-CREATED mechanism: **RETRACTED** (2026-11-04).
  No artifact carrying that behavior exists anywhere in the tree; the
  nearest rationale is the anti-vacuity comment at
  `marketplace-test.yml:31` (decorative no-op must not fake a pass).
  Terminal unless a real mechanism lands in a future commit.
- ggen PRE-FIX state: BLOCKED (by design) until the 26.11.3 req bump
  lands.
