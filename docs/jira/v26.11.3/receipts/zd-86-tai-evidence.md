# ZD-86 — TAI / docs_deep CI evidence-lane failures (ZB-19 unmapped causes)

Standing: REFUSED

Date: 2026-11-04 · Subject head: `58a3b4108d6b7cf98a2b8429511ec3afa497d771` (branch `v26-11-3-wave`)
Runs inspected: gbb-certification run **41816930105**, CI run **41816929805**

## Verdict: both ZB-19 causes were misattributed from echoed script text

Neither failure touched the code ZB-19 suspected. The step-echo in GitHub's
failed-log view prints the entire `case` statement (including the
`REFUSED:UNKNOWN_EVIDENCE_LANE` arm and the install-action bash-check script
body) even when those branches never executed. Actual findings, per run logs:

### (1) TAI / "18 standards" (gbb-certification.yml, job `certification-law`)

- Workflow file: `.github/workflows/gbb-certification.yml`
- Failing step (per run 41816930105 step census): **"Manufacture TAI-REBUILD-001 twice"**, exit 1
- Not failing: every provisioning step. The taiki-e install-action steps ran to
  `conclusion=success` (durations 553 ms / <1 s; log ids
  `__self.__taiki-e_install-action{,_2}`). The "install-action requires bash"
  string in the log is the printed source of the action's untaken
  Alpine-bootstrap branch, not an error.
- Toolchain provisioning is already the repo-standard composite
  (`./.github/actions/setup-ggen-build`, SHA-pinned dtolnay + taiki-e), so the
  hypothesized "pin shell: bash" fix had no defect to attach to. **No workflow
  change made or needed.**
- Real cause: `ERROR: [FM-LAW-015] EA admission gate:
  examples/tai-enterprise-rebuild/ea.graph.json is absent` — the file is a
  required read-only admission input of the new Stage 2c gate
  (`crates/ggen-engine/src/sync.rs:353-400`, fail-closed on absent input) but
  was **untracked in git** (`git status`: `?? examples/tai-enterprise-rebuild/ea.graph.json`),
  so CI's fresh checkout lacked it.

### (2) Deep / docs_deep (ci.yml, lane `docs_deep`)

- Dispatcher: `.github/workflows/ci.yml:118-142` ("Execute owned evidence").
  The lane registry is the `case` arm list; `docs_deep` **is registered**
  (line 139: `git diff --check "$BASE"...HEAD`) and **was the arm that ran**.
  No catch-all was hit; no lane was renamed or missing. The
  `REFUSED:UNKNOWN_EVIDENCE_LANE` text in the log is the echoed catch-all arm.
- Real cause: `git diff --check "$BASE"...HEAD` **failed with exit 2** on real
  whitespace defects in the committed range
  `b21cbfa5a9312901be2915afef23fb880a308f69...58a3b4108`:
  - `CLAUDE.md:72` — new blank line at EOF
  - `docs/jira/v26.11.3/receipts/zd-16-eof-sweep.md:22` — trailing whitespace
- Fail-closed behavior is correct and kept; the catch-all stays, no registry
  change.

## Fixes applied (SLA minimal-unblock disclosures — files owned by other lanes)

Both defect files are landed (committed) files outside this lane's ownership;
per the compile-freeze SLA these are disclosed minimal unblocks:

1. `CLAUDE.md` — stripped the EOF blank line (diff: 1 deletion, no content
   change). Owner: repo-facts / doc lanes (ZD-27 / ZD-24 class).
2. `docs/jira/v26.11.3/receipts/zd-16-eof-sweep.md:22` — stripped trailing
   space. Owner: ZD-16 lane.
3. `examples/tai-enterprise-rebuild/ea.graph.json` — **staged (`git add`, not
   committed)** so the coordinator's integration commit tracks the admission
   input CI needs. Validated with the real kernel before staging:
   `cargo run -p ggen-abb-sbb --example validate_ea_graph -- <file>` →
   `schema ok: ggen.ea.graph.v1`, `validate: ok`,
   `graph_digest: sha256:53af6280c6c5...`, admission probe `ADMITTED
   sbb:tai-rebuild-manufacture`, exit 0.

No `.github/workflows/` byte changed; the three workflows inspected
(ci.yml, gbb-certification.yml, tai-enterprise-rebuild.yml) parse clean
(`python3 -c yaml.safe_load`, all OK). `examples/tai-enterprise-rebuild/ggen.toml`
carries another lane's in-flight modification — untouched, unstaged.

## Verification ladder (commands + exits)

| Command | Result |
|---|---|
| `gh run view 41816930105` | fail step: "Manufacture TAI-REBUILD-001 twice", FM-LAW-015 |
| `gh run view 41816929805 --log-failed` | docs_deep: diff-check exit 2, 2 defects named |
| `git diff --check b21cbfa5...HEAD` (before) | exit 2, 2 defects |
| `git diff --check -- CLAUDE.md zd-16-eof-sweep.md` (after) | working tree exit 0 |
| `git diff --check b21cbfa5...HEAD` (after) | still exit 2; clears at next commit |
| `cargo run -p ggen-abb-sbb --ex validate_ea_graph` | exit 0; sbb ADMITTED |
| `python3 -c yaml.safe_load(...)` x3 workflows | all OK |

## Standing

- Workflow-level: ALIVE (inspected, no defect) — ZB-19's two suspected defects
  do not exist.
- Whitespace unblocks: fixed on disk, verified exit 0 working-tree.
- ea.graph.json: staged + kernel-validated; tracking lands with the next
  integration commit (coordinator).
- Remaining UNVERIFIED: the full TAI job green end-to-end on GitHub runners —
  requires the next push; not run in this session.
