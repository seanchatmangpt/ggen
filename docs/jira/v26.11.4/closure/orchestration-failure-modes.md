# Orchestration Failure Modes (v26.11.4 session)

Session receipt: ~200 background lanes, 15+ dispatch waves, single 36m+ tail
lanes. Mined from the session transcript
(`52c976f6-3908-4aee-9cae-cad873ef3b9f.jsonl`, 9312 lines). Every instance
below was observed, not inferred; line refs are transcript lines (Lnnnn).

## Summary table

| Class | Inst | Root cause | Prevention |
|---|---|---|---|
| 429 kills (8+) | 7 lanes (below) | glm sheds load at 20 lanes | slot re-dispatch, retries capped |
| shared-index sweep (3) | D32, D57, D83 | commit sweeps staged work | pre-commit attribution gate |
| premise rot (9+) | 9 lanes (below) | stale premise; moved substrate | premise-recheck in courts |
| duplicate work (3) | D28, D65, dispatch | no landed-work check | pre-land probe; lane-stop rule |
| wrong-ID stop (1) | E-q (meant E-s) | ID assumption, no listing | ListAgents before TaskStop |
| port contention (2) | E-q, D58 (vs D33) | two lanes, one gate port | per-lane ports; port lease |
| wait-gate (keep, 11) | D88 | — | typed gates, honest fallback |
| push verify (keep, 656) | fleet-wide | — | ls-remote grep HEAD |

Lane detail: BB lanes hit the 429 rate limiter under the 20-lane fleet
(CG5, BB-01, BB-02, BB-07, BB-04, BB-10 twice, D25); every recovery was a
verified on-disk inventory, never the dead lane's report. Sweep collisions:
D32 swept D25's 25 staged deletions (post-429 inventory checked tree not
index), D57 swept D60's M14 rename, D83 swept D81's 9 proxy/_build
deletions. Premise rot: D74 (markers are generator merge delimiters),
D67 (README was a destructive commit, not a conflict), D77 (frozen-skip
premise unsatisfiable by design), D50/D52 (E-h file-level grep premise
corrected to symbol-level), Q2 (1MiB bound false on disk), V3 (BLOCKED
state predated V1's repair), P10 (cbmqs-audit live, not residue),
R2 (ML-KEM runs under existing go.mod floor).

## Class detail

### 1. 429 rate-limit kills (8+)

- CG5 (census gate script) died mid-flight; recovered by inventorying owned
  files before re-dispatch (L941-943).
- BB-01 died twice ("datasets done, Module 01 writing" — partial state on
  disk); BB-02 died mid-fix; BB-07 died; BB-04 died; BB-10 died twice
  (retry also 429'd). Pattern: rate limiter cycles on a specific lane under
  a 20-lane fleet; fix was re-dispatch as slots open, not stacked retries
  (L2817-3016).
- D25 died on a 429 with staged `git rm` deletions already in the index —
  the recovery inventory checked the working tree, NOT the index, which
  directly caused failure class 2 (L6175, L6336).
- Loss profile: partial edits plus no receipt. The lane law (inventory
  owned surface before re-dispatch) held every time it was applied.
- Prevention: dispatch-time concurrency cap tied to observed 429 rate;
  inventory step extended to `git status` (index) not just working tree.

### 2. Shared-index sweep collisions (3)

- D32 landed all four KD registers but its ggen commit swept 25
  pre-staged deletions belonging to dead D25's first run. End state was
  correct (the deletions were mandated work) but attribution was broken;
  D25r had to be redirected to code-only scope (L6336-6342).
- Wave 3's M14 rename swept into D57's commit via shared-index collision;
  both lanes verified byte-intact landing by digest, so no data loss
  (L6959).
- D83 disclosed 9 pre-staged `proxy/_build` deletions rode along — D81's
  concurrently staged work; D81's own receipt then recorded the fold-over
  with ownership (L7605, L7679).
- Root cause in all three: `git add`/`git commit` on the one canonical
  checkout is a checkout-global transition; another lane's staged-but-
  uncommitted work is invisible to the committing lane.
- Prevention: a mechanical pre-commit attribution gate (every staged path
  must match the lane's owned-file set or be explicitly disclosed);
  coordinator-owned staging per lane; extend post-mortem inventory to the
  index.

### 3. Premise rot / inverted premises (9+)

- D74: CLAUDE.md "conflict markers" were the generator's own merge-region
  delimiters written on first-time wrap — nothing was ever lost; sync
  verified byte-identical (L7341).
- D67: ggen README "merge conflict" was actually a destructive commit that
  lost the generated shell; restored from last-good, version sources
  bumped, re-rendered through the real sync (L7165).
- D77: the dispatch's "zero frozen-skips" premise was unsatisfiable by
  design — under checksum freeze policy, pin==disk means safe-to-regenerate
  (L7730).
- D50/D52: E-h's grep premise (file-level "dead" symbols) corrected to
  symbol-level — 5 of 8 candidates were live court API (L6726, L6831).
- Q2: the "1MiB payload bound" premise was false on disk (L3770).
- V3: its cbmqs-cli BLOCKED state predates V1's own repair, already landed
  (L2351).
- P10: cbmqs-audit "residue" was a live workspace member rebuilt that day
  (L4789).
- R2: ML-KEM premise-rot resolved empirically — `crypto/mlkem` runs under
  the existing go.mod floor; no bump needed (L1094).
- Also: LANE-16 "841 Tera syntax errors" premise did not exist (L294); D22
  caught hardcoded-seed premise rot in scripts (L6322).
- Pattern: courts and sweeps written against stale or secondhand premises
  produced alarming findings that dissolved on disk-check. Prevention:
  premise-recheck step (verify the pinned claim against current disk
  before acting) in every court template; the existing premise-rot rule
  covers this but was applied by discipline, not mechanically.

### 4. Duplicate / redundant work (3)

- D28: coordinator landed its verified-complete work directly (a86d167)
  after its 10-min window passed, then D28 kept running 46 min until
  stopped to prevent a duplicate landing (L8073, L8248, L8275).
- D65: warmup conflict reconciled — the recorded digest had simply gone
  stale when a lawful rename regenerated the header, already re-sealed by
  D59 (L7257).
- A late 20-lane dispatch described work that had already landed this
  session; required premise reconciliation before execution (L8745).
- Prevention: pre-dispatch landed-work probe (grep HEAD for the artifact);
  overdue-lane stop rule (window passed + output verified landed → stop,
  don't let it commit a duplicate).

### 5. Wrong-ID coordinator stop (1)

- Coordinator stopped E-q (smalt gates final check) believing it was E-s.
  No real loss only because E-q's mandate was covered by E-z3's running
  witness and E-q was stuck in a port-contention loop anyway (L8477).
- Prevention: ListAgents-before-TaskStop rule; stop by verified mandate,
  not assumed ID.

### 6. Port contention (2)

- E-q spent its life in a substrate port-contention loop colliding with
  D33's gate process (L8477).
- D58's witnessed run of D33's committed gate flip was port-blocked by
  D33's own concurrent run; the clean witness had to be re-dispatched
  later (L7777).
- Prevention: per-lane private ports or a port lease before dispatching
  two lanes that touch the same gate service.

### 7. Positive disciplines to keep (witnessed working)

- D88's wait-gate design: poll-gates on named lanes plus a 45-min honest
  IN-FLIGHT-AT-FILL fallback; landing with PARTIAL_ALIVE and the lint FAIL
  recorded as-is rather than blocked on perfection (L7835, L8040).
- Push verification via ls-remote (656 transcript refs), e.g. "push
  confirmed via ls-remote, not push output" (L6959).
- Coordinator nudge as a tail-lane accelerator: single SendMessage nudge to
  the sole remaining gate lane at 37 min elapsed, naming exactly what
  remained (commit + push) (L7985).
- Lane law on 429 deaths: inventory owned files before re-dispatch, never
  trust the dead lane's report — applied at every kill, zero lost-work
  incidents beyond the index blind spot (class 2).

## Feeds O2 runbook

Mechanized gates ranked by incidence: (1) pre-commit staged-path
attribution gate, (2) 429-aware dispatch cap + index-inclusive inventory,
(3) pre-dispatch landed-work probe, (4) ListAgents-before-TaskStop,
(5) port lease for gate-runner lanes.

## See Also

`docs/jira/v26.11.4/closure/` (sibling closure receipts)
