# Runbook: 20-Lane Implementation-Fleet Wave

Reusable spec for consistently running ~20 implementation-as-exploration
agents over one canonical checkout set. Authored 2026-11-04 from the
v26.11.4 session's proven mechanics; every rule below cites its witness:
an O1 instance (transcript class, see
`orchestration-failure-modes.md`, file refs are transcript lines Lnnnn)
or a landed mechanism (receipt/file on disk this session). Lines <=100
chars. Companion docs: `wave-closing-receipt.md` (fill pattern),
`next-wave-seeds.md` (backlog census), `open-items-register.md` (OI
rows), `session-learnings.md` (FM-LAW-015, premise-rot instances).

## 0. Definitions

- Lane: one Agent with a contract block (§2), an owned-file list, a
  gate set, and a receipt obligation. Lanes are leases, not copies.
- Wave: one dispatch round of N lanes over the same checkout set.
- Coordinator: the one session that owns git transitions, dispatch,
  and drain. Lanes never run `git add`/`commit`/`push` on shared
  repos unless the contract explicitly grants scoped commit rights.
- Wait-gate: a named-file/named-lane dependency with a bounded poll
  and a typed fallback (§4).

## 1. Pre-flight (before wave 1)

1. **Backlog census** — enumerate candidate work from the register,
   not from memory: open-items-register rows, next-wave-seeds rows,
   PLANS.md UNVERIFIED rows. Every dispatch target must have an
   on-disk referent (O1 class 4: a late 20-lane dispatch described
   already-landed work, L8745 — session-context rows are not
   dispatchable until materialized; see next-wave-seeds.md's
   reconstruct-or-demote section).
2. **Backlog-targeting rule** — registered seeds with named
   falsifiers outrank open-ended sweeps. A seed without a falsifier
   gets one written before dispatch or is demoted.
3. **Lane-ID registry** — mint lane IDs before dispatch and record
   them in a wave doc on disk (this session's D88/D107-class IDs were
   session-context only; the consolidation lane had to reconstruct
   them in-writing, OI-8 pattern). Rule: an ID not on disk before
   dispatch is an ID that cannot be receipted after.
4. **Slot map** — assign each lane: repo paths (absolute), owned-file
   list (disjoint), gate set, port assignments. Disjoint ownership is
   the same-checkout-fanout law; verify by listing every file in two
   lanes' sets and asserting the intersection is empty (D57/D83
   collisions, O1 class 2).
5. **Landed-work probe** — before each dispatch, grep HEAD/log for
   the lane's target artifact (O1 class 4: D28's work landed as
   a86d167 while the lane ran on; D65 re-derived an already-resealed
   digest). If landed: cancel the dispatch.
6. **Port leases** — any lane that runs a gate service or witness
   process gets a private port recorded in the slot map (O1 class 6:
   E-q vs D33, D58 vs D33 both collided on one port).
7. **429-aware cap** — size the fleet to the observed rate-limit
   behavior, not a fixed 20 (O1 class 1: 8+ kills across 7 lanes
   under the 20-lane fleet). Start smaller; re-dispatch as slots
   open, never stacked retries on a cycling lane (BB-10's retry
   429'd too).
8. **Coordinator git check** — coordinator confirms branch, clean
   status, and push verification readiness
   (`git ls-remote | grep $(git rev-parse HEAD)` is the gate, never
   push output; 656 transcript refs, O1 class 8).

## 2. Dispatch template

Every lane prompt = one contract block, verbatim skeleton:

```
LANE <id> — <one-line mandate>
REPO(S): /abs/path/to/repo[, ...]        # canonical checkout only
OWNED FILES: <disjoint explicit list>    # your only writable surface
GATES: <exact commands, e.g. just lint; just test-lib>
  - exit-checked; record command + exit in your receipt
COURTS: <falsifier per claim>            # claim without falsifier = no dispatch
WAIT-GATES: <file-or-lane, poll <=60s interval, <=45min cap,
  fallback: typed IN-FLIGHT-AT-FILL in your receipt>
PORTS: <private port or "none">          # no shared gate-runner ports
GIT: coordinator owns transitions        # or scoped commit rights named here
RECEIPT (mandatory, file path): <path>.md
  - Subject identity: repo, HEAD sha, branch, dirty count
  - Commands + exits actually run (exit-checked derivations)
  - Standing word: ALIVE / PARTIAL_ALIVE / BLOCKED / IN-FLIGHT-AT-FILL
  - Disclosures: collisions, skips, premise-rot re-pins, partial work
DO NOT: hand-edit generated output; commit build artifacts;
  resume another lane's in-flight file without naming the fix in
  your receipt (compile-freeze SLA disclosure, both receipts)
```

Skeleton provenance: each field is a landed mechanism — subject
identity + commands/exits + Standing = wave-closing-receipt.md §0-§5
pattern; falsifier-per-claim = every next-wave-seeds.md row;
wait-gate = D88's design (O1 class 7); owned-files + GIT clause =
same-checkout-fanout law + O1 class 2; ports = O1 class 6.

## 3. In-flight law

1. **Collision disclosure** — a lane that finds another lane's
   staged/in-flight work in its commit surface must either stop at
   its owned files or carry the rode-along with the owner named in
   BOTH receipts (D83 disclosed 9 pre-staged deletions riding its
   commit; D81's receipt recorded the fold-over; M14 rename swept
   into D57's commit, digest-verified intact — O1 class 2).
2. **Pre-commit attribution gate (target mechanization)** — every
   staged path must be owned by the committing lane or disclosed as
   swept with owner named. Currently applied by discipline; ranked
   #1 mechanization candidate (O1 §"Feeds O2 runbook").
3. **Post-429 inventory is index-inclusive** — a killed lane leaves
   partial edits in the working tree AND possibly in the index
   (D25's staged `git rm` deletions sat in the index; the working-
   tree-only inventory caused the D32 sweep). Inventory = `git
   status` (which covers the index) + owned-file check, before any
   re-dispatch. Never resume the dead lane's report as truth.
4. **Premise-rot re-check** — before acting on an alarming finding,
   verify the pinned premise against current disk (9+ instances, O1
   class 3: D74/D67/D77/Q2/V3/P10/R2 — findings dissolved on disk-
   check; D50/D52's file-level "dead" grep was corrected to
   symbol-level, 5 of 8 candidates were live court API). The honest
   fix is re-pinning to lawful new behavior, not reverting
   (session-learnings.md premise-rot section concurs).
5. **Nudge window** — one SendMessage nudge to a tail lane at ~37
   min elapsed, naming exactly what remains (witness: coordinator
   nudged the sole remaining gate lane at 37 min with "commit +
   push" — L7985). One nudge; not a poll loop.
6. **Coordinator landing rights** — if a lane misses its window and
   its verified-complete output is on disk, the coordinator lands it
   directly (D28: landed as a86d167 after the 10-min window), then
   STOPS the lane per the overdue-lane rule (§3.7) to prevent a
   duplicate landing.
7. **Overdue-lane stop rule** — window passed + output verified
   landed → stop the lane. D28 kept running 46 min post-landing and
   had to be stopped to prevent a duplicate (L8073/L8248/L8275).
8. **ListAgents-before-TaskStop** — never stop by assumed name/ID;
   list live agents, verify the target's mandate is covered
   elsewhere before reaping (E-q was stopped when E-s was meant;
   zero loss only by luck of E-z3 overlap — O1 class 5).
9. **Compile-freeze SLA** — a lane edit that breaks the shared
   compile is fixed or reverted by its owner within ~10 min
   (same-checkout-fanout law 2; minimal unblock fixes disclosed in
   both lanes' receipts).

## 4. Drain sequence

1. **Wait-gates fire first** — the fill/closing lane polls each
   named dependency (file existence or lane completion) at <=60s
   interval, cap 45 min. No sleeps-as-coordination; the poll is on
   a real observable (D88's design; no-sleep doctrine).
2. **Typed fallback** — a dependency still missing at cap lands as
   `IN-FLIGHT-AT-FILL` with its evidence line (this session: D28
   carried in-flight working-tree edits at fill; the runbook you are
   reading waited on O1's table with a poll + typed fallback).
3. **Partial-honest landing** — land with PARTIAL_ALIVE and record
   the failing gate as-is (lint exit 101 with 2 clippy errors
   recorded in wave-closing-receipt.md rather than blocked on
   perfection). Never relax a threshold to reach green.
4. **Stop-on-redundancy** — before authoring a lane whose output a
   sibling already produced, stop; receipt the redundancy (GG1's
   COMPLETE-BY-OTHERS verdict is the pattern: re-verify each contract
   item on disk, verdict, zero edits).
5. **Addendum** — late-tail commits append rows to the consolidated
   receipt's addendum table (d-wave-consolidated-receipt.md pattern),
   each row: lane, repo, commit, on-origin y/n, one-line artifact.
6. **Standing mint** — wave standing is re-read from receipt bodies
   at fill, never carried from memory (wave-closing-receipt.md §5
   header: "every count read live at fill"); statuses flip only with
   a live command/output receipt (open-items-register header law).
7. **Push verify** — `git ls-remote | grep $(git rev-parse HEAD)`
   per repo; push output alone masks silent failures.

## 5. Stopping criterion + saturation grammar

- **Per-lane stop**: receipt minted with Standing word + disclosures,
  owned files landed or handed to coordinator, falsifier run or
  registered.
- **Wave stop** (skip-the-next-wave test): LOW doc-drift findings
  AND zero test regressions across the wave's audit surface → the
  next wave is skipped as redundant (this session's E-f/E-y audit
  waves returned only noun/figure drift fixes; a repeat wave would
  re-find nothing new). Any landed code fix or open OI row keeps the
  wave alive.
- **Saturation**: a full wave returns zero defects → saturated (same
  definition as entropic-annealing-doctrine §3); waves before that
  are vibration. Corollary: N lanes agreeing is not N observations —
  each lane's receipt must carry its own commands/exits.
- **Fleet stop**: 429 cycling on re-dispatch (BB-10's retry also
  429'd) → shrink fleet, not retry; backlog re-census before the
  next wave.

## 6. Failure-mode table → prevention cross-reference

Source: `orchestration-failure-modes.md` (O1), classes 1-8. Every
mechanism below lives in §1-§4; this table is the traceability index.

| O1 class | Instances | Mechanism (runbook ref) |
|---|---|---|
| 1. 429 kills (8+) | CG5, BB-01..07, BB-10(x2), D25 | 429-aware cap + slot re-dispatch, no stacked retries (§1.7); index-inclusive post-429 inventory (§3.3) |
| 2. Shared-index sweep (3) | D32→D25, D57→D60, D83→D81 | Disjoint owned-file lists (§1.4); collision disclosure in both receipts (§3.1); pre-commit attribution gate (§3.2, mechanization #1) |
| 3. Premise rot (9+) | D74, D67, D77, D50/D52, Q2, V3, P10, R2 | Premise-rot re-check before acting (§3.4); pre-dispatch landed-work probe (§1.5); re-pin don't revert |
| 4. Duplicate work (3) | D28 (a86d167), D65, stale dispatch (L8745) | Pre-flight landed-work probe (§1.5); overdue-lane stop (§3.7); coordinator landing rights + stop (§3.6) |
| 5. Wrong-ID stop (1) | E-q stopped, E-s meant | ListAgents-before-TaskStop (§3.8); verify mandate coverage before reaping |
| 6. Port contention (2) | E-q vs D33, D58 vs D33 | Port leases in the slot map (§1.6); per-lane private ports |
| 7. Wait-gate (positive, 11 markers) | D88 | Named-lane poll <=60s, <=45min, typed IN-FLIGHT-AT-FILL fallback (§4.1-4.2) |
| 8. Push verify (positive, 656 refs) | fleet-wide | ls-remote grep gate (§1.8, §4.7) |

## 7. Receipt for this document

- Subject: /Users/sac/ggen/docs/jira/v26.11.4/closure/RUNBOOK-20-LANES.md
- Sources re-read on disk this session: orchestration-failure-modes.md
  (wait-gated, landed mid-run), wave-closing-receipt.md,
  d-wave-consolidated-receipt.md, next-wave-seeds.md,
  open-items-register.md, session-learnings.md, gg1-closure.md,
  publish-runbook.md.
- Every rule traceable to an O1 instance (with transcript line ref)
  or a landed mechanism (named file). No rule without a witness.
- Standing: minted at commit time; see commit message.
