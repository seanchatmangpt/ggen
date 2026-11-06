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

### 1.8 Fleet-audit-first-action slots law (P11)

- The fleet-audit line (live agent count + per-slot mandate) is the
  SOLE valid basis for a dispatch decision. The wave ledger is a
  hypothesis until the audit re-reads it; the audit is the proof.
  Never dispatch into a slot because the ledger says it is free —
  dispatch because the audit says it is free (ListAgents read at
  dispatch time, not carried from the previous drain).
- A slot is freed by a VERIFIED RECEIPT LANDING, not by the lane's
  process exit. A lane that exited with output on disk but no
  receipted landing still occupies its slot until the coordinator
  lands it (§3.6) or the receipt is minted. Counting exits as
  freed slots double-books the fleet (O1 class 1/4 overlap:
  exited-but-unlanded work re-dispatched as if the slot were idle).
- Per-tick order: audit fleet slots → reconcile ledger vs audit →
  dispatch only into audit-verified free slots → record the delta
  in the wave ledger with the audit as its evidence line.

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
- **Saturation**: TWO consecutive waves returning only LOW-severity
  doc drift with zero test regressions → saturated; a single such
  wave is a signal, not a stop (P9). The counter RESETS on any
  landed code fix, newly-registered open-item row, or MEDIUM+
  finding. Definition matches entropic-annealing-doctrine §3
  (saturation = findings-per-wave AND severity both below
  threshold — "zero defects" was never the doctrine's bar; the
  prior "same definition as §3" citation misquoted it and is
  corrected here). Corollary: N lanes agreeing is not N
  observations — each lane's receipt carries its own commands/exits.
- **Fleet stop**: 429 cycling on re-dispatch (BB-10's retry also
  429'd) → shrink fleet, not retry; backlog re-census before the
  next wave.

## 6. Failure-mode table → prevention cross-reference

Source: `orchestration-failure-modes.md` (O1), classes 1-8. Every
mechanism below lives in §1-§4; this table is the traceability index.

Mechanism refs are runbook sections; one entry per O1 class.

- Class 1, 429 kills (8+; CG5, BB-01..07, BB-10 x2, D25):
  429-aware cap + slot re-dispatch, no stacked retries (§1.7);
  index-inclusive post-429 inventory (§3.3).
- Class 2, shared-index sweep (3; D32→D25, D57→D60, D83→D81):
  disjoint owned files (§1.4); collision disclosure in both
  receipts (§3.1); pre-commit attribution gate (§3.2, mech. #1).
- Class 3, premise rot (9+; D74, D67, D77, D50/D52, Q2, V3, P10,
  R2): premise re-check before acting (§3.4); landed-work probe
  (§1.5); re-pin don't revert.
- Class 4, duplicate work (3; D28 a86d167, D65, L8745):
  landed-work probe (§1.5); overdue-lane stop (§3.7); coordinator
  land+stop (§3.6).
- Class 5, wrong-ID stop (1; E-q stopped, E-s meant):
  ListAgents-before-TaskStop (§3.8); mandate-coverage check.
- Class 6, port contention (2; E-q vs D33, D58 vs D33):
  port leases in the slot map (§1.6); per-lane private ports.
- Class 7, wait-gate positive (11 markers; D88): named-lane poll
  <=60s, <=45min, typed IN-FLIGHT-AT-FILL fallback (§4.1-4.2).
- Class 8, push verify positive (656 refs; fleet-wide): ls-remote
  grep gate (§1 item 8, §4.7).

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

## 8. Overnight Annex (P12)

Mechanics for running the fleet unattended overnight (dispatch
continues while the operator sleeps). This annex defines the ramp,
the refill tick, the typed stop conditions, and the morning
receipt set. The coordinator session owns git transitions
throughout; §1-§4 laws apply unchanged overnight.

### 8.1 Ramp law

- Start at N=6 lanes (429-aware cap, §1.7, sized conservative for
  unattended operation — no operator to catch a rate-limit storm).
- +2 lanes per CLEAN SOAK: a soak is one full refill tick with zero
  429s and zero lane failures across the running fleet.
- Hard ceiling: 12 lanes overnight, regardless of soak count.
  Unattended fleets do not exceed the ceiling; surplus registered
  backlog waits for morning (backlog-targeting law: idle-while-
  backlog-empty is lawful; quota-filling is not).

### 8.2 Refill tick (15 min, loop skill)

A loop-skill tick every 15 minutes executes, in order:

1. **Fleet audit first** — live-agent audit re-read
   (§1.8 law: the audit line, not the ledger, is the dispatch
   basis); slots decrement only on verified receipt landing.
2. **Dead-lane inventory** — for any lane killed/429'd since the
   last tick: index-inclusive inventory of its owned files (§3.3)
   before any re-dispatch.
3. **Ledger reconciliation** — reconcile wave ledger vs audit;
   record the delta with the audit as evidence line.
4. **Dispatch** — next registered backlog item(s) into
   audit-verified free slots, up to the ramp rule (§8.1).
5. **Nudge check** — any lane silent past ~37 min gets the one
   §3.5 nudge.
6. **Stop-condition check** — evaluate the three typed stops
   (§8.3); if any fires, halt the loop and leave the morning
   receipt half written per its type.

### 8.3 Typed stop conditions (any one halts the loop)

- `STOP-429-CYCLING` — re-dispatch attempts 429 on consecutive
  ticks (BB-10 class): shrink to N=6 or stop the loop; never
  stacked retries (§5 fleet-stop law).
- `STOP-BACKLOG-EMPTY` — registered backlog drained and all
  in-flight lanes receipted: stop; idle is lawful saturation, do
  not mint make-work lanes.
- `STOP-SOAK-BROKEN` — two consecutive ticks with lane failures
  or shared-compile breaks (compile-freeze SLA violations, §3.9):
  the overnight environment is degraded; stop and leave state for
  the morning crew.

### 8.4 Morning receipt set

The overnight run is complete only when all of the following
exist on disk at fill:

- `fleet-slots.json` — per-tick fleet audit snapshots (tick
  timestamp, lane IDs, mandate, slot state); the machine-readable
  audit trail the §1.8 law requires.
- `overnight-wave-ledger.md` — dispatch log with Standing line
  (receipt-standing-law vocabulary), one row per tick: slots
  audited, dispatched, landed, stopped; deltas carry the audit
  snapshot as evidence.
- Per-lane receipts per the §2 contract (mandatory), plus a
  consolidated morning receipt per the §4 drain sequence (typed
  fallbacks, partial-honest landings, push verify per repo).
