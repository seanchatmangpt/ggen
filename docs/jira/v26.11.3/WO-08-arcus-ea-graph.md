# WO-08: Author arcus's EA Graph and Qualify Its SBBs — First Real abb-sbb Consumer

## Date

2026-11-03

## Repo

/Users/sac/arcus (executed here; canonical checkout, no worktrees). Owned and coordinated
under /Users/sac/ggen milestone v26.11.3 — receipts land in this file's milestone directory.

## Standing

PARTIAL-ALIVE (2026-11-04 reconciliation: arcus-side close record
/Users/sac/arcus/docs/jira/v26.11.3/closure/waves-wo08-wo09.md §"WO-08 standing" reads
PARTIAL-ALIVE — graph qualified, 5 SBBs ADMITTED, all sync gates green, with the
F2-dependent declarative-admission witness registered as follow-up; RECEIPTS.md open-WO row
reads "landed on disk — PARTIAL-ALIVE per the close record").

## Lane

WRITER / consumer-qualification lane. Depends on WO-01 (Stage 2c admission wiring) and
WO-04 (EA graph schema + projection rule) landing first. Owns arcus-side files only;
no concurrent lane may edit /Users/sac/arcus during execution.

## Ground Truth (re-verify at use time)

- The ggen kernel consumes `<root>/ea.graph.json` at sync.rs:1046; zero real consumer
  graphs exist anywhere in the fleet today. arcus is the first.
- The ABB register is defined: 21 ABBs (A1-A21) from the arcus plans/dx/dflss corpus,
  each with SELECT / MANUFACTURE / OUT-OF-GENGEN classification.
- Candidate SBB packs (5): gdmcp-pack, docs-diataxis-pack, github-actions-pack,
  cgap-pack, mdbook-pattern-language-pack. Zero are kernel-admissible today:
  /Users/sac/ggen-marketplace baseline.json is ~4 weeks stale, entries are mostly
  status-only without consequence_sha256; docs-diataxis-pack and cgap-pack have no
  qualification record at all.

## Task

Author arcus's EA graph, qualify its SBBs, and make arcus the first real abb-sbb consumer.
Five sub-steps:

### 1. Author the EA graph

Recommended route (per WO-04): Turtle + SHACL in a pack → projection rule → ea.graph.json.
If the projection rule from WO-04 has not landed by execution time, the INTERIM path is a
hand-built `/Users/sac/arcus/ea.graph.json` against the exemplar
`fixtures/ea-graph.json`, conforming to:

- schema string `ggen.ea.graph.v1`
- `deny_unknown_fields` semantics (no keys outside the v1 schema)
- qualifications digest-bound (source_sha256 + consequence_sha256 carried on each
  qualified SBB entry)
- `mutable: false` on all graph elements
- authority ≤ CONSTRUCT on every element
- pack ids disjoint from element ids (a pack id never names an element and vice versa)

Interim output must be replaced by the projection route once WO-04 lands — record which
route produced the shipped graph in the receipt.

### 2. Fresh qualification evidence

Re-run `/Users/sac/ggen-marketplace/scripts/qualify_packs.py` for all 5 candidate packs.
Discard the stale baseline claims; only fresh consequence_sha256 / source_sha256 evidence
from this run may feed the graph's qualification digests. A pack whose qualification
cannot produce both digests is NOT kernel-admissible and must not enter the graph.

### 3. ArchitectureContracts per ABB

For each of A1-A21, author an ArchitectureContract with:

- `max_artifacts` (bounded, from the ABB register)
- `authority_ceiling: CONSTRUCT`
- `required_ports` derived from the ABB register's edges

### 4. plan() decision table

Run `plan()` per ABB. The SELECT‖MANUFACTURE decision table is the deliverable:

- **SELECT** for docs / CI / CGAP-ABI SBBs (select an existing qualified pack)
- **MANUFACTURE** for the arcus-pack ABBs (generate new)
- **OUT-OF-GENGEN** ABBs are excluded from the graph entirely, with an explicit
  exclusion register naming each excluded ABB and the reason — they never enter the
  graph as SBBs, directly or transitively.

### 5. Wire into arcus sync

Place `ea.graph.json` at the arcus repo root so ggen sync's Stage 2c gates it. Confirm
the sync path picks the graph up at the kernel's `<root>/ea.graph.json` seam
(ggen sync.rs:1046 equivalent in the installed binary version used by arcus).

## Acceptance

Falsifiable, command-cited. All from `/Users/sac/arcus` unless noted:

1. **Stage 2c over the real graph, exit 0.** `ggen sync run` in arcus executes Stage 2c
   admission over the real `ea.graph.json` and exits 0. Cite the command, exit code,
   and the admission summary line(s).
2. **Decision table receipt.** `plan()` output covers all 21 ABBs and matches the
   SELECT‖MANUFACTURE decision table receipted in this WO's completion note — every ABB
   classified, OUT-OF-GENGEN ABBs absent from the graph and present in the exclusion
   register instead.
3. **STALE_QUALIFICATION witnessed.** Mutate one qualified SBB (touch/alter its content
   so its consequence_sha256 no longer matches the graph's digest) → re-run sync →
   a typed `STALE_QUALIFICATION` refusal is observed. Restore the SBB, re-run →
   exit 0. Cite both runs' outputs.
4. **Fresh qualification receipts.** The `qualify_packs.py` run output for all 5 packs
   is attached (or its artifact path cited); the graph's digest fields equal the fresh
   run's digests, not baseline.json's.
5. **Schema conformance.** The shipped graph validates against the v1 schema with
   `deny_unknown_fields`: no extra keys, all elements `mutable: false`, authority
   ≤ CONSTRUCT, pack ids disjoint from element ids (`grep`-level check on id
   intersections plus the schema validator's exit 0).
6. **No hand-written Go.** `grep -rn "\.go\b" <files authored in this WO>` (or the
   arcus-equivalent language check) confirms no hand-written Go composition — SBBs are
   selected from packs or generated via ggen, per composition-first.

## Constraints

- Composition-first: no hand-written Go (or arcus-equivalent) implementation of
  admission/planning logic; that is the kernel's job.
- OUT-OF-GENGEN ABBs never enter the graph as SBBs — exclusion register only.
- Interim hand-built JSON is a bridge, not the endpoint; the WO-04 projection route
  supersedes it.
- One canonical checkout of /Users/sac/arcus; no worktrees, no shadow copies.

## See Also

WO-01-stage-2c-wiring.md · WO-04-ea-graph-schema.md · RESOLUTIONS.md · README.md
