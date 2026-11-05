# posh-integration-plan

Consolidation of the 19-lane posh census (P1-P19) into the integration/rename
plan. This document is the input to the rename/integrate implementation wave.
Census lanes: smalt lib imports, posh-internal inventory, build/config, tests,
receipts, docs, scripts/gates, meta, ggen, ggen-marketplace, arcus,
cgap/lsp-max, closure/SEED, KD registers, ontologies/config, deployment/CI,
binaries/artifacts, port/protocol, git history.

## Executive Summary

**What posh is.** Posh is the orchestration/membrane layer born in smalt
(docs/jira/posh/v26.10.30) as a Go tree plus an Elixir seam. The P1 census
reframes the whole integration: `lib/smalt/posh` in smalt is a pure-Elixir BEAM
reimplementation of the posh engine; the Go-linked `port.ex`/`client.ex` are
latent-never-armed — the in-process BEAM engine plays the control-plane face
today, not the Go membrane. The Go tree (`smalt/posh/`, 35.5K Go LOC, 192
files, module `smalt.dev/posh`, 305 tests per P2) is a latent membrane that has
never been the live path.

**Integration scale.** The census bounds the surface:
- 2-file Elixir seam (`lib/smalt/posh/port.ex`, `client.ex`) — latent.
- 4 load-bearing build integration points (P3): justfile imports
  (`justfile.d/58-posh.just`, `59-posh-ops.just`), 2 GitHub Actions, Elixir
  copy-out, port-9443 contract.
- 4 cross-language byte-contract pairs needing LOCKSTEP migration plus 3
  binary-path couplings (P4).
- 8 must-touch CI/deploy files (P16).
- Config: 5 files (posh.yaml + 3 allowlist copies + 2 k8s ConfigMaps); priv/ has
  zero posh config (P15).
- Docs: non-jira docs are thin — CLAUDE.md 5 refs, README 1 section (P6), meta
  README POSH section only (P8), ggen 6 doc files (P9), marketplace clean
  (P10), lsp-max clean, cgap ~230 refs are cgap's own lane history (P12).
- Receipts split into LAW (survive verbatim), COURT-RESULT (move with code),
  DECISION/HISTORICAL (P5). KD registers: canonical 0 rows, posh register 9
  rows (4 LAW / 3 COURT-RESULT / 3.5 OPEN) (P14).
- History: posh is 5 days old, 17 commits, mainline-only, open-source scrub
  already applied (P19) — rename is cheap precisely because the history is
  shallow.

## Phased Migration Plan

### Phase A — Go tree relocation

Move `smalt/posh/` (Go tree) to arcus under `internal/`, following P11's
package correspondence (arcus = 33 prior-art docs, zero posh code today; the
package correspondence is mapped, BLAKE3 sourcing divergence named as an open
decision below). Preserve git history with `git mv`-equivalent history-carrying
moves where the trees share a repo boundary — since posh's 17 commits are
mainline-only in smalt, the move is a smalt-side deletion + arcus-side import
committed as one atomic transition owned by the coordinator. Effort: M.
Risk: LOW-MED — mainline-only shallow history; risk concentrates in the 4
byte-contract pairs (P4) whose LOCKSTEP migration must not be split across
phases. Depends on: coordinator ruling on BLAKE3 sourcing.

### Phase B — Elixir seam rewiring (decision gate)

The 2 latent files `lib/smalt/posh/port.ex`/`client.ex` are never-armed. The
decision point: **arm them against arcus** (wire port.ex to the relocated Go
membrane, making the Go path live for the first time) **or retire them** (the
pure-Elixir engine is the live path and the Go membrane stays latent). Effort:
S per option. Risk: MED for arm (first-ever arm of a never-armed path = full
verification ladder on a brand-new transport), LOW for retire (deletion is a
permanent entropy sink per ERRC). Depends on: Phase A landing and the P18
ruling on membrane role post-cordon.

### Phase B/C gate — byte-contract LOCKSTEP

The 4 cross-language byte-contract pairs (P4) must migrate in lockstep: Elixir
and Go sides change in one coordinator-owned atomic transition, with the 3
binary-path couplings retargeted in the same wave. Splitting lockstep pairs
across phases or lanes is the top integration risk of the whole plan.

### Phase C — build/CI/k8s retarget

Retarget the 4 build integration points (P3: justfile imports
58-posh.just/59-posh-ops.just, 2 GitHub Actions, Elixir copy-out, port-9443
contract) and the 8 must-touch CI/deploy files (P16) to arcus paths. k8s
ConfigMaps (2) and allowlist copies (3) retarget in the same wave (P15). Note
(P18): posh does NOT bind 9443 in the serve path — UDS + optional TCP; the
port-9443 contract is a build/CI-facing artifact, not a runtime dependency.
Effort: M. Risk: MED (contract drift); the 7 gates' boot path has zero posh
post-W2 (P7), so gate scripts split BUILD/RUN-INT vs OPERATIONAL vs DOC —
only BUILD/RUN-INT scripts migrate, OPERATIONAL stays, DOC-side mentions are
doc-phase work. Depends on: Phase A.

### Phase D — docs/receipts/KD

- Receipts (P5): LAW receipts survive verbatim in place; COURT-RESULT receipts
  move with the code; DECISION/HISTORICAL receipts stay historical. No bulk
  rewrite of LAW rows. LAW rows are citations, not paths.
- Docs (P6/P8): CLAUDE.md 5 refs, README 1 section, meta README POSH section,
  ggen 6 doc files updated to point at arcus; no bulk doc migration.
- KD (P14): canonical register has 0 posh rows; posh register's 9 rows
  (4 LAW / 3 COURT-RESULT / 3.5 OPEN) move with the code; OPEN rows carry
  their close conditions to arcus.
- Closure/SEED (P13): migrate 2 seed rows, 1 law row, 4 historical rows, and
  `replay-commands.sh` path updates — scope precisely enumerated, no
  discovery work remains.
- Effort: S-M. Risk: LOW. Depends on: A (and B decision for COURT-RESULT
  placement).

## Open Decisions Requiring Coordinator Rulings

1. **BLAKE3 sourcing divergence** (named by P11): the Elixir-side blake3.ex and
   the Go tree source BLAKE3 differently. Ruling needed: unify on one
   implementation and one provenance receipt, or register the divergence as a
   KD row with a close condition.
2. **kv7 mount landed in smalt posh** — does it move to arcus with the Go tree
   or stay BEAM-face in the pure-Elixir engine? Recommendation: stay BEAM-face
   (live path) unless Phase B decides to arm.
3. **In-process BEAM engine vs Go membrane role post-cordon** (P18): the BEAM
   engine plays the control-plane face today; posh does not bind 9443 in the
   serve path (UDS + optional TCP). Ruling needed: is the Go membrane
   (a) promoted to live control plane, (b) retired, or (c) kept latent with a
   registered KD row and a close condition.

## 20-Lane Curriculum Dispatch Reconciliation

C0's census wave: 18/20 LANDED. The dispatch's remaining genuine work maps to
the 7 court-less modules, already dispatched as C1/C2. The census lanes P1-P19
are complete as read-only waves; no census lane has residue. The
rename/integrate wave's dispatch should therefore draw its lane partition from
this plan's phases, not re-run census lanes.

## What Does NOT Migrate

- The pure-Elixir engine (`lib/smalt/posh` minus the 2 latent files) stays in
  smalt — it is the live control-plane face (P1, P18).
- ggen-marketplace is clean — zero posh surface (P10).
- cgap's ~230 POSH lane IDs/courts/docs corpus are cgap's own history — they
  reference posh by name, not by path, and do not migrate (P12).
- LAW receipts survive verbatim (P5); DECISION/HISTORICAL receipts stay put.
- lsp-max is clean (P12).
