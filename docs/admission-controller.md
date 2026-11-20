# Admission Controller — Reverse-80/20 §5 Synthesis (R80-14)

The admission controller is the ordered, fail-closed gate pipeline that turns
a batch of proposed edits into one journaled decision: authority → blast
radius → sjira admission → transition journaling. This document is the §5
synthesis: every component is cited to its landed file, every R80 item to
its landed or deferred status. Status vocabulary: ALIVE (ran this session,
cited), PARTIAL, UNVERIFIED, REFUSED, DEFERRED.

## Components

| # | Component | Landed file(s) | Receipt | Status |
|---|-----------|----------------|---------|--------|
| 1 | State transition oracle (admission kernel) | `crates/ggen-sjira/src/admit.rs`, `work_order.rs` | x1-sjira-kernel | ALIVE — `cargo test -p ggen-sjira controller` green |
| 2 | Frontier / selection oracle | `crates/ggen-sjira/src/frontier.rs` | — | ALIVE (landed by a sibling lane this wave; consumed read-only) |
| 3 | Deterministic pre-execution validation: blast radius (R80-6) | `crates/ggen-engine/src/blast_radius.rs` (gate) + `crates/ggen-sjira/src/controller.rs` (composed gate) | this wave, r80-14 | ALIVE — radius closure + protected-path intersection witnessed by `radius_closure_reaches_callers_and_protected_intersection_refuses` |
| 4 | SCIP symbol index (R80-3) | — | — | DEFERRED — the `SymbolGraph` trait is the seam in both `blast_radius.rs` and `controller.rs`; a SCIP index implements it unchanged (open items) |
| 5 | Compiler-assisted execution: transactional journal (R80-5) | `crates/ggen-engine/src/transactional.rs` | this wave | ALIVE — `FileEdit`/`TransactionalEdit`/`Journal` landed |
| 6 | Passive feedback: ggen-lsp diagnostics (R80-12) | `crates/ggen-lsp/src/analyzers/` | — | ALIVE as author-time analyzers (`GGEN-*`, `E001x`); deferred: server-driven applyEdit into editors |
| 7 | Cryptographic logging (R7 = R80-7) | `crates/ggen-sjira/src/receipts.rs` `TransitionJournal` | — | ALIVE — blake3-chained transitions; tamper located at depth, witnessed in-repo |
| 7b | Cryptographic logging (journal — code half) | `crates/ggen-sjira/src/controller.rs` `propose` | r80-14 | ALIVE — every outcome journaled; "radius never ran" is tamper-evident (`blast_radius_digest: None` in the chain) |
| 8 | Authority index + pinning | `crates/ggen-sjira/src/authority.rs` | — | ALIVE (sibling lane) — `pins()` pins IRI→digest, ambiguous pairs pin nothing (fail closed) |
| 9 | Ledger (event journal, epoch markers) | `crates/ggen-sjira/src/ledger.rs` | — | one minimal unblock applied this session (disclosed) |
| 10 | Edits→WorkOrder bridge | `crates/ggen-sjira/src/edits_bridge.rs` | — | one minimal unblock applied this session (disclosed) |
| 10b | Composition function | `crates/ggen-sjira/src/controller.rs` | r80-14 | ALIVE — `propose(...)` with ordered fail-closed gates |
| 11 | Compositor deployment of ggen-lsp | — | R80-13 | DEFERRED — open item |
| 12 | Durability of the transition journal (disk) | — | — | DEFERRED — journal is in-memory (receipts.rs module docs) |

## The composed pipeline (`propose`)

```
propose(edits, order, graph, authority, journal, protected_paths)
  │
  ├─ Gate 1 Authority: origin_authority pinned in the injected index?
  │    no → REFUSED{authority}  — journal(None)   (radius never runs;
  │                                               sjira never runs)
  ├─ Gate 2 Blast radius: callers*/subtypes* closure over injected
  │    SymbolGraph; any def site on a protected surface →
  │    REFUSED{blast-radius} — journal(Some(radius))  (sjira never runs)
  │    truncated closure → REFUSED fail-closed (lower bound ≠ proof)
  ├─ Gate 3 sjira admission: admit_work_order — typed refusals,
  │    standing-at-UNKNOWN, authority stamped NONE
  └─ Gate 4 Journal: every outcome appended to TransitionJournal
       (blake3 chain); decision carries journal_hash + head
```

Design laws realized by `propose`:

- **Fail closed**: missing `origin_authority` field, unpinned IRI, truncated
  radius — all refuse rather than degrade to a pass. PARTIAL cannot certify
  ALIVE: a truncated radius is a lower bound, so it refuses instead of
  passing on an unproven radius (matches ggen-engine's R80-6 gate, which
  degrades clean verdicts to PARTIAL under truncation — here the composed
  controller refuses outright; the stricter choice is disclosed).
- **Fail closed**: missing `origin_authority` field, unpinned IRI, truncated
  radius — all refuse rather than degrade to a pass. A truncated radius is a
  lower bound, so it refuses instead of passing on an unproven radius
  (stricter than ggen-engine's R80-6 gate, which degrades a clean verdict to
  PARTIAL under truncation; the stricter choice is disclosed here).
- **Refusal receipts are as strong as admission receipts**: every outcome —
  admitted OR refused at any stage — is appended to the blake3-chained
  `TransitionJournal`, so a denial is tamper-evident and located by depth,
  never silent. The chain also records `blast_radius_digest: None` on
  authority-stage refusals, making "the radius never ran" a chain fact, not
  a claim.
- **Ordered halting**: each gate early-returns; the authority gate runs
  before the radius, and the radius gate runs before the sjira kernel, so a
  critical-path write is never admitted on a malformed order and an
  unpinned authority is never radiused at all. The ordering laws are
  witnessed by the negative tests in `controller_test.rs` (a malformed
  order + protected-path edits must yield the BLAST-RADIUS refusal, not the
  admission one; an unpinned authority + protected-path edits must yield
  the AUTHORITY refusal with `radius_digest: None`).
- **Authority NONE invariant**: `propose` never mints authority; the
  admitted decision carries the stamped `NONE` from `admit_work_order`, and
  the authority gate only CHECKS pins (via the injected `AuthorityPin`
  seam, implemented for the `BTreeMap<String, String>` that
  `authority::pins` returns).
- **IO-free composition**: every collaborator is injected — the symbol
  graph and authority index as traits (the R80-3 SCIP index implements the
  same seam unchanged), the journal as `&mut TransitionJournal`.

## Open items (full blueprint remaining)

| Item | Blueprint ref | Gap | Unblocks |
|---|---|---|---|
| SCIP cross-repo symbol index | R80-3 | `SymbolGraph` is trait-only; the fixture impls are hand-written | callers*/subtypes* radius over the whole workspace, cross-repo |
| Server-driven applyEdit into editors | R80-12 (apply half) | ggen-lsp diagnostics are author-time analyzers only; no applyEdit round-trip | closed feedback loop editor⇄controller |
| Compositor deployment of ggen-lsp | R80-13 | ggen-lsp is not deployed by the compositor | passive feedback at edit time |
| Journal durability | receipts.rs module docs | `TransitionJournal` is in-memory | replay across processes |
| Frontiers → controller wiring | blueprint §5 | `propose` is not yet called from the CLI/compositor write path | end-to-end ALIVE from `ggen sync` |
| Protected-path convention unification | R80-6 | engine gate uses crates-relative paths; real repo paths carry `crates/` | one convention for both gates |
