# DS-1 Receipt: reconciler.ex -> src/reconcile.rs (PG-2 Slice 3)

Lane: DS-1, DMEDI. Repo: /Users/sac/ggen, crate crates/ggen-sjira.
Date: 2026-11-03. Git: never run (lane contract).

## DEFINE

Charter: port `ggen_igniter/lib/ggen_igniter/semantic_jira/reconciler.ex` (188L)
to `crates/ggen-sjira/src/reconcile.rs`. Read the Elixir source first: `reconcile/4`
(fetch -> definition check -> snapshot well-formedness -> idempotent replay ->
admit -> origin law -> vc law -> candidate_sha -> project -> promote -> append),
`project/2` + `frontier/4` (log-projection wrappers over the igniter kernel),
`tail_digest/1` (chain tail, genesis for empty log).

## MEASURE

Baseline (before any edit):

```
cargo test -p ggen-sjira  ->  146 passed; 0 failed  (13 binaries)
```

Landed APIs this port composes (all read from source, all REUSED):
- `ledger::fetch/append/AppendStatus/AppendOpts/vc_concurrent/LedgerError`
  (src/ledger.rs) - the ledger read is the first court; `AppendStatus` is
  reused for the idempotency signal, never a parallel enum.
- `promote::promote(&AdmittedWorkOrder, &str, &Value)` (src/promote.rs,
  PG-2 Slice 1) - the pure promotion calculus, consumed directly.
- `authority::AuthorityIndex::build/resolve` + `OriginRefusal` (src/authority.rs).
- `admit::admit_work_order` + `valid_digest` (src/admit.rs).
- `work_order::digest_of` (canonical sorted-key sha256).
- frontier progression law (UNKNOWN never skips to ALIVE) re-applied in the
  projection fold; `Standing::parse` reuses the vocabulary.

## EXPLORE

Every Elixir `{:refused, reason}` mapped to a typed `ReconcileRefusal` variant:

| Elixir | Rust |
|---|---|
| `{:ledger_refused, reason}` | `Ledger(LedgerError)` |
| `:definition_mismatch` | `DefinitionMismatch { expected, found }` |
| `:invalid_snapshot_digest` | `InvalidSnapshotDigest { found }` |
| `:already_recorded` | NOT a refusal: `Ok(Reconciled{status: AppendStatus::AlreadyRecorded})` reusing `ledger::AppendStatus` |
| work-order admission refusal | `Unadmitted(crate::admit::Refusal)` |
| `{:authority_index_unavailable, path, _}` | `AuthorityIndexUnavailable { detail }` (None index refuses fail-closed) |
| `{:origin_not_admitted, refusal}` | `OriginNotAdmitted(OriginRefusal)` |
| `:candidate_sha_mismatch` | `CandidateShaMismatch { expected, found }` |
| `{:vc_concurrent, incoming, last}` | `VcConcurrent { incoming, last }` |
| promote refusal | `PromotionRefused(PromoteRefusal)` |

REUSED, never invented: `ledger::AppendStatus`, `LedgerError`, `admit::Refusal`,
`OriginRefusal`, `PromoteRefusal` (each embedded, not duplicated).
`frontier/4` + `project/2` wrappers are NOT ported as separate functions: the
reconcile path folds the projection inline (standing vocabulary +
progression law), and the frontier module already owns that surface.

## DEVELOP

New: `src/reconcile.rs` (372 lines + fixes), `tests/reconcile_test.rs` (330 lines).
`lib.rs`: `pub mod reconcile;` + re-exports
(`reconcile`, `tail_digest`, `ReconcileOpts`, `ReconcileRefusal`, `Reconciled`, `GENESIS_DIGEST`).

API: `reconcile(order_json, receipt, ledger_path, opts) -> Result<Reconciled, ReconcileRefusal>`
with `Reconciled { event: Value, status: AppendStatus }`;
`tail_digest(&[Value]) -> String`; `GENESIS_DIGEST = "sha256:" + 64 zeros`.

Disclosed divergences (in the module doc, per the igniter moduledoc law):
1. `candidate_sha` law is checked BEFORE admission: the fifteen-field
   `WorkOrder` struct carries no `candidate_sha` field and admission refuses
   unknown fields, so checking after admission would make the variant
   unreachable; the mismatch is the more precise refusal, and the order then
   still refuses `Unadmitted`. The Elixir nil-is-vacuous semantics are
   preserved (absent order candidate_sha never trips the law).
2. `ReconcileOpts.authority` is `Option<AuthorityIndex>`; `None` refuses
   `AuthorityIndexUnavailable` (fail closed). The Elixir default (canonical
   semantic-jira-pack index) has no Rust equivalent - a caller must supply
   the index.
3. The Elixir `project/2`/`frontier/4` wrappers are folded into the reconcile
   projection; the frontier module owns the standalone surface.

## IMPLEMENT (gates + exits)

```
cargo test -p ggen-sjira --test reconcile_test   -> 13 passed; 0 failed
cargo test -p ggen-sjira reconcile               -> 0 failures (filter run)
cargo test -p ggen-sjira                         -> 197 passed; 0 failed (19 binaries)
```

Test court inventory (all Chicago: real ndjson ledgers in temp dirs, real
sha256, real authority JSON graphs; file-state asserts on the ledger):

- happy path appends exactly 1 event (event count read back from disk)
- second identical call -> AlreadyRecorded, ledger still exactly 1 event on disk
- definition_mismatch / invalid_snapshot_digest / candidate_sha_mismatch
  refusal courts (each also asserts nothing written where reachable)
- vc_concurrent court: {"a":1} then {"b":1} - neither dominates - refuses
  BEFORE writing (ledger still 1 event); ordered vc {"a":2} stamps + appends
  (from=PARTIAL_ALIVE carried on the second event)
- tampered ledger (event body mutated on disk) -> ReconcileRefusal::Ledger
- origin-unresolved (fresh IRI) -> OriginNotAdmitted; missing index ->
  AuthorityIndexUnavailable; unwitnessed court -> PromotionRefused; malformed
  order -> Unadmitted
- tail_digest: genesis for empty log, last event_digest otherwise

## Concurrent-lane notes (disclosure)

- Mid-run the shared surface ExecuteOpts changed twice under me (a
  concurrent lane landing epoch/lease fields); execute_test.rs broke and
  self-healed when the owning lane landed its fix. Never touched by this
  lane; my gate was judged on the settled state.
- The two lib warnings (unused BTreeMap import in pack_health.rs, deprecated
  oxigraph query in generated.rs) are pre-existing/concurrent-lane, not
  introduced by DS-1.

## Standing

- reconcile.rs port + 13 courts: ALIVE (commands + exits above, this session).
- promote.rs integration: ALIVE (promote() consumed directly by reconcile).
- tail_digest/GENESIS_DIGEST: ALIVE.
- Files owned by this lane: src/reconcile.rs (new), tests/reconcile_test.rs
  (new), src/lib.rs (module + re-export lines only), this receipt.
