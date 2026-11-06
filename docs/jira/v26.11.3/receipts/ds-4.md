# DS-4 Receipt: projection surface (PG-2 Slice 6)

Standing: ALIVE

Lane: DS-4, DMEDI. Repo: /Users/sac/ggen, crate crates/ggen-sjira.
Date: 2026-11-03. Git: never run (lane contract).

## DEFINE

Charter: the orphaned `src/generated.rs` placeholder becomes the projection
surface: `project(order, pack_dir, target_dir, opts) → Result<ProjectionOutcome,
ProjectionRefusal>`. RE-SCOPE of
`ggen_igniter/lib/ggen_igniter/semantic_jira/projection.ex` (390L) — ruled a
re-scope because the Elixir module is IO-heavy through collaborators with no
ggen-sjira counterparts (PackLock, Reconcile, Actuate, GateVerify). Each hop
re-implemented at minimal lawful form; each divergence disclosed.

## MEASURE

Baseline: src/generated.rs was a 4-line placeholder (placeholder comment only;
no hand-written code had grown into it — the designated tenant is the
projection surface). Suite baseline at slice start:
`cargo test -p ggen-sjira` → 197 passed / 0 failed (19 binaries) per ds-1's
receipt; measured live at 197-equivalent pre-edit state (compile broke
mid-session on concurrent-lane files, self-healed by their owners).

Landed APIs REUSED (read from source, not invented):
- `work_order::digest_of` (canonical sorted-key sha256) — pack digest,
  render digest, and the tests' digest equality asserts all route through it.
- The ggen-engine `write.rs` `plan_write` decision table's default arm
  (path-escape refusal → unless_exists skip → absent→Written →
  identical→Unchanged → differs→refuse), cited from write.rs lines 201-209
  (FM-WRITE-005 silent-clobber law) and the leaf-first symlink probe
  (write.rs lines 558-577).
- Workspace oxigraph load/query precedent from
  `crates/ggen-graph/src/dialect.rs` (`load_from_reader`/
  `load_from_slice` + `#[allow(deprecated)]` over `Store::query` — the same
  allow the workspace ships in dialect.rs).
- In-crate oxigraph + ggen-graph deps landed by DS-3 (shacl.rs adapter).

## EXPLORE

Hop-by-hop mapping (each refusal typed, fail-closed):

| projection.ex hop | generated.rs form |
|---|---|
| `PackLock.check/2` + `digest_checked/1` | pack-tree digest via `digest_of` (sorted rel-path → bytes); no pack.lock in this crate |
| `{:capability_unknown, id}` | `CapabilityUnknown` — `projections/<id>.rq` must exist |
| `{:falsifier_gates_missing, id}` | `FalsifierGatesMissing` — falsifiers present but no `projections/<id>.unbound.rq` |
| staging copy (`File.cp_r/2`) | `tempfile::TempDir` staging copy, removed after run (explicit `remove_dir_all` + TempDir Drop) |
| render (`Reconcile.run/1`) | real Oxigraph SELECT over `ontology.ttl`, rows → deterministic JSON (sorted rows/keys), minimal Tera one-off bind (autoescape off — codegen, not HTML) |
| `{:bad_output_path, _}` | `BadOutputPath` — absolute / traversal / symlink-escape refused before any byte is written |
| `Actuate.write_file!/3` | guarded write tri-state `Written / Unchanged / SkippedExists`; differs → `GuardedWriteDiffers` (write.rs FM-WRITE-005's law) |
| `{:verification_failed, _}` | `VerificationFailed` — `gates/*.rq` ≥ 1 row, `verify/*.unbound.rq` = 0 rows, fail-closed with the actuated file on disk |

`{:pack_lock_failed, _}` / `{:target_pack_mismatch, _, _}` refusals have no
counterpart (no pack.lock, no `sj:targetPack` world check in this re-scope);
the pack-tree digest is the pinning surface.

## DEVELOP

New: `src/generated.rs` (placeholder replaced, ~600 lines), `tests/projection_test.rs`
(12 courts, ~370 lines). `lib.rs` untouched (module was already declared;
tests compose via `ggen_sjira::generated::*` pub APIs).

API: `project(order, pack_dir, target_dir, ProjectionOpts) → Result<ProjectionOutcome, ProjectionRefusal>`
with `ProjectionOutcome { order_id, capability_id, pack_digest, render_digest,
write_outcome, gate_results, target_path }` and tri-state
`WriteOutcome { Written, Unchanged, SkippedExists }` (snake_case serde).

Disclosed divergences (in the module docs, per the igniter moduledoc law):
1. No PackLock: pack pinned by deterministic pack-tree digest via
   `work_order::digest_of`.
2. Minimal Tera one-off bind vs the Elixir's Reconcile pack-template render —
   pack template FILES are not yet consumed; follow-on.
3. Row values use oxigraph term `Display` (RDF literals carry quotes) —
   the serialization law pinned by the happy-path court.
4. `#[allow(deprecated)]` over `Store::query`, mirroring the workspace
   precedent in ggen-graph dialect.rs.

## IMPLEMENT (gates + exits)

```
cargo test -p ggen-sjira --test projection_test  → 12 passed; 0 failed
cargo test -p ggen-sjira                         → 19 binaries, all
                                                   "test result: ok"; 211
                                                   passed; 0 failed
```

Test court inventory (all Chicago: real mini-pack on real tempdirs, real
Oxigraph SPARQL, real guarded writes; state asserts, no mocks):

- happy path: Written receipt, digest equality (render digest == digest of
  the exact bytes on disk), gate census all-pass, staging-removal differential
  court (snapshot before, no NEW staging dir after — tolerant of pollution
  from earlier aborted runs)
- re-run → Unchanged, no rewrite (file bytes compared before/after), stable
  pack digest
- differing target content → GuardedWriteDiffers, existing file untouched
- unless_exists + existing → SkippedExists, file untouched
- unknown capability → CapabilityUnknown, no byte written
- traversal (`../escaped.rs`) → BadOutputPath, nothing lands outside root
- falsifiers without `.unbound.rq` → FalsifierGatesMissing
- projected falsifier gates ride the census (2 falsifiers → 2 pass gates);
  original pack gains NO verify/ dir (staging law file-state assert)
- zero-row gate → VerificationFailed, actuated file on disk (fail-closed
  honesty), refusal names the failing gate
- pack byte change moves pack_digest; write stays Unchanged (digest is the
  pinning surface, not the write)
- template-less render → deterministic JSON rows

## Concurrent-lane notes (disclosure)

- reconcile.rs broke mid-session (concurrent lane in-flight); self-healed by
  its owner before my gate run. execute_test.rs broke once (ExecuteOpts
  gained fields); also self-healed. Both judged on the settled state.
- generated.rs was externally touched on disk twice mid-flight (formatter /
  concurrent pass); each time the current disk state was adopted, never
  reverted.
- Cargo.toml is a shared seam: this lane added ONLY the two Slice-6 dep
  lines (tempfile, tera) plus their disclosure comment.

## Standing

- Projection surface (generated.rs) + 12 courts: ALIVE (commands + exits
  above, this session).
- Files owned by this lane: src/generated.rs, tests/projection_test.rs,
  Cargo.toml (two dep lines), docs/jira/v26.11.3/receipts/ds-4.md.
- PG-A6 overlap check (charter note): crates/cgap-aoq is absent from this
  workspace (no crates/cgap-aoq directory); ggen-sjira's generated.rs is its
  own crate surface — no naming overlap to disclose.
- Follow-ons (not this slice): pack template-file consumption (Tera files
  from the pack), pack.lock integration, `ggen-graph run_query(store, q)`
  seam when ggen-graph exposes it.
