# DS-2 Receipt: complete.ex -> src/complete.rs (PG-2 Slice 4)

Standing: exit 0, CONFIRMED TWICE CONSECUTIVELY on the settled tree

Lane DS-2, DMEDI. Repo: /Users/sac/ggen, crate crates/ggen-sjira.
Date: 2026-11-03. Git: never run (lane contract).

## DEFINE

Charter: port `/Users/sac/ggen_igniter/lib/ggen_igniter/semantic_jira/complete.ex`
(349L) to `crates/ggen-sjira/src/complete.rs`. Source read first: `run/1`,
`sanitize/1`, the three-exit vocabulary `{0|1|2, map}`.

## MEASURE

Baseline at MEASURE time: `execute.rs` (E80-8) landed; `reconcile.rs`
(DS-1) NOT yet landed. Scope decision recorded at MEASURE: compose via
pub APIs only, and reconcile is NOT a missing dependency — the reconcile
hop lives inside `execute.rs`'s own chain (E80-8), so no
reconcile.rs stub was needed. When DS-1's `reconcile.rs` landed mid-build
later, complete.rs neither consumed nor stubbed it: the compose surface
stayed `ledger::fetch` + `frontier::frontier` + `execute::run` +
`authority::AuthorityIndex::build_pinned`.

Landed APIs this port composes (all read from source, all REUSED):
- `ledger::fetch` + `LedgerError` (src/ledger.rs) — the frontier hop's
  real ledger read; tampered/undecodable ledger refuses
  `frontier_refused` before anything runs.
- `frontier::frontier(orders, events, evidence, authority)` +
  `FrontierInput`/`EligibleCandidate`/`complete::BlockedEntry`
  (src/frontier.rs) — the same kernel frontier execute's loop uses.
- `execute::run` (E80-8), driven with a single-order input slice:
  execute's own loop closes exactly that order (its pass-2 frontier is
  empty) or refuses with the ledger byte-unchanged (its per-order
  fail-closed property). Eligibility seen here is eligibility execution
  enforces — so strictly that a per-order refusal mid-loop is
  structurally unreachable through this composition (the driver and the
  executor re-derive the identical frontier from the same ledger); the
  `order_refused` branch exists fail-closed anyway.
- `authority::AuthorityGraph` + `AuthorityIndex::build_pinned` — the ONE
  origin law; `authority=None` → exit-1 `authority_index_unavailable`.

## EXPLORE

Scope per PG-2: the additive security pre-step (complete.ex lines
169-245) is OUT — ggen-engine owns diff review; recorded as disclosed
deferred scope in the moduledoc. Three scoping findings, each recorded
in the module doc:

1. **Security pre-step OUT.** `security_blocked` /
   `security_review_diff_unavailable` refusal reasons are not ported;
   deferred to the ggen-engine slice that owns diff review.
2. **Pack dirs carried, not actuated.** The local kernel has no pack
   execution backend: the per-order resolution `packs_root/<target_pack>`
   is computed and surfaced in the per-order receipt echo
   (`resolved_pack_dir`), never actuated. Disclosed deviation: complete.ex
   reads the target pack from the loose order map
   (`order["target_pack"]`); the typed Rust `WorkOrder` refuses unknown
   fields (`deny_unknown_fields`), so the resolution input rides
   `CompleteOpts.target_packs` (identity-keyed BTreeMap).
3. **Authority file parsing stays in authority.rs** (`from_path`/
   `from_json`), where a present-but-unparseable graph already refuses
   typed; this module's `authority=None` → exit-1
   `authority_index_unavailable` marks the same boundary.

Exit vocabulary ported: `0` complete / `1` refused
(`order_refused` | `max_iterations_exhausted` with `remaining_eligible`
| `frontier_refused` | `authority_index_unavailable`) / `2`
invalid_invocation (pack-source exclusivity, before any ledger IO).
Default budget 1000 (complete.ex line 49); `sanitize/1` ported.

## DEVELOP

New: `src/complete.rs`, `tests/complete_test.rs` (8 tests).
`lib.rs`: one line added, `pub mod complete;` (survived a concurrent
DS-3 edit to the same file). Known stale prose: the lib.rs header lists
complete among "Deferred to later slices" follow-on targets —
stale-by-construction once complete.rs lands. One-line fix applied after
the concurrent edits settled (see IMPLEMENT).

## IMPLEMENT (gates, real output this session)

- `cargo test -p ggen-sjira --test complete_test` → **8 passed; 0
  failed** (witnessed final state after all fixes).
- Full suite `cargo test -p ggen-sjira` → 19 test binaries, all `ok`,
  exit 0, CONFIRMED TWICE CONSECUTIVELY on the settled tree (complete_test
  8 in every run; the binary also passed 3x consecutively in isolation).
  During the concurrent-edit window, aggregate runs twice showed flake
  classes while lanes were mid-edit: (a) a projection_test failure
  (passes 5/5 alone and in every settled-tree full run) and (b) three
  complete_test failures with an `exhausted (pass 64 of 64)` verdict on
  fresh ledgers — a fresh-ledger first append cannot return
  AlreadyRecorded, so that signature can only be in-flight execute.rs
  mid-edit state (the E80 lease/epoch lane was landing exactly then;
  execute.rs mtime postdates those runs). Neither reproduces on the
  settled tree. Recorded as in-flight artifacts, NOT defects in DS-2
  files; if the class reappears on a settled tree, it is a real
  cross-binary flake to file against projection/execute.
- `cargo clippy -p ggen-sjira --all-targets` → 0 warnings on complete.rs
  (other files' warnings are their lanes').
- `cargo doc -p ggen-sjira --no-deps` → exit 0; the one ambiguous-link
  warning pointing at complete.rs fixed; the 6 remaining doc warnings
  point at execute.rs/reconcile.rs/lease.rs (other lanes' files).
- lib.rs header prose updated (the stale "deferred" line) after the
  concurrent edits settled; `pub mod complete;` verified present
  (lib.rs lines 35-52 re-checked on disk).

## CONCURRENT-LANE SLA UNBLOCKS (disclosed in both receipts per law)

Three landed unblocks to other lanes' in-flight files, each under the
compile-freeze SLA while their owners were mid-edit; one attempted
unblock did NOT land (owner's own fix won the race) and is disclosed as
such:

1. `src/shacl.rs:86-88` (DS-3's file): collapsed a mid-edit split
   `std`/`.fs::` typo. One-line collapse, no semantic change.
2. `src/generated.rs:380-394` (DS-3's file, 8 compile errors):
   oxigraph 0.5.11 API renames, verified against the vendored oxigraph
   0.5.11 source before editing — `load_from(...)` →
   `load_from_slice(RdfParser::from_format(RdfFormat::Turtle), bytes)`;
   `GraphNameRef::DEFAULT_GRAPH` param dropped (parser targets the
   default graph); `QueryResults::Select` → `QueryResults::Solutions`;
   `GraphFormat` → `RdfFormat`. Exact sites only.
3. `src/reconcile.rs` (DS-1's file): appended two additive `From` impls
   (`LedgerError` and `PromoteRefusal` → `ReconcileRefusal`) for the
   `?` conversions their hops need. Module-level impls, no existing
   line touched; complete.rs composes execute::run for the reconcile
   hop, so these impls are independent of DS-2's compose surface.
4. `tests/execute_test.rs:264` (E80 lane): ATTEMPTED, DID NOT LAND —
   the file changed under my edit and the owner's own fix
   (lease/epoch_watermark fields, then a later state I re-read showing
   their final initializer) won. Item recorded as owner-resolved, not a
   DS-2 change.

## FALSIFIER

Falsifier for the module: a two-order backlog whose frontier empties
without closing both, or an exit that does not carry the typed refusal
reason. Falsified by `two_order_backlog_closes_both`,
`refusal_stops_loop_and_ledger_is_byte_unchanged`,
`max_one_refuses_with_remaining_eligible`,
`pack_dir_and_packs_root_are_mutually_exclusive`, and the 4 remaining
complete_test tests (8/8 green witnessed above).

## HANDOFF

complete.rs is the settled driver: exit vocabulary, fail-closed laws,
deferred scope, and the composition-reachability note are all pinned in
the module doc; the tests pin the receipt shape. No open flag for the
coordinator (the lib.rs prose is fixed; `pub mod complete;` verified on
disk).
