# SJ-1 Receipt — ggen-sjira PG-2 Wave-2 Slices

Lane: SJ-1 (IMPLEMENTATION), wave 2. Repo: /Users/sac/ggen, crate
`crates/ggen-sjira`. Coordinator owns git; this lane ran no git commands.

## Commands + exits

| Command | Exit | Result |
|---|---|---|
| `cargo test -p ggen-sjira` (baseline, before edits) | 0 | 125 passed, 0 failed |
| `cargo test -p ggen-sjira` (final gate) | 0 | **146 passed, 0 failed** |
| `cargo doc -p ggen-sjira --no-deps` | 0 | clean of new warnings; 1 pre-existing `lease.rs:16` unresolved-link warning remains (other lane's file, disclosed) |

## Slice standing (PG-2 roadmap)

| Slice | Standing | Notes |
|---|---|---|
| 0 — lib.rs:15-19 stale doc fix | **ALIVE (landed)** | Deferred list now cites the real ported modules; promote/descriptor noted as landed this slice |
| 7 — tests/unwitnessed_refusal_test.rs | **ALIVE (landed)** | 4 tests, test-only (no src changes) |
| 1 — src/promote.rs | **ALIVE (landed)** | 7 unit tests in-module |
| 2 — src/descriptor.rs (~1100L) + tests/descriptor_test.rs (356L, 10 tests) | **ALIVE (landed)** | Two refusal enums per PG-2's list, verbatim |
| 3 (reconcile.rs) | DEFERRED | needs promote + ledger/authority compose; next wave |
| 4 (complete.rs) | DEFERRED | needs Slices 1+2+3 |
| 5 (pack: shacl.rs adapter + target_pack.rs) | DEFERRED | needs ggen-graph dep added to Cargo.toml |
| 6 (generated.rs / projection) | DEFERRED | needs Slice 5 |

## Landed surface

- `src/promote.rs` — `promote(order, target, evidence)` →
  `Result<PromotionIntent, PromoteRefusal>`; transition_digest via
  `work_order::digest_of` over the intent minus its digest key; pure,
  IO-free; 7 typed refusal variants (PG-2's list plus two disclosed
  additions: `AcceptanceUnwitnessed`, `InheritedStanding`).
- `src/descriptor.rs` — `SCHEMA`, `identity/1`, `build/4`, `tail_digest/1`,
  `build_xaas_contract/4`, `receipt_from_xaas/3` (full ten-refusal matrix),
  `receipt_digest/1`; `DescriptorRefusal` + `ReceiptRefusal` +
  `CourtMapRefusal` enums per PG-2's table verbatim;
  `DescriptorOpts<'a>`/`BuildAttrs` carry options.
- `tests/unwitnessed_refusal_test.rs` — RadiusTruncated depth-cap path
  (controller.rs:286) journaled + verified chain; boundary law pinned:
  a chain of exactly DEFAULT_MAX_DEPTH links truncates (deepest caller pops
  at depth == cap), the clean boundary is cap-1 links;
  MalformedWorkOrder refusal journaled through journal.record with
  `refusal:sha256:` digest class, controller-path refusals use the
  controller's `blake3:` refusal-digest class — the two classes are pinned
  as distinct in the tests.
- `tests/descriptor_test.rs` — Chicago: real frontier/admit/digest
  machinery, state assertions on returned Values; the ten-refusal matrix
  (one refusal per reason, plus digest round-trip and alive guards).

## Elixir↔Rust mapping notes (for Slice 3/4 lanes)

- `digest/1`/`digest_exact/1` ≡ `work_order::digest_of` (canonical
  sorted-pair sha256); `receipt_digest/1` = digest_of minus the
  receipt_digest key.
- `standing/1` target law: `UNKNOWN` refuses `UnknownStanding`; an
  unparseable name refuses `InvalidTarget` — admission owns UNKNOWN.
- The Elixir `promotion_checks/4` order is preserved; the igniter returns
  the failed-name LIST, this port fails on the FIRST failed check — the
  failed-list shape is a successor-slice option if reconcile needs it.
- `Reconciler.project/2`'s standing fold is inlined in descriptor.rs
  (`projected_orders`) — last event by `seq` wins; Slice 3's reconcile.rs
  should reuse or promote it, not re-derive it.
- `tail_digest/1` is inlined in descriptor.rs (genesis =
  `sha256:` + 64 zeros); Slice 3 should import from descriptor or move to
  ledger.rs.
- `receipt_from_xaas` check order is load-bearing: bridge shape → digest
  → bridge equality → final_head → outcome → head_verified → alive
  guards. Digest runs BEFORE head_verified, so any body mutation must
  re-seal before other refusals are observable (pinned in the test
  matrix).
- Frontier eligibility vs admission order: an unadmitted row surfaces at
  the frontier stage as `NotOnFrontier("admission_refused: …")` — the
  fail-closed gate order, not the Elixir `Unadmitted` variant. `Unadmitted`
  stays for the post-frontier admit path.

## Disclosures

1. Two extra PromoteRefusal variants beyond PG-2's list:
   `AcceptanceUnwitnessed{name}`, `InheritedStanding` — the Elixir failed-
   check list has them; folding them into other variants would have
   conflated classes.
2. `build/4`'s `package` key is caller-supplied via `DescriptorOpts::package`
   (kernel `execution_package/4` is unported); absent ⇒ key omitted, never
   invented. Dependencies list present-but-empty (core WorkOrder carries no
   dependencies field); `DependencyNotAlive`/`UnsupportedRequiredStanding`
   declared but unreachable until the field lands.
3. render/1 and to_a2a_task deferred with typed rationale in the module
   doc (`IdentityMismatch` declared, carries no behavior).
4. Slice-0 lib.rs doc edit + this receipt are the only non-crate files
   touched. No git commands run. No worktrees. Compile-freeze SLA: no
   minimal-unblock applied to any other lane's file.
5. Environment disclosures: two baseline runs failed on shared-target
   churn from concurrent lanes (missing rlib/rmeta mid-compile, sccache
   hashing deleted artifacts) — transient, resolved by re-run; the 125-test
   baseline was witnessed green on a clean run before edits landed.
6. `projected_orders`/`tail_digest` inlined per the roadmap's
   "inline in descriptor.rs" instruction; Slice 3 may promote them into
   reconcile.rs rather than re-derive.

result: 146/146 tests green; Slices 0/7/1/2 of PG-2's roadmap landed in
ggen-sjira (lib.rs doc fix, unwitnessed-refusal court, promote.rs,
descriptor.rs) with full refusal matrices journaled through the blake3
chain; Slices 3-6 deferred with dependency order recorded.
