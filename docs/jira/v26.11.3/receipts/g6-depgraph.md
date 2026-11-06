# WO-07 Receipt — Depgraph Hardening (lane G6)

Standing: PASS

Date: 2026-11-03. Canonical checkout `/Users/sac/ggen`. No git commands run (coordinator owns transitions).

## Subject

- `crates/ggen-abb-sbb/src/depgraph.rs` (owned; edited)
- `crates/ggen-abb-sbb/tests/depgraph_hardening_test.rs` (owned; new, 13 tests)
- `crates/ggen-abb-sbb/tests/property_admission_test.rs` (NOT owned — one-line minimal unblock: added `requires: BTreeSet::new()` to the `ConsumerEdges` literal at :373, disclosed here per the lane SLA; owning lane to supersede)

## Landed

1. **Port normalization law — reject-at-extract.** Canonical port = non-empty, whitespace-free,
   lowercase `[a-z0-9-]`. `is_canonical_port` + `canonical_port_set` enforce at extract time in
   `extract_pack_manifest` (`provides`/`requires`) and `extract_consumer_edges` (`requires`).
   Non-canonical ports refuse `MalformedGraph { reason: "NON_CANONICAL_PORT: pack '<pack>' field '<field>' port '<port>' …" }`,
   naming pack + field + port. Non-string port entries now refuse instead of the previous silent
   `filter_map` drop. Trim/case-fold-at-comparison law was rejected per WO-07 (fail-open by
   normalization); no RESOLUTIONS append needed since the mandated law landed.
2. **Doc-comment honesty.** Reworded `depgraph.rs:1-7` module doc. **Premise correction
   (disclosed):** WO-07 defect 2 claimed the module's collision check is intra-pack only — false.
   Gate 2 (`resolve_sync_order`) refuses two packs in one graph writing the same path, pinned by the
   pre-existing test `refuses_duplicate_artifact_path_across_packs` (ran green this session). The
   reword therefore states verified scope: Gate 2 refuses within-graph cross-pack collisions;
   collisions across separately composed graphs remain the composer's responsibility. No cross-pack
   logic was added or removed (fence honored).
3. **Consumer `requires`.** `ConsumerEdges` gains `requires: BTreeSet<String>` (`#[serde(default)]`,
   legacy `ggen.toml` without `[graph] requires` parses unchanged — court-pinned). Extracted in
   `extract_consumer_edges` under the same canonical-port law; Gate 4 checks consumer requires
   against ALL packs' `provides` (consumer consumes every pack); unbound consumer port refuses
   `UnboundPort { pack: <consumer name> }`. Both directions court-pinned (satisfied passes through
   Gate 4; unsatisfiable refuses naming the consumer).
4. **Closure bound.** Fixpoint factored into `pub fn closure_fixpoint(edges, max_iterations)` with a
   typed refusal on exceed: `MalformedGraph { reason: "CLOSURE_BOUND_EXCEEDED: …" }`. Production
   bound = `2*|packs|^2` with floor 16. Justification (documented at the call site): a converging
   fixpoint over V nodes needs ≤ V passes, so 2*V² + floor is generous headroom — hitting it means
   the rules cannot converge. **Design note (disclosed):** with fixed edges no input graph can trip
   the production bound (≤ V passes needed, bound ≥ 2V²), so the court fires the guard through the
   `closure_fixpoint` seam with a tight bound on a real 6-node chain that genuinely needs more
   passes — real collaborator, real refusal observed; the same chain converges through the real gate
   at the production bound (no false positive).
5. **Kahn re-refusal kept loud**, comment now cites the invariant (Gate 3 refuses
   `transitive_dep(P,P)`; non-empty `remaining` with no ready node implies a cycle Gate 3 must have
   refused; reachable only on a resolver invariant violation, never a partial order).

## Deviation from WO-07 / dispatch contract (disclosed, flip-ready)

The dispatch contract and WO-07 both name additive `Refusal` variants
(`NonCanonicalPort`, `ClosureBoundExceeded`) in `lib.rs`. `lib.rs` is fenced — refusal surface is
lane L01/G3's ownership row, and my dispatch contract says "Do NOT touch lib.rs". Both refusals
land as **typed-code-prefixed `MalformedGraph` reasons** (`NON_CANONICAL_PORT:` / `CLOSURE_BOUND_EXCEEDED:`),
not dedicated variants. Per R2's spirit these are typed codes across the boundary, but they are
prose-embedded, which R2 disfavors. Flip path: when L01 adds the variants, replace the two
`MalformedGraph` constructions in `depgraph.rs`; the courts pin the
`contains("NON_CANONICAL_PORT")` / `contains("CLOSURE_BOUND_EXCEEDED")` substrings, which survive a
variant flip.

## Mutation non-vacuity (acceptance 7)

Baseline sha256(depgraph.rs) = `befbbae91ee71d50d7f537bb307c24c53937a8f45004efa814392e6e486b36ed`;
each shim followed by byte-exact restore (sha re-read from disk after restore).

| Shim | Mutation | Court | Under mutation | After restore |
|---|---|---|---|---|
| M1 | `is_canonical_port` → accept-all | `case_variant_port_provider_side_refuses` | FAILED (exit 1) | PASS, sha match |
| M2 | bound guard disabled (`if false &&`) | `closure_bound_guard_fires_on_chain_graph_with_tight_bound` | FAILED (exit 1) | PASS, sha match |
| M3 | overclaiming comment restored (+ marker) | scope-honesty grep gate | grep shows unqualified claim at :4 | grep qualified-only, sha match |
| M4 | consumer-requires check disabled | `consumer_requires_unsatisfied_refuses_naming_consumer` | FAILED M4 court exit 1 | PASS, sha match |

Shims applied via python replace with `assert`-on-no-match (fail-closed); restore via `cp` from
/tmp snapshot, sha256 verified after each restore.

## Verification ladder (commands + exits)

- `cargo test -p ggen-abb-sbb depgraph` → 11 unit + 3 property tests ok, exit 0
- `cargo test -p ggen-abb-sbb` → 103 passed, 0 failed across 9 binaries, exit 0 (witnessed twice;
  first full run had a transient `refusal_crosswalk_test::court_every_wire_code_has_ctor` FAIL that
  passed on immediate rerun — I touched no refusal surface; recorded as suspected concurrent-lane
  contention on `generated/refusals.rs`, not a regression from this lane)
- `cargo build -p ggen-abb-sbb` → Finished, exit 0
- `grep -n "artifact collisions" crates/ggen-abb-sbb/src/disk:4,216` — both scope-qualified
  (Gate-2 phrasing: "two packs writing one path")
- Prior property tests present and green unchanged:
  `depgraph_closure_matches_independent_bfs_on_100_random_dags`,
  `depgraph_closure_matches_bfs_with_dense_and_empty_edges`,
  `depgraph_topological_order_respects_every_edge` (3 passed, exit 0), and internal
  `refuses_cycle_with_real_path` / `refuses_unbound_port` untouched.

## Standing

ALIVE for the landed surface (commands above, exits 0). **PARTIAL on refusal variants**: the
`NonCanonicalPort`/`ClosureBoundExceeded` dedicated `Refusal` variants remain UNLANDED pending L01's
lib.rs refusal surface (fenced to me); landed as typed-code-prefixed `MalformedGraph` until flip.
Standing update gated on L01's landing.

## Falsifiers status

All WO-07 falsifiers cleared except the variant-premise one, registered as a disclosed deviation
above. WO-07's defect-2 premise (intra-pack-only collision check) is REFUTED by Gate 2 + its pinning
test; the honest-comment deliverable landed with corrected scope.
