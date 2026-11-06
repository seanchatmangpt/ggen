# waves-g1-g8

Wave closure record for the v26.11.3 G1-G8 lane waves (WO-01 through WO-07
closed; WO-08/09 remain in-progress). Written 2026-11-03 by the lane-close
lane. All gates re-run this session; exits recorded in `../RECEIPTS.md`.

## Standing summary

7 of 9 work orders closed ALIVE; 2 in-progress. Zero gates red at close.

| WO | Deliverable | Standing |
|---|---|---|
| WO-01 | Stage 2c admission wiring + CLI flag + fail-closed depgraph | ALIVE |
| WO-02 | `ggen ea` CLI surface (plan/admit/manufacture/replay/verify) | ALIVE |
| WO-03 | Compose plan persistence + topological order | ALIVE |
| WO-04 | Canonical EA graph schema + parity court + Turtle projection | ALIVE |
| WO-05 | Typed boundary-refusal fences (generated registry) | ALIVE |
| WO-06 | Envelope court reconcile (R7/R8/R9/R10) | ALIVE |
| WO-07 | Depgraph hardening + typed-refusal flip | ALIVE |
| WO-08 | arcus EA graph consumer | IN-PROGRESS |
| WO-09 | Working example end-to-end | IN-PROGRESS |

Disclosure: WO-09 receipts (`x4-consumer-guide.md` ALIVE, K6's
`ea-run-transcript.md` ALIVE) landed on disk at 13:59 during this
lane-close run. This record does not close WO-08/09 — that close is the
coordinator's call; rows above reflect the lane-close contract scope.

## Landed deliverables per WO

- WO-01: `abb_envelope_caps` in
  `/Users/sac/ggen/crates/ggen-engine/src/envelope.rs`; Stage 2c region in
  `/Users/sac/ggen/crates/ggen-engine/src/sync.rs` (FM-LAW-015 fail-closed
  default, digest pinning, caps wiring); `SyncOptions.skip_ea_admission`;
  k8 closed G1's CLI-flag blocker (flag in `ggen_cli_nouns.rdf` + verb
  threading) and flipped the pre-existing fail-open Stage 2c block 1
  (`FM-PACK-015` typed refusal on malformed/refused manifests).
- WO-02: `crates/ggen-cli/src/cmds/ea/` (plan/admit/manufacture/replay/
  verify_receipt + mod registry); `ea_verbs_test.rs` 7 courts; full binary
  demo loop witnessed against the kernel fixture.
- WO-03: `PackCompositionPlan` serialization + `transitive_deps` closure;
  stale `order.sort()` canonicalization removed from the compose verb;
  `--out` persistence flag; determinism re-pinned to the kernel's BTreeSet
  Kahn tie-break; shim-court flip witnessed (re-inserted sort → RED).
- WO-04: `ontologies/shapes/ea-graph.shacl.ttl` + parity court (4/4) in
  ggen-config; X3's Turtle→JSON projection (one-rule ggen manifest + 7
  per-node .rq + Tera template) byte-identical to the kernel fixture,
  admitted by kernel and Stage 2c gate; projection court 5/5.
- WO-05: boundary-refusal SHACL + generation rule in `/Users/sac/ggen/ggen.toml`;
  regenerated `crates/ggen-abb-sbb/src/generated/refusals.rs` via real
  `ggen sync run` (determinism witnessed byte-identical x3 runs);
  refusal_crosswalk court (registry vs generated vs rule VALUES, both
  directions).
- WO-06: R7 verified landed (G3, envelope_court.rs digest-before-policy);
  R8 doc comment added naming `verify_receipt` as freshness authority;
  R10→R11 supersession row in RESOLUTIONS.md (`measured <= declared`
  ceiling law supersedes exact-equality R10); R9 `ENVELOPE_CAP_FALLBACK`
  structured warn in `crates/ggen-engine/src/envelope.rs`.
- WO-07: canonical-port law reject-at-extract; consumer `requires` under
  the same law; `closure_fixpoint` typed bound
  (`CLOSURE_BOUND_EXCEEDED`); k1 flipped the prose-embedded
  `MalformedGraph` refusals to typed `Refusal::NonCanonicalPort` /
  `Refusal::ClosureBoundExceeded` with exhaustive variant-match courts
  pinning serde tags, payloads, Display remediation text.

## Gates re-run this session (all exit 0)

- `cargo test -p ggen-abb-sbb` — 129 tests + 3 doctests, 0 failed across
  11 binaries (covers WO-05, WO-07, X3 court, WO-06 envelope courts).
- `cargo test -p ggen-engine --test cli_boundary` — 32/0.
- `cargo test -p ggen-engine --test abb_sbb_stage2c_wiring_e2e` — 7/0.
- `cargo test -p ggen-engine --lib envelope` — 8/0 (WO-06 spec cmd).
- `cargo test -p ggen-cli-lib --test ea_verbs_test` — 7/0.
- `cargo test -p ggen-cli-lib --test pack_compose_test` — 6/0.
- `cargo test -p ggen-marketplace --lib packs_registry::composer` — 5/0.
- `cargo test -p ggen-config --test ea_graph_schema_parity_test` — 4/0.

## Open follow-ups

- WO-04 projection landed via X3 with a disclosed deviation: ONE
  `[[generation.rules]]` entry (the declarative-rules engine refuses
  duplicate `output_file` across rules) instead of per-node-type rules;
  the seven per-node `.rq` files remain the single source via the
  `sparql()` Tera function. Open: per-node rule shape if the engine ever
  lifts the duplicate-output_file refusal.
- WO-06 R11 supersession: `measured <= declared` ceiling law (HIGH-1 fix)
  supersedes WO-06 R10 exact-equality; RESOLUTIONS.md R11 row is the
  register. Open: nothing — recorded for replay readers of R10.
- F2's FM-LAW-015 fixture fallout inventory: G1 landed the fail-closed
  default and patched 8 test literals to `skip_ea_admission: true`;
  k8 found G1's "9 red" list already stale (G2's scaffold repair landed
  ea.graph.json in the fixture first). Open: audit remaining test
  fixtures across the workspace that sync without an EA graph and are
  not yet explicitly opted out or fixture-repaired — count unknown,
  UNVERIFIED.
- WO-02 residuals disclosed by G8: per-variant refusal test matrix and
  R6 sbb help cross-link remain UNVERIFIED (out of G8's file ownership).

## Named coordinator items

- K8's flagged `consumer_mode` RDF drift: `consumer_mode` is in the sync_run
  verb signature but absent from `ggen_cli_nouns.rdf` — pre-existing drift,
  disclosed not fixed (outside k8's owned surface). Coordinator item.
- M1-era binary-identity confound: k8 recorded the canonical ggen binary is
  built via `cargo build -p ggen-cli-lib --bin ggen`, not `cargo build -p
  ggen` (root package deliberately lacks ggen-engine). M1-era receipts
  that named `-p ggen` as the binary build path carry that confound; replay
  readers should re-read the k8 receipt for the canonical command.
- K8's post-court concurrent sync.rs edit (sha moved c75f9f0c… → e7c7c208…)
  was disclosed and the block re-verified intact; noted so the sha in k8's
  receipt is not mistaken for the current tree's sha.
