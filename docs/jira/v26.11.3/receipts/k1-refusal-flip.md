# Receipt — K1 refusal flip (lane K1, WO-07 deviation resolution)

Date: 2026-11-03. Canonical checkout `/Users/sac/ggen`. No git commands run (coordinator owns transitions).

## Subject

- `crates/ggen-abb-sbb/src/lib.rs` (owned; edited — two variants appended to the kernel `Refusal` enum)
- `crates/ggen-abb-sbb/src/depgraph.rs` (owned; edited — two call sites flipped)
- `crates/ggen-abb-sbb/tests/depgraph_hardening_test.rs` (owned; edited — G6's substring assertions converted to variant matches + 2 new courts)

## Landed

1. **`Refusal::NonCanonicalPort { pack, field, port }`** (lib.rs, after `ReplayMismatch`).
   Display = `NON_CANONICAL_PORT: pack '<pack>' field '<field>' port '<port>' is not canonical —
   rename the port to a non-empty, whitespace-free, lowercase a-z / 0-9 / dash identifier in the
   manifest, then re-run sync`. Serde tag `NON_CANONICAL_PORT` comes from the enum's existing
   `#[serde(tag = "refusal", rename_all = "SCREAMING_SNAKE_CASE")]` law — no per-variant attribute.
2. **`Refusal::ClosureBoundExceeded { max_iterations: usize }`**. Display keeps the wire prefix +
   bound naming and adds the remediation sentence (`inspect it for a cycle or non-monotonic
   dependency … then re-run sync`).
3. **depgraph.rs flip.** `non_canonical_port_refusal` now constructs
   `Refusal::NonCanonicalPort` directly (the prose-embedded `MalformedGraph` is gone); the
   `closure_fixpoint` bound guard constructs `Refusal::ClosureBoundExceeded { max_iterations }`.
   All other `MalformedGraph` uses (TOML parse errors, missing keys, non-string entries) remain
   `MalformedGraph` — they are genuinely malformed-input refusals, not the two hardened laws.
4. **Test flip.** G6's five substring assertions
   (`contains("NON_CANONICAL_PORT")` ×4, `contains("CLOSURE_BOUND_EXCEEDED")` ×1) converted to
   exhaustive variant matches asserting exact pack/field/port/max_iterations state. Two new
   positive courts pin the canonical serde tags
   (`v["refusal"] == "NON_CANONICAL_PORT"` / `"CLOSURE_BOUND_EXCEEDED"`), field payloads,
   round-trip deserialize, and the Display prefix + remediation text.

## Registry interaction (disclosed assessment)

The contract's G5/WO-05 clause evaluated: the generated registry
(`src/generated/refusals.rs`, projected from `ontologies/shapes/boundary-refusal.shacl.ttl`) is a
separate closed vocabulary (14 boundary wire codes E-/FD-/…, its own `generated::refusals::Refusal`
type). The kernel `Refusal` enum's variants (MalformedGraph, UnboundPort, …) have never had
registry rows there, and `tests/refusal_crosswalk_test.rs` courts only the generated vocabulary —
no kernel↔registry mapping exists for these variants, so no registry row or regeneration was
needed. Witness: `court_registry_vs_generated_both_directions` and
`court_every_wire_code_has_ctor` green unchanged post-flip.

## Verification ladder (commands + exits)

- `cargo build -p ggen-abb-sbb` → Finished, exit 0 (no warnings)
- `cargo test -p ggen-abb-sbb` → **108 passed, 0 failed** across 10 binaries, exit 0
  (G3's 106 + G6's 13 converted in place + 2 new variant-tag courts; depgraph_hardening_test.rs
  now 15)
- All 3 refusal_crosswalk courts green unchanged (no registry drift)

## Standing

ALIVE for the flipped surface (commands above, exits 0). G6's receipt
(`g6-depgraph.md` §Standing, PARTIAL on refusal variants) is now resolved: the dedicated variants
are landed; G6's Standing note can be updated to ALIVE at coordinator integration.

## Falsifiers

- G6's deviation falsifier ("courts pin the substring, which survives a variant flip") — the
  substring assertions did survive, and were then replaced by strictly stronger variant matches;
  the flip is pinned by positive serde-tag courts, not by the weaker substrings.
- Non-vacuity inherited: G6's M1/M2 shims (accept-all port law, disabled bound guard) would still
  flip the converted variant-match courts, since they assert on the exact refusal variant those
  mutations control.
