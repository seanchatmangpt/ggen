# k8s-pack-DESIGN — status notes

**This is a design sketch, not a working pack.** See `pack.toml`'s `description` for the
full standing statement; this file is the shorter, scannable version.

## What this proves

That a real, 3-level-deep k8s object nesting (`PodSpec` → `Container` →
`ResourceRequirements`) is representable in ggen's ontology/gate conventions — the exact
shape `packs/dspy-pack/gates/010_admission.rq` structurally cannot admit (its
`PydanticModelField` is required to carry only `dspy:pythonType`; nesting a
`PydanticModel` inside a `PydanticModel` has no property to express it at all, gate aside).

Verified, not just asserted:
- `ontology.ttl` parses as valid Turtle and the 3-level nesting round-trips through a real
  `ggen sync run` against a scratch consumer with hand-written facts.
- `gates/010_admission.rq` actually fires on a hand-crafted bad fact (a
  `ResourceRequirements` individual missing `k8s:cpuLimit`) and passes on the valid facts —
  proving it is admission control, not decoration, per `pack-authoring-pack`'s own stated
  rule (`docs/pack-authoring/explanation.md`, "Gates are admission control, not test
  assertions").

## What this does NOT do

- **No schema-to-ontology generator.** This ontology was hand-authored against one real
  k8s object family (matching `io.k8s.api.core.v1.Container`/`ResourceRequirements`), not
  generated from the full k8s OpenAPI spec (~700 definitions). ROADMAP.md gap #3 is
  unaddressed.
- **Not composable.** No `ggen.toml` anywhere references `k8s-pack-DESIGN`. Nothing breaks
  if you delete this directory; nothing depends on it.
- **Not cleared the real-source-verification bar** dspy-pack set
  (`docs/pack-authoring/advanced-practices.md` #4) — no real k8s cluster, `kubectl`, or
  `client-go`/`kubernetes` Python client output was used to confirm these are the exact
  real field names/types; they were written from direct knowledge of the k8s API shape,
  which is a weaker bar than dspy-pack's `inspect.signature()`-against-the-installed-
  package standard.
- **No Chicago-TDD e2e test** in `crates/ggen-engine/tests/` — deliberate, so this doesn't
  read as "covered, therefore real" in the pack coverage ratchet
  (`docs/pack-authoring/errc-testing-pattern.md`'s baseline count).

## ROADMAP.md correspondence

Satisfies step 2 (`build-typed-k8s-object-schema`) only — the one action the ROADMAP's own
solved PDDL plan marks precondition-free. Does not satisfy step 4
(`build-schema-to-ontology-generator`) or step 5 (`author-k8s-pack`, whose PDDL
precondition is `(and (has-schema-to-ontology-generator) (has-nested-pydantic-support))` —
neither exists yet, and `has-nested-pydantic-support` is a `dspy-pack`-side change this
directory does not touch).

## To promote this to a real pack

1. Build the schema-to-ontology generator (ROADMAP step 4) and re-derive this ontology from
   its real output against the full k8s OpenAPI spec, not from this hand-authored seed.
2. Verify against a real cluster or a real k8s client library's parsed types (close the
   real-source-verification gap above).
3. Add a `crates/ggen-engine/tests/k8s_pack_e2e.rs` using the standard
   `scaffold_pack`/`assert_idempotent`/`assert_gate_refuses` four-part pattern.
4. Rename the directory `k8s-pack-DESIGN` → `k8s-pack`, drop the `-design` version suffix
   and the "DESIGN SKETCH" language from `pack.toml`, and only then compose it into a real
   consumer's `ggen.toml`.
