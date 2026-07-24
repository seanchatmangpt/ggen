# TICKET-007: Author pack.toml for the projection phase

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Extend the existing minimal pack.toml (written in the TTL-authoring phase, [pack] name/version/description only) with whatever additional fields this repo's pack.toml schema supports/requires once the pack starts being consumed by ggen sync run, without inventing undocumented fields.

## Current state

PARTIAL_ALIVE — pack.toml exists with [pack] name/version/description; not yet exercised by `ggen sync run` since this pack has never been synced.

## Target state

pack.toml validated against the real `#[serde(deny_unknown_fields)]` pack.toml struct in ggen-config, confirmed to load without error via a dry-run.

## Projection classification

- Template: 100%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 0%
- Expected ratio: 100/0
- Custom-code justification: NONE — pack.toml is configuration data, not code.

## Inputs

- crates/ggen-config/src/manifest (ground truth for the pack.toml schema)
- packs/wasm4pm-interview-assist-pack/pack.toml (existing)

## Outputs

- packs/wasm4pm-interview-assist-pack/pack.toml (updated if the schema requires additional fields)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

N/A — this is configuration, not a template.

## Domain-data responsibility

pack description text may reference InterviewAssist by name (identity, not vocabulary) per the resource-identity rule already applied in the TTL phase.

## Custom-code boundary

NONE.

## Exclusions

- no undocumented pack.toml fields
- no deny_unknown_fields violations

## Implementation steps

1. Read the live #[serde(deny_unknown_fields)] pack.toml struct definition.
2. Confirm the existing pack.toml's fields are exactly the accepted set.
3. Add any genuinely required fields the schema demands for consumption (none currently known to be missing, per this session's TTL-phase pack.toml).
4. Dry-run load the pack.toml through the real deserializer (unit test or `ggen doctor`) rather than assuming it's valid.

## Admission gates



## Acceptance criteria

- Given the live pack.toml struct, when pack.toml is deserialized, then it succeeds with no deny_unknown_fields error.

## Negative tests

- Add a deliberately invalid extra field and confirm deserialization fails, proving the deny_unknown_fields guard is real, not assumed.

## Verification ladder

- Unit: deserialize pack.toml through the real ggen-config struct
- Integration: N/A with reason — single-file config, no cross-component integration
- End-to-end: N/A with reason — pack.toml has no runtime behavior of its own
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: deserialization test pass/fail

## Receipts

- pack.toml hash

## Dependencies

(none — entry point of the dependency graph)

## Falsifier

If pack.toml fails to deserialize through the real ggen-config struct, this ticket is not complete.

## Handoff

TICKET-010 (idempotent sync) depends on a validated pack.toml.

## Definition of done

- pack.toml deserializes cleanly through the real struct
- no invented fields
