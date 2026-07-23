# ggen Part Passport

## Status

`PARTIAL_ALIVE` until repository CI compiles the Rust surface and validates the RDF/SHACL fixtures on the PR head.

## Thesis

An AC adapter label is a compressed constitutional document. It names the manufacturer and exact model, declares the input range it accepts, promises a bounded output, fixes polarity, states resource capacity, identifies isolation and deployment conditions, carries conformity and independent certification marks, and explains lifecycle disposal.

A Genesis-bearing interchangeable software part needs the same object for causality:

```text
uncontrolled world
  -> Erlang/OTP/AtomVM conditioning
  -> admitted event envelope O*
  -> Rust/WASM deterministic part
  -> bounded consequence A
  -> receipt R
```

The passport does not claim control over the world that produced an event. It controls what may cross the part boundary and what consequence the part may produce.

## AC adapter to ggen mapping

| Adapter nameplate | ggen passport |
|---|---|
| Manufacturer, model, lot | `PartIdentity` and receipt coordinates |
| `100-240 V~` | `InputEnvelope` representation, schema, and protocol range |
| `50/60 Hz` | `TemporalProfile` clock and event cadence |
| Maximum input current | `ResourceEnvelope` memory, fuel, time, queue, concurrency |
| AC to DC symbols | External representation to canonical admitted representation |
| Output voltage/current | `OutputContract` consequence type and capacity |
| Center-positive polarity | `CausalPolarity`: consumes, produces, requires authority, emits intent |
| Connector dimensions | WIT/ABI/schema compatibility |
| Double-square Class II | `IsolationClass::DoubleInsulated` plus `ambient_io = false` |
| Indoor-use/IP rating | `HostProfile` and hostile-environment policy |
| CE-like mark | `ConformityMark`, explicitly self-declared or external |
| UL/TUV-like mark | `VerifierMark` with evidence and exact artifact digest |
| EMC mark | `NonInterferenceProfile` |
| Regional approval | `jurisdiction_profiles` |
| Crossed-out bin | `LifecyclePolicy` and `RetirementPolicy` |
| Serial/date/file number | artifact digest, source graph digest, batch, build receipt |

## Source law

The public vocabulary is:

```text
ontologies/ggen-part-passport.ttl
```

Admission shapes are:

```text
ontologies/shapes/ggen-part-passport.shacl.ttl
```

The shapes enforce the public semantic surface independently of the Rust implementation. Important constraints include:

- exact artifact and source-graph digest syntax;
- positive input, output, and resource bounds;
- complete structural links from passport to input, output, polarity, resource, noninterference, and lifecycle objects;
- double-insulated parts cannot use ambient IO;
- stable parts require deterministic serialization, receipts, conformity, and a passed verifier;
- a surface cannot be simultaneously writable/emittable and forbidden.

## Rust API

The implementation lives at:

```text
crates/ggen-marketplace/src/marketplace/part_passport.rs
```

Primary objects:

```rust,ignore
use ggen_marketplace::{PartPassport, PassportBinding};

let passport: PartPassport = serde_json::from_str(passport_json)?;
let validation = passport.validate();
assert!(validation.is_valid());

let binding = PassportBinding::from_manifest(&manifest, passport);
assert!(binding.validate().is_valid());
```

The passport remains a separate sidecar or embedded custom section rather than adding a required field to the legacy marketplace `Manifest`. This preserves existing struct literals and manifest serialization while binding the passport to the manifest ID and version mechanically.

## Deterministic nameplate projection

`PartPassport::render_nameplate()` emits a stable human-readable label from machine facts. The label is not source truth. It is a projection of the passport and its evidence coordinates.

Example marks:

| Mark | Meaning |
|---|---|
| `O*` | conditioned admitted input |
| `D` | deterministic output serialization |
| `NAA` | no ambient authority |
| `II` | double-insulated isolation |
| `LCS` | limited consequence surface |
| `IV` | passed independent verifier |
| `NI` | noninterference profile has no internal conflict |
| `RET` | separate retirement with receipt preservation |

## Interchangeability law

An API match is insufficient. A candidate part may replace a required part only when substitution preserves the causal socket.

```rust,ignore
let report = candidate.can_substitute_for(&required);
if !report.compatible {
    for violation in report.violations {
        eprintln!("{:?}: {}", violation.code, violation.message);
    }
}
```

The current substitution law checks:

- input representation and schema equality;
- protocol-range coverage;
- input capacity and accepted event coverage;
- no new required input fields;
- output meaning, deterministic serialization, receipt obligation, and capacity;
- exact causal polarity;
- resource draw within the existing host ceiling;
- no new ambient authority;
- equal or stronger isolation;
- host and jurisdiction profile coverage;
- conformity-profile coverage;
- no widening of read/write/emit authority;
- installable lifecycle state.

This is contextual substitutability, not byte identity.

## Canonical fixture

Machine-readable JSON:

```text
examples/part-passport/crown-kernel.json
```

Public RDF instance:

```text
examples/part-passport/crown-kernel.ttl
```

The fixture models the Crown trace kernel as a double-insulated, no-ambient-authority part that consumes admitted trace events and emits a receipted Crown result.

## Falsifiers

The passport claim fails if any of the following is demonstrated:

1. a part passes substitution while changing causal polarity;
2. a replacement requires more authority or resources than the socket grants;
3. a stable passport has no independent passed verifier;
4. a conformity/verifier mark is bound to a different artifact digest;
5. the RDF and Rust projections admit incompatible objects;
6. a human nameplate can assert a mark not derivable from the machine passport;
7. a revoked or deprecated part is accepted for new installation;
8. a part mutates a surface its noninterference profile forbids.

## Verification

Repository CI should execute at minimum:

```bash
cargo test -p ggen-marketplace part_passport
cargo test -p ggen-marketplace --test part_passport_fixture
cargo clippy -p ggen-marketplace --all-targets -- -D warnings
```

RDF/SHACL verification should parse the ontology and validate the Crown fixture against the shapes. Missing SHACL tooling must produce `BLOCKED_TOOLCHAIN`, not success.
