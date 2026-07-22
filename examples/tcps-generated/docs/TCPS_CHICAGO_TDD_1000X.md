# TCPS Chicago TDD 1000x Validation Fabric

This document describes the executable validation compiler in
`tests/tcps_chicago_tdd_1000x.rs`.

## Purpose

The fabric gives the complete checked-in TCPS product one traceable Chicago-TDD
constitution without duplicating the same filesystem execution for every rule.
It compiles:

```text
43 capabilities × 4 constitutional surfaces × 7 universal laws
= 1,204 independently addressable obligations
```

A single real observation of a capability artifact discharges the 28 obligations
attached to that capability. Obligation identities remain separate in the RDF and
OCEL projections even though execution is compressed.

## Chicago TDD posture

The implementation follows the classicist form used by the repository's vendored
`chicago-tdd-tools` crate:

- assertions inspect observable product state;
- probes use the real checked-in TCPS filesystem;
- no collaborator is mocked;
- every capability has a first-class `TestContract`;
- refusal is explicit when an expected product artifact is absent;
- outputs are receipts and evidence, not private implementation assertions.

## Capability population

The 43 capabilities include:

- all 26 public TCPS core modules;
- the standard-library, FFI, WASM, and CLI runtimes;
- CI and target declarations;
- Debian, RPM, npm, and NuGet packaging;
- CycloneDX, SPDX, and in-toto evidence;
- the source manifest;
- lifecycle and reproducible-build machinery;
- the ggen fixed-point lock surface.

## Constitutional surfaces

Each capability receives obligations across four aggregate surfaces. Each surface
contains three execution or evidence channels, for twelve channels total.

| Surface | Channels |
|---|---|
| Source | ontology, template, generated source |
| Runtime | native, `no_std`, CLI |
| Boundary | FFI, WASM, packaging |
| Evidence | receipt, provenance, lifecycle |

## Universal laws

Every capability-surface pair receives these seven laws:

1. deterministic;
2. state observable;
3. real collaborators;
4. refusal preserved;
5. authority separated;
6. replayable;
7. idempotent.

## Evidence compiler

The executable test fabric produces four isolated artifacts during its artifact
bundle scenario:

- `fabric-receipt.json`;
- `obligations.ttl`;
- `evidence.ocel.json`;
- `execution-plan.tsv`.

The semantic fingerprint excludes the occurrence timestamp. Replaying the same
TCPS state therefore preserves semantic identity while occurrence evidence may
retain a different wall-clock time.

## Deterministic sharding

All 1,204 obligations can be assigned to any nonzero number of workers through
rendezvous hashing. The same obligation and shard count always produce the same
owner. The eight-shard acceptance case proves total, deterministic assignment.

## Running

From `examples/tcps-generated`:

```bash
scripts/validate-1000x.sh
```

Equivalent direct command:

```bash
cargo test --test tcps_chicago_tdd_1000x -- --nocapture
```

## Standing boundary

The fabric validates the TCPS product currently checked into this consumer. It does
not replace the pack-level generation law, ggen sync conformance suite, or existing
byte-identity and sabotage proofs. It composes those product surfaces into one
complete Chicago-TDD obligation graph.
