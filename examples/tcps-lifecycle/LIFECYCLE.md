# TCPS software-production lifecycle

## Purpose

This document is executable standard work for taking one downstream software
demand from observation through a Fortune 5 production deployment. The
reference implementation beside this document is authoritative for deriving the
first unified `tcps-pack`; the pack reproduces that implementation into canonical
project paths.

## System roles

| System | Production role |
|---|---|
| TCPS | Governs pull, standard work, jidoka, andon, authority separation, kaizen, and restart. |
| Praxis | Admits observations, binds plans to admitted standards, issues authorization capabilities, checks Lean proofs, and receipts law. |
| cargo-cicd | Observes Cargo/Git state, manufactures verification evidence, executes workcells, and emits process evidence. |
| ggen | Projects admitted production definitions into canonical Rust, Lean, test, release, deployment, and documentation artifacts. |
| Charon/Aeneas | Independently functionalizes the safe Rust kernel and supplies the reverse semantic audit surface. |

The in-process `ReferencePraxisGate` and `ReferenceCargoCicdExecutor` model these
authority boundaries. A Fortune 5 adapter must replace them with authenticated,
signed external receipts without weakening the typestate protocol.

## Lifecycle state machine

```text
Observed
  -> Admitted by Praxis
  -> Planned from cargo-cicd and Lean evidence
  -> Authorized by a plan-and-standard-bound capability
  -> Receipted execution through the named workcell

Authorized
  -> Stopped on abnormality
  -> Observed only after standard work changes
```

There is no successful `Executed` state without a receipt. The Rust API moves
from `Authorized` directly to `Receipted`; an abnormal execution moves to
`Stopped`. The Lean theorem `successful_has_receipt` states the corresponding
logical exclusion.

## Phase 1 — Observe downstream demand

Input is a concrete repository change, defect, dependency update, or release
request. `cargo-cicd` observes the real workspace and Git state. The observation
is represented in `praxis/observation.json` and the consumer facts in
`schema/domain.ttl`.

Acceptance requires positive downstream demand, a named observation, a current
standard-work hash, a named workcell, and a proof obligation.

## Phase 2 — Admit through Praxis

Praxis judges the observation as a law object. Admission binds the observation
identifier and standard hash into a receipt. The typestate transition recomputes
that binding before accepting the receipt, so a malformed gate cannot advance
the line. Zero demand, empty standard work, an unknown standard, or a malformed
receipt refuses before planning.

```bash
my-conforming-project law judge "$(cat praxis/observation.json)"
```

## Phase 3 — Project canonical artifacts with ggen

`ggen sync run` loads `schema/domain.ttl` and `packs/tcps-pack/ontology.ttl`,
runs pack gates, and projects into ordinary project paths. Repeated sync with
unchanged admitted input must be byte-identical.

```bash
ggen sync run
ggen receipt verify
```

The pack must not write a `generated/` directory.

## Phase 4 — Manufacture repository evidence

The cargo-cicd workcell manufactures evidence from the source digest, normalized
affected scope, build status, tests, compile-fail tests, and Lean status. Callers
cannot construct or mutate `CargoCicdEvidence` directly.

```bash
cargo-cicd workspace doctor
cargo-cicd test changed
cargo-cicd trybuild full
cargo-cicd verify
cargo-cicd sbom generate
```

The local reference also runs `cargo test --all-targets`, Rust doctests, and
`lake build` directly so the core evidence does not depend on one wrapper.

## Phase 5 — Lean admission and Rust refinement

`lake build` checks `TCPSLifecycle/Basic.lean` and
`TCPSLifecycle/Refinement.lean`. The proof rail establishes that pre-authorized
states cannot execute, successful outcomes contain receipts, plan and receipt
digests correspond, and unchanged or empty standards cannot restart a stopped
line.

```bash
praxis-l4 verify --root . --receipts receipts/lean.jsonl
```

Charon/Aeneas extraction alone is not correspondence. `verify-aeneas.sh` returns
a refused standing until an axiom-free binding imports the generated model and
links its `step` symbol to `TCPSLifecycle.Refinement.rustStep_sound`.

## Phase 6 — Plan and authorize

Green evidence is consumed into a plan digest. Praxis alone constructs the
authorization capability. The capability binds:

- plan digest;
- admitted standard hash;
- approver identity;
- authorization digest.

The typestate transition recomputes the authorization digest and refuses any
plan, standard, or digest mismatch before actuation. The capability is opaque,
non-`Clone`, non-`Copy`, and non-deserializable.

## Phase 7 — Execute and receipt atomically

The named executor returns artifact evidence only. It cannot manufacture a
success receipt. The typestate transition checks executor/workcell identity,
refuses empty artifact evidence, and constructs the receipt atomically.

The receipt binds:

- observation identifier;
- plan digest;
- authorization digest;
- cargo-cicd evidence digest;
- executor identity;
- artifact digest;
- receipt digest.

An executor abnormality raises an andon and returns `ProductionLine<Stopped>`.
The line can return to `Observed` only with a different, non-empty standard hash,
after which fresh Praxis admission is required.

## Phase 8 — Release manufacture

`release/release.toml` requires a clean tree, affected tests, compile-fail tests,
Lean kernel acceptance, Praxis admission, SBOM, provenance, checksums, signature,
and immutable digest promotion. No environment rebuilds the artifact;
development, integration, preproduction, and production receive the same digest.

## Phase 9 — Fortune 5 deployment

`deploy/production.toml` requires an artifact digest from the release receipt,
canary rollout, bounded promotion, automatic rollback thresholds, workload
identity, network policy, non-root read-only execution, observability, a
deployment receipt, and distinct selection, authorization, and execution actors.
The enterprise deployment broker is the only actuator.

## Phase 10 — Observe, improve, and replay

Runtime evidence returns to the observation layer. Any failure becomes an andon,
then a Praxis consequence plan, then a change to ontology, standard work, proof,
fixture, or production policy. A one-off patch is not closure.

The complete replay input is:

```text
ontology + pack version + templates + toolchains + lockfiles + receipts
```

## Acceptance matrix

| Gate | Named artifact |
|---|---|
| Semantic admission | Praxis law receipt |
| Projection | ggen sync receipt |
| Rust protocol law | `src/lib.rs` and compile-fail doctest |
| Rust behavior | `tests/lifecycle.rs` |
| Lean law | `TCPSLifecycle/Basic.lean` and `TCPSLifecycle/Refinement.lean` |
| Reverse semantic audit | `scripts/verify-aeneas.sh` and binding receipt |
| Repository evidence | `cicd.toml` and cargo-cicd receipts |
| Release law | `release/release.toml` |
| Deployment law | `deploy/production.toml` |
| Aggregate judgment | `receipts/verification.json` |
| Graph evidence | `evidence/checks.ttl` |
| Adversarial standing | `ADVERSARIAL_REVIEW.md` |

## Pack extraction rule

A file enters `tcps-pack` only because the working example proves it is required
for one of these lifecycle gates. The example remains the conformance oracle.
Pack promotion requires deleting the consumer projection, re-syncing from the
pack, and obtaining byte-identical artifacts plus green Rust, Lean, Praxis,
cargo-cicd, and Aeneas-binding receipts.
