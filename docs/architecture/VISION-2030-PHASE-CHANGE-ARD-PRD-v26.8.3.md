# Vision 2030 — 1,000× Phase Change Capability Program

**Document type:** Product Requirements Document + Architecture Requirements Document  
**Status:** IMPLEMENTED CONTROL-PLANE CHECKPOINT — capability realizations require independent evidence  
**Date:** 2026-08-02  
**Stacked baseline:** `agent/sbb-capability-density@28ceeb35a24edfee8bdd56e8d41d545f541a2073`  
**Product surface:** `ggen vision2030 schema|inspect|validate|roadmap|blue-ocean|dx|qol|doctor|receipt|replay`

---

## 0. Vision

Vision 2030 changes the unit of production from a file, function, ticket, or agent session into an admitted system capability:

```text
O* → architecture contract → capability SBB → distribution → consequence → receipt → replay
```

The target is not a faster coding assistant. It is a software-manufacturing substrate that can describe a world once and lawfully project domain models, contracts, APIs, CLIs, MCP+ surfaces, A2A coordination, Doctor diagnostics, Wizard construction flows, deployment artifacts, tests, policies, documentation, passports, receipts, and replay across local, browser, edge, fog, and cloud runtimes.

A 1,000× phase change is admitted only when measured delivered capability instances divided by canonical maintenance units is at least 1,000. The numerator must come from an eligible SBB density report. The denominator must come from the same report. A declared multiplier is never accepted as evidence.

---

## 1. Governing laws

1. **Chatman Equation:** `A = μ(O*)`. Only admitted observations enter manufacturing. Every program result is a derived artifact with a digest and replay path.
2. **Blue River Dam:** control upstream semantics, lawful state, guards, receipts, and release. Downstream artifacts are projections.
3. **Zero unreceipted actuation:** observation and planning are pure. Writes are bounded to deterministic report/receipt emission. External actuation requires a separate execution grant and broker path.
4. **LLMs do not decide standing:** an LLM may propose catalog entries or text. It cannot admit, verify, certify, sign, or promote a capability.
5. **External acceptance:** `ALIVE` requires an independently issued acceptance bound to the exact SBB report digest. The program cannot certify itself.
6. **No modeled-as-observed claims:** catalog entries, roadmaps, targets, and generated plans remain `DESIGNED` until evidence exists.
7. **No private-ontology lock-in:** capabilities bind stable IRIs and public semantic projections.
8. **No autonomous authority expansion:** capability authority is explicit. Healthcare surfaces may observe or recommend only. Doctor surfaces may diagnose and construct remediation plans but may not actuate. Other actuation requires an execution grant.
9. **Dependency closure:** a capability cannot be admitted while any declared dependency is missing or unaccepted.
10. **Exact horizon closure:** Vision 2030 requires every horizon gate from 2026 through 2030 to meet its minimum accepted capability count.
11. **Domain closure:** every required domain must contain at least one externally accepted capability.
12. **Replay closure:** a capability without a matching replay witness is not accepted.

---

## 2. Required capability domains

The default Vision 2030 profile spans twelve domains.

| Domain | Production consequence |
|---|---|
| `dx` | local-first developer control plane, deterministic diagnostics, explainable refusal, one-command projection |
| `qol` | low-friction, reversible, accessible, offline-capable operating experience |
| `doctor` | non-actuating diagnostics, root-cause evidence, remediation plans, environment passports |
| `healthcare` | regulated decision support, provenance, policy checks, no autonomous clinical authority |
| `marketplace` | capability discovery, compatibility, lockfiles, passports, installation and retirement |
| `mcp-plus` | stable external control surface over CLI, MCP, A2A, POWL, Doctor, Wizard, and Telco capabilities |
| `planning` | POWL v2 strategy/process IR, lawful concurrency, build-order grammar, route validation |
| `runtime` | WASM interchangeable parts across browser, native, edge, fog, container, and cloud |
| `coordination` | Erlang/AtomVM actor supervision, routing, federation, and bounded recovery |
| `process-intelligence` | event → trace → variant → conformance → replay → audit with OCEL/object-centric evidence |
| `governance` | receipts, passports, policy, independent release control, exceptions, migration and retirement |
| `manufacturing` | ontology-authoritative projection into every admitted textual and executable form |

A program may add domains but may not silently remove required ones.

---

## 3. Blue Ocean contract

Every capability declares one primary ERRC move:

- **Eliminate:** remove coordination, hand translation, duplicate maintenance, hidden state, or unverifiable review work.
- **Reduce:** reduce WIP, drift, latency, runtime weight, dependency surface, or operator effort.
- **Raise:** raise correctness, observability, accessibility, portability, provenance, or replayability.
- **Create:** create a capability unavailable to manual or session-oriented delivery, such as lawful regeneration across an entire distribution product space.

The aggregate report counts accepted capabilities by ERRC move. Vision closure requires at least one accepted capability in each move.

---

## 4. Evidence carrier

A capability realization binds exactly seven evidence roles:

```text
sbb_report
positive
negative
verifier
receipt
replay
external_acceptance
```

Each binding contains a safe manifest-relative locator and BLAKE3 digest. Remote URIs, absolute paths, and parent traversal are refused.

The `sbb_report` must be a `ggen.sbb.capability-density-report.v1` artifact that:

- is eligible for external admission;
- has claim ceiling `PARTIAL_ALIVE`;
- reports at least one commit-equivalent unit;
- reports a nonzero distribution context count;
- reports delivered instances consistent with canonical units and contexts.

The result receipt must bind the same report digest. The replay witness must report `REPLAY_MATCH`. The external acceptance must bind the capability IRI and exact report digest, name an independent issuer, and declare `ACCEPTED`.

An actuating capability additionally requires an execution-grant binding.

---

## 5. Phase-change arithmetic

For each accepted capability SBB:

```text
canonical_units = commit_equivalent_units
delivered_instances = delivered_capability_instances
capability_multiplier = delivered_instances / canonical_units
```

For the program:

```text
program_multiplier = Σ delivered_instances / Σ canonical_units
```

The program achieves the phase change only when:

```text
program_multiplier >= phase_change_target
AND all required domains have accepted capability coverage
AND every horizon minimum is met
AND every declared capability is accepted
AND ERRC coverage is complete
AND violations = 0
```

The standard target is `1000`.

---

## 6. Horizon gates

### 2026 — Constitution and vertical proof

- ontology-authoritative capability carrier;
- SBB density admission;
- receipts and replay;
- local-first CLI;
- one complete vertical witness from ontology through external acceptance.

### 2027 — Interface and marketplace closure

- MCP+ facade;
- local capability marketplace;
- Doctor and Wizard flows;
- typed compatibility, lockfiles, passports, migration, and retirement.

### 2028 — Runtime and coordination closure

- interchangeable WASM parts;
- browser/native/edge/fog/cloud parity;
- Erlang/AtomVM supervision and federation;
- deterministic degraded-mode operation.

### 2029 — Process and regulated closure

- object-centric event substrate;
- streaming conformance and replay;
- healthcare and regulated-edge decision-support profiles;
- policy exchange and independent release control.

### 2030 — Civilizational capacity surface

- all declared capability domains externally accepted;
- 1,000× measured distribution multiplier;
- self-hosting manufacturing closure;
- no silent skips, unverifiable runtime claims, or authority expansion;
- every consequence reproducible from admitted observations.

---

## 7. CLI requirements

| Command | Requirement |
|---|---|
| `schema` | return exact schemas, domains, evidence roles, ERRC moves, authority classes, and target law |
| `inspect` | return the complete deterministic program report without mutation |
| `validate` | return achievement status, standing, multiplier, coverage, and violations |
| `roadmap` | return horizon progress and missing accepted capability counts |
| `blue-ocean` | return ERRC counts and uncovered moves |
| `dx` | diagnose local-first, deterministic, explainable, replayable developer-experience coverage |
| `qol` | diagnose reversibility, accessibility, offline operation, automation, and low-friction coverage |
| `doctor` | return evidence-backed defects and remediation actions without actuation |
| `receipt` | atomically emit report plus chained intent/result receipts |
| `replay` | recompute the report and return only `REPLAY_MATCH` or `REPLAY_DIVERGED` |

Every verb must remain a thin CLI adapter. Domain logic belongs in reusable modules.

---

## 8. Refusal catalogue

| Code | Condition |
|---|---|
| `V2030-001` | unsupported program schema |
| `V2030-002` | target year is not 2030 or phase target is below 1,000 |
| `V2030-003` | required domain removed or duplicated |
| `V2030-004` | missing or duplicate horizon |
| `V2030-005` | duplicate capability identity or IRI |
| `V2030-006` | unknown dependency or dependency cycle |
| `V2030-007` | unsafe or digest-divergent evidence |
| `V2030-008` | incomplete evidence-role set |
| `V2030-009` | ineligible or inconsistent SBB report |
| `V2030-010` | receipt or replay does not bind the SBB report |
| `V2030-011` | external acceptance missing, self-issued, rejected, or digest-divergent |
| `V2030-012` | healthcare authority exceeds observe/recommend |
| `V2030-013` | Doctor authority attempts actuation |
| `V2030-014` | actuation lacks execution grant |
| `V2030-015` | ERRC, horizon, or domain coverage incomplete |
| `V2030-016` | phase-change multiplier below target |
| `V2030-017` | receipt chain invalid |
| `V2030-018` | replay divergence |

---

## 9. Implementation map

| Layer | Artifact |
|---|---|
| Ontology | `ontology/vision-2030-phase-change.ttl` |
| SHACL | `ontology/vision-2030-phase-change.shacl.ttl` |
| JSON carrier | `packs/vision-2030-phase-change-pack/schema/vision-2030-program.schema.json` |
| Capability catalog | `packs/vision-2030-phase-change-pack/catalog/vision-2030-capabilities.json` |
| Evaluator | `crates/ggen-cli/src/cmds/vision2030/evaluation.rs` |
| Receipts/replay | `crates/ggen-cli/src/cmds/vision2030/receipts.rs` |
| CLI adapters/types | `crates/ggen-cli/src/cmds/vision2030/mod.rs` |
| Regression tests | `crates/ggen-cli/src/cmds/vision2030/tests.rs` |

---

## 10. Standing

This checkpoint implements the program control plane, semantics, carrier, blue-ocean analysis, DX/QoL/Doctor lenses, horizon gates, evidence admission, receipt chain, replay, and a canonical capability catalog.

It does **not** claim that all catalog capabilities already exist. Catalog entries are design commitments. Only externally accepted, digest-bound SBB realizations count toward Vision 2030. Until those realizations close, aggregate standing remains `PARTIAL_ALIVE` or `DESIGNED`, never fabricated `ALIVE`.
