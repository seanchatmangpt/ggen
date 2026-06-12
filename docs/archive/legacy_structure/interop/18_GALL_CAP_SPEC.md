<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL-CAP: Capability Admissibility & Process Interchangeability Layer](#gall-cap-capability-admissibility--process-interchangeability-layer)
  - [1. Mission](#1-mission)
  - [2. Core Question](#2-core-question)
  - [3. Capability Process Analysis (CPA)](#3-capability-process-analysis-cpa)
  - [4. Capability Geometry](#4-capability-geometry)
  - [5. Capability Cell Structure](#5-capability-cell-structure)
  - [6. Execution Enforcement](#6-execution-enforcement)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL-CAP: Capability Admissibility & Process Interchangeability Layer

## 1. Mission
Determine whether a software component is:
- Decomposable
- Substitutable
- Externally verifiable
- Process-conformant
- Replay-safe
- Capability-complete
- Admissibly manufacturable

## 2. Core Question
GALL-CAP does not ask, *"Does the code work?"*
GALL-CAP asks, **"Can this capability survive adversarial operational substitution?"**

## 3. Capability Process Analysis (CPA)
CPA is the engine of GALL-CAP. It evaluates the manufacturing topology itself. It bridges the gap between OCEL/POWL evidence, public ontology meshes, executable WASM/Rust artifacts, and enterprise-scale manufacturing.

## 4. Capability Geometry
Every manufactured capability cell MUST expose the following rigid geometry:

| Surface | Requirement |
| :--- | :--- |
| **Inputs** | Explicit and typed. |
| **Outputs** | Explicit and typed. |
| **Preconditions** | Explicitly validated before execution. |
| **Consequences** | Explicitly observable state transitions. |
| **Replay** | Perfectly deterministic. |
| **Refusal modes** | Observable and cryptographically receipted. |
| **Evidence** | Emitted as object-centric artifacts. |
| **Timing** | Measurable operational latency. |
| **Dependencies** | Declared and isolated in the membrane. |
| **Constraints** | Externally verifiable via SHACL/QLever. |

## 5. Capability Cell Structure
Each interchangeable part manufactured by the foundry becomes a layered cell:

| Layer | Description |
| :--- | :--- |
| **POWL route** | Lawful operational motion. |
| **OCEL evidence** | Object-centric execution traces. |
| **PROV graph** | Lineage and derivation mapping. |
| **SHACL rules** | Admissibility constraints. |
| **SPDX identity** | Artifact evidence and checksums. |
| **Replay fixture** | Deterministic proof harness. |
| **Sabotage corpus** | Refusal proof harness. |
| **Capability classification** | SKOS concept mapping in Open Ontologies. |
| **Runtime package** | WASM / AtomVM / Rust executable body. |
| **External verifier ring** | Hostile replay environment. |

## 6. Execution Enforcement
Any component that hides behind generic abstractions, fails to generate refusal artifacts upon sabotage, or cannot perfectly replay its state transition violates GALL-CAP and must be rejected from the foundry.