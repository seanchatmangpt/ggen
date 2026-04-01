# Clean Crates — No Significant Issues

These crates have no stubs, no dead code, and no architectural issues requiring core team attention.

---

## ggen-receipt

**Path:** `crates/ggen-receipt/`
**Role:** Ed25519 cryptographic receipts and hash chains
**Status:** Clean. One warning: unused `SigningKey` import in `chain.rs:5`. Fix: remove import.
**Test count:** 116

---

## ggen-ontology-core

**Path:** `crates/ggen-ontology-core/`
**Role:** TripleStore (Oxigraph wrapper), SparqlGenerator, EntityMapper
**Status:** Clean. Standalone (only depends on ggen-utils).
**Test count:** 94

---

## ggen-canonical

**Path:** `crates/ggen-canonical/`
**Role:** Deterministic canonicalization and hash verification
**Status:** Clean. Standalone (no ggen-* deps).
**Test count:** 50

---

## ggen-transport

**Path:** `crates/ggen-transport/`
**Role:** Transport layer with session management, streaming, origin validation
**Status:** Clean. Fully standalone (zero ggen-* deps).
**Test count:** 85

---

## ggen-a2a

**Path:** `crates/ggen-a2a/`
**Role:** A2A task state machine, in-process message transport
**Status:** Clean. Defines TaskStateMachine with clear state transitions.
**Test count:** 58

---

## a2a-generated

**Path:** `crates/a2a-generated/`
**Role:** Ontology-generated A2A types (Agent, Message, Port, Task)
**Status:** Clean. Generated code. 1,874 tests.
**Test count:** 1,874

---

## ggen-a2a-registry

**Path:** `crates/ggen-a2a-registry/`
**Role:** Multi-agent orchestration registry
**Status:** Clean.
**Test count:** 33

---

## ggen-prompt-mfg

**Path:** `crates/ggen-prompt-mfg/`
**Role:** Prompt compilation via SPARQL CONSTRUCT
**Status:** Clean.
**Test count:** 34

---

## ggen-codegen

**Path:** `crates/ggen-codegen/`
**Role:** Generic codegen framework (Queryable/Renderable traits)
**Status:** Clean. Minimal — only 2 inline tests.
**Test count:** 2

---

## ggen-yawl

**Path:** `crates/ggen-yawl/`
**Role:** YAWL workflow generation from ontologies
**Status:** Clean. Minor `#[allow(dead_code)]` in `java_rules.rs:30` and `template/context.rs:100`.
**Test count:** 96

---

## ggen-craftplan

**Path:** `crates/ggen-craftplan/`
**Role:** RDF → Elixir codegen (5-stage μ pipeline)
**Status:** Clean.
**Test count:** 21

---

## ggen-process-mining

**Path:** `crates/ggen-process-mining/`
**Role:** Process mining (Alpha++, XES/OCEL, PetriNet)
**Status:** Clean. Minor `#[allow(dead_code)]` in `discovery.rs:425,435`. No dedicated test directory (66 inline tests).
**Test count:** 66

---

## ggen-cli-validation

**Path:** `crates/ggen-cli-validation/`
**Role:** IO validation and security for CLI operations
**Status:** Clean.
**Test count:** 16

---

## ggen-node

**Path:** `crates/ggen-node/`
**Role:** Node.js N-API bindings
**Status:** Clean.
**Test count:** 15

---

## ggen-e2e

**Path:** `crates/ggen-e2e/`
**Role:** Cross-platform E2E testing
**Status:** Clean.
**Test count:** 45

---

## ggen-test-audit

**Path:** `crates/ggen-test-audit/`
**Role:** Mutation testing, assertion analysis
**Status:** Suspicious — only 1 test for an audit tool.
**Test count:** 1

---

## ggen-test-opt

**Path:** `crates/ggen-test-opt/`
**Role:** Test value scoring, Pareto selection
**Status:** Clean but no dedicated test directory (28 inline tests).
**Test count:** 28
