# Risk Register

## 1. Scope
This register tracks architectural, conceptual, and integration risks that threaten the stability of the Genesis-bearing parts doctrine.

## 2. Top Architectural Risks

| Risk ID | Description | Severity | Countermeasure |
| :--- | :--- | :--- | :--- |
| **RSK-001** | **Boundary Blurring** (Genesis becomes an adapter) | CRITICAL | Strictly isolate `genesis-core` as `#[no_std]` Rust with no IO dependencies. |
| **RSK-002** | **Namespace Laundering** (Private predicates escape) | HIGH | Mandate Open-Ontologies SHACL checkpoints before enterprise promotion. |
| **RSK-003** | **Index Authority** (Treating views as truth) | HIGH | Force index rebuilds from `Construct8` packet streams during replay audits. |
| **RSK-004** | **Page Overflow** (Silent dropping of symbols) | CRITICAL | Implement strict `Need257` split laws and fail hard if unhandled. |
| **RSK-005** | **Mock Matter Counted** (Unaddressed triples) | HIGH | Enforce source-address digest binding on every `RelationPage` input. |
| **RSK-006** | **Triple Bloat** (Treating Pair2 as RDF) | HIGH | Ensure the predicate remains purely at the `RelationPage` level, never on the tuple. |
| **RSK-007** | **Replay Mismatch** (Non-deterministic output) | CRITICAL | Run deterministic property testing across all `Construct8` evaluations. |
| **RSK-008** | **AtomVM Overhead** (Erlang bridge too slow) | MEDIUM | Use NIFs/Ports for bulk `RelationPage` evaluation, keeping byte loops in Rust. |

## 3. Review Process
This register must be reviewed during the "Analyze" and "Verify" tollgates of the DFLSS charter.
