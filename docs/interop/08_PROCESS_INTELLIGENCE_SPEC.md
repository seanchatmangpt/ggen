# Process Intelligence Interop Specification

## 1. Process Evidence Thesis
Process mining no longer begins *after* logs. Logs are downstream projections. Process evidence starts inside Genesis-bearing parts at the exact moment of operational motion. 

## 2. Source-Adjacent Construction Cells
Genesis-bearing parts act as source-adjacent construction cells. They do not emit raw, unstructured text logs; they construct receipted relation matter that mathematically guarantees the sequence of events.

## 3. ggen Projection Role
ggen reads the receipted `Construct8` packets and projects them into standard process mining formats (OCEL 2.0, XES, POWL).

## 4. wasm4pm Consumption Role
`wasm4pm` acts as the ultra-fast, local validation engine. It consumes the OCEL projections emitted by ggen and runs conformance checking against expected process models.

## 5. pictl Control-Plane Role
`pictl` acts as the human/operator interface to query the results of the `wasm4pm` conformance checks, enabling analysts to visualize deviations.

## 6. OCEL 2.0 Projection Requirements
The ggen projection MUST map:
- `Pair2` subject/object IDs to OCEL Object IDs.
- `RelationPage` predicates to OCEL Event Activities.
- Receipt timestamps to OCEL Timestamps.

## 7. POWL Route Proof Requirements
Projections must support Partially Ordered Workflow Language (POWL) analysis, proving that the causal chain of Genesis receipts conforms to authorized `O*` routes.

## 8. Conformance and Replay Relationship
Process conformance checking is a secondary validation layer. If `wasm4pm` detects a deviation, the Truex Replay Cursor can perfectly reconstruct the exact Genesis state that caused the deviation.

## 9. Process Receipt & Refusal Surfaces
If a process artifact fails `wasm4pm` validation, this does NOT invalidate the Genesis construction (if Genesis accepted it). It means the process deviated from the model. However, a Genesis Refusal means the process event *never legally occurred*.

## 10. Avatar JTBD Mapping
- **Enterprise Process Intelligence Owner:** Reconstructs object-centric operational motion from source-adjacent proof (OCEL 2.0 projected from Genesis receipts).
- **Manufacturing Traceability Director:** Traces parts and defects using `wasm4pm` conformance over ggen outputs.

## 11. Artifact Interop Table

| Process artifact | Constructed from | ggen projection | Consumed by | Validation | Receipt |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **OCEL 2.0 JSON** | `Construct8` Stream | `project_ocel2` | `wasm4pm` | Conformance | Packet Hash |
| **XES Log** | `Construct8` Stream | `project_xes` | ProM / Celonis | Case Analysis | Segment Hash |
| **POWL Model** | `O*` Constraints | `project_powl` | `pictl` | Structural | `O*` Hash |

## 12. Definition of Done
Process intelligence interop is DONE when ggen can continuously stream OCEL 2.0 JSON from a live Genesis-bearing part, and `wasm4pm` can ingest that stream in real-time to perform zero-copy conformance checking.

| Status | Component | File/Artifact Evidence |
| :--- | :--- | :--- |
| **PARTIAL** | OCEL Projection | `crates/ggen-projection/src/lib.rs` (`project_ocel2`) |
| **IMPLEMENTED** | wasm4pm engine | `~/wasm4pm` |
| **MISSING** | Real-time Streaming Bridge | Needs implementation |
