# Truex Lifecycle Specification

## 1. Truex Thesis
Truex governs the enterprise lifecycle of receipted consequence. Genesis is the kernel that constructs local consequence inside the part. Truex manages how that local consequence is authorized, promoted, and audited across the broader system.

## 2. Relationship to Genesis
Genesis handles the `A = μ(O)` pure math construction. Truex wraps this mathematically sound packet in business-level execution trust. Genesis proves "what happened"; Truex decides "is this allowed to become enterprise reality".

## 3. Relationship to ggen
ggen builds the part and projects the output. Truex provides the orchestration policies that tell ggen *when* to project and to whom.

## 4. Relationship to Interchangeable Parts
Truex tracks the inventory of Genesis-bearing interchangeable parts. It verifies their cryptographic manifests to ensure only authorized parts are contributing to the enterprise corpus.

## 5. Lifecycle States
1. **Attempt:** Raw source motion mapped into a `RelationPage`.
2. **Hook:** Pre-construction intercept.
3. **Admission/Refusal:** Genesis `Construct8` evaluation.
4. **Durable Motion:** Receipt generation.
5. **Promotion:** Truex authorizes the rollup into the enterprise corpus.

## 6. Promotion Criteria
A local receipt is promoted to a Shard only if:
- The receipt signature is valid.
- The `O*` used was authorized for that part.
- No refusal artifacts block the path.

## 7. Accounting Model
Truex maintains the ledger of what parts consumed what inputs and produced what consequence, enabling chargeback and audit trailing.

## 8. Receipt Model
Truex verifies Genesis receipts but adds its own "Promotion Receipt" indicating that the enterprise accepted the part's proof.

## 9. Refusal Model
Truex treats Genesis refusals as actionable incidents, triggering human review, automated remediation, or source system alerts.

## 10. Replay Model
Truex orchestrates the replay of historical receipts for audit purposes, feeding old inputs back into Genesis to prove identical outputs.

## 11. Audit/Export Model
Truex triggers ggen to export PROV and DCAT records for compliance regulators.

## 12. Role in Blue River Dam
Truex is the gatekeeper at the dam. The interchangeable parts sit at the riverhead; Truex decides which streams are permitted to flow into the downstream lakehouse.

## 13. Definition of Done
Truex is DONE when it can automatically verify a Genesis receipt, promote it to a Shard, log the accounting, and correctly flag a Genesis refusal for review.

| Stage | Input | Owner | Output | Receipt | Replay | Refusal | Promotion rule |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Attempt** | Source Stream | ggen | `RelationPage` | None | None | Format Error | None |
| **Admission**| `RelationPage`| Genesis| `Construct8` | Packet Hash | Cursor | Need9/Need257 | Must have receipt |
| **Promotion**| `Construct8` | Truex | Shard | Segment Hash| Full Trace | Policy Block | Signature valid |

| Status | Component | File/Artifact Evidence |
| :--- | :--- | :--- |
| **MISSING** | Truex Promotion Logic | Needs implementation |
| **PARTIAL** | Truex Gate Shell | `tools/truth-gate/src/main.rs` |
