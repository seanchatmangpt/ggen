# 07. Truex Lifecycle Spec

**[DOC_ONLY]** Truex governs the enterprise lifecycle of receipted consequence. Genesis is the kernel that constructs local consequence inside the part.

## 1. Truex Thesis
**[DOC_ONLY]** Truex is the governing lifecycle for receipted execution trust. It does not compute state transitions; it orchestrates, verifies, and accounts for the verifiable transitions (receipts) produced by Genesis-bearing interchangeable parts. It provides the enterprise context around mathematically pure operations.

## 2. Relationship to Genesis
**[DOC_ONLY]** Genesis is the pure Chatman Equation foundation that guarantees `A = μ(O)`. Truex operates entirely outside of Genesis. It receives the `BLAKE3` cryptographic receipts that Genesis produces. Truex must never attempt to recalculate consequence, parse internal Genesis state directly, or act as an alternative evaluation kernel. It only validates the cryptographic proofs and unbroken receipt chains.

## 3. Relationship to ggen
**[DOC_ONLY]** `ggen` is the foundry and membrane that manufactures the interchangeable parts and projects their evidence.
**[PARTIAL]** Truex relies on `ggen` (`crates/ggen-projection/`) to project the Genesis output so it can be ingested into the enterprise lifecycle. Truex is the rule-engine governing the gates (e.g., `tools/truth-gate/src/main.rs`) that authorize these projections.

## 4. Relationship to Interchangeable Parts
**[DOC_ONLY]** The interchangeable parts exist in AtomVM or WASM custody. Truex treats these parts as black boxes characterized completely by their manifest (binary hash, identity) and their cryptographic receipts. Truex ensures that only authorized parts contribute to the global state and manages the lifecycle of these parts from deployment to decommissioning.

## 5. Lifecycle States
1. **Attempt:** An external trigger attempts to provide input to a part. **[DOC_ONLY]**
2. **Hook:** Truex pre-intercepts the action to verify part authorization. **[DOC_ONLY]**
3. **Admission/Refusal:** Genesis kernel physically accepts or rejects the input (`Construct8`). **[DOC_ONLY]**
4. **Durable Motion:** Genesis performs the local cursor transition. **[DOC_ONLY]**
5. **Receipt:** Genesis emits a `BLAKE3` proof of the state change. **[IMPLEMENTED]** (`crates/ggen-core/src/receipt.rs`)
6. **Projection:** The `ggen` membrane projects the receipt to the Truex boundary. **[PARTIAL]**
7. **Promotion:** Truex authorizes the receipt and promotes it to the enterprise Shard. **[MISSING]**

## 6. Promotion Criteria
**[DOC_ONLY]** Promotion is the act of accepting a local part's consequence into the global enterprise reality. Truex will only promote if:
- The receipt's `BLAKE3` chain strictly links to the last known enterprise state.
- The part's WASM hash exactly matches the authorized Truex manifest.
- The execution trace passes strict anti-cheating validation (no mocked evidence).

## 7. Accounting Model
**[MISSING]** Truex must implement a ledger that tracks tuple volume and computation per part. Because Truex observes every receipted state change, it handles chargeback, rate-limiting, and economic resource allocation without complicating the Genesis kernel.
**Definition of Done:** A Truex accounting module capable of aggregating receipted tuple counts by part identity over a time window.

## 8. Receipt Model
**[IMPLEMENTED]** Truex consumes the standard Genesis `BLAKE3` receipts.
**[MISSING]** Truex must emit its own "Promotion Receipt" when an enterprise Shard accepts a segment of Genesis receipts. This higher-level receipt proves to downstream validators (like `wasm4pm`) that the consequence was not only mathematically valid but administratively authorized.

## 9. Refusal Model
**[DOC_ONLY]** When Genesis refuses a transition (e.g., producing a refusal state instead of durable motion), Truex classifies and routes the refusal. A local physical boundary mismatch becomes an actionable enterprise incident (e.g., routing to a human review queue or triggering an automated remediation workflow).

## 10. Replay Model
**[MISSING]** Execution trust requires determinism. Truex orchestrates replays by feeding historical inputs back into the exact WASM binary of a part. If the resulting Genesis receipt does not perfectly match the historically promoted receipt, Truex flags a catastrophic trust violation.
**Definition of Done:** A Truex replay command that accepts an input log and a WASM part, executing it in custody to assert identical `BLAKE3` output.

## 11. Audit/Export Model
**[PARTIAL]** Truex prepares the receipted history for downstream consumption by external engines. It triggers the export of OCEL logs for `wasm4pm` / `pictl` process intelligence validation, and PROV/DCAT records for enterprise regulators. Truex ensures no data is projected to `open-ontologies` unless it has passed the promotion lifecycle.

## 12. Role in Blue River Dam
**[DOC_ONLY]** If the enterprise architecture is the Blue River Dam:
- **Interchangeable Parts** are the turbines.
- **Genesis** is the laws of physics operating inside the turbines.
- **ggen** is the foundry that casts the turbines and the penstock that channels water to them.
- **Truex** is the control room. It monitors the wattage (receipts), decides which turbines are online, records the power output, and halts turbines that vibrate out of spec (refusals/mismatches).

## 13. Definition of Done
**[MISSING]** The Truex Lifecycle is considered fully implemented when:
1. `tools/truth-gate/` successfully parses a stream of Genesis receipts from an AtomVM/WASM part.
2. Truex cryptographically verifies the unbroken chain of the receipts without needing to run Genesis internally.
3. Truex evaluates the Shard promotion criteria and emits a "Promotion Receipt".
4. Truex successfully detects and rejects simulated/mocked traces (adhering to the Anti-Cheating doctrine).
5. The replay orchestrator can successfully prove historical determinism of a promoted transaction.

## Lifecycle Stage Routing

| Stage | Input | Owner | Output | Receipt | Replay | Refusal | Promotion rule |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Attempt** | External Stream | Truex | Validated Request | None | None | Identity/Auth Error | N/A |
| **Hook** | Validated Request | Truex | Routed Input | None | None | Policy Block | N/A |
| **Admission**| Routed Input | Genesis | `Construct8` / Need | None | None | Boundary Mismatch | N/A |
| **Durable Motion** | `Construct8` | Genesis | Local State Transition | Local `BLAKE3` | Cursor Sync | Local invariant failure | N/A |
| **Projection** | Transition | ggen | Evidence payload | Segment Receipt | Log capture | Projection Failure | N/A |
| **Promotion** | Segment Receipt | Truex | Enterprise Shard | Promotion Receipt | Full Execution Replay | Signature/Chain Invalid | Must match authorized part hash, unbroken chain |

## Interop Boundaries

### Truex ↔ Genesis (via Part Custody)
- **Owner:** Truex
- **Input:** `BLAKE3` Genesis Receipts
- **Output:** Promotion / Rejection
- **Proof:** Promotion Receipt
- **Replay:** Truex feeds inputs to part, matches output receipt
- **Refusal:** Invalid signature, broken chain, unmatched hash
- **Validation Path:** Cryptographic ledger verification (`tools/truth-gate/`) **[PARTIAL]**

### Truex ↔ ggen
- **Owner:** Truex
- **Input:** ggen manufactured part and projection logs
- **Output:** Execution Policy and Deployment Auth
- **Proof:** Policy Manifest Hash
- **Replay:** N/A (Static policy enforcement)
- **Refusal:** Policy violation during deployment
- **Validation Path:** Manifest validation at load time **[MISSING]**

### Truex ↔ wasm4pm / External Checkers
- **Owner:** wasm4pm
- **Input:** OCEL/PROV exported by Truex
- **Output:** Process Conformance Score
- **Proof:** pm4py analysis report
- **Replay:** Re-ingest OCEL logs
- **Refusal:** Temporal or dependency invariant failure in process
- **Validation Path:** `wasm4pm` offline validation pipeline **[PARTIAL]**
