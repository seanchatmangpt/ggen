# C4 Level 5: Evidence Plane - Separation of Evidence from Control/Data Planes

**Document Purpose**: Define the evidence plane as a distinct, immutable ledger separate from the data and control planes. Prove tamper-free execution through hash-chain verification, SPARQL evidence extraction, and cryptographic receipts.

**Version**: 1.0 | **Date**: 2026-01-25

---

## Three-Plane Architecture

The autonomic reconciliation engine separates concerns across **3 distinct planes**:

| Plane | Purpose | State | Storage | Queries |
|-------|---------|-------|---------|---------|
| **Control Plane** | Governance logic, policy evaluation, decision-making | Stateless (ephemeral) | Memory, GCS (policy cache) | Real-time (FSM state) |
| **Data Plane** | Action execution, resource mutation, effects | Mutable (GCP resources change) | GCP infrastructure (IAM, firewall, quotas) | GCP APIs (describe resource state) |
| **Evidence Plane** | Immutable action audit trail, decision proof, tamper detection | Append-only ledger | Firestore (region-local) + Cloud Logging (archive) | SPARQL queries (reconstruct timeline) |

---

## Evidence Plane Diagram

```mermaid
graph TD
    subgraph Control["Control Plane (Ephemeral)"]
        FSM["gen_statem FSM<br/>(policy evaluation)"]
        PolicyCache["Policy Cache<br/>(policy-pack.ttl)"]
    end

    subgraph Data["Data Plane (Mutable)"]
        IAMBindings["IAM Bindings<br/>(GCP)"]
        FirewallRules["Firewall Rules<br/>(GCP)"]
        ResourceQuotas["Resource Quotas<br/>(GCP)"]
    end

    subgraph Evidence["Evidence Plane (Immutable)"]
        ReceiptGen["Receipt Generator<br/>(Evidence Sidecar)"]
        HashChain["Hash-Chain Builder<br/>(Merkle-linked)"]
        FirestoreLedger["Firestore Ledger<br/>(region-local)"]
        CloudLogging["Cloud Logging<br/>(audit mirror)"]
        GCSArchive["GCS Archive<br/>(long-term)"]
    end

    subgraph Verify["Verification (SPARQL)"]
        ReceiptVerifier["Receipt Verifier<br/>(Signature validation)"]
        ChainValidator["Chain Validator<br/>(Hash proof)"]
        TimelineReconstructor["Timeline Reconstructor<br/>(SPARQL queries)"]
    end

    FSM -->|Decision<br/>(action_type,<br/>parameters)| Data
    Data -->|Action Result<br/>(success/failure)| ReceiptGen

    ReceiptGen -->|Receipt<br/>(action, decision_path,<br/>entitlement_context)| HashChain
    HashChain -->|Merkle Link<br/>(hash_chain_link=<br/>sha256(prev))| FirestoreLedger
    FirestoreLedger -->|Stream Export| CloudLogging
    CloudLogging -->|Archive| GCSArchive

    FirestoreLedger -->|Load Receipts| ReceiptVerifier
    ReceiptVerifier -->|Verify Signatures<br/>(Ed25519)| ChainValidator
    ChainValidator -->|Verify Hash Chain<br/>(Merkle proof)| TimelineReconstructor

    TimelineReconstructor -->|SPARQL SELECT<br/>Audit Trail| Verify

    classDef ephemeral fill:#FFE5E5
    classDef mutable fill:#FFD9D9
    classDef immutable fill:#45B7D1
    classDef verify fill:#96CEB4

    class FSM,PolicyCache ephemeral
    class IAMBindings,FirewallRules,ResourceQuotas mutable
    class ReceiptGen,HashChain,FirestoreLedger,CloudLogging,GCSArchive immutable
    class ReceiptVerifier,ChainValidator,TimelineReconstructor verify
```

---

## Evidence Plane Architecture

### Separation Principle: "No Receipt, No Action"

The core rule of the evidence plane is **jidoka enforcement**:

```
IF action_executed AND receipt_generation_failed:
  THEN refuse_entire_action (rollback, emit refusal receipt)
ELSE IF action_executed AND receipt_generated:
  THEN action_committed (immutable proof available)
```

**Implementation**:
```rust
// Pseudocode: Evidence Plane Jidoka Enforcement
pub async fn execute_action_with_evidence(
    action: Action,
    signal_trace: Vec<SignalId>,
    decision_path: Vec<PolicyRule>
) -> Result<Receipt> {
    // Step 1: Execute action in data plane
    let action_result = data_plane::execute(action.clone()).await?;

    // Step 2: Generate receipt in evidence plane
    let receipt = evidence_plane::generate_receipt(
        action_id: action.id,
        signal_trace,
        decision_path,
        action_result,
        entitlement_context: &current_entitlement,
    ).await?;

    // Step 3: If receipt generation fails, REFUSE ENTIRE ACTION (jidoka)
    // No partial states allowed
    receipt
}
```

---

## Evidence Plane Components

### 1. Receipt Generator (Evidence Sidecar)

**Responsibility**: Create cryptographic proof of action

**Input**:
- action_id (UUID)
- timestamp (ISO 8601 UTC)
- signal_trace (Vec[SignalId])
- decision_path (Vec[(RuleName, Result)])
- action_executed (ActionType)
- action_result (success | failure | refused | postponed)
- entitlement_context (TokenHash, Principal, SKUScope)

**Output**: Receipt JSON (unsigned)

**Example Receipt**:
```json
{
  "action_id": "recv-2026-01-25-001",
  "timestamp": "2026-01-25T14:32:45.123Z",
  "region": "us-central1",
  "signal_trace": [
    "sig-2026-01-25-monitoring-spike-001",
    "sig-2026-01-25-alert-threshold-002"
  ],
  "decision_path": [
    {
      "rule": "policy/security/threat-detection",
      "matched": true,
      "result": true
    },
    {
      "rule": "policy/scope/within-sku",
      "matched": true,
      "result": true
    },
    {
      "rule": "policy/consent/auto-approve",
      "matched": false,
      "result": null
    }
  ],
  "action_executed": {
    "type": "iam_binding_update",
    "parameters": {
      "binding_update": "remove_service_account_from_editor_role",
      "resource": "projects/gov-buyer-project-123"
    },
    "capability_required": "iam.serviceAccountAdmin"
  },
  "action_result": {
    "status": "success",
    "affected_resources": 1,
    "side_effects": [
      "service_account_removed_from_editor_role"
    ]
  },
  "entitlement_context": {
    "token_hash": "sha256(eyJhbGc...)",
    "principal": "service-autonomics@gov-buyer-project-123.iam.gserviceaccount.com",
    "sku_scope": "security-autonomics-premium",
    "sku_capabilities": [
      "iam.serviceAccountAdmin",
      "compute.securityAdmin",
      "storage.admin"
    ]
  }
}
```

---

### 2. Hash-Chain Builder (Merkle-Linked Proof)

**Responsibility**: Link current receipt to prior receipt, forming immutable chain

**Algorithm**:
```
1. Load previous receipt from Firestore (or null if first receipt)
2. Serialize previous receipt to canonical JSON string
3. Compute: prev_hash = SHA256(previous_receipt_json)
4. Insert into current receipt: hash_chain_link = prev_hash
5. Return modified receipt (ready for signing)
```

**Hash-Chain Example** (3 receipts):

```
Receipt 1:
  action_id: recv-001
  timestamp: 2026-01-25T14:30:00Z
  hash_chain_link: null (first receipt)
  ──────────────────────────────────────
  sha256(receipt_1_json) = H1 = "abc123..."

Receipt 2:
  action_id: recv-002
  timestamp: 2026-01-25T14:31:00Z
  hash_chain_link: H1 = "abc123..."
  ──────────────────────────────────────
  sha256(receipt_2_json) = H2 = "def456..."

Receipt 3:
  action_id: recv-003
  timestamp: 2026-01-25T14:32:00Z
  hash_chain_link: H2 = "def456..."
  ──────────────────────────────────────
  sha256(receipt_3_json) = H3 = "ghi789..."

Proof: H1 → H2 → H3 (immutable chain)
To forge receipt 2: Must also change H2, then must change receipt 3's hash_chain_link (detected as inconsistency)
```

**Storage** (Firestore):
```
Collection: evidence/{region}/receipts
Document ID: recv-{action_id}

Fields:
  action_id: "recv-001"
  timestamp: Timestamp(2026-01-25T14:30:00Z)
  hash_chain_link: "abc123..."
  signature: "ed25519(...)"
  ... (all receipt fields)

Composite Index:
  (timestamp ASC, result ASC)  → enable "all failures on date X"
  (timestamp ASC, action_type ASC)  → enable "all IAM updates on date X"
```

---

### 3. Signature Generator (Ed25519 Cryptographic Proof)

**Responsibility**: Sign receipt to prove integrity + authenticity

**Algorithm**:
```
1. Serialize receipt to canonical JSON (sorted keys, no whitespace)
2. Compute: msg = Bytes(canonical_json)
3. Sign: sig = Ed25519_sign(msg, private_key)
4. Insert into receipt: signature = hex_encode(sig)
5. Store receipt in Firestore
```

**Key Management**:
- Private Key: Stored in GCP Secret Manager
  - Secret ID: `autonomics-receipt-signer-{region}`
  - Rotation: Monthly (automatic)
  - Access: Cloud Run service account only
- Public Key: Distributed to auditors (hardcoded or fetched from well-known endpoint)

**Verification** (Auditor):
```
1. Load receipt from Firestore
2. Extract signature field (hex-decode to bytes)
3. Remove signature from receipt, re-serialize to canonical JSON
4. Verify: Ed25519_verify(canonical_json, signature, public_key)
5. IF signature valid: Receipt tamper-free
6. IF signature invalid: Receipt forged or modified (ALERT)
```

---

### 4. Firestore Ledger (Region-Local)

**Collection Schema**:

```
evidence/{region}/receipts/{action_id}
  ├── action_id (String): UUID
  ├── timestamp (Timestamp): ISO 8601 UTC
  ├── signal_trace (Array[String]): List of signal IDs
  ├── decision_path (Array[Map]): Policy rules evaluated
  ├── action_executed (Map): Type + parameters
  ├── action_result (Map): Status + side effects
  ├── entitlement_context (Map): Token hash + principal + SKU scope
  ├── hash_chain_link (String): SHA256 of previous receipt
  ├── signature (String): Ed25519 signature (hex)
  └── _metadata (Map): Firestore-managed (createTime, updateTime, etc.)
```

**Indexes** (Composite):
```
Index 1: (timestamp ASC, result ASC)
  → Query: "All refusals on 2026-01-25"
  → SQL: SELECT * WHERE timestamp >= 2026-01-25 AND result = "refused"

Index 2: (timestamp ASC, action_type ASC)
  → Query: "All IAM updates between 2026-01-20 and 2026-01-25"
  → SQL: SELECT * WHERE action_type = "iam_binding_update" AND timestamp >= ... AND timestamp <= ...

Index 3: (principal ASC, timestamp ASC)
  → Query: "All actions by principal X on 2026-01-25"
  → SQL: SELECT * WHERE entitlement_context.principal = "..." AND timestamp >= ...
```

**Replication**:
- Region-local writes: Cloud Run (us-central1) writes to regional Firestore instance
- Global replication: Firestore multi-region reads (eventual consistency, <5s typical)
- Fallback: If Firestore unavailable, queue to Cloud Storage (local cache file)

**Retention**:
- Active ledger: 7 days (fast queries, hot data)
- Archive: Cloud Storage (nightly export from Cloud Logging, 7-year retention)

---

### 5. Cloud Logging Mirror (Audit Trail)

**Purpose**: Create tamper-proof copy of receipts outside Firestore (defense in depth)

**Flow**:
```
Receipt → Evidence Sidecar → Pub/Sub (evidence-stream topic)
                                    ↓
                        Cloud Logging Export (subscription)
                                    ↓
                        Cloud Logging (audit log entry)
                                    ↓
                        Cloud Storage (automatic export, nightly)
                                    ↓
                        GCS Archive (gs://autonomics-audit-archive/{date}.json.gz)
```

**Log Entry Format** (Cloud Logging):
```json
{
  "logName": "projects/gov-buyer-project-123/logs/autonomics-evidence",
  "timestamp": "2026-01-25T14:32:45.123Z",
  "severity": "INFO",
  "jsonPayload": {
    "event_type": "receipt_generated",
    "action_id": "recv-001",
    "receipt": { ... full receipt JSON ... }
  },
  "resource": {
    "type": "cloud_run_revision",
    "labels": {
      "service_name": "autonomics-governor",
      "region": "us-central1"
    }
  }
}
```

**Retention**:
- Cloud Logging: 30 days (fast search)
- Cloud Storage Archive: 7 years (long-term compliance)

---

## Verification Path (Evidence Extraction & Validation)

### SPARQL Evidence Queries

**Use Case 1: "Show all actions on 2026-01-25"**

```sparql
PREFIX receipt: <http://autonomics.ggen/receipt/>
PREFIX evidence: <http://autonomics.ggen/evidence/>

SELECT ?action_id ?timestamp ?action_type ?result
WHERE {
  ?receipt a receipt:AutonomicReceipt ;
    receipt:actionId ?action_id ;
    receipt:timestamp ?timestamp ;
    receipt:actionExecuted ?action ;
    receipt:result ?result .
  ?action evidence:type ?action_type .
  FILTER(?timestamp >= "2026-01-25T00:00:00Z"^^xsd:dateTime)
  FILTER(?timestamp < "2026-01-26T00:00:00Z"^^xsd:dateTime)
}
ORDER BY ?timestamp
```

**Use Case 2: "Show all refusals by reason"**

```sparql
PREFIX receipt: <http://autonomics.ggen/receipt/>

SELECT ?reason (COUNT(?action_id) as ?count)
WHERE {
  ?receipt a receipt:AutonomicReceipt ;
    receipt:actionId ?action_id ;
    receipt:result "refused" ;
    receipt:refusalReason ?reason .
}
GROUP BY ?reason
ORDER BY ?count DESC
```

**Use Case 3: "Show all interventions requiring consent"**

```sparql
PREFIX receipt: <http://autonomics.ggen/receipt/>
PREFIX intervention: <http://autonomics.ggen/intervention/>

SELECT ?action_id ?timestamp ?consent_result ?approver_id
WHERE {
  ?receipt a receipt:AutonomicReceipt ;
    receipt:actionId ?action_id ;
    receipt:timestamp ?timestamp ;
    receipt:interventionType "warn" ;
    receipt:interventionDetails ?details .
  ?details intervention:consentResult ?consent_result ;
    intervention:approverId ?approver_id .
}
ORDER BY ?timestamp DESC
```

---

### Receipt Verification Algorithm (Auditor)

```rust
pub async fn verify_receipt_chain(
    action_ids: Vec<String>,
    start_timestamp: DateTime,
    end_timestamp: DateTime,
    expected_count: usize,
) -> Result<VerificationReport> {
    // Step 1: Load all receipts from Firestore
    let mut receipts = Vec::new();
    for action_id in action_ids {
        let receipt = firestore::load_receipt(action_id).await?;
        receipts.push(receipt);
    }

    // Step 2: Sort by timestamp (should form ordered sequence)
    receipts.sort_by_key(|r| r.timestamp);

    // Step 3: Verify each receipt individually
    let mut verification_report = VerificationReport::new();
    for (i, receipt) in receipts.iter().enumerate() {
        // 3a. Verify Ed25519 signature
        let canonical_json = serde_json::to_string(&receipt)?;
        let is_valid = ed25519::verify(
            &canonical_json.as_bytes(),
            &hex::decode(&receipt.signature)?,
            &PUBLIC_KEY,
        )?;
        if !is_valid {
            verification_report.add_failure(
                receipt.action_id.clone(),
                VerificationFailure::SignatureInvalid,
            );
            continue;
        }

        // 3b. Verify hash-chain link
        if i == 0 {
            // First receipt should have null hash_chain_link
            if receipt.hash_chain_link.is_some() {
                verification_report.add_failure(
                    receipt.action_id.clone(),
                    VerificationFailure::FirstReceiptHasChainLink,
                );
            }
        } else {
            // Verify link to previous receipt
            let prev_receipt = &receipts[i - 1];
            let prev_json = serde_json::to_string(&prev_receipt)?;
            let prev_hash = sha256(&prev_json.as_bytes());
            if receipt.hash_chain_link != Some(prev_hash.to_string()) {
                verification_report.add_failure(
                    receipt.action_id.clone(),
                    VerificationFailure::ChainLinkMismatch,
                );
            }
        }

        // 3c. Verify timestamp ordering
        if i > 0 {
            let prev_receipt = &receipts[i - 1];
            if receipt.timestamp <= prev_receipt.timestamp {
                verification_report.add_failure(
                    receipt.action_id.clone(),
                    VerificationFailure::TimestampOutOfOrder,
                );
            }
        }

        // 3d. Verify entitlement context (principal, SKU scope)
        // ... additional validation ...
    }

    // Step 4: Summary
    verification_report.set_total_receipts(receipts.len());
    verification_report.set_expected_count(expected_count);
    if receipts.len() != expected_count {
        verification_report.add_warning(
            format!("Expected {} receipts, found {}", expected_count, receipts.len())
        );
    }

    Ok(verification_report)
}
```

---

## Jidoka Enforcement: "No Receipt, No Action"

The evidence plane enforces the critical rule:

```
∀ action ∈ data_plane:
  IF receipt_generated(action) THEN action_committed
  ELSE action_refused (rollback, emit refusal_receipt)
```

**Implementation in Governor Service**:

```rust
// FSM State: executing
// Event: action_result
pub fn handle_action_result(
    event: ActionResult,
    current_state: &mut FSMState,
) -> Result<FSMEvent> {
    // Step 1: Route action result to Evidence Sidecar
    let receipt = evidence_sidecar::generate_receipt(
        action_id: event.action_id,
        action_result: event.result,
        ... other context ...
    ).await?;

    // Step 2: If receipt generation FAILS (return Err), REFUSE ACTION
    match receipt {
        Ok(receipt) => {
            // Receipt generated successfully
            // Action is now immutable (proof exists)
            current_state.transition_to(FSMState::Emitting);
            Ok(FSMEvent::ReceiptGenerated(receipt))
        }
        Err(e) => {
            // Receipt generation failed (e.g., Firestore unavailable)
            // REFUSE entire action (jidoka enforcement)
            log::error!("Receipt generation failed, refusing action: {}", e);

            // Emit refusal receipt (even though primary action failed)
            let refusal_receipt = evidence_sidecar::generate_refusal_receipt(
                action_id: event.action_id,
                refusal_reason: "receipt_generation_failed",
            ).await?;

            current_state.transition_to(FSMState::Idle);
            Err(AutonomicsError::ActionRefused(refusal_receipt))
        }
    }
}
```

---

## Glossary Cross-Reference

- **Evidence Plane**: Immutable, append-only ledger for action audit trail (Firestore + Cloud Logging)
- **Receipt**: Cryptographically signed proof of action (includes decision path, signal trace, result)
- **Hash-Chain**: Merkle-linked receipts; each receipt includes SHA256 of previous (immutable proof)
- **Jidoka**: Stop-the-line enforcement rule: "No receipt, no action" (fail-safe)
- **Signature Generator**: Ed25519 cryptographic proof of receipt authenticity
- **SPARQL Evidence Query**: RDF query language for reconstructing action timeline from receipts
- **Canonical JSON**: Deterministic JSON serialization (sorted keys, no whitespace) for signing
- **Firestore Ledger**: Region-local immutable document store for receipts
- **Cloud Logging Mirror**: Tamper-proof audit trail copy (defense in depth)
- **Verification Path**: Process to validate receipt integrity (signatures, hash-chain, ordering)

---

## Receipt Contract (Evidence Plane)

**Storage Guarantee**:
- Every action generates a receipt (success, failure, refusal, or postponement)
- Every receipt is signed (Ed25519)
- Every receipt is hash-chained (Merkle-linked to prior)
- Every receipt is persisted (Firestore + Cloud Logging mirror)
- Every receipt is queryable (SPARQL, within 5s of generation)

**Verification Guarantee**:
- Auditor can verify entire receipt chain from any starting point
- Signature validation proves receipt authenticity (not forged)
- Hash-chain validation proves receipt ordering (not reordered)
- Timestamp ordering validation proves timeline accuracy (not backdated)
- Entitlement context validation proves principal authorization (not spoofed)

**Refusal Guarantee**:
- If receipt generation fails → Action refused (jidoka enforcement)
- Refusal receipt generated and persisted (even on failure)
- Timeline reconstructable (no lost actions, no silent failures)
- Auditors can prove what happened (or didn't) on any date

---

## Definition of Done

- [ ] Three-plane diagram: Control (ephemeral), Data (mutable), Evidence (immutable)
- [ ] Evidence plane flowchart: Receipt → Hash-chain → Firestore → Cloud Logging → GCS
- [ ] Receipt Generator: Input/output documented, JSON schema defined
- [ ] Hash-Chain Builder: Merkle linking algorithm explained with 3-receipt example
- [ ] Signature Generator: Ed25519 process documented, key management specified
- [ ] Firestore schema: Collection structure, fields, composite indexes
- [ ] Cloud Logging mirror: Flow diagram, log entry format, export strategy
- [ ] SPARQL queries: 3 evidence queries provided (timeline, refusals, interventions)
- [ ] Verification algorithm: Rust pseudocode for receipt chain validation
- [ ] Jidoka enforcement: "No receipt, no action" rule implemented
- [ ] Canonical JSON: Sorting and serialization rules specified
- [ ] Signature verification: Algorithm for auditor validation
- [ ] Hash-chain validation: Proof of immutability through Merkle linking
- [ ] Timeline reconstruction: SPARQL evidence extraction documented
- [ ] Performance SLOs: Receipt generation <50ms, Firestore write <1s
- [ ] Retention policy: 7-day active ledger, 7-year archive
- [ ] Tamper detection: How signature/hash-chain violations are detected

---

## Key Takeaways: Evidence Plane as Fifth Pillar

The evidence plane is **not a afterthought** — it's a **co-equal pillar** alongside control and data planes:

1. **Control Plane**: Decides what to do (policy evaluation, FSM)
2. **Data Plane**: Does it (executes action, mutates resources)
3. **Evidence Plane**: Proves what happened (receipt, hash-chain, SPARQL queries)

**Separation Benefits**:
- **Auditability**: Complete timeline reconstructable from receipts
- **Tamper Detection**: Signature validation proves integrity
- **Immutability**: Hash-chain makes rewriting history impossible (detectable)
- **Compliance**: Regulatory proof without manual investigation
- **Troubleshooting**: SPARQL queries isolate problems to specific signals/actions

**Jidoka Enforcement**:
- System fails safely (refuses action if receipt generation fails)
- No partial states (action committed iff receipt committed)
- No silent failures (every action → receipt → ledger)

---

**Complete**: See [c4-context.md](c4-context.md) for Level 1 government buyer context.
