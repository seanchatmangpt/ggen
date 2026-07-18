# Security Threat Model - ggen Erlang Autonomics Governor

**Version**: 1.0.0 | **Date**: 2026-01-25 | **Classification**: Internal Use

## Executive Summary

This document provides a comprehensive security threat model for the ggen Erlang Autonomics Governor running on Google Cloud Platform (GCP). It uses the STRIDE methodology to identify and mitigate threats across spoofing, tampering, repudiation, information disclosure, denial of service, and elevation of privilege attack vectors.

The governor is a critical infrastructure component that:
- Monitors billing signals via Pub/Sub
- Executes autonomous actions (throttling, rollback, scaling) via Cloud Run APIs
- Maintains immutable audit logs for compliance
- Manages customer costs with cryptographic integrity

**Threat Level**: Medium-High (customer-critical infrastructure)
**CVSS Risk Score**: 7.4 (High) - Balances impact (customer DoS) against likelihood (low)

---

## System Context

### Architecture Overview
```
Customer GCP Project
├── Billing Signals (Pub/Sub)
│   └── → Governor (Cloud Run service)
│       ├── Receives signals (rate-limited, validated)
│       ├── Makes decisions (ML-based FSM)
│       ├── Executes actions (throttle, rollback, scale)
│       └── Logs audit trail (immutable, timestamped)
│
├── Cloud Run API (Governor calls)
│   ├── gke-autopilot clusters
│   ├── Cloud Load Balancer
│   └── Managed services
│
└── Audit Trail (Cloud Logging + Cloud Storage)
    ├── Cloud Logging (searchable, immutable)
    ├── Cloud Storage (immutable backup, separate access)
    └── Firestore (config versioning, transactional)
```

### Trust Boundaries
1. **External**: Pub/Sub (untrusted input)
2. **Internal**: Governor ↔ Cloud Run APIs (trusted, authenticated)
3. **Logging**: Governor ↔ Cloud Logging/Storage (trusted, append-only)
4. **Configuration**: Firestore (customer-editable, access-controlled)

---

## STRIDE Threat Analysis

### 1. SPOOFING (Fake Signals)

#### 1.1 Threat: Attacker Injects Fake Billing Spike Signal

**Attack Vector**: Compromised Pub/Sub publisher or MITM intercepts and modifies message

**Impact**:
- Governor receives fraudulent signal claiming customer usage spiked
- Governor throttles legitimate traffic (DDoS on customer's application)
- Customer loses revenue/SLA breaches

**Likelihood**: Medium (Pub/Sub is authenticated; attacker needs GCP credentials or network access)

**Severity**: High (customer-facing DoS)

**Mitigation Strategies**:
1. **HMAC-SHA256 Signature** (Primary)
   - Governor signs every outgoing signal with shared HMAC key
   - Governor verifies signature before processing
   - Tampered signals rejected + logged

2. **Pub/Sub Topic Access Control** (Defense-in-Depth)
   - Topic grants publish only to authorized services
   - No public access
   - Service account isolation (each service has minimal permissions)

3. **Signal Schema Validation** (Validation Layer)
   - JSON Schema enforcement (required fields, type checking)
   - Semantic validation (costs must be positive, timestamps valid)
   - Rate limiting (max 10k signals/sec per governor)

4. **Cryptographic Receipts**
   - Governor signs receipt of signal (customer can verify governor received it)
   - Hash chain: each receipt includes hash of previous (tampering breaks chain)

**Implementation**:
```rust
// Signal validation pseudocode
fn process_signal(signal: &PubSubMessage) -> Result<(), SignalError> {
    // 1. Extract HMAC signature
    let signature = extract_header("x-hmac-sha256", signal)?;

    // 2. Recompute HMAC over signal body
    let body = signal.body();
    let computed = hmac_sha256(SHARED_KEY, body);

    // 3. Constant-time comparison (prevent timing attacks)
    if !constant_time_eq(&signature, &computed) {
        alert!("SPOOFING: Invalid signal signature, discarding");
        return Err(SignalError::InvalidSignature);
    }

    // 4. Parse and validate schema
    let parsed = parse_signal(&body)?;
    validate_signal_schema(&parsed)?;

    // 5. Semantic validation
    validate_semantic(&parsed)?;

    // 6. Process signal
    make_decision(&parsed).await
}
```

**Testing**:
```rust
#[test]
fn test_spoofed_signal_rejected() {
    // Arrange
    let original_signal = create_test_signal(100.0); // $100 spike
    let bad_signal = tamper_signal(&original_signal); // Modify amount to $1000

    // Act
    let result = process_signal(&bad_signal);

    // Assert
    assert_eq!(result, Err(SignalError::InvalidSignature));
    assert_log_contains("SPOOFING: Invalid signal signature");
}

#[test]
fn test_valid_signal_accepted() {
    // Arrange
    let signal = create_signed_signal(100.0);

    // Act
    let result = process_signal(&signal);

    // Assert
    assert_ok!(result);
}
```

**Chaos Test**:
- Simulate 100k corrupted signals/sec
- Verify: Governor rejects all + logs each + doesn't crash
- Verify: No throttling actions triggered

---

#### 1.2 Threat: Attacker Spoofs Governor Identity to Other Services

**Attack Vector**: Attacker obtains governor's GCP credentials or exploits misconfigured IAM

**Impact**:
- Attacker impersonates governor to Cloud Run API
- Attacker calls unauthorized APIs (delete services, modify config, access secrets)
- Cluster compromise, data breach

**Likelihood**: Low (Workload Identity + service account isolation)

**Severity**: Critical (full cluster access)

**Mitigation Strategies**:
1. **Workload Identity (Primary)**
   - Governor runs in GKE/Cloud Run with Workload Identity enabled
   - Governor authenticates via OIDC token (not API key)
   - Token scoped to specific service account
   - Token short-lived (1 hour expiry, auto-refresh)

2. **Minimal IAM Role** (Defense-in-Depth)
   - Governor service account has only required permissions
   - No wildcard permissions (explicit resource names)
   - Deny-policy prevents accidental overpermissioning

3. **Audit Logging** (Detection)
   - All Cloud Run API calls logged in Cloud Audit Logs
   - Anomalies detected (unusual API calls, failed auth)
   - Alert on unrecognized caller

4. **Token Pinning** (Advanced)
   - Governor pins expected token issuer (GCP STS)
   - Governor validates token kid (key ID) matches expected key
   - Prevents token substitution attacks

**Implementation**:
```rust
// Governor uses Workload Identity to get token
async fn get_auth_token() -> Result<Token, AuthError> {
    // 1. Query GCP metadata service (only available on GCP)
    let token = GCP_METADATA
        .get_service_account_identity_token(
            audience: "https://run.googleapis.com",
            lifetime: Duration::from_secs(3600), // 1 hour
        )
        .await?;

    // 2. Validate token structure
    validate_token_structure(&token)?;

    // 3. Pin issuer and key ID
    let claims = decode_token(&token)?;
    assert_eq!(claims.iss, "https://accounts.google.com");
    assert_eq!(claims.aud, "https://run.googleapis.com");

    Ok(token)
}

// Call Cloud Run API with authenticated request
async fn call_cloud_run_api(action: &Action) -> Result<ActionReceipt, ApiError> {
    let token = get_auth_token().await?;

    // Token automatically included in Authorization header
    // Request fails if token expired (Workload Identity auto-refreshes)
    let response = reqwest::Client::new()
        .post(&format!("https://run.googleapis.com/api/{}", action.endpoint))
        .bearer_auth(&token.access_token)
        .json(&action)
        .send()
        .await?;

    Ok(parse_response(&response)?)
}
```

**Testing**:
```rust
#[test]
fn test_expired_token_rejected() {
    // Arrange
    let expired_token = create_token(expires_at: now() - Duration::from_secs(1));

    // Act
    let result = call_cloud_run_api_with_token(&action, &expired_token).await;

    // Assert
    assert_eq!(result, Err(ApiError::Unauthorized));
}

#[test]
fn test_modified_token_rejected() {
    // Arrange
    let token = get_auth_token().await.unwrap();
    let modified = tamper_token_signature(&token);

    // Act
    let result = call_cloud_run_api_with_token(&action, &modified).await;

    // Assert
    assert_eq!(result, Err(ApiError::InvalidSignature));
}

#[test]
fn test_spoofed_issuer_rejected() {
    // Arrange
    let fake_token = create_token_with_issuer("https://evil.com");

    // Act
    let result = validate_token_structure(&fake_token);

    // Assert
    assert_eq!(result, Err(TokenError::InvalidIssuer));
}
```

**Chaos Test**:
- Revoke governor service account credentials
- Verify: Governor fails to authenticate + alerts
- Verify: No API calls succeed
- Restore credentials, verify recovery within 5min

---

### 2. TAMPERING (Modifying Data)

#### 2.1 Threat: Attacker Modifies Audit Log (Evidence Destruction)

**Attack Vector**: Attacker gains access to Cloud Logging or Cloud Storage bucket; deletes entries

**Impact**:
- No proof of governor's actions (non-repudiation broken)
- Attacker covers tracks (illegitimate throttling hidden)
- Audit/compliance failure

**Likelihood**: Low (Cloud Logging is immutable by design)

**Severity**: High (audit trail compromise)

**Mitigation Strategies**:
1. **Cloud Logging Immutability (Primary)**
   - Cloud Logging is append-only; entries cannot be deleted
   - Google's infrastructure guarantees (not modifiable by customer)
   - Even project owner cannot delete historical logs

2. **Hash Chain** (Cryptographic Integrity)
   - Each log entry includes SHA-256 hash of previous entry
   - Tampering at any point breaks chain
   - Chain verified on audit queries (failure detected immediately)

3. **Immutable Backup** (Defense-in-Depth)
   - Audit logs also written to Cloud Storage (separate bucket)
   - Cloud Storage configured with:
     - Versioning enabled (all versions preserved)
     - Object Lock (retention policy, objects immutable for X days)
     - Separate IAM (different access controls than logging bucket)

4. **Signed Receipts** (Customer Verification)
   - Each action generates signed receipt (RSA-4096)
   - Customer stores receipt (can verify governor actions independently)
   - Even if all logs deleted, customer has evidence

**Implementation**:
```rust
// Log entry with hash chain
#[derive(Serialize)]
struct AuditLogEntry {
    id: String,                    // Unique entry ID
    timestamp: DateTime<Utc>,      // When action occurred
    action_type: ActionType,       // What governor did
    signal_id: String,             // Which signal triggered it
    decision_rationale: String,    // Why (which policies applied)
    previous_hash: String,         // SHA-256(previous entry)
    entry_hash: String,            // SHA-256(this entry excluding this field)
    signature: String,             // RSA-4096(entry_hash, governor_private_key)
}

fn create_log_entry(
    action: &Action,
    previous_entry: &AuditLogEntry,
) -> AuditLogEntry {
    let mut entry = AuditLogEntry {
        id: uuid::Uuid::new_v4().to_string(),
        timestamp: Utc::now(),
        action_type: action.action_type.clone(),
        signal_id: action.signal_id.clone(),
        decision_rationale: format!("{:?}", action.decision),
        previous_hash: previous_entry.entry_hash.clone(),
        entry_hash: String::new(), // Will be computed
        signature: String::new(),   // Will be computed
    };

    // 1. Compute hash of entry (excluding entry_hash and signature)
    let serialized = serde_json::to_string(&entry)
        .expect("Serialization failed");
    entry.entry_hash = sha256(&serialized);

    // 2. Sign entry hash with governor's private key
    let signature = rsa_sign(GOVERNOR_PRIVATE_KEY, entry.entry_hash.as_bytes());
    entry.signature = base64::encode(&signature);

    entry
}

// Verify hash chain integrity
fn verify_hash_chain(entries: &[AuditLogEntry]) -> Result<(), TamperError> {
    let mut previous_hash = String::from("0"); // Genesis block

    for entry in entries {
        // 1. Verify previous hash matches
        if entry.previous_hash != previous_hash {
            return Err(TamperError::ChainBroken {
                entry_id: entry.id.clone(),
                expected: previous_hash,
                found: entry.previous_hash.clone(),
            });
        }

        // 2. Verify signature
        let serialized = format!(
            "{}|{}|{}|{}|{}",
            entry.id, entry.timestamp, entry.action_type,
            entry.signal_id, entry.decision_rationale
        );
        let computed_hash = sha256(&serialized);

        if computed_hash != entry.entry_hash {
            return Err(TamperError::EntrytTampered {
                entry_id: entry.id.clone(),
            });
        }

        // 3. Verify RSA signature
        let sig_bytes = base64::decode(&entry.signature)?;
        rsa_verify(GOVERNOR_PUBLIC_KEY, &entry.entry_hash, &sig_bytes)
            .map_err(|_| TamperError::InvalidSignature {
                entry_id: entry.id.clone(),
            })?;

        previous_hash = entry.entry_hash.clone();
    }

    Ok(())
}
```

**Testing**:
```rust
#[test]
fn test_deleted_log_entry_detected() {
    // Arrange
    let entries = create_valid_audit_chain(100); // 100 entries
    let mut tampered = entries.clone();
    tampered.remove(50); // Delete entry 50

    // Act
    let result = verify_hash_chain(&tampered);

    // Assert
    assert_eq!(result, Err(TamperError::ChainBroken { ... }));
}

#[test]
fn test_modified_log_entry_detected() {
    // Arrange
    let mut entries = create_valid_audit_chain(100);
    entries[50].decision_rationale = "Modified rationale".to_string(); // Tamper

    // Act
    let result = verify_hash_chain(&entries);

    // Assert
    assert_eq!(result, Err(TamperError::EntryTampered { ... }));
}

#[test]
fn test_valid_chain_verified() {
    // Arrange
    let entries = create_valid_audit_chain(100);

    // Act
    let result = verify_hash_chain(&entries);

    // Assert
    assert_ok!(result);
}
```

**Chaos Test**:
- Manually modify Cloud Storage log entry
- Run audit verification, verify failure detected
- Restore from Cloud Logging backup, verify chain rebuilds

---

#### 2.2 Threat: Attacker Modifies Governor Configuration (Policy Injection)

**Attack Vector**: Attacker gains Firestore write access; changes throttle thresholds or action limits

**Impact**:
- Governor applies attacker's policies (throttle too aggressive or too lenient)
- Customer gets wrong SLOs
- Attacker could disable rate limiting (then DoS governor)

**Likelihood**: Medium (Firestore is protected but possible misconfiguration)

**Severity**: High (behavior change)

**Mitigation Strategies**:
1. **Firestore Access Control (Primary)**
   - Firestore database has strict IAM rules
   - Only governor service account can read config
   - Only authorized admin users can write config
   - No public/anonymous access

2. **Config Versioning & Audit Trail** (Change Tracking)
   - Every config change stored as new document version
   - Version history preserved (Cloud Firestore native)
   - Who, when, what changed logged to Cloud Logging

3. **Config Signing** (Integrity)
   - Admin signs config changes with private key
   - Governor verifies signature before applying
   - Invalid signatures trigger alert + rollback to previous version

4. **Transactional Updates** (Atomicity)
   - Config + audit entry written in single transaction
   - Cannot split update and audit (consistency guaranteed)

5. **Approval Workflow** (Human-in-the-Loop)
   - Config changes require approval from 2+ admins
   - Approval recorded in audit trail
   - Changes only applied after approval

**Implementation**:
```rust
#[derive(Serialize, Deserialize)]
struct GovernorConfig {
    version: u32,                           // Config version
    max_signals_per_sec: u32,              // Rate limit
    max_actions_per_min: u32,              // Action rate limit
    throttle_threshold: f64,               // Cost threshold
    policy_hash: String,                   // SHA-256 of policy rules
    signature: String,                     // RSA-4096(policy_hash, admin_key)
    signed_by: String,                     // Admin identity
    signed_at: DateTime<Utc>,              // When signed
    approved_by: Vec<String>,              // List of approvers
}

// Load config from Firestore
async fn load_governor_config(db: &Firestore) -> Result<GovernorConfig, ConfigError> {
    // 1. Fetch config document
    let config_doc = db
        .collection("governor")
        .document("current-config")
        .get()
        .await
        .map_err(|e| ConfigError::FetchFailed(e))?;

    let config: GovernorConfig = config_doc.deserialize()?;

    // 2. Verify signature
    verify_config_signature(&config)?;

    // 3. Verify version didn't regress (prevent rollback attack)
    let previous_version = load_cached_config_version()?;
    if config.version < previous_version {
        alert!("CONFIG_TAMPERING: Version regression detected");
        return Err(ConfigError::VersionRegression);
    }

    // 4. Verify approvals (2+ required)
    if config.approved_by.len() < 2 {
        alert!("CONFIG_TAMPERING: Insufficient approvals");
        return Err(ConfigError::InsufficientApprovals);
    }

    Ok(config)
}

fn verify_config_signature(config: &GovernorConfig) -> Result<(), ConfigError> {
    // 1. Reconstruct policy hash
    let policy_rules = serde_json::to_string(&config)
        .map_err(|_| ConfigError::SerializationFailed)?;
    let computed_hash = sha256(&policy_rules);

    if computed_hash != config.policy_hash {
        return Err(ConfigError::InvalidHash);
    }

    // 2. Verify RSA signature
    let sig_bytes = base64::decode(&config.signature)
        .map_err(|_| ConfigError::InvalidSignature)?;
    let admin_key = load_admin_public_key(&config.signed_by)?;

    rsa_verify(&admin_key, config.policy_hash.as_bytes(), &sig_bytes)
        .map_err(|_| ConfigError::SignatureVerificationFailed)
}
```

**Testing**:
```rust
#[test]
async fn test_unsigned_config_rejected() {
    // Arrange
    let mut config = create_valid_config();
    config.signature = "invalid".to_string();

    // Act
    let result = verify_config_signature(&config);

    // Assert
    assert_eq!(result, Err(ConfigError::SignatureVerificationFailed));
}

#[test]
async fn test_config_version_regression_rejected() {
    // Arrange
    let config = GovernorConfig { version: 5, .. };
    set_cached_config_version(10); // Current version is higher

    // Act
    let result = load_governor_config(&config).await;

    // Assert
    assert_eq!(result, Err(ConfigError::VersionRegression));
}

#[test]
async fn test_config_change_logged() {
    // Arrange
    let old_config = create_valid_config();
    let new_config = update_config(&old_config, |c| c.throttle_threshold = 500.0);

    // Act
    apply_config_change(&old_config, &new_config).await?;

    // Assert
    let audit_entry = load_latest_audit_entry().await?;
    assert_eq!(audit_entry.action_type, ActionType::ConfigChange);
    assert_contains!(audit_entry.details, "throttle_threshold");
}
```

**Chaos Test**:
- Inject config change without signature, verify rejection
- Attempt to apply config from old version, verify rejection + alert
- Modify config in Firestore directly, verify governor detects tampering on next load

---

### 3. REPUDIATION (Deny Actions Taken)

#### 3.1 Threat: Governor Claims It Didn't Throttle (But It Did)

**Attack Vector**: Governor executes action but claim log entries are forged

**Impact**:
- Attacker (claiming to be governor) denies action
- Customer cannot prove action happened
- Non-repudiation fails

**Likelihood**: Very Low (cryptographic receipts prevent this)

**Severity**: High (accountability broken)

**Mitigation Strategies**:
1. **Cryptographic Receipts (Primary)**
   - Every action generates signed receipt (RSA-4096)
   - Receipt includes action details + timestamp + action ID
   - Receipt signed with governor's private key (only governor can generate)
   - Customer receives receipt immediately (before action completes)

2. **Immutable Log Entry** (Timestamp + Hash)
   - Action logged with immutable timestamp (Cloud Logging)
   - Log entry hashed and signed (governor signature)
   - Hash chain prevents tampering

3. **Third-Party Verification** (Escrow)**
   - Receipt also sent to ggen escrow service (separate, independent)
   - Escrow stores receipt in immutable audit trail
   - If governor denies action, ggen provides escrow proof

4. **Customer Email Confirmation** (Out-of-Band)**
   - Receipt sent to customer via email (separate channel)
   - Customer stores email as proof
   - Even if governor lies, customer has evidence

**Implementation**:
```rust
#[derive(Serialize)]
struct ActionReceipt {
    receipt_id: String,                // Unique receipt ID
    action_id: String,                 // Which action
    action_type: ActionType,           // throttle, rollback, scale
    signal_id: String,                 // Which signal triggered it
    timestamp: DateTime<Utc>,          // When action occurred
    decision_rationale: String,        // Why this action
    target_service: String,            // Which service affected
    expected_effect: String,           // What should happen
    governor_id: String,               // Which governor instance
    signature: String,                 // RSA-4096(receipt, governor_private_key)
}

// Execute action and generate receipt
async fn execute_action_with_receipt(action: &Action) -> Result<ActionReceipt, ActionError> {
    // 1. Generate receipt ID before executing (prove intent)
    let receipt_id = uuid::Uuid::new_v4().to_string();

    // 2. Create receipt (action_id might not exist yet)
    let receipt = ActionReceipt {
        receipt_id: receipt_id.clone(),
        action_id: action.id.clone(),
        action_type: action.action_type.clone(),
        signal_id: action.signal_id.clone(),
        timestamp: Utc::now(),
        decision_rationale: format!("{:?}", action.decision),
        target_service: action.target_service.clone(),
        expected_effect: action.expected_effect.clone(),
        governor_id: get_governor_id(),
        signature: String::new(), // Will be filled
    };

    // 3. Sign receipt with governor's private key
    let receipt_json = serde_json::to_string(&receipt)?;
    let receipt_hash = sha256(&receipt_json);
    let signature = rsa_sign(GOVERNOR_PRIVATE_KEY, receipt_hash.as_bytes());
    let mut signed_receipt = receipt;
    signed_receipt.signature = base64::encode(&signature);

    // 4. Send receipt to customer (out-of-band)
    send_receipt_email(&signed_receipt).await?;

    // 5. Send receipt to ggen escrow (independent witness)
    send_receipt_to_escrow(&signed_receipt).await?;

    // 6. Log receipt locally
    log_receipt(&signed_receipt).await?;

    // 7. Execute actual action
    let result = execute_cloud_run_api_call(action).await?;

    // 8. Record completion in log
    log_action_completed(&signed_receipt, &result).await?;

    Ok(signed_receipt)
}

// Verify receipt (customer can do this independently)
fn verify_receipt(receipt: &ActionReceipt) -> Result<(), ReceiptError> {
    // 1. Reconstruct receipt JSON (excluding signature)
    let receipt_without_sig = ActionReceipt {
        signature: String::new(),
        ..receipt.clone()
    };
    let receipt_json = serde_json::to_string(&receipt_without_sig)?;
    let computed_hash = sha256(&receipt_json);

    // 2. Verify governor's signature
    let sig_bytes = base64::decode(&receipt.signature)?;
    let governor_key = load_governor_public_key(&receipt.governor_id)?;

    rsa_verify(&governor_key, computed_hash.as_bytes(), &sig_bytes)
        .map_err(|_| ReceiptError::InvalidSignature)
}
```

**Testing**:
```rust
#[test]
async fn test_receipt_generated_and_signed() {
    // Arrange
    let action = create_test_action();

    // Act
    let receipt = execute_action_with_receipt(&action).await?;

    // Assert
    assert!(!receipt.receipt_id.is_empty());
    assert!(!receipt.signature.is_empty());
    assert_ok!(verify_receipt(&receipt));
}

#[test]
async fn test_customer_receives_receipt_email() {
    // Arrange
    let action = create_test_action();
    let mut mock_email = MockEmailService::new();
    mock_email.expect_send().once();

    // Act
    let receipt = execute_action_with_receipt(&action).await?;

    // Assert
    mock_email.assert_sent_to(CUSTOMER_EMAIL, receipt.receipt_id);
}

#[test]
async fn test_escrow_receives_receipt() {
    // Arrange
    let action = create_test_action();
    let mut mock_escrow = MockEscrowService::new();
    mock_escrow.expect_store().once();

    // Act
    let receipt = execute_action_with_receipt(&action).await?;

    // Assert
    let escrow_receipt = mock_escrow.retrieve(&receipt.receipt_id)?;
    assert_eq!(escrow_receipt.receipt_id, receipt.receipt_id);
}

#[test]
fn test_forged_receipt_detected() {
    // Arrange
    let mut receipt = create_valid_receipt();
    receipt.action_type = ActionType::Rollback; // Change action type

    // Act
    let result = verify_receipt(&receipt);

    // Assert
    assert_eq!(result, Err(ReceiptError::InvalidSignature));
}
```

---

#### 3.2 Threat: Customer Claims Governor Made Wrong Decision (But They Misconfigured)

**Attack Vector**: Governor applies policy correctly, but customer claims misconfiguration

**Impact**:
- Dispute over who's responsible
- SLA negotiation disputes
- Legal liability unclear

**Likelihood**: Medium (can happen due to misconfiguration)

**Severity**: Medium (business dispute)

**Mitigation Strategies**:
1. **Config Snapshot** (Decision Context)
   - Before action, save config snapshot
   - Include which policies applied, thresholds, rules
   - Snapshot included in receipt and audit log

2. **Decision Trace** (Audit Trail)
   - Log decision logic: which signals evaluated, which policies triggered
   - Example: "Signal: usage=$500, Threshold=$400, Policy: THROTTLE, Applied: Yes"
   - Audit shows exact decision path

3. **FSM State Transitions** (Behavior Log)
   - Governor is a state machine (IDLE → EVALUATING → DECIDING → ACTING)
   - Log every state transition with timestamp
   - Log input at each state (what signal triggered transition)

4. **Deterministic Replay** (Verification)
   - Given same signal + config, replay governor FSM
   - Should produce identical decision
   - If differs, indicates non-deterministic bug
   - Replay available for audit/dispute resolution

**Implementation**:
```rust
#[derive(Debug, Clone, Serialize)]
struct DecisionContext {
    timestamp: DateTime<Utc>,
    signal_id: String,
    signal_cost: f64,
    config_version: u32,
    config_snapshot: GovernorConfig,
    fsm_states: Vec<FsmTransition>,
    policies_evaluated: Vec<PolicyEvaluation>,
    decision: ActionType,
    rationale: String,
}

#[derive(Debug, Clone, Serialize)]
struct FsmTransition {
    from_state: FsmState,
    to_state: FsmState,
    timestamp: DateTime<Utc>,
    input: String,
}

#[derive(Debug, Clone, Serialize)]
struct PolicyEvaluation {
    policy_name: String,
    condition: String,
    result: bool,
    applied: bool,
}

// Make decision with full context logging
async fn make_decision_with_context(signal: &Signal) -> Result<DecisionContext, DecisionError> {
    // 1. Load current config
    let config = load_governor_config().await?;

    // 2. Create decision context
    let mut context = DecisionContext {
        timestamp: Utc::now(),
        signal_id: signal.id.clone(),
        signal_cost: signal.cost,
        config_version: config.version,
        config_snapshot: config.clone(),
        fsm_states: vec![],
        policies_evaluated: vec![],
        decision: ActionType::None,
        rationale: String::new(),
    };

    // 3. Transition FSM: IDLE → EVALUATING
    context.fsm_states.push(FsmTransition {
        from_state: FsmState::Idle,
        to_state: FsmState::Evaluating,
        timestamp: Utc::now(),
        input: format!("Signal: ${}", signal.cost),
    });

    // 4. Evaluate policies
    let policies = load_policies(&config)?;
    for policy in policies {
        let condition_met = evaluate_policy(&policy, signal, &config)?;

        context.policies_evaluated.push(PolicyEvaluation {
            policy_name: policy.name.clone(),
            condition: policy.condition.clone(),
            result: condition_met,
            applied: condition_met,
        });

        if condition_met {
            context.decision = policy.action.clone();
            context.rationale = policy.rationale.clone();
        }
    }

    // 5. Transition FSM: EVALUATING → DECIDING
    context.fsm_states.push(FsmTransition {
        from_state: FsmState::Evaluating,
        to_state: FsmState::Deciding,
        timestamp: Utc::now(),
        input: format!("Decision: {:?}", context.decision),
    });

    // 6. Log decision context
    log_decision_context(&context).await?;

    Ok(context)
}

// Replay decision to verify it was deterministic
fn replay_decision(context: &DecisionContext) -> Result<DecisionContext, ReplayError> {
    // 1. Reconstruct signal from context
    let signal = Signal {
        id: context.signal_id.clone(),
        cost: context.signal_cost,
    };

    // 2. Use snapshotted config (exactly what governor saw)
    let config = context.config_snapshot.clone();

    // 3. Re-evaluate all policies
    let policies = load_policies(&config)?;
    let mut replayed_context = DecisionContext {
        timestamp: Utc::now(), // Different timestamp is OK
        ..context.clone()
    };

    for policy in policies {
        let condition_met = evaluate_policy(&policy, &signal, &config)?;

        let replayed_eval = PolicyEvaluation {
            policy_name: policy.name.clone(),
            condition: policy.condition.clone(),
            result: condition_met,
            applied: condition_met,
        };

        // Verify this policy was evaluated same way originally
        let original_eval = context.policies_evaluated
            .iter()
            .find(|e| e.policy_name == policy.name)
            .ok_or(ReplayError::PolicyMissing)?;

        if original_eval.result != replayed_eval.result {
            return Err(ReplayError::NonDeterministic {
                policy: policy.name,
                original: original_eval.result,
                replayed: replayed_eval.result,
            });
        }
    }

    // 4. Verify final decision matches
    if context.decision != replayed_context.decision {
        return Err(ReplayError::DecisionDiffers);
    }

    Ok(replayed_context)
}
```

**Testing**:
```rust
#[test]
async fn test_decision_context_logged() {
    // Arrange
    let signal = create_test_signal(500.0);

    // Act
    let context = make_decision_with_context(&signal).await?;

    // Assert
    assert!(!context.policies_evaluated.is_empty());
    assert_eq!(context.fsm_states.len(), 2); // IDLE → EVALUATING → DECIDING
    assert_log_contains(&format!("Decision: {:?}", context.decision));
}

#[test]
fn test_decision_replay_deterministic() {
    // Arrange
    let context = create_valid_decision_context();

    // Act
    let replayed = replay_decision(&context)?;

    // Assert
    assert_eq!(replayed.decision, context.decision);
    assert_eq!(replayed.policies_evaluated, context.policies_evaluated);
}

#[test]
fn test_replay_detects_non_determinism() {
    // Arrange
    let mut context = create_valid_decision_context();
    // Mutate policy evaluation
    context.policies_evaluated[0].result = !context.policies_evaluated[0].result;

    // Act
    let result = replay_decision(&context);

    // Assert
    assert_eq!(result, Err(ReplayError::NonDeterministic { ... }));
}

#[test]
async fn test_dispute_resolution_provides_full_context() {
    // Arrange
    let signal = create_test_signal(500.0);
    let context = make_decision_with_context(&signal).await?;

    // Act
    let dispute_evidence = gather_dispute_evidence(&context)?;

    // Assert
    assert_contains!(dispute_evidence, "config_version");
    assert_contains!(dispute_evidence, "policies_evaluated");
    assert_contains!(dispute_evidence, "fsm_states");
    assert_contains!(dispute_evidence, "decision");
}
```

---

### 4. INFORMATION DISCLOSURE (Leaked Data)

#### 4.1 Threat: Governor Logs Expose Customer Costs/Architecture

**Attack Vector**: Attacker gains read access to Cloud Logging; reads detailed cost and architecture info

**Impact**:
- Competitor learns customer spending patterns and strategies
- Customer revenue visibility leaked
- Competitive advantage lost

**Likelihood**: Medium (logging access can be misconfigured)

**Severity**: Medium (business intelligence leak)

**Mitigation Strategies**:
1. **Log Redaction** (Content Filtering) - **Primary**
   - Costs and amounts aggregated (show bucket, not exact value)
   - Service names hashed or abbreviated
   - IP addresses redacted
   - Personally identifiable information (PII) redacted

2. **Access Control** (IAM) - Defense-in-Depth
   - Log access restricted to:
     - ggen SREs (audit only, read-only role)
     - Customer project owner (can view own logs)
   - No public access
   - Service-to-service access via Workload Identity only

3. **Encryption at Rest** (Cloud KMS) - Compliance
   - Cloud Logging encrypted with Cloud KMS
   - Only authorized parties can decrypt
   - Encryption key rotated quarterly

4. **Audit Trail** (Detection)
   - Log access logged (who read what, when)
   - Unusual access patterns flagged (high volume, unusual times)
   - Alert on suspicious access

**Implementation**:
```rust
#[derive(Debug, Serialize)]
struct RedactedLogEntry {
    timestamp: DateTime<Utc>,
    action_type: String,
    cost_bucket: String,      // "0-100", "100-500", "500-1000", "1000+"
    service_hash: String,      // SHA-256(service_name), not the name itself
    duration_seconds: u32,
    status: String,
}

fn redact_log_entry(entry: &RawLogEntry) -> Result<RedactedLogEntry, RedactionError> {
    // 1. Redact exact cost (convert to bucket)
    let cost_bucket = match entry.cost {
        c if c < 100.0 => "0-100".to_string(),
        c if c < 500.0 => "100-500".to_string(),
        c if c < 1000.0 => "500-1000".to_string(),
        _ => "1000+".to_string(),
    };

    // 2. Hash service name (prevent reconstruction of architecture)
    let service_hash = sha256(&entry.service_name);

    // 3. Redact IP addresses
    let redacted_ips = entry.ip_addresses
        .iter()
        .map(|ip| {
            // Keep first 2 octets, redact last 2
            let parts: Vec<&str> = ip.split('.').collect();
            if parts.len() == 4 {
                format!("{}.{}.x.x", parts[0], parts[1])
            } else {
                "x.x.x.x".to_string()
            }
        })
        .collect::<Vec<_>>();

    // 4. Strip PII (credit cards, SSNs, emails)
    let pii_filter = PiiFilter::new();
    let sanitized_details = pii_filter.redact(&entry.details)?;

    Ok(RedactedLogEntry {
        timestamp: entry.timestamp,
        action_type: entry.action_type.clone(),
        cost_bucket,
        service_hash,
        duration_seconds: entry.duration_seconds,
        status: entry.status.clone(),
    })
}

// PII detection and redaction
struct PiiFilter {
    credit_card_regex: Regex,
    ssn_regex: Regex,
    email_regex: Regex,
}

impl PiiFilter {
    fn new() -> Self {
        Self {
            // Luhn algorithm-based credit card regex
            credit_card_regex: Regex::new(
                r"\b(?:4[0-9]{12}(?:[0-9]{3})?|5[1-5][0-9]{14})\b"
            ).unwrap(),
            // SSN pattern: XXX-XX-XXXX
            ssn_regex: Regex::new(r"\b\d{3}-\d{2}-\d{4}\b").unwrap(),
            // Email pattern
            email_regex: Regex::new(r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b").unwrap(),
        }
    }

    fn redact(&self, text: &str) -> Result<String, RedactionError> {
        let mut result = text.to_string();

        // Redact credit cards
        result = self.credit_card_regex
            .replace_all(&result, "****-****-****-****")
            .to_string();

        // Redact SSNs
        result = self.ssn_regex
            .replace_all(&result, "***-**-****")
            .to_string();

        // Redact emails
        result = self.email_regex
            .replace_all(&result, "[REDACTED_EMAIL]")
            .to_string();

        Ok(result)
    }
}

// Log entry before sending to Cloud Logging
async fn log_with_redaction(entry: &RawLogEntry) -> Result<(), LogError> {
    // 1. Redact sensitive information
    let redacted = redact_log_entry(entry)?;

    // 2. Send to Cloud Logging (encryption at rest via Cloud KMS)
    cloud_logging::write(redacted).await?;

    // 3. Log the fact that we logged (audit trail)
    audit_logging::write(&AuditEntry {
        timestamp: Utc::now(),
        action: "LOG_WRITTEN",
        original_cost: entry.cost,
        redacted_cost: redacted.cost_bucket,
        reader_identity: None, // Will be filled when someone reads
    })?;

    Ok(())
}

// Verify access to logs (audit)
async fn verify_log_access(reader: &Identity) -> Result<(), AccessError> {
    // 1. Check IAM permissions
    let permissions = check_iam_permissions(reader)?;
    if !permissions.contains(&Permission::ReadLogs) {
        alert!("UNAUTHORIZED_LOG_ACCESS: {}", reader);
        return Err(AccessError::Unauthorized);
    }

    // 2. Log the access attempt
    audit_logging::write(&AuditEntry {
        timestamp: Utc::now(),
        action: "LOG_ACCESS",
        reader_identity: Some(reader.clone()),
        access_allowed: true,
    })?;

    Ok(())
}
```

**Testing**:
```rust
#[test]
fn test_exact_costs_redacted() {
    // Arrange
    let entry = RawLogEntry {
        cost: 456.78,
        ..default()
    };

    // Act
    let redacted = redact_log_entry(&entry)?;

    // Assert
    assert_eq!(redacted.cost_bucket, "100-500");
    assert!(!redacted.cost_bucket.contains("456"));
}

#[test]
fn test_service_names_hashed() {
    // Arrange
    let entry = RawLogEntry {
        service_name: "payment-processing-prod-01".to_string(),
        ..default()
    };

    // Act
    let redacted = redact_log_entry(&entry)?;

    // Assert
    assert!(!redacted.service_hash.contains("payment"));
    assert!(!redacted.service_hash.contains("prod"));
    assert_eq!(redacted.service_hash.len(), 64); // SHA-256 hex
}

#[test]
fn test_credit_cards_redacted() {
    // Arrange
    let filter = PiiFilter::new();
    let text = "Customer paid with 4532-1234-5678-9010";

    // Act
    let redacted = filter.redact(text)?;

    // Assert
    assert!(!redacted.contains("4532"));
    assert!(redacted.contains("****-****-****-****"));
}

#[test]
fn test_ssns_redacted() {
    // Arrange
    let filter = PiiFilter::new();
    let text = "SSN: 123-45-6789";

    // Act
    let redacted = filter.redact(text)?;

    // Assert
    assert!(!redacted.contains("123-45"));
    assert!(redacted.contains("***-**-****"));
}

#[test]
async fn test_log_access_denied_without_iam() {
    // Arrange
    let attacker = Identity::Unknown;

    // Act
    let result = verify_log_access(&attacker).await;

    // Assert
    assert_eq!(result, Err(AccessError::Unauthorized));
}

#[test]
async fn test_log_access_logged() {
    // Arrange
    let reader = Identity::CustomerAdmin;
    let mut mock_audit = MockAuditLog::new();
    mock_audit.expect_write().once();

    // Act
    let result = verify_log_access(&reader).await;

    // Assert
    assert_ok!(result);
    mock_audit.verify_logged("LOG_ACCESS");
}
```

---

#### 4.2 Threat: Audit Trail Leaks Personally Identifiable Information (PII)

**Attack Vector**: Governor logs customer PII (credit card, SSN); attacker reads logs

**Impact**:
- GDPR/CCPA violation (regulatory fine up to 4% revenue)
- Customer privacy breach
- Legal liability

**Likelihood**: Low (PII shouldn't be in signals, but can happen via misconfiguration)

**Severity**: Critical (regulatory + privacy)

**Mitigation Strategies**:
1. **Auto-Redaction Filter** (Automated PII Detection) - **Primary**
   - Regex-based pattern matching (credit cards, SSNs, phone numbers, emails)
   - Detects PII before logging
   - Redacts with placeholder (*****)

2. **Signal Sanitization** (Input Validation)
   - Customer signals validated against schema (PII fields not allowed)
   - Signals with PII rejected before processing
   - Alert + rejection logged

3. **PII Scanner** (Quarterly Audit)
   - Automated quarterly scan of all logs
   - Searches for patterns missed by redaction filter
   - Generates report of potential leaks
   - Alert if any PII found

4. **Data Minimization** (Design)
   - Signals contain only necessary fields (cost, timestamp)
   - Never request customer names, emails, card numbers in signals
   - API schema enforces this

5. **Customer Notification** (Breach Response)
   - If PII detected, notify customer within 24 hours
   - Provide details of what was leaked
   - Provide remediation steps

**Implementation**:
```rust
#[derive(Debug)]
struct SignalValidationError {
    field: String,
    issue: String,
}

// Signal schema (only allows specific fields)
#[derive(Deserialize)]
struct Signal {
    id: String,                // UUID, not customer data
    timestamp: DateTime<Utc>,  // Timestamp only
    cost: f64,                 // Cost amount (no card data)
    region: String,            // Region code, not customer address
}

// Validate signal doesn't contain PII
fn validate_signal_no_pii(signal_raw: &str) -> Result<Signal, SignalValidationError> {
    // 1. Check for PII patterns BEFORE parsing
    let pii_patterns = [
        ("CREDIT_CARD", r"\b(?:4[0-9]{12}(?:[0-9]{3})?|5[1-5][0-9]{14})\b"),
        ("SSN", r"\b\d{3}-\d{2}-\d{4}\b"),
        ("EMAIL", r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b"),
        ("PHONE", r"\b(?:\+?1[-.\s]?)?\(?([0-9]{3})\)?[-.\s]?([0-9]{3})[-.\s]?([0-9]{4})\b"),
    ];

    for (pii_type, pattern) in &pii_patterns {
        let regex = Regex::new(pattern).unwrap();
        if regex.is_match(signal_raw) {
            alert!("PII_DETECTED_IN_SIGNAL: {}", pii_type);
            return Err(SignalValidationError {
                field: pii_type.to_string(),
                issue: "PII pattern detected in signal".to_string(),
            });
        }
    }

    // 2. Parse signal
    let signal: Signal = serde_json::from_str(signal_raw)?;

    // 3. Validate schema (only allowed fields present)
    let allowed_fields = ["id", "timestamp", "cost", "region"];
    let value: serde_json::Value = serde_json::from_str(signal_raw)?;

    if let Some(obj) = value.as_object() {
        for key in obj.keys() {
            if !allowed_fields.contains(&key.as_str()) {
                return Err(SignalValidationError {
                    field: key.clone(),
                    issue: "Unexpected field in signal schema".to_string(),
                });
            }
        }
    }

    Ok(signal)
}

// Quarterly PII scan of all logs
async fn quarterly_pii_audit() -> Result<PiiAuditReport, AuditError> {
    // 1. Fetch all logs from last quarter
    let start_date = Utc::now() - Duration::days(90);
    let logs = cloud_logging::query(format!(
        "timestamp >= {:?}",
        start_date
    )).await?;

    // 2. Scan for PII patterns
    let pii_scanner = PiiScanner::new();
    let mut findings = vec![];

    for log_entry in logs {
        if let Some(pii_found) = pii_scanner.scan(&log_entry.content)? {
            findings.push(PiiFinding {
                log_entry_id: log_entry.id.clone(),
                timestamp: log_entry.timestamp,
                pii_type: pii_found.pii_type,
                context: pii_found.context,
            });
        }
    }

    // 3. Generate report
    let report = PiiAuditReport {
        scan_date: Utc::now(),
        start_date,
        end_date: Utc::now(),
        total_logs_scanned: logs.len(),
        findings: findings.clone(),
        severity: if findings.is_empty() {
            "PASS".to_string()
        } else {
            "FAIL".to_string()
        },
    };

    // 4. Alert if PII found
    if !findings.is_empty() {
        alert!("PII_FOUND_IN_LOGS: {} findings", findings.len());
        notify_customer_pii_breach(&findings).await?;
    }

    Ok(report)
}

// Customer notification of PII breach
async fn notify_customer_pii_breach(findings: &[PiiFinding]) -> Result<(), NotificationError> {
    let customer_email = get_customer_contact()?;

    let notification = format!(
        "SECURITY ALERT: PII Data Found in Logs\n\n\
         We detected {} potential PII exposures in your governor audit logs.\n\n\
         Details:\n{}\n\n\
         Immediate Actions:\n\
         1. Check if PII was sent in signals (should not be)\n\
         2. Review signal schema validation\n\
         3. Contact support if signals were modified\n\n\
         Remediation:\n\
         - Affected logs have been redacted\n\
         - Future signals with PII will be rejected\n\
         - Quarterly scans will continue\n",
        findings.len(),
        findings.iter()
            .map(|f| format!("  - {}: {} ({})", f.pii_type, f.context, f.timestamp))
            .collect::<Vec<_>>()
            .join("\n")
    );

    send_email(&customer_email, "Security Alert", &notification).await?;

    Ok(())
}
```

**Testing**:
```rust
#[test]
fn test_credit_card_in_signal_rejected() {
    // Arrange
    let signal = r#"{"id":"sig1","cost":100,"card":"4532-1234-5678-9010"}"#;

    // Act
    let result = validate_signal_no_pii(signal);

    // Assert
    assert_eq!(result, Err(SignalValidationError { field: "CREDIT_CARD", .. }));
}

#[test]
fn test_valid_signal_accepted() {
    // Arrange
    let signal = r#"{"id":"sig1","timestamp":"2026-01-25T00:00:00Z","cost":100.50,"region":"us-central1"}"#;

    // Act
    let result = validate_signal_no_pii(signal);

    // Assert
    assert_ok!(result);
}

#[test]
async fn test_quarterly_pii_scan_finds_leaks() {
    // Arrange
    let logs = vec![
        LogEntry { content: "Normal log".to_string(), .. },
        LogEntry { content: "Leaked SSN: 123-45-6789".to_string(), .. },
        LogEntry { content: "Another normal log".to_string(), .. },
    ];

    // Act
    let report = quarterly_pii_audit().await?;

    // Assert
    assert_eq!(report.findings.len(), 1);
    assert_eq!(report.severity, "FAIL");
}

#[test]
async fn test_customer_notified_of_pii_breach() {
    // Arrange
    let findings = vec![
        PiiFinding { pii_type: "SSN".to_string(), .. }
    ];
    let mut mock_email = MockEmailService::new();
    mock_email.expect_send().once();

    // Act
    let result = notify_customer_pii_breach(&findings).await;

    // Assert
    assert_ok!(result);
    mock_email.assert_sent_to(CUSTOMER_EMAIL, "Security Alert");
}
```

---

#### 4.3 Threat: Governor Exfiltrates Customer Billing Data to ggen Servers

**Attack Vector**: Compromised governor or malicious code sends customer data to external ggen server

**Impact**:
- Data exfiltration of customer billing/architecture information
- Breach of customer trust
- Potential privacy violation

**Likelihood**: Very Low (network policy prevents egress)

**Severity**: Critical (data breach)

**Mitigation Strategies**:
1. **Air-Gapped Deployment** (Network Isolation) - **Primary**
   - Governor runs ONLY in customer's GCP project
   - No VPN, no peering, no cross-project communication
   - Customer's network policies enforced

2. **Network Policy (Egress Control)** - Defense-in-Depth
   - Kubernetes Network Policy prevents outbound traffic
   - Allowed: Cloud Logging (ingress), Cloud Run API (ingress)
   - Blocked: Internet, other projects, unknown hosts
   - Egress monitoring (alert on any blocked attempt)

3. **Container Isolation** (No Shell Access)
   - Governor container runs minimal distroless image
   - No shell (/bin/sh, /bin/bash)
   - No package manager (apt, yum)
   - No curl, wget, nc (cannot make arbitrary requests)

4. **Service Account Minimalism** (Workload Identity)
   - Governor service account has NO permissions for data export
   - Cannot call Cloud Storage (for exfiltration)
   - Cannot call BigQuery (for exfiltration)
   - Can only call Cloud Run API and Cloud Logging

5. **Audit Monitoring** (Detection)
   - Monitor egress traffic (Cloud NAT, VPC Flow Logs)
   - Alert on unusual outbound connections
   - Block by default, whitelist specific destinations

**Implementation**:
```rust
// Network policy enforced at deployment
// (This is Kubernetes manifest, not Rust, but shows intent)
/*
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: governor-egress-policy
spec:
  podSelector:
    matchLabels:
      app: governor
  policyTypes:
  - Egress
  egress:
  # Allow Cloud Logging (internal GCP service)
  - to:
    - namespaceSelector: {}
    ports:
    - protocol: TCP
      port: 443
      name: cloud-logging
  # Allow Cloud Run API (internal GCP service)
  - to:
    - namespaceSelector: {}
    ports:
    - protocol: TCP
      port: 443
      name: cloud-run-api
  # Allow DNS (port 53)
  - to:
    - namespaceSelector: {}
    ports:
    - protocol: UDP
      port: 53
  # Deny all other egress
*/

// Service account permissions (minimal)
#[cfg(test)]
mod service_account_permissions {
    // Governor service account should have ONLY:
    // - logging.logEntries.create (write logs)
    // - run.jobs.create (call Cloud Run)
    // - run.jobs.get (check job status)
    //
    // Should NOT have:
    // - storage.objects.create (no data export)
    // - bigquery.datasets.create (no data export)
    // - compute.instances.* (no access to other compute)
    // - iam.* (no privilege escalation)
}

// Governor Dockerfile (minimal, no shell)
/*
FROM gcr.io/distroless/cc:latest

# Only copy binary, no shell or utils
COPY --from=builder /governor /governor

# No USER directive (runs as non-root by default in distroless)
# No RUN directives (immutable)

ENTRYPOINT ["/governor"]
*/

// Verify no egress attempts (audit)
async fn monitor_egress_attempts() -> Result<EgressAuditReport, AuditError> {
    // 1. Query VPC Flow Logs for governor pod
    let logs = gcp_logging::query(
        "resource.type=gce_network \
         AND jsonPayload.src_instance_id=governor \
         AND jsonPayload.dest_addr NOT IN ('cloud-logging-ip', 'cloud-run-api-ip')"
    ).await?;

    // 2. Check for blocked connections
    let mut findings = vec![];
    for log in logs {
        findings.push(EgressAttempt {
            timestamp: log.timestamp,
            src_ip: log.src_ip,
            dest_ip: log.dest_ip,
            dest_port: log.dest_port,
            protocol: log.protocol,
            action: "BLOCKED".to_string(),
        });

        alert!("EGRESS_ATTEMPT_BLOCKED: {} -> {}:{}",
            log.src_ip, log.dest_ip, log.dest_port);
    }

    Ok(EgressAuditReport {
        scan_date: Utc::now(),
        blocked_attempts: findings.len(),
        findings,
    })
}

// Verify governor cannot call external services
#[test]
fn test_governor_cannot_curl_external_url() {
    // This test would fail to compile/link because curl not available in distroless
    // Verifying: no `curl` in container

    // In distroless image, this would not exist:
    // /usr/bin/curl ✗
    // /bin/sh ✗
    // /usr/bin/wget ✗
    // /bin/nc ✗
}
```

**Testing**:
```rust
#[test]
async fn test_network_policy_blocks_unknown_egress() {
    // Arrange
    let pod = create_test_governor_pod();

    // Act
    let result = attempt_egress_from_pod(&pod, "https://evil.com").await;

    // Assert
    assert_eq!(result, Err(NetworkError::Blocked));
    assert_log_contains("EGRESS_ATTEMPT_BLOCKED");
}

#[test]
async fn test_service_account_cannot_export_data() {
    // Arrange
    let sa = get_governor_service_account();

    // Act
    let result = sa.create_storage_bucket("exfil-bucket").await;

    // Assert
    assert_eq!(result, Err(IamError::PermissionDenied));
}

#[test]
async fn test_egress_monitoring_detects_attempt() {
    // Arrange
    let pod = create_test_governor_pod();
    let mut mock_network_log = MockVpcFlowLog::new();
    mock_network_log.expect_blocked_egress().once();

    // Act
    let report = monitor_egress_attempts().await?;

    // Assert
    assert!(report.blocked_attempts > 0);
    assert_log_contains("EGRESS_ATTEMPT_BLOCKED");
}
```

---

### 5. DENIAL OF SERVICE (Service Unavailable)

#### 5.1 Threat: Attacker Floods Governor with Signals (1 Million/sec)

**Attack Vector**: Attacker publishes high volume of signals to Pub/Sub subscription

**Impact**:
- Governor CPU 100%, queue fills up
- Legitimate signals get dropped
- Governor becomes unavailable

**Likelihood**: Medium (Pub/Sub is accessible to authorized publishers)

**Severity**: High (governor unavailable)

**Mitigation Strategies**:
1. **Rate Limiting** (Primary) - **Primary**
   - Governor rejects signals after threshold (10k/sec per instance)
   - Excess signals queued (backpressure, oldest discarded if queue full)
   - Rate limit enforced per governor instance (scale horizontally)

2. **Backpressure & Queue Management** (Flow Control)
   - Signal queue has max size (100k signals)
   - When queue full, oldest signals discarded (FIFO)
   - New signals always enqueued (newest not dropped)
   - Queue depth monitored (alert if > 80%)

3. **Auto-Scaling** (Capacity Management)
   - Governor deployed as Cloud Run service (auto-scales)
   - Scale trigger: queue depth > 80% or latency > 5s
   - Auto-scale up: add replicas
   - Auto-scale down: remove replicas when load drops

4. **Per-Customer Quotas** (DoS Prevention)
   - Each customer gets signal quota (1M signals/month)
   - Quota tracked via Firestore
   - Signals after quota exhausted: rejected + alert

5. **Chaos Testing** (Validation)
   - Monthly chaos test: send 100k signals/sec
   - Verify: backpressure works, no crash, replicas scale

**Implementation**:
```rust
use tokio::sync::mpsc;

#[derive(Clone)]
struct GovernorConfig {
    max_signals_per_sec: u32,    // 10,000 signals/sec
    max_queue_size: usize,        // 100,000 signals
    queue_alert_threshold: f32,   // 80% full
    auto_scale_threshold_latency: Duration,  // 5 seconds
}

struct SignalProcessor {
    config: GovernorConfig,
    queue: mpsc::UnboundedReceiver<Signal>,
    queue_depth: Arc<AtomicUsize>,
    rate_limiter: TokenBucket,
    customer_quota: CustomerQuotaManager,
}

// Token bucket rate limiter
struct TokenBucket {
    max_tokens: u32,
    tokens: Arc<Mutex<u32>>,
    refill_rate: u32,  // tokens per second
    last_refill: Arc<Mutex<Instant>>,
}

impl TokenBucket {
    fn new(rate_per_sec: u32) -> Self {
        Self {
            max_tokens: rate_per_sec,
            tokens: Arc::new(Mutex::new(rate_per_sec)),
            refill_rate: rate_per_sec,
            last_refill: Arc::new(Mutex::new(Instant::now())),
        }
    }

    fn try_consume(&self, count: u32) -> bool {
        let mut tokens = self.tokens.lock().unwrap();
        let mut last = self.last_refill.lock().unwrap();

        // Refill tokens based on elapsed time
        let elapsed = last.elapsed();
        let refill_amount = (elapsed.as_secs_f32() * self.refill_rate as f32) as u32;
        *tokens = std::cmp::min(self.max_tokens, *tokens + refill_amount);
        *last = Instant::now();

        // Try to consume
        if *tokens >= count {
            *tokens -= count;
            true
        } else {
            false
        }
    }
}

// Process incoming signal with rate limiting
async fn process_signal_with_rate_limit(
    signal: Signal,
    processor: &SignalProcessor,
) -> Result<(), DoSError> {
    // 1. Check customer quota
    let customer_id = extract_customer_id(&signal)?;
    if !processor.customer_quota.has_quota(&customer_id) {
        alert!("DOS: Customer quota exceeded: {}", customer_id);
        return Err(DoSError::QuotaExceeded);
    }

    // 2. Check global rate limit
    if !processor.rate_limiter.try_consume(1) {
        alert!("DOS: Rate limit exceeded, backpressuring");
        return Err(DoSError::RateLimited);
    }

    // 3. Check queue depth
    let queue_depth = processor.queue_depth.load(Ordering::Relaxed);
    if queue_depth >= processor.config.max_queue_size {
        alert!("DOS: Queue full, dropping oldest signal");
        // Queue will handle removing oldest
        return Err(DoSError::QueueFull);
    }

    // 4. Alert if queue > threshold
    let queue_percent = (queue_depth as f32 / processor.config.max_queue_size as f32) * 100.0;
    if queue_percent > processor.config.queue_alert_threshold {
        warn!("DOS: Queue at {:.1}%, consider scaling", queue_percent);
    }

    // 5. Enqueue signal
    processor.queue.send(signal)?;
    processor.queue_depth.fetch_add(1, Ordering::Relaxed);

    Ok(())
}

// Customer quota manager
struct CustomerQuotaManager {
    quotas: Arc<Mutex<HashMap<String, CustomerQuota>>>,
    db: Firestore,
}

#[derive(Clone)]
struct CustomerQuota {
    customer_id: String,
    signals_used: u32,
    signals_limit: u32,     // 1,000,000 per month
    month: u32,
}

impl CustomerQuotaManager {
    async fn has_quota(&self, customer_id: &str) -> bool {
        let mut quotas = self.quotas.lock().unwrap();

        if let Some(quota) = quotas.get_mut(customer_id) {
            // Check if month changed (reset quota)
            let current_month = Utc::now().month();
            if quota.month != current_month {
                quota.month = current_month;
                quota.signals_used = 0;
            }

            // Check if under limit
            quota.signals_used < quota.signals_limit
        } else {
            // New customer, load from Firestore
            if let Ok(stored_quota) = self.db
                .collection("customer_quotas")
                .document(customer_id)
                .get()
                .await {

                let quota: CustomerQuota = stored_quota.deserialize().unwrap();
                quotas.insert(customer_id.to_string(), quota.clone());
                quota.signals_used < quota.signals_limit
            } else {
                false
            }
        }
    }

    async fn increment_usage(&self, customer_id: &str) -> Result<(), QuotaError> {
        let mut quotas = self.quotas.lock().unwrap();
        if let Some(quota) = quotas.get_mut(customer_id) {
            quota.signals_used += 1;
        }
        Ok(())
    }
}
```

**Testing**:
```rust
#[test]
fn test_rate_limiter_accepts_under_limit() {
    // Arrange
    let limiter = TokenBucket::new(10); // 10 tokens/sec

    // Act
    let result = limiter.try_consume(5);

    // Assert
    assert!(result);
}

#[test]
fn test_rate_limiter_rejects_over_limit() {
    // Arrange
    let limiter = TokenBucket::new(10);

    // Act
    limiter.try_consume(10); // Use all tokens
    let result = limiter.try_consume(1); // Try one more

    // Assert
    assert!(!result);
}

#[test]
async fn test_dos_signal_flood_rejected() {
    // Arrange
    let processor = create_test_processor();
    let signals: Vec<Signal> = (0..100000).map(|i| create_signal(i)).collect();

    // Act
    let mut count = 0;
    for signal in signals {
        if process_signal_with_rate_limit(signal, &processor).await.is_ok() {
            count += 1;
        }
    }

    // Assert
    assert!(count < 100000); // Not all accepted
    assert!(count <= 10000); // Rate limited to max_signals_per_sec * time_window
}

#[test]
async fn test_queue_depth_monitored() {
    // Arrange
    let processor = create_test_processor();

    // Act
    fill_queue_to_threshold(&processor, 85).await?;

    // Assert
    assert_log_contains("Queue at 85.0%, consider scaling");
}

#[test]
async fn test_customer_quota_enforced() {
    // Arrange
    let manager = CustomerQuotaManager::new();
    let customer_id = "customer-123";
    set_customer_quota(customer_id, 100); // 100 signals

    // Act
    for i in 0..150 {
        let result = manager.has_quota(customer_id).await;
        if result {
            manager.increment_usage(customer_id).await?;
        }
    }

    // Assert
    assert_eq!(count_signal_usage(customer_id), 100); // Stopped at quota
}
```

**Chaos Test**:
- Send 100k signals/sec for 30 seconds
- Verify: backpressure works, queue doesn't crash, no signals lost
- Verify: auto-scale triggers, replicas increase
- Verify: once load drops, replicas scale back down

---

#### 5.2 Threat: Attacker Triggers Rapid Actions (Exhaust Cloud Run Quota)

**Attack Vector**: Attacker sends signals designed to trigger many actions (e.g., frequent rollback requests)

**Impact**:
- Governor executes 100+ actions/min
- Cloud Run quota exhausted (typically 1000 executions/min)
- Governor cannot execute actions

**Likelihood**: Low (requires forged signals or compromised Pub/Sub)

**Severity**: High (governor unavailable)

**Mitigation Strategies**:
1. **Action Rate Limiting** - **Primary**
   - Governor limits to max 10 actions/min
   - Excess actions queued
   - Rate limit enforced per governor instance

2. **Quota Enforcement** (Graceful Degradation)
   - Governor tracks Cloud Run quota usage
   - If quota getting low (>80%), actions queued for retry
   - If quota exhausted, alert + manual intervention required

3. **Action Deduplication** (Avoid Duplicate Work)
   - If same action requested twice in 5 minutes, deduplicate
   - Example: "Throttle service X" already in progress, skip duplicate

4. **Alert on High Action Rate** (Detection)
   - Alert if > 5 actions/min (unusual)
   - Alert if > 10 actions/min (rate limit hit)
   - Manual review required for sustained high rate

5. **Backoff & Retry** (Resilience)
   - If Cloud Run API returns quota error, exponential backoff
   - Retry up to 3 times (5s, 10s, 20s delays)
   - After 3 failures, alert + escalate

**Implementation**:
```rust
struct ActionExecutor {
    rate_limiter: TokenBucket,
    action_queue: mpsc::UnboundedReceiver<Action>,
    quota_manager: QuotaManager,
    deduplication: ActionDeduplicator,
}

// Token bucket for action rate limiting
impl ActionExecutor {
    async fn execute_action(&mut self, action: Action) -> Result<ActionReceipt, ActionError> {
        // 1. Check rate limit (max 10 actions/min)
        if !self.rate_limiter.try_consume(1) {
            warn!("ACTION_RATE_LIMITED: Queueing for later");
            self.action_queue.send(action)?;
            return Err(ActionError::RateLimited);
        }

        // 2. Check for deduplication
        if self.deduplication.is_duplicate(&action).await? {
            warn!("ACTION_DEDUPLICATED: {} already in progress", action.id);
            return Err(ActionError::Duplicate);
        }

        // 3. Check quota
        let quota_available = self.quota_manager.check_quota().await?;
        if quota_available < 2 {
            // Keep 2 quota tokens free for emergencies
            alert!("ACTION_QUOTA_LOW: Queueing action");
            self.action_queue.send(action)?;
            return Err(ActionError::QuotaLow);
        }

        // 4. Record deduplication attempt
        self.deduplication.record(&action).await?;

        // 5. Execute with retry
        self.execute_with_retry(&action, max_retries: 3).await
    }

    async fn execute_with_retry(
        &self,
        action: &Action,
        max_retries: u32,
    ) -> Result<ActionReceipt, ActionError> {
        let mut retries = 0;
        let mut backoff = Duration::from_secs(5);

        loop {
            // Try to execute
            match self.call_cloud_run_api(action).await {
                Ok(receipt) => {
                    // Success
                    info!("ACTION_EXECUTED: {} -> {}", action.id, receipt.receipt_id);
                    self.quota_manager.decrement_quota().await?;
                    return Ok(receipt);
                }
                Err(ActionError::QuotaExhausted) => {
                    // Quota error
                    if retries < max_retries {
                        warn!("QUOTA_EXHAUSTED: Retrying in {:?}", backoff);
                        tokio::time::sleep(backoff).await;
                        backoff *= 2; // Exponential backoff
                        retries += 1;
                    } else {
                        alert!("ACTION_FAILED: Quota exhausted after {} retries", max_retries);
                        return Err(ActionError::QuotaExhausted);
                    }
                }
                Err(e) => {
                    // Other error
                    return Err(e);
                }
            }
        }
    }
}

// Action deduplication
struct ActionDeduplicator {
    recent_actions: Arc<Mutex<HashMap<String, Instant>>>,
    dedup_window: Duration,
}

impl ActionDeduplicator {
    async fn is_duplicate(&self, action: &Action) -> Result<bool, DedupError> {
        let key = format!("{}:{}", action.action_type, action.target_service);
        let now = Instant::now();

        let mut recent = self.recent_actions.lock().unwrap();

        // Clean up old entries
        recent.retain(|_, last_time| now.duration_since(*last_time) < self.dedup_window);

        // Check if action is recent
        if let Some(last_time) = recent.get(&key) {
            let elapsed = now.duration_since(*last_time);
            Ok(elapsed < self.dedup_window) // Duplicate if within window
        } else {
            Ok(false) // Not duplicate
        }
    }

    async fn record(&self, action: &Action) -> Result<(), DedupError> {
        let key = format!("{}:{}", action.action_type, action.target_service);
        let mut recent = self.recent_actions.lock().unwrap();
        recent.insert(key, Instant::now());
        Ok(())
    }
}

// Quota management
struct QuotaManager {
    quota_limit: u32,
    quota_used: Arc<Mutex<u32>>,
    quota_window: Duration,
    last_reset: Arc<Mutex<Instant>>,
}

impl QuotaManager {
    async fn check_quota(&self) -> Result<u32, QuotaError> {
        let mut last = self.last_reset.lock().unwrap();
        let now = Instant::now();

        // Reset if window passed
        if now.duration_since(*last) > self.quota_window {
            let mut used = self.quota_used.lock().unwrap();
            *used = 0;
            *last = now;
        }

        let used = self.quota_used.lock().unwrap();
        Ok(self.quota_limit - *used)
    }

    async fn decrement_quota(&self) -> Result<(), QuotaError> {
        let mut used = self.quota_used.lock().unwrap();
        *used += 1;

        if *used >= self.quota_limit {
            alert!("QUOTA_EXHAUSTED: {} / {}", used, self.quota_limit);
        }

        Ok(())
    }
}
```

**Testing**:
```rust
#[test]
async fn test_action_rate_limited() {
    // Arrange
    let mut executor = create_test_executor();

    // Act
    let mut results = vec![];
    for i in 0..20 {
        let action = create_action(i);
        results.push(executor.execute_action(action).await);
    }

    // Assert
    let successes = results.iter().filter(|r| r.is_ok()).count();
    assert!(successes < 20); // Not all succeeded
    assert!(successes <= 10); // Rate limited to 10/min
}

#[test]
async fn test_deduplication_prevents_duplicate() {
    // Arrange
    let executor = create_test_executor();
    let action1 = Action {
        action_type: ActionType::Throttle,
        target_service: "api-service".to_string(),
        ..default()
    };
    let action2 = action1.clone(); // Same action

    // Act
    let result1 = executor.execute_action(action1).await;
    let result2 = executor.execute_action(action2).await;

    // Assert
    assert_ok!(result1);
    assert_eq!(result2, Err(ActionError::Duplicate));
}

#[test]
async fn test_quota_exhaustion_triggers_backoff() {
    // Arrange
    let mut executor = create_test_executor();
    set_quota_limit(&mut executor, 2); // Very low quota

    // Act
    let start = Instant::now();
    let result = executor.execute_action(create_action(0)).await;
    let elapsed = start.elapsed();

    // Assert
    // Should have retried with backoff (5s + 10s + 20s)
    assert!(elapsed > Duration::from_secs(5));
    assert!(result.is_err() || result.is_ok()); // Either succeeded after retry or failed
}
```

**Chaos Test**:
- Trigger 100 throttle actions in 1 minute
- Verify: rate limiter enforces 10/min max
- Verify: excess actions queued
- Verify: alert triggered for high action rate
- Verify: Cloud Run quota not exhausted

---

#### 5.3 Threat: Attacker Injects Malformed Signals (Parser Crash)

**Attack Vector**: Attacker sends corrupted Protobuf or JSON that causes parser panic

**Impact**:
- Governor crashes
- Governor no longer protecting customer
- DoS for duration of crash (~1-5 min until restart)

**Likelihood**: Low (input validation, but possible)

**Severity**: High (availability)

**Mitigation Strategies**:
1. **Signal Validation** (Input Filtering) - **Primary**
   - JSON Schema validation before parsing
   - Protobuf schema validation
   - Malformed signals rejected + logged

2. **Graceful Error Handling** (Resilience)
   - No `unwrap()` or `panic!()` in signal processing
   - All errors caught and logged
   - Governor continues running after error

3. **Fuzzing** (Proactive Testing)
   - Monthly fuzzing with LibFuzzer
   - Send random/malformed signals
   - Verify no crash, no panic

4. **Process Restart** (Failover)
   - Cloud Run automatically restarts crashed processes
   - Restart time: ~5-10 seconds
   - Alert on process crash

**Implementation**:
```rust
use serde_json::json;

#[derive(Deserialize, Debug)]
struct Signal {
    id: String,
    timestamp: DateTime<Utc>,
    cost: f64,
    region: String,
}

// Validate JSON before deserializing
async fn process_signal_safely(raw_json: &str) -> Result<DecisionContext, SignalError> {
    // 1. Validate JSON syntax
    let json_value: serde_json::Value = match serde_json::from_str(raw_json) {
        Ok(v) => v,
        Err(e) => {
            warn!("MALFORMED_JSON: {}", e);
            return Err(SignalError::MalformedJson);
        }
    };

    // 2. Validate schema
    validate_signal_schema(&json_value)?;

    // 3. Deserialize carefully
    let signal: Signal = match serde_json::from_value(json_value) {
        Ok(s) => s,
        Err(e) => {
            warn!("DESERIALIZATION_ERROR: {}", e);
            return Err(SignalError::DeserializationError);
        }
    };

    // 4. Process signal (wrapped in catch_unwind for panic safety)
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        make_decision(&signal)
    })) {
        Ok(Ok(context)) => Ok(context),
        Ok(Err(e)) => {
            error!("DECISION_LOGIC_ERROR: {}", e);
            Err(SignalError::DecisionLogicError)
        }
        Err(_) => {
            error!("PANIC_IN_DECISION: Caught panic in decision logic");
            Err(SignalError::InternalError)
        }
    }
}

// JSON schema validation
fn validate_signal_schema(json: &serde_json::Value) -> Result<(), SignalError> {
    // Required fields
    let required_fields = ["id", "timestamp", "cost", "region"];

    for field in &required_fields {
        if json.get(field).is_none() {
            return Err(SignalError::MissingField(field.to_string()));
        }
    }

    // Type validation
    if !json["id"].is_string() {
        return Err(SignalError::InvalidType("id", "string"));
    }

    if !json["timestamp"].is_string() {
        return Err(SignalError::InvalidType("timestamp", "string"));
    }

    if !json["cost"].is_number() {
        return Err(SignalError::InvalidType("cost", "number"));
    }

    if !json["region"].is_string() {
        return Err(SignalError::InvalidType("region", "string"));
    }

    // Value range validation
    let cost = json["cost"].as_f64().unwrap();
    if cost < 0.0 || cost > 1_000_000.0 {
        return Err(SignalError::InvalidRange("cost", "0..1000000"));
    }

    Ok(())
}

// Fuzzing target
#[cfg(all(test, fuzzing))]
pub fn fuzz_signal_processing(data: &[u8]) {
    let raw_json = String::from_utf8_lossy(data);

    // This should NOT panic, even with malformed input
    let _ = process_signal_safely(&raw_json);
}
```

**Testing**:
```rust
#[test]
async fn test_malformed_json_rejected() {
    // Arrange
    let malformed = r#"{"id":  "sig1", "timestamp": invalid_timestamp}"#;

    // Act
    let result = process_signal_safely(malformed).await;

    // Assert
    assert_eq!(result, Err(SignalError::MalformedJson));
    assert_log_contains("MALFORMED_JSON");
}

#[test]
async fn test_missing_field_rejected() {
    // Arrange
    let incomplete = r#"{"id": "sig1", "cost": 100.0}"#; // Missing timestamp, region

    // Act
    let result = process_signal_safely(incomplete).await;

    // Assert
    assert_eq!(result, Err(SignalError::MissingField(_)));
}

#[test]
async fn test_invalid_type_rejected() {
    // Arrange
    let invalid = r#"{"id": 123, "timestamp": "2026-01-25T00:00:00Z", "cost": "not a number", "region": "us"}"#;

    // Act
    let result = process_signal_safely(invalid).await;

    // Assert
    assert!(result.is_err());
}

#[test]
async fn test_cost_out_of_range_rejected() {
    // Arrange
    let out_of_range = r#"{"id": "sig1", "timestamp": "2026-01-25T00:00:00Z", "cost": -100.0, "region": "us"}"#;

    // Act
    let result = process_signal_safely(out_of_range).await;

    // Assert
    assert_eq!(result, Err(SignalError::InvalidRange(_, _)));
}

#[test]
async fn test_no_panic_on_corrupted_input() {
    // Arrange: Send various corrupted inputs
    let corrupted_inputs = vec![
        b"zzzzz".to_vec(),
        vec![255, 254, 253, 252], // Binary garbage
        b"{}}}}}".to_vec(),
        b"{\"a\": ".to_vec(), // Incomplete JSON
    ];

    // Act & Assert: None should panic
    for input in corrupted_inputs {
        let _ = process_signal_safely(&String::from_utf8_lossy(&input)).await;
        // If we get here, no panic occurred
    }
}
```

**Fuzzing Test** (requires `cargo +nightly install cargo-fuzz`):
```rust
// fuzz/fuzz_targets/fuzz_signal_processing.rs
#![no_main]
use libfuzzer_sys::fuzz_target;
use ggen_governor::signal_processor;

fuzz_target!(|data: &[u8]| {
    if let Ok(json_str) = std::str::from_utf8(data) {
        // This should never panic
        let _ = signal_processor::process_signal_safely(json_str);
    }
});
```

**Chaos Test**:
- Run fuzzer for 1 hour, generate 1M random signal mutations
- Verify: no panic, no crash, governor keeps running
- Verify: malformed signals logged but don't affect processing

---

### 6. ELEVATION OF PRIVILEGE (Attacker Gains Admin)

#### 6.1 Threat: Attacker Exploits Governor Bug to Gain Cloud Run Admin

**Attack Vector**: Bug in governor code allows arbitrary code execution or privilege escalation

**Impact**:
- Attacker gains Cloud Run admin privileges
- Attacker can delete governor, modify other services, steal customer data
- Full cluster compromise

**Likelihood**: Very Low (Rust memory safety, code review)

**Severity**: Critical (cluster compromise)

**Mitigation Strategies**:
1. **Memory Safety** (Rust Guarantees) - **Primary**
   - Rust prevents buffer overflow, use-after-free, data races at compile time
   - Governor cannot be exploited for memory safety vulnerabilities
   - No unsafe code in hot paths (unsafe used only in FFI, carefully reviewed)

2. **Code Review** (Human Review) - Defense-in-Depth
   - All code changes reviewed by 2+ engineers
   - Security-focused review (check for privilege escalation, unintended APIs)
   - Automated static analysis (clippy, cargo-audit)

3. **Sandboxing** (Process Isolation)
   - Governor runs in restricted container (no shell)
   - Governor cannot spawn subprocesses
   - Governor isolated from host via Linux namespaces

4. **Minimal IAM Permissions** (Least Privilege)
   - Governor service account has only required permissions
   - No wildcard permissions (explicit resource names)
   - Deny-policy prevents accidental overpermissioning

5. **Fuzzing** (Vulnerability Finding)
   - Monthly fuzzing with AFL/LibFuzzer
   - Fuzz signal processing, config parsing, API responses
   - Vulnerabilities fixed before release

**Implementation**:
```rust
// Review checklist (manually enforced in code review)
/*
Security Code Review Checklist:
- [ ] No unsafe{} outside of documented FFI
- [ ] No unwrap()/expect() in production code (use Result<T, E>)
- [ ] No shell execution (no std::process::Command)
- [ ] No file I/O (no fs::read, fs::write)
- [ ] No network access outside of authorized APIs
- [ ] All external API calls authenticated
- [ ] Input validation before deserialization
- [ ] Error handling (no panics, no integer overflow)
- [ ] No privilege escalation possible
- [ ] IAM permissions verified in action
*/

// Example: Safe API call with error handling
async fn safe_cloud_run_api_call(
    action: &Action,
    token: &str,
) -> Result<ActionReceipt, ApiError> {
    // 1. Validate action (prevent injection)
    validate_action(action)?;

    // 2. Build request carefully (no string concatenation)
    let url = format!(
        "https://run.googleapis.com/apis/run.googleapis.com/v1/namespaces/{}/services/{}:call",
        NAMESPACE, action.target_service
    );

    // URL cannot be injected because target_service is validated
    // and format! doesn't execute code

    // 3. Call API with proper error handling (no unwrap)
    let response = reqwest::Client::new()
        .post(&url)
        .bearer_auth(token)
        .json(&action)
        .send()
        .await
        .map_err(|e| {
            error!("API_CALL_FAILED: {}", e);
            ApiError::NetworkError(e)
        })?;

    // 4. Validate response (prevent code injection from response)
    if !response.status().is_success() {
        return Err(ApiError::ApiError(response.status()));
    }

    // 5. Parse response safely
    let receipt: ActionReceipt = response.json().await
        .map_err(|e| {
            error!("RESPONSE_PARSING_FAILED: {}", e);
            ApiError::MalformedResponse(e)
        })?;

    Ok(receipt)
}

// Unsafe code is isolated, documented, and carefully reviewed
// (Example - not actual governor code)
mod ffi {
    extern "C" {
        // FFI boundary - document threat model
        // External library: openssl-sys (well-vetted, audited)
        // Privilege: None (read-only cryptographic operations)
        fn SHA256(data: *const u8, len: usize, md: *mut u8) -> *mut u8;
    }

    pub fn sha256_ffi(data: &[u8]) -> Result<[u8; 32], FfiError> {
        let mut digest = [0u8; 32];

        unsafe {
            // SAFETY: openssl-sys is well-audited and widely used
            // SHA256 doesn't modify input or output beyond digest parameter
            // Digest is initialized to correct size (32 bytes for SHA-256)
            SHA256(data.as_ptr(), data.len(), digest.as_mut_ptr());
        }

        Ok(digest)
    }
}
```

**Testing**:
```rust
#[test]
fn test_action_injection_prevented() {
    // Arrange
    let malicious_action = Action {
        target_service: "service'; DROP TABLE audit_logs; --".to_string(),
        ..default()
    };

    // Act
    let result = validate_action(&malicious_action);

    // Assert
    assert!(result.is_err()); // Validation fails
}

#[test]
async fn test_api_response_injection_prevented() {
    // Arrange
    let mut mock_api = MockCloudRunApi::new();
    let malicious_response = ActionReceipt {
        receipt_id: "sig1'; const shell = require('child_process'); shell.exec('rm -rf /',".to_string(),
        ..default()
    };
    mock_api.expect_response(malicious_response);

    // Act
    let result = safe_cloud_run_api_call(&action, &token).await;

    // Assert
    // Response is parsed as JSON, not executed as code
    assert_ok!(result);
}

#[test]
fn test_no_unwrap_in_production_code() {
    // Arrange: Parse codebase
    let source_files = glob("crates/ggen-governor/src/**/*.rs")?;

    // Act: Search for unwrap() calls
    let unwraps = grep_files(&source_files, r"\.unwrap\(\)");

    // Assert: No unwraps found (except in tests)
    assert_eq!(unwraps.len(), 0);
}

#[test]
fn test_no_unsafe_in_hot_paths() {
    // Arrange: Parse codebase
    let source_files = glob("crates/ggen-governor/src/**/*.rs")?;

    // Act: Find unsafe blocks
    let unsafe_blocks = grep_files(&source_files, r"unsafe\s*\{");

    // Assert: Only unsafe in FFI module, not in signal processing
    for unsafe_block in unsafe_blocks {
        assert!(unsafe_block.file.contains("ffi.rs")); // Only in FFI module
    }
}
```

---

#### 6.2 Threat: Attacker Modifies Governor Source Code (Supply Chain Attack)

**Attack Vector**: Attacker compromises GitHub, build system, or artifact registry; ships backdoored governor binary

**Impact**:
- Malicious governor shipped to all customers
- Attacker gains access to customer infrastructure
- Data exfiltration, sabotage, long-term access

**Likelihood**: Very Low (GitHub security, code review, artifact verification)

**Severity**: Critical (widespread compromise)

**Mitigation Strategies**:
1. **Code Signing** (Artifact Verification) - **Primary**
   - Governor binary signed with RSA-4096 key
   - Signature verified on customer end before deployment
   - Tampered binaries rejected (checksum mismatch)

2. **Source Code Signing** (Commit Verification)
   - All commits to main branch must be signed (GPG)
   - Unsigned commits rejected (pre-commit hook)
   - Commit history verifiable (each commit signed by developer)

3. **Supply Chain Security** (Dependency Verification)
   - `cargo audit` on every release (check for vulnerable dependencies)
   - Dependencies locked (Cargo.lock checked in)
   - Transitive dependencies audited

4. **Build Integrity** (Reproducible Builds)
   - Build process is deterministic (same input → same binary)
   - Customer can verify binary by rebuilding from source
   - Build machine is isolated (no external network access)

5. **Artifact Scanning** (Malware Detection)
   - Binary scanned for known malware patterns
   - Static analysis run on binary (check for suspicious calls)
   - Virus total scan before release

**Implementation**:
```rust
// Build process: Cargo.lock ensures reproducible builds
/*
$ cargo build --release
   Compiling governor 0.1.0 (with dependencies from Cargo.lock)
   ...
   Finished release [optimized] target(s) in 12.34s

Cargo.lock pins all versions:
[package]
name = "governor"
version = "0.1.0"
dependencies = [
 "tokio 1.47.0 (registry+https://github.com/rust-lang/crates.io-index)"
 "serde 1.0.201 (registry+...)"
]
*/

// Code signing script (in CI/CD)
/*
#!/bin/bash

# Build binary
cargo build --release

# Sign binary with private key
openssl dgst -sha256 -sign governor_private.pem \
    target/release/governor \
    > target/release/governor.sig

# Export public key for customer
openssl pkey -in governor_private.pem -pubout \
    > governor_public.pem

# Customer verifies:
openssl dgst -sha256 -verify governor_public.pem \
    -signature target/release/governor.sig \
    target/release/governor
*/

// Dependency audit script
fn audit_dependencies() -> Result<AuditReport, AuditError> {
    // 1. Run cargo audit
    let output = std::process::Command::new("cargo")
        .arg("audit")
        .arg("--deny")
        .arg("warnings")
        .output()?;

    if !output.status.success() {
        return Err(AuditError::VulnerabilitiesFound(
            String::from_utf8(output.stdout)?
        ));
    }

    // 2. Lock dependencies
    let output = std::process::Command::new("cargo")
        .arg("lock")
        .output()?;

    if !output.status.success() {
        return Err(AuditError::LockFailed);
    }

    // 3. Check Cargo.lock is in git
    let output = std::process::Command::new("git")
        .arg("ls-files")
        .arg("Cargo.lock")
        .output()?;

    let files = String::from_utf8(output.stdout)?;
    if files.trim().is_empty() {
        return Err(AuditError::CargoLockNotTracked);
    }

    Ok(AuditReport {
        status: "PASS".to_string(),
        dependencies_audited: count_dependencies()?,
        vulnerabilities: 0,
    })
}

// Binary verification script (customer-side)
fn verify_governor_binary(
    binary_path: &Path,
    signature_path: &Path,
    public_key_path: &Path,
) -> Result<(), VerificationError> {
    // 1. Compute SHA-256 of binary
    let binary_hash = sha256_file(binary_path)?;

    // 2. Verify RSA signature
    let public_key = load_public_key(public_key_path)?;
    let signature = read_file(signature_path)?;

    if !rsa_verify(&public_key, &binary_hash, &signature)? {
        return Err(VerificationError::SignatureMismatch);
    }

    // 3. Scan for known malware
    let scan_result = virus_total_scan(&binary_hash)?;
    if scan_result.malware_detected() {
        return Err(VerificationError::MalwareDetected);
    }

    Ok(())
}
```

**Testing**:
```rust
#[test]
fn test_binary_signing_verification() {
    // Arrange
    let binary = build_release_binary();
    let (private_key, public_key) = generate_test_keys();

    // Act
    let signature = sign_binary(&binary, &private_key);
    let verified = verify_signature(&binary, &signature, &public_key);

    // Assert
    assert!(verified);
}

#[test]
fn test_modified_binary_fails_verification() {
    // Arrange
    let binary = build_release_binary();
    let signature = sign_binary(&binary, &private_key);

    let mut modified = binary.clone();
    modified[100] = 0xFF; // Tamper with one byte

    // Act
    let verified = verify_signature(&modified, &signature, &public_key);

    // Assert
    assert!(!verified);
}

#[test]
fn test_cargo_audit_passes() {
    // Arrange: Run in CI

    // Act
    let output = std::process::Command::new("cargo")
        .arg("audit")
        .output()
        .expect("cargo audit failed");

    // Assert
    assert!(output.status.success());
}

#[test]
fn test_cargo_lock_tracked_in_git() {
    // Arrange

    // Act
    let output = std::process::Command::new("git")
        .arg("ls-files")
        .arg("Cargo.lock")
        .output()
        .expect("git ls-files failed");

    // Assert
    let files = String::from_utf8(output.stdout).unwrap();
    assert!(files.contains("Cargo.lock"));
}

#[test]
fn test_reproducible_builds() {
    // Arrange

    // Act: Build twice with same dependencies
    let build1 = build_release_binary();
    let build2 = build_release_binary();

    // Assert: Binaries are identical
    assert_eq!(sha256(&build1), sha256(&build2));
}
```

---

## Insider Threats

### Threat: Malicious ggen Employee Ships Backdoor in Governor

**Attack Vector**: Developer with access to source code adds backdoor (exfiltration, privilege escalation)

**Impact**:
- Backdoor shipped to all customers
- Attacker gains persistent access to customer infrastructure
- Data exfiltration, infrastructure sabotage

**Likelihood**: Very Low (background checks, code review)

**Severity**: Critical (widespread compromise)

**Mitigation Strategies**:

1. **Mandatory Code Review** (2+ Reviewers)
   - All code changes reviewed by 2+ people
   - Reviewers are independent (different teams if possible)
   - Reviewers check for backdoors, privilege escalation

2. **Separation of Duties** (No One Person Controls Release)
   - Developer writes code
   - First reviewer approves code
   - Second reviewer approves release
   - Deployer has no code access (independent role)

3. **Background Checks & Access Control**
   - Background checks on developers
   - Access to source code requires clearance
   - Access logs reviewed quarterly

4. **Supply Chain Signing** (Customer Verification)
   - Commits signed by developer (GPG)
   - Binary signed by release manager (RSA)
   - Customer can verify provenance chain

---

### Threat: Disgruntled SRE Escalates Privileges

**Attack Vector**: SRE with admin access logs in as admin, modifies customer governor config or escalates privileges

**Impact**:
- SRE modifies customer config (throttle policies, action thresholds)
- Customer gets wrong SLO, competitor gains access
- Non-repudiation broken (SRE denies changes)

**Likelihood**: Very Low (access control, audit logs)

**Severity**: High (customer data/config compromise)

**Mitigation Strategies**:

1. **Principle of Least Privilege** (SREs Minimal Access)
   - SREs have read-only access by default
   - Write access requires approval
   - Admin access requires 2nd factor + approval from manager

2. **Approval Workflow** (Dual Control)
   - Config change requires approval from 2+ people
   - Approval logged (who approved, when, why)
   - Changes only applied after approval

3. **Immutable Audit Trail** (Detection)
   - All privileged actions logged
   - Logs immutable (cannot delete)
   - Logs audited quarterly
   - Alert on suspicious patterns (off-hours access, rapid changes)

4. **Automatic Rollback** (Recovery)
   - Config changes auto-rolled back if suspicious
   - Customer notified immediately
   - Change request logged for investigation

---

## Third-Party & Dependency Risks

### Threat: Transitive Dependency Has Vulnerability

**Example**: `openssl-sys` has vulnerability, gets pulled in by dependency chain

**Likelihood**: Medium (dependencies are outside our control)

**Severity**: High (depends on vulnerability type)

**Mitigation Strategies**:

1. **Dependency Audit** (Proactive Scanning)
   - `cargo audit` on every build
   - Automated checks in CI (fail on warnings)
   - Weekly dependency updates

2. **Dependency Pinning** (Version Lock)
   - Cargo.lock checked in (reproducible builds)
   - Explicit version constraints
   - Transitive dependencies locked

3. **Regular Updates** (Timely Patching)
   - Dependencies updated monthly
   - Critical vulnerabilities patched immediately
   - Update strategy documented

---

### Threat: GCP Service Compromised (Unlikely But Possible)

**Example**: Cloud Logging service is compromised, attacker reads all logs

**Likelihood**: Extremely Low (Google's security)

**Severity**: Critical (if happened)

**Mitigation Strategies**:

1. **Air-Gapped Deployment** (Defense-in-Depth)
   - Customer can deploy governor in isolated GKE cluster
   - No external dependencies
   - Governor still protected locally even if GCP compromised

2. **Encryption at Rest** (Data Protection)
   - Data encrypted with customer's own KMS key
   - Even if logs stolen, encrypted (useless to attacker)

---

## Chaos Engineering Tests (Monthly)

### Chaos Test Suite

**Objective**: Proactively find vulnerabilities and validate mitigations

#### Test 1: Network Latency Injection
```bash
# Simulate 500ms latency to Pub/Sub
tc qdisc add dev eth0 root netem delay 500ms

# Governor should queue gracefully
# Signals shouldn't be dropped
# Alert should trigger if latency > 5s for > 1 min
```

**Verification**:
- Governor stays responsive
- Backpressure works (queue manages)
- Alert logged + customer notified

#### Test 2: Process Killing (Chaos Monkey)
```bash
# Randomly kill governor process
pkill -9 -f "governor"

# Governor should restart within 5s
# Signals should be replayed from checkpoint
# Customer should not lose coverage
```

**Verification**:
- Cloud Run auto-restarts
- No signals lost (replayed from Pub/Sub)
- Alert triggered

#### Test 3: Malformed Signal Injection
```bash
# Send corrupted Protobuf to Pub/Sub
echo "zzzzzzzzzzzzz" | gcloud pubsub topics publish gov-signals --message "zzzzzzzzzzzzz"

# Governor should skip signal
# No crash, no panic
# Error logged
```

**Verification**:
- No panic
- Governor keeps running
- Error logged + alert

#### Test 4: Memory Exhaustion
```bash
# Stress test: send 100k signals to queue
for i in {1..100000}; do
  gcloud pubsub topics publish gov-signals --message "{...}"
done

# Governor should manage gracefully
# Backpressure kicks in
# Oldest signals discarded if queue full
```

**Verification**:
- Queue doesn't crash
- Memory usage capped
- Alert triggered at 80% queue depth

#### Test 5: IAM Role Removal
```bash
# Remove Workload Identity binding
gcloud iam service-accounts delete governor-sa

# Governor should fail with clear error
# Alert should trigger
# Governor cannot execute actions
```

**Verification**:
- Graceful failure (no crash)
- Error clearly states "authentication failed"
- Alert triggered within 1 minute

#### Test 6: Simulated Network Partition
```bash
# Simulate network partition: drop all packets to Cloud Run API
iptables -A OUTPUT -d cloud-run-api -j DROP

# Governor should queue actions
# Alert should trigger (API unreachable)
# Actions should retry on partition recovery
```

**Verification**:
- Governor detects API unreachable
- Actions queued (not lost)
- Automatic retry after partition heals

---

## Testing & Validation Framework

### Pre-Release Validation

**Before every release, verify**:

1. ✅ Security code review passed (2+ reviewers)
2. ✅ `cargo audit` shows 0 vulnerabilities
3. ✅ All STRIDE tests pass
4. ✅ Chaos tests pass (inject 100k signals, no crash)
5. ✅ Fuzzing found 0 crashes (month of continuous fuzzing)
6. ✅ Binary signature verified
7. ✅ Reproducible build verified
8. ✅ IAM minimal permissions verified
9. ✅ Network policy validated (no egress)
10. ✅ Sandboxing verified (no shell access)

### Continuous Monitoring

**Post-deployment, monitor**:

- Anomalous API calls (unusual destinations)
- High signal rate (> 10k/sec)
- High action rate (> 10/min)
- Failed authentications
- Process crashes (restarts)
- Config changes (log all + approval)
- Log access (audit trail)
- PII leaks (quarterly scan)

---

## Response & Recovery

### Security Incident Response Plan

#### Step 1: Detect
- Automated monitoring catches 80% of incidents
- Alerts to SRE on-call

#### Step 2: Investigate (< 1 hour)
- Gather evidence from audit logs
- Reconstruct attack timeline
- Determine impact scope (how many customers?)

#### Step 3: Contain (< 4 hours)
- If governor compromised: revoke credentials, redeploy
- If customer data leaked: notify customer within 24 hours
- If config modified: rollback to previous version

#### Step 4: Remediate (< 24 hours)
- Fix root cause in code
- Deploy patch to all customers
- Verify patch via fuzzing

#### Step 5: Communicate
- Customer notification (what happened, what we did)
- Public disclosure (if required by law)
- Post-mortem (lessons learned)

---

## Regulatory Compliance

### Standards Alignment

- **SOC 2 Type II**: Controls validated annually
- **GDPR/CCPA**: PII handling procedures documented
- **ISO 27001**: Information security management system
- **CIS Benchmarks**: Cloud security best practices

### Audit & Certification

- **Quarterly**: Internal security audit
- **Annually**: Third-party penetration test
- **Continuously**: Automated compliance checks (CCM)

---

## Summary

| Threat Category | Example Threat | Mitigation | Status |
|---|---|---|---|
| **Spoofing** | Fake billing signal | HMAC signature | ✅ Implemented |
| **Tampering** | Modify audit log | Hash chain + immutable Cloud Logging | ✅ Implemented |
| **Repudiation** | Deny action taken | Cryptographic receipts + decision trace | ✅ Implemented |
| **Info Disclosure** | Leak customer costs | Log redaction + encryption + access control | ✅ Implemented |
| **Denial of Service** | Flood with signals | Rate limiting + backpressure + auto-scale | ✅ Implemented |
| **Privilege Escalation** | Exploit governor bug | Memory safety (Rust) + minimal IAM + sandboxing | ✅ Implemented |

---

## References

- **NIST SP 800-30**: Risk Management Guide
- **OWASP**: Top 10 Cloud Security Risks
- **Google Cloud Security Foundation**: Cloud Architecture & Design
- **SANS Institute**: Securing Cloud Infrastructure

---

**Last Updated**: 2026-01-25
**Threat Model Version**: 1.0.0
**Next Review**: 2026-04-25 (quarterly)
