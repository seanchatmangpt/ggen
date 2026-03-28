<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Jidoka Boundary Gates and Receipt System](#jidoka-boundary-gates-and-receipt-system)
  - [Overview](#overview)
  - [The Fundamental Principle: Stop the Line](#the-fundamental-principle-stop-the-line)
    - [Abnormality Detection](#abnormality-detection)
    - [What Constitutes an Abnormality](#what-constitutes-an-abnormality)
  - [Boundary Gates: GREEN/YELLOW/RED](#boundary-gates-greenyellowred)
    - [Gate Modes](#gate-modes)
    - [Gate Implementation](#gate-implementation)
    - [No Negotiation - Only Routing](#no-negotiation---only-routing)
  - [Receipt Structure: Hash-Linked Chain](#receipt-structure-hash-linked-chain)
    - [Base Receipt Structure](#base-receipt-structure)
    - [Receipt Chain Example](#receipt-chain-example)
  - [Narrative Replaced by Verifiable Artifacts](#narrative-replaced-by-verifiable-artifacts)
    - [Traditional Approach: Narrative](#traditional-approach-narrative)
    - [Jidoka Approach: Verifiable Artifacts](#jidoka-approach-verifiable-artifacts)
    - [Concrete Example: Entitlement Expiration](#concrete-example-entitlement-expiration)
  - [Receipt Verification and Audit Trails](#receipt-verification-and-audit-trails)
    - [Verification Algorithm](#verification-algorithm)
    - [Audit Trail Queries](#audit-trail-queries)
  - [Implementation in Rust with Cryptographic Signatures](#implementation-in-rust-with-cryptographic-signatures)
    - [Complete Receipt System](#complete-receipt-system)
    - [Usage Example](#usage-example)
  - [Real-World Scenarios](#real-world-scenarios)
    - [Scenario 1: Rate Limit Exceeded (Yellow Mode)](#scenario-1-rate-limit-exceeded-yellow-mode)
    - [Scenario 2: Circuit Breaker Opens (Red Mode)](#scenario-2-circuit-breaker-opens-red-mode)
    - [Scenario 3: Entitlement Expiration During Request](#scenario-3-entitlement-expiration-during-request)
  - [Summary](#summary)
    - [Key Principles](#key-principles)
    - [Benefits](#benefits)
    - [The Manufacturing Paradigm Applied](#the-manufacturing-paradigm-applied)
  - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Jidoka Boundary Gates and Receipt System

**Version**: 1.0.0
**Last Updated**: 2026-02-09
**Status**: Canonical Reference

## Overview

In traditional software, errors are handled through exception catching, retry logic, and hope. In the manufacturing paradigm, errors trigger **Jidoka** - the automatic halt of production to prevent defect propagation.

This document explains how ggen implements Jidoka through boundary gates and cryptographic receipts, replacing narrative promises with verifiable artifacts.

```
Traditional Approach          Jidoka Approach
─────────────────────────────────────────────
"We'll handle errors"      ←→  Cryptographic proof of handling
"Request succeeded"        ←→  Receipt chain verification
Logs (searchable text)     ←→  Immutable ledger (hash-linked)
Hope nothing broke         ←→  Mathematical certainty
Retry on failure           ←→  Stop the line on abnormality
```

## The Fundamental Principle: Stop the Line

### Abnormality Detection

**Abnormality** = Any deviation from expected system behavior.

In Jidoka, when an abnormality is detected:

1. **STOP**: Immediately halt processing of that request
2. **SIGNAL**: Emit cryptographic receipt documenting the refusal
3. **ROUTE**: Direct the request to appropriate handling (dead letter, retry with backoff, manual intervention)
4. **NEVER NEGOTIATE**: The decision is deterministic, not interpretive

### What Constitutes an Abnormality

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Abnormality {
    // Structural violations
    MissingRequiredField { field_name: String },
    InvalidSignature { expected: Hash, actual: Hash },
    ChainHashMismatch { expected: Hash, actual: Hash },

    // Business rule violations
    EntitlementExpired { entitlement_id: Uuid, expired_at: DateTime<Utc> },
    QuotaExceeded { quota_type: QuotaType, limit: u64, current: u64 },
    PermissionDenied { principal: String, resource: String, action: Action },

    // State violations
    InvalidStateTransition { from: State, to: State, reason: String },
    CircuitBreakerOpen { service: String, failure_count: u32 },

    // Resource violations
    RateLimitExceeded { requests_per_second: u32, limit: u32 },
    WorkerPoolExhausted { pool_size: usize, in_use: usize },
    DeadlineExceeded { started_at: DateTime<Utc>, deadline: DateTime<Utc> },
}
```

**Key Insight**: Every abnormality is **typed** and **enumerable**. There are no "unexpected errors" - only finite, closed-set cases.

---

## Boundary Gates: GREEN/YELLOW/RED

### Gate Modes

Boundary gates operate in three modes, determining what can enter the system:

```
┌─────────────────────────────────────────────────────────────┐
│ GREEN MODE: Normal Operation                                │
├─────────────────────────────────────────────────────────────┤
│ • All validated requests accepted                           │
│ • Full throughput capacity                                  │
│ • Receipt chain extends normally                            │
│ • Circuit breakers: CLOSED                                  │
└─────────────────────────────────────────────────────────────┘
                            ↓
                    (Warning threshold)
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ YELLOW MODE: Degraded Operation                             │
├─────────────────────────────────────────────────────────────┤
│ • Rate limiting engaged                                     │
│ • Non-critical requests queued                              │
│ • Critical requests prioritized                             │
│ • Circuit breakers: HALF_OPEN (testing recovery)           │
│ • Receipt chain documents throttling decisions             │
└─────────────────────────────────────────────────────────────┘
                            ↓
                    (Critical threshold)
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ RED MODE: Emergency Stop                                    │
├─────────────────────────────────────────────────────────────┤
│ • New requests REFUSED immediately                          │
│ • Existing requests drained (bounded timeout)               │
│ • Circuit breakers: OPEN                                    │
│ • Receipt chain documents each refusal                      │
│ • No negotiation - deterministic refusal                    │
└─────────────────────────────────────────────────────────────┘
```

### Gate Implementation

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Sha256, Digest};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum GateMode {
    Green,
    Yellow {
        throttle_percentage: u8,  // 0-100
        reason: String,
    },
    Red {
        reason: String,
        since: DateTime<Utc>,
    },
}

#[derive(Debug, Clone)]
pub struct BoundaryGate {
    mode: GateMode,
    circuit_breaker_state: CircuitBreakerState,
    rate_limiter: RateLimiter,
    metrics: GateMetrics,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CircuitBreakerState {
    Closed,    // GREEN: Normal operation
    HalfOpen,  // YELLOW: Testing recovery
    Open,      // RED: Service failing
}

impl BoundaryGate {
    /// Process incoming request through gate
    pub fn process<T>(&mut self, request: Request<T>) -> Result<Accepted<T>, Refused<T>> {
        // Step 1: Check gate mode
        match &self.mode {
            GateMode::Red { reason, since } => {
                return Err(self.refuse_red_mode(request, reason.clone(), *since));
            }
            GateMode::Yellow { throttle_percentage, reason } => {
                if !self.should_accept_yellow(*throttle_percentage) {
                    return Err(self.refuse_yellow_mode(request, reason.clone()));
                }
            }
            GateMode::Green => {
                // Proceed to validation
            }
        }

        // Step 2: Validate request structure
        self.validate_structure(&request)?;

        // Step 3: Verify cryptographic signature
        self.verify_signature(&request)?;

        // Step 4: Check authorization
        self.check_authorization(&request)?;

        // Step 5: Verify entitlement
        self.verify_entitlement(&request)?;

        // Step 6: Check quota
        self.check_quota(&request)?;

        // Step 7: Rate limit check
        self.rate_limiter.acquire()?;

        // Step 8: Circuit breaker check
        if self.circuit_breaker_state == CircuitBreakerState::Open {
            return Err(self.refuse_circuit_open(request));
        }

        // All gates passed - accept request
        Ok(self.accept(request))
    }

    fn refuse_red_mode<T>(
        &mut self,
        request: Request<T>,
        reason: String,
        since: DateTime<Utc>
    ) -> Refused<T> {
        let receipt = RefusalReceipt::new(
            request.id,
            RefusalReason::RedModeActive { reason: reason.clone(), since },
            Utc::now(),
        );

        self.metrics.refused_red_mode += 1;

        Refused {
            request,
            receipt,
            http_status: 503, // Service Unavailable
        }
    }

    fn refuse_yellow_mode<T>(
        &mut self,
        request: Request<T>,
        reason: String
    ) -> Refused<T> {
        let receipt = RefusalReceipt::new(
            request.id,
            RefusalReason::YellowModeThrottled { reason },
            Utc::now(),
        );

        self.metrics.refused_yellow_mode += 1;

        Refused {
            request,
            receipt,
            http_status: 429, // Too Many Requests
        }
    }

    fn should_accept_yellow(&self, throttle_percentage: u8) -> bool {
        // Random sampling: accept with probability = (100 - throttle_percentage) / 100
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let threshold = 100 - throttle_percentage;
        rng.gen_range(0..100) < threshold
    }
}

#[derive(Debug, Default)]
struct GateMetrics {
    accepted: u64,
    refused_red_mode: u64,
    refused_yellow_mode: u64,
    refused_validation: u64,
    refused_authorization: u64,
    refused_quota: u64,
    refused_circuit_open: u64,
}
```

### No Negotiation - Only Routing

**Critical Principle**: Gates do NOT negotiate. They make deterministic decisions.

```rust
// ❌ WRONG: Negotiation (traditional approach)
fn handle_request(req: Request) -> Response {
    match validate(req) {
        Err(ValidationError) => {
            // "Maybe we can still process this..."
            if req.is_important() {
                // Try anyway
                process_anyway(req)
            } else {
                retry_later(req)
            }
        }
        Ok(validated) => process(validated)
    }
}

// ✅ CORRECT: Deterministic routing (Jidoka)
fn handle_request(req: Request) -> Result<Accepted, Refused> {
    // NO negotiation - only validation
    let validated = gate.validate(req)?;

    // Passed all gates - accept
    Ok(Accepted {
        request: validated,
        receipt: AcceptanceReceipt::new(),
    })
    // OR failed a gate - refuse with receipt
}
```

**The Decision Tree is Closed**:

```rust
pub enum GateDecision {
    Accept { receipt: AcceptanceReceipt },
    Refuse { receipt: RefusalReceipt, routing: RefusalRouting },
}

pub enum RefusalRouting {
    DeadLetter,           // Discard permanently (e.g., invalid signature)
    RetryWithBackoff,     // Temporary failure (e.g., quota exceeded)
    ManualIntervention,   // Requires human decision (e.g., policy violation)
}
```

---

## Receipt Structure: Hash-Linked Chain

### Base Receipt Structure

Every operation produces a **cryptographic receipt** that forms a hash-linked chain:

```rust
use sha2::{Sha256, Digest};
use base64::{Engine as _, engine::general_purpose};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    /// Unique receipt ID
    pub id: Uuid,

    /// Receipt type
    pub kind: ReceiptKind,

    /// Timestamp (UTC, nanosecond precision)
    pub timestamp: DateTime<Utc>,

    /// Decision made
    pub decision: Decision,

    /// Context metadata
    pub context: ReceiptContext,

    /// Type-specific details
    pub details: serde_json::Value,

    /// SHA-256 hash of previous receipt (base64 encoded)
    pub prev_chain_hash: String,

    /// SHA-256 hash of this receipt (computed on serialization)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub self_hash: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReceiptKind {
    // Acceptance receipts
    SignalReceived,
    ActionAttempted,
    ActionCompleted,

    // Refusal receipts
    ValidationFailed,
    SignatureInvalid,
    AuthorizationDenied,
    EntitlementExpired,
    QuotaExceeded,
    CircuitBreakerOpen,
    RedModeActive,

    // State transition receipts
    GateModeChanged,
    CircuitBreakerStateChanged,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Decision {
    Accept,   // Request allowed, action proceeded
    Refuse,   // Request denied, action blocked
    Unknown,  // Unable to determine (dependency failure)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptContext {
    pub project_id: String,
    pub repo: String,
    pub branch: String,
    pub sku_id: Option<Uuid>,
    pub account_id: Option<Uuid>,
}

impl Receipt {
    /// Compute SHA-256 hash of this receipt
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();

        // Serialize receipt deterministically (exclude self_hash field)
        let serialized = serde_json::to_string(&ReceiptForHashing {
            id: self.id,
            kind: self.kind.clone(),
            timestamp: self.timestamp,
            decision: self.decision.clone(),
            context: self.context.clone(),
            details: self.details.clone(),
            prev_chain_hash: self.prev_chain_hash.clone(),
        }).expect("Failed to serialize receipt");

        hasher.update(serialized.as_bytes());
        let result = hasher.finalize();

        general_purpose::STANDARD.encode(result)
    }

    /// Verify this receipt's chain link
    pub fn verify_chain_link(&self, previous: &Receipt) -> Result<(), ChainError> {
        let expected_prev_hash = previous.compute_hash();

        if self.prev_chain_hash != expected_prev_hash {
            return Err(ChainError::HashMismatch {
                expected: expected_prev_hash,
                actual: self.prev_chain_hash.clone(),
            });
        }

        // Verify timestamp ordering
        if self.timestamp <= previous.timestamp {
            return Err(ChainError::TimestampViolation {
                previous: previous.timestamp,
                current: self.timestamp,
            });
        }

        Ok(())
    }
}

#[derive(Debug, Serialize)]
struct ReceiptForHashing {
    id: Uuid,
    kind: ReceiptKind,
    timestamp: DateTime<Utc>,
    decision: Decision,
    context: ReceiptContext,
    details: serde_json::Value,
    prev_chain_hash: String,
}

#[derive(Debug, thiserror::Error)]
pub enum ChainError {
    #[error("Chain hash mismatch: expected {expected}, got {actual}")]
    HashMismatch { expected: String, actual: String },

    #[error("Timestamp violation: current {current} <= previous {previous}")]
    TimestampViolation { previous: DateTime<Utc>, current: DateTime<Utc> },
}
```

### Receipt Chain Example

```
Receipt 1 (Genesis)
┌─────────────────────────────────────────────────────────┐
│ id: 00000000-0000-0000-0000-000000000001                │
│ kind: SignalReceived                                    │
│ timestamp: 2026-02-09T10:00:00.000000000Z              │
│ decision: Accept                                        │
│ prev_chain_hash: "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="    │
│ self_hash: "7f3a9b2c4d1e5f6a8b9c0d1e2f3a4b5c..."      │
└─────────────────────────────────────────────────────────┘
                         ↓
Receipt 2 (Action Attempted)
┌─────────────────────────────────────────────────────────┐
│ id: 00000000-0000-0000-0000-000000000002                │
│ kind: ActionAttempted                                   │
│ timestamp: 2026-02-09T10:00:01.123456789Z              │
│ decision: Accept                                        │
│ prev_chain_hash: "7f3a9b2c4d1e5f6a8b9c0d1e2f3a4b5c..." │ ← Links to Receipt 1
│ self_hash: "4d2e1a9f3c5b7d8e6f9a0b1c2d3e4f5a..."      │
└─────────────────────────────────────────────────────────┘
                         ↓
Receipt 3 (Action Completed)
┌─────────────────────────────────────────────────────────┐
│ id: 00000000-0000-0000-0000-000000000003                │
│ kind: ActionCompleted                                   │
│ timestamp: 2026-02-09T10:00:45.987654321Z              │
│ decision: Accept                                        │
│ prev_chain_hash: "4d2e1a9f3c5b7d8e6f9a0b1c2d3e4f5a..." │ ← Links to Receipt 2
│ self_hash: "9e8f7a6b5c4d3e2f1a0b9c8d7e6f5a4b..."      │
└─────────────────────────────────────────────────────────┘
```

**Properties of the Chain**:

1. **Immutability**: Cannot modify past receipts without breaking chain
2. **Verifiability**: Anyone can verify chain integrity
3. **Ordering**: Timestamps must increase monotonically
4. **Completeness**: No gaps allowed in chain
5. **Determinism**: Same inputs → Same hash

---

## Narrative Replaced by Verifiable Artifacts

### Traditional Approach: Narrative

```
Developer: "I deployed the feature to production."
Manager: "Did it work?"
Developer: "I think so. The logs look good."
Manager: "Are you sure?"
Developer: "Pretty sure. I don't see any errors."
```

**Problem**: Trust-based, narrative-driven, no proof.

### Jidoka Approach: Verifiable Artifacts

```rust
// Every operation produces verifiable receipt
let receipt = deploy_feature(&spec)?;

// Receipt contains cryptographic proof
assert_eq!(receipt.kind, ReceiptKind::ActionCompleted);
assert_eq!(receipt.decision, Decision::Accept);

// Receipt is hash-linked to entire operation history
receipt.verify_chain(&previous_receipts)?;

// Receipt is persisted immutably
receipt_store.persist(&receipt)?;

// Anyone can verify the deployment
let verified = receipt_store.verify_receipt_chain(receipt.id)?;
assert!(verified);
```

**Evidence**:

1. **Receipt ID**: `a1b2c3d4-e5f6-7a8b-9c0d-1e2f3a4b5c6d`
2. **Timestamp**: `2026-02-09T10:00:45.987654321Z` (nanosecond precision)
3. **Hash**: `9e8f7a6b5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9e8d7c6b5a4f3e2d1c0b9a8`
4. **Chain Link**: Verifiable link to previous receipt
5. **Decision**: `Accept` (deterministic, no ambiguity)

### Concrete Example: Entitlement Expiration

**Narrative Version**:
```
"We disabled the customer's access because their subscription expired."
```

**Receipt Version**:
```json
{
  "id": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
  "kind": "EntitlementExpired",
  "timestamp": "2026-02-09T10:15:23.456789012Z",
  "decision": "Refuse",
  "context": {
    "project_id": "ggen-prod-123456",
    "repo": "github.com/acme/catalog",
    "branch": "main",
    "sku_id": "550e8400-e29b-41d4-a716-446655440000",
    "account_id": "650e8400-e29b-41d4-a716-446655440001"
  },
  "details": {
    "entitlement_id": "750e8400-e29b-41d4-a716-446655440002",
    "expired_at": "2026-02-08T23:59:59.999999999Z",
    "reason": "subscription_ended",
    "auto_renew_failed": false,
    "grace_period_days": 0,
    "next_action": "notify_customer"
  },
  "prev_chain_hash": "4d2e1a9f3c5b7d8e6f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d",
  "self_hash": "3c2d1e0f9e8d7c6b5a4f3e2d1c0b9a8b7c6d5e4f3a2b1c0d9e8f7a6b5c4d3e2"
}
```

**Verification**:

```rust
// Load receipt from immutable store
let receipt = receipt_store.load("f47ac10b-58cc-4372-a567-0e02b2c3d479")?;

// Verify hash chain
receipt.verify_chain_link(&previous_receipt)?;

// Verify timestamp ordering
assert!(receipt.timestamp > previous_receipt.timestamp);

// Verify decision is deterministic
assert_eq!(receipt.decision, Decision::Refuse);

// Verify expiration date
let details: EntitlementExpiredDetails = serde_json::from_value(receipt.details)?;
assert!(details.expired_at < receipt.timestamp);

// Mathematical certainty: Receipt proves expiration
```

---

## Receipt Verification and Audit Trails

### Verification Algorithm

```rust
pub struct ReceiptChainVerifier;

impl ReceiptChainVerifier {
    /// Verify entire receipt chain from genesis to latest
    pub fn verify_chain(receipts: &[Receipt]) -> Result<VerificationReport, ChainError> {
        if receipts.is_empty() {
            return Err(ChainError::EmptyChain);
        }

        let mut report = VerificationReport::new();

        // Verify genesis receipt
        let genesis = &receipts[0];
        if genesis.prev_chain_hash != Self::genesis_hash() {
            report.errors.push(VerificationError::InvalidGenesis {
                expected: Self::genesis_hash(),
                actual: genesis.prev_chain_hash.clone(),
            });
        }

        // Verify each link in chain
        for i in 1..receipts.len() {
            let previous = &receipts[i - 1];
            let current = &receipts[i];

            // Verify hash link
            match current.verify_chain_link(previous) {
                Ok(_) => report.verified_links += 1,
                Err(e) => report.errors.push(VerificationError::ChainBroken {
                    receipt_id: current.id,
                    index: i,
                    error: e,
                }),
            }

            // Verify timestamp monotonicity
            if current.timestamp <= previous.timestamp {
                report.errors.push(VerificationError::TimestampViolation {
                    receipt_id: current.id,
                    current_ts: current.timestamp,
                    previous_ts: previous.timestamp,
                });
            }

            // Verify hash self-consistency
            let computed_hash = current.compute_hash();
            if let Some(ref self_hash) = current.self_hash {
                if &computed_hash != self_hash {
                    report.errors.push(VerificationError::HashMismatch {
                        receipt_id: current.id,
                        expected: computed_hash,
                        actual: self_hash.clone(),
                    });
                }
            }
        }

        report.total_receipts = receipts.len();
        report.is_valid = report.errors.is_empty();

        Ok(report)
    }

    fn genesis_hash() -> String {
        // Genesis hash: 32 zero bytes, base64 encoded
        general_purpose::STANDARD.encode([0u8; 32])
    }
}

#[derive(Debug)]
pub struct VerificationReport {
    pub total_receipts: usize,
    pub verified_links: usize,
    pub errors: Vec<VerificationError>,
    pub is_valid: bool,
}

impl VerificationReport {
    fn new() -> Self {
        Self {
            total_receipts: 0,
            verified_links: 0,
            errors: Vec::new(),
            is_valid: false,
        }
    }
}

#[derive(Debug)]
pub enum VerificationError {
    InvalidGenesis { expected: String, actual: String },
    ChainBroken { receipt_id: Uuid, index: usize, error: ChainError },
    TimestampViolation { receipt_id: Uuid, current_ts: DateTime<Utc>, previous_ts: DateTime<Utc> },
    HashMismatch { receipt_id: Uuid, expected: String, actual: String },
}
```

### Audit Trail Queries

```rust
pub struct ReceiptAuditor;

impl ReceiptAuditor {
    /// Find all refusals in time range
    pub fn find_refusals(
        store: &ReceiptStore,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<Vec<Receipt>, AuditError> {
        store.query()
            .filter_by_decision(Decision::Refuse)
            .filter_by_timestamp_range(start, end)
            .execute()
    }

    /// Trace request lifecycle
    pub fn trace_request(
        store: &ReceiptStore,
        request_id: Uuid,
    ) -> Result<RequestTrace, AuditError> {
        let receipts = store.query()
            .filter_by_context("request_id", request_id)
            .order_by_timestamp_asc()
            .execute()?;

        Ok(RequestTrace {
            request_id,
            lifecycle: receipts,
        })
    }

    /// Verify specific receipt and its ancestry
    pub fn verify_receipt_ancestry(
        store: &ReceiptStore,
        receipt_id: Uuid,
    ) -> Result<VerificationReport, AuditError> {
        // Load receipt
        let receipt = store.load(receipt_id)?;

        // Walk back chain to genesis
        let mut ancestry = vec![receipt];
        let mut current_hash = ancestry[0].prev_chain_hash.clone();

        while current_hash != ReceiptChainVerifier::genesis_hash() {
            let prev_receipt = store.find_by_hash(&current_hash)?;
            current_hash = prev_receipt.prev_chain_hash.clone();
            ancestry.insert(0, prev_receipt);
        }

        // Verify entire chain
        ReceiptChainVerifier::verify_chain(&ancestry)
    }

    /// Generate compliance report
    pub fn generate_compliance_report(
        store: &ReceiptStore,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<ComplianceReport, AuditError> {
        let all_receipts = store.query()
            .filter_by_timestamp_range(start, end)
            .execute()?;

        let mut report = ComplianceReport::new(start, end);

        for receipt in all_receipts {
            match receipt.decision {
                Decision::Accept => report.accepted += 1,
                Decision::Refuse => {
                    report.refused += 1;
                    report.refusal_reasons.entry(receipt.kind.clone())
                        .and_modify(|c| *c += 1)
                        .or_insert(1);
                }
                Decision::Unknown => report.unknown += 1,
            }

            report.total += 1;
        }

        Ok(report)
    }
}

#[derive(Debug)]
pub struct RequestTrace {
    pub request_id: Uuid,
    pub lifecycle: Vec<Receipt>,
}

#[derive(Debug)]
pub struct ComplianceReport {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
    pub total: usize,
    pub accepted: usize,
    pub refused: usize,
    pub unknown: usize,
    pub refusal_reasons: std::collections::HashMap<ReceiptKind, usize>,
}

impl ComplianceReport {
    fn new(start: DateTime<Utc>, end: DateTime<Utc>) -> Self {
        Self {
            start,
            end,
            total: 0,
            accepted: 0,
            refused: 0,
            unknown: 0,
            refusal_reasons: std::collections::HashMap::new(),
        }
    }
}
```

---

## Implementation in Rust with Cryptographic Signatures

### Complete Receipt System

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Sha256, Digest};
use uuid::Uuid;
use anyhow::Result;
use thiserror::Error;

/// Receipt persistence layer
pub struct ReceiptStore {
    // In production: Firestore, PostgreSQL, or other immutable store
    receipts: std::sync::Arc<std::sync::Mutex<Vec<Receipt>>>,
}

impl ReceiptStore {
    pub fn new() -> Self {
        Self {
            receipts: std::sync::Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }

    /// Persist receipt immutably
    pub fn persist(&self, receipt: &Receipt) -> Result<()> {
        let mut receipts = self.receipts.lock().unwrap();

        // Verify chain link if not genesis
        if !receipts.is_empty() {
            let previous = receipts.last().unwrap();
            receipt.verify_chain_link(previous)?;
        }

        receipts.push(receipt.clone());
        Ok(())
    }

    /// Load receipt by ID
    pub fn load(&self, id: Uuid) -> Result<Receipt> {
        let receipts = self.receipts.lock().unwrap();
        receipts.iter()
            .find(|r| r.id == id)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("Receipt not found: {}", id))
    }

    /// Get all receipts (for verification)
    pub fn all(&self) -> Vec<Receipt> {
        self.receipts.lock().unwrap().clone()
    }

    /// Query builder
    pub fn query(&self) -> ReceiptQuery {
        ReceiptQuery::new(self.receipts.clone())
    }
}

/// Query builder for receipt store
pub struct ReceiptQuery {
    receipts: std::sync::Arc<std::sync::Mutex<Vec<Receipt>>>,
    filters: Vec<Box<dyn Fn(&Receipt) -> bool + Send>>,
}

impl ReceiptQuery {
    fn new(receipts: std::sync::Arc<std::sync::Mutex<Vec<Receipt>>>) -> Self {
        Self {
            receipts,
            filters: Vec::new(),
        }
    }

    pub fn filter_by_decision(mut self, decision: Decision) -> Self {
        self.filters.push(Box::new(move |r| r.decision == decision));
        self
    }

    pub fn filter_by_timestamp_range(
        mut self,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Self {
        self.filters.push(Box::new(move |r| {
            r.timestamp >= start && r.timestamp <= end
        }));
        self
    }

    pub fn filter_by_kind(mut self, kind: ReceiptKind) -> Self {
        self.filters.push(Box::new(move |r| r.kind == kind));
        self
    }

    pub fn execute(self) -> Result<Vec<Receipt>> {
        let receipts = self.receipts.lock().unwrap();
        Ok(receipts.iter()
            .filter(|r| self.filters.iter().all(|f| f(r)))
            .cloned()
            .collect())
    }
}

/// Receipt builder for ergonomic construction
pub struct ReceiptBuilder {
    id: Uuid,
    kind: ReceiptKind,
    decision: Decision,
    context: ReceiptContext,
    details: serde_json::Value,
    prev_chain_hash: Option<String>,
}

impl ReceiptBuilder {
    pub fn new(kind: ReceiptKind, decision: Decision) -> Self {
        Self {
            id: Uuid::new_v4(),
            kind,
            decision,
            context: ReceiptContext::default(),
            details: serde_json::json!({}),
            prev_chain_hash: None,
        }
    }

    pub fn context(mut self, context: ReceiptContext) -> Self {
        self.context = context;
        self
    }

    pub fn details(mut self, details: serde_json::Value) -> Self {
        self.details = details;
        self
    }

    pub fn prev_chain_hash(mut self, hash: String) -> Self {
        self.prev_chain_hash = Some(hash);
        self
    }

    pub fn build(self) -> Receipt {
        let prev_chain_hash = self.prev_chain_hash.unwrap_or_else(|| {
            // Genesis hash: 32 zero bytes, base64 encoded
            base64::engine::general_purpose::STANDARD.encode([0u8; 32])
        });

        let mut receipt = Receipt {
            id: self.id,
            kind: self.kind,
            timestamp: Utc::now(),
            decision: self.decision,
            context: self.context,
            details: self.details,
            prev_chain_hash,
            self_hash: None,
        };

        // Compute and store self hash
        receipt.self_hash = Some(receipt.compute_hash());
        receipt
    }
}

impl Default for ReceiptContext {
    fn default() -> Self {
        Self {
            project_id: "unknown".to_string(),
            repo: "unknown".to_string(),
            branch: "unknown".to_string(),
            sku_id: None,
            account_id: None,
        }
    }
}
```

### Usage Example

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_chain_verification() {
        let store = ReceiptStore::new();

        // Create genesis receipt
        let receipt1 = ReceiptBuilder::new(
            ReceiptKind::SignalReceived,
            Decision::Accept,
        )
        .context(ReceiptContext {
            project_id: "test-project".to_string(),
            repo: "github.com/test/repo".to_string(),
            branch: "main".to_string(),
            sku_id: Some(Uuid::new_v4()),
            account_id: Some(Uuid::new_v4()),
        })
        .details(serde_json::json!({
            "signal_id": "signal-001",
            "signal_type": "launch",
        }))
        .build();

        store.persist(&receipt1).unwrap();

        // Create second receipt linked to first
        let receipt2 = ReceiptBuilder::new(
            ReceiptKind::ActionAttempted,
            Decision::Accept,
        )
        .context(receipt1.context.clone())
        .details(serde_json::json!({
            "action_id": "action-001",
            "signal_id": "signal-001",
        }))
        .prev_chain_hash(receipt1.compute_hash())
        .build();

        store.persist(&receipt2).unwrap();

        // Verify chain
        let all_receipts = store.all();
        let report = ReceiptChainVerifier::verify_chain(&all_receipts).unwrap();

        assert!(report.is_valid);
        assert_eq!(report.verified_links, 1);
        assert_eq!(report.total_receipts, 2);
        assert!(report.errors.is_empty());
    }

    #[test]
    fn test_refusal_receipt() {
        let store = ReceiptStore::new();

        // Create refusal receipt
        let receipt = ReceiptBuilder::new(
            ReceiptKind::QuotaExceeded,
            Decision::Refuse,
        )
        .details(serde_json::json!({
            "quota_type": "rate_limit",
            "limit": 1000,
            "current": 1001,
            "reset_at": "2026-02-10T00:00:00Z",
        }))
        .build();

        store.persist(&receipt).unwrap();

        // Query refusals
        let refusals = store.query()
            .filter_by_decision(Decision::Refuse)
            .execute()
            .unwrap();

        assert_eq!(refusals.len(), 1);
        assert_eq!(refusals[0].kind, ReceiptKind::QuotaExceeded);
    }

    #[test]
    fn test_chain_tampering_detection() {
        let store = ReceiptStore::new();

        let receipt1 = ReceiptBuilder::new(
            ReceiptKind::SignalReceived,
            Decision::Accept,
        ).build();

        store.persist(&receipt1).unwrap();

        // Attempt to create receipt with wrong previous hash
        let mut receipt2 = ReceiptBuilder::new(
            ReceiptKind::ActionAttempted,
            Decision::Accept,
        )
        .prev_chain_hash("WRONG_HASH".to_string())
        .build();

        // This should fail chain verification
        let result = store.persist(&receipt2);
        assert!(result.is_err());
    }
}
```

---

## Real-World Scenarios

### Scenario 1: Rate Limit Exceeded (Yellow Mode)

**Trigger**: System detects 1200 requests/second (limit: 1000/s)

```rust
// Gate detects high load
gate.transition_to_yellow(20, "High request rate: 1200 req/s".to_string());

// Next request arrives
let request = Request::new(/* ... */);

// Gate applies throttling
match gate.process(request) {
    Err(refused) => {
        // Receipt generated automatically
        assert_eq!(refused.receipt.kind, ReceiptKind::YellowModeThrottled);
        assert_eq!(refused.receipt.decision, Decision::Refuse);

        // Receipt details explain why
        let details: YellowModeDetails =
            serde_json::from_value(refused.receipt.details)?;
        assert_eq!(details.throttle_percentage, 20);
        assert_eq!(details.reason, "High request rate: 1200 req/s");

        // Receipt is persisted immutably
        receipt_store.persist(&refused.receipt)?;

        // Return 429 Too Many Requests
        Ok(HttpResponse::TooManyRequests(refused.receipt))
    }
    Ok(accepted) => {
        // Request passed throttling (probabilistic)
        process_request(accepted)
    }
}
```

### Scenario 2: Circuit Breaker Opens (Red Mode)

**Trigger**: External service fails 5 times in 10 seconds

```rust
// Circuit breaker opens
gate.circuit_breaker_state = CircuitBreakerState::Open;
gate.mode = GateMode::Red {
    reason: "External service failing: 5 failures in 10s".to_string(),
    since: Utc::now(),
};

// All new requests refused immediately
let request = Request::new(/* ... */);

match gate.process(request) {
    Err(refused) => {
        // Receipt proves refusal
        assert_eq!(refused.receipt.kind, ReceiptKind::RedModeActive);
        assert_eq!(refused.receipt.decision, Decision::Refuse);

        // Receipt chains to previous receipts
        refused.receipt.verify_chain_link(&last_receipt)?;

        // Deterministic routing: No retry
        assert_eq!(refused.routing, RefusalRouting::DeadLetter);

        // Return 503 Service Unavailable
        Ok(HttpResponse::ServiceUnavailable(refused.receipt))
    }
    Ok(_) => unreachable!("No requests accepted in RED mode"),
}
```

### Scenario 3: Entitlement Expiration During Request

**Trigger**: Request starts with valid entitlement, expires mid-flight

```rust
// Request accepted initially
let receipt1 = ReceiptBuilder::new(
    ReceiptKind::SignalReceived,
    Decision::Accept,
).build();

receipt_store.persist(&receipt1)?;

// Start processing action
let receipt2 = ReceiptBuilder::new(
    ReceiptKind::ActionAttempted,
    Decision::Accept,
)
.prev_chain_hash(receipt1.compute_hash())
.build();

receipt_store.persist(&receipt2)?;

// Mid-processing: Entitlement expires
let entitlement_expired = ReceiptBuilder::new(
    ReceiptKind::EntitlementExpired,
    Decision::Refuse,
)
.prev_chain_hash(receipt2.compute_hash())
.details(serde_json::json!({
    "entitlement_id": "ent-123",
    "expired_at": "2026-02-09T10:15:00Z",
    "reason": "subscription_ended",
}))
.build();

receipt_store.persist(&entitlement_expired)?;

// Action must be reverted
let receipt3 = ReceiptBuilder::new(
    ReceiptKind::ActionFailed,
    Decision::Refuse,
)
.prev_chain_hash(entitlement_expired.compute_hash())
.details(serde_json::json!({
    "action_id": "action-123",
    "error": "EntitlementExpired",
    "cleanup_performed": true,
}))
.build();

receipt_store.persist(&receipt3)?;

// Entire operation recorded in verifiable chain
let trace = ReceiptAuditor::trace_request(&receipt_store, request_id)?;
assert_eq!(trace.lifecycle.len(), 4);
assert_eq!(trace.lifecycle[3].kind, ReceiptKind::ActionFailed);
```

---

## Summary

### Key Principles

1. **Abnormality Triggers Stop-the-Line**: Any deviation halts processing immediately
2. **GREEN/YELLOW/RED Gates**: Three modes determine what enters the system
3. **No Negotiation**: Decisions are deterministic, not interpretive
4. **Hash-Linked Receipts**: Every operation produces verifiable cryptographic proof
5. **Narrative → Artifacts**: Replace stories with mathematical certainty
6. **Immutable Audit Trail**: Complete operation history, tamper-proof

### Benefits

- **Zero Trust**: Verify everything, trust nothing
- **Complete Auditability**: Every decision has cryptographic proof
- **Deterministic Debugging**: Trace exact request lifecycle
- **Compliance**: Immutable records for regulatory requirements
- **No Silent Failures**: Every refusal documented and verifiable

### The Manufacturing Paradigm Applied

In traditional software, errors are exceptional. In the manufacturing paradigm, errors are **controlled**.

- **Traditional**: "Try and hope it works"
- **Manufacturing**: "Verify and prove it works"

Jidoka receipts are the mechanism that makes software manufacturing possible.

---

## Further Reading

- [01-regime-split.md](./01-regime-split.md) - SELECT/DO vs CONSTRUCT regimes
- [10-packet-discipline.md](./10-packet-discipline.md) - Type-safe work orders
- [04-no-moving-parts.md](./04-no-moving-parts.md) - Immutable architecture
- [/docs/tps-reference/10-jidoka.md](/docs/tps-reference/10-jidoka.md) - Jidoka implementation
- [/docs/99-appendix/receipt-schema.md](/docs/99-appendix/receipt-schema.md) - Receipt schema reference
- [/docs/30-autonomics/refusal-modes.md](/docs/30-autonomics/refusal-modes.md) - Refusal patterns

---

**Document Hash**: `sha256:TBD`
**Generated**: 2026-02-09
**Regime**: CONSTRUCT (this document is specification-driven)
