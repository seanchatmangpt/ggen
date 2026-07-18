# Marketplace Autonomic Patterns Guide

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Architecture Patterns
**Audience**: Architects, Lead Engineers, Pattern Designers

---

## Table of Contents

1. [FSM as Reliability Primitive](#fsm-as-reliability-primitive)
2. [Composition Patterns](#composition-patterns)
3. [Idempotence Patterns](#idempotence-patterns)
4. [Compensation & Rollback Patterns](#compensation--rollback-patterns)
5. [Timeout & Escalation Patterns](#timeout--escalation-patterns)
6. [Error Recovery Patterns](#error-recovery-patterns)
7. [Data Consistency Patterns](#data-consistency-patterns)
8. [Performance Patterns](#performance-patterns)

---

## FSM as Reliability Primitive

### Core Concept

FSMs are the foundation of reliability in the marketplace system. Rather than treating state as implicit (scattered across databases), FSMs make state **explicit, type-safe, and verifiable**.

```rust
// ❌ ANTI-PATTERN: State hidden in data
pub struct Subscription {
    customer_id: String,
    is_active: bool,           // ← Implicit state
    is_trial: bool,            // ← Contradictory (can both be true)
    is_cancelled: bool,        // ← Invalid combinations possible
}

// ✅ PATTERN: Explicit FSM state
pub enum SubscriptionState {
    Trial,     // Cannot be cancelled AND trial
    Active,    // Cannot be expired AND active
    Cancelled, // Terminal, no further transitions
    Expired,   // Terminal, no further transitions
}

// Impossible states cannot be represented
```

### Type Safety Pattern

**Pattern Name**: Compile-Time State Enforcement
**Benefit**: Impossible states become compiler errors, not runtime bugs

```rust
// Entitlement FSM enforces valid transitions at compile time
impl EntitlementGovernor {
    pub fn transition(&mut self, event: EntitlementEvent) -> Result<()> {
        match (self.state, event) {
            // Valid transitions
            (EntitlementState::PendingApproval, EntitlementEvent::Approve) => {
                self.state = EntitlementState::Active;
                Ok(())
            }

            // Invalid transition → Compile error
            (EntitlementState::PendingApproval, EntitlementEvent::Suspend) => {
                Err(EntitlementError::InvalidTransition { .. })
            }

            // Impossible state combos caught by compiler
            _ => Err(EntitlementError::InvalidTransition { .. }),
        }
    }
}

// Caller cannot accidentally put system in invalid state
let gov = EntitlementGovernor::new();
gov.transition(EntitlementEvent::Suspend)?;  // ✗ Only valid from Active
```

### Audit Trail Pattern

**Pattern Name**: Cryptographic Proof per Transition
**Benefit**: Every state change is provably immutable

```rust
// Every transition produces an audit entry
pub struct AuditEntry {
    timestamp: DateTime<Utc>,
    from_state: String,
    to_state: String,
    event: String,
    actor: String,
    data_hash: String,      // SHA-256 of context data
    signature: String,      // Ed25519 signature
}

// Implementation
impl EntitlementGovernor {
    pub fn transition(&mut self, event: EntitlementEvent) -> Result<AuditEntry> {
        let from_state = self.state;

        // Validate and execute transition
        self.validate_transition(&from_state, &event)?;
        self.state = self.compute_next_state(&from_state, &event)?;

        // Generate immutable proof
        let entry = AuditEntry {
            timestamp: Utc::now(),
            from_state: format!("{:?}", from_state),
            to_state: format!("{:?}", self.state),
            event: format!("{:?}", event),
            actor: event.actor().clone(),
            data_hash: sha256(&self.serialize()),
            signature: self.sign_entry(),  // Ed25519
        };

        // Append to immutable ledger
        self.receipt_ledger.append(entry.clone());

        Ok(entry)
    }
}
```

### Timeout-Based Escalation Pattern

**Pattern Name**: Automatic Escalation on Timeout
**Benefit**: No manual intervention needed for time-dependent transitions

```rust
// Entitlement stuck in pending_approval for 24h?
impl EntitlementGovernor {
    pub fn check_timeouts(&mut self) -> Result<Vec<AuditEntry>> {
        let mut escalations = Vec::new();

        match self.state {
            EntitlementState::PendingApproval => {
                if Utc::now() >= self.created_at + Duration::hours(24) {
                    // Auto-escalate to manager review
                    let event = EntitlementEvent::EscalateToManager {
                        reason: "24h approval timeout".to_string(),
                    };

                    let entry = self.transition(event)?;
                    escalations.push(entry);

                    // Broadcast to notification system
                    self.emit(EntitlementEvent::ApprovalEscalated);
                }
            }

            EntitlementState::Suspended => {
                if Utc::now() >= self.suspension_start + Duration::hours(72) {
                    // Auto-issue refund
                    let entry = self.transition(
                        EntitlementEvent::AutoRefundIssued
                    )?;
                    escalations.push(entry);

                    // Broadcast to billing system
                    self.emit(EntitlementEvent::RefundRequested);
                }
            }

            _ => {}
        }

        Ok(escalations)
    }
}

// Orchestrator calls this periodically
pub async fn orchestrator_check_timeouts(orchestrator: &mut Orchestrator) {
    for governor in &mut orchestrator.governors {
        if let Ok(escalations) = governor.check_timeouts() {
            for entry in escalations {
                // Cascade escalation events through system
                orchestrator.broadcast(entry);
            }
        }
    }
}
```

### Deterministic Behavior Pattern

**Pattern Name**: No Randomness, No Concurrency Surprises
**Benefit**: Behavior is reproducible and testable

```rust
// ❌ ANTI-PATTERN: Randomness in FSM
pub fn decide_throttle_delay() -> u64 {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    rng.gen_range(100..10000)  // Non-deterministic delay
}

// ✅ PATTERN: Deterministic exponential backoff
pub fn compute_throttle_delay(retry_count: u32) -> Duration {
    // Same input → Same output, always
    // No randomness, no surprises
    let delay_ms = 100 * 2_u64.pow(retry_count.min(7));  // Cap at 2^7
    Duration::from_millis(delay_ms.min(10000))  // Max 10s
}

// Test the pattern (deterministic = testable)
#[test]
fn test_throttle_delay_is_deterministic() {
    assert_eq!(compute_throttle_delay(0), Duration::from_millis(100));
    assert_eq!(compute_throttle_delay(1), Duration::from_millis(200));
    assert_eq!(compute_throttle_delay(2), Duration::from_millis(400));
    assert_eq!(compute_throttle_delay(7), Duration::from_millis(10000));  // Capped
}
```

---

## Composition Patterns

### Pattern #1: Sequential Governor Composition

**Use Case**: Event requires sequential processing (one governor must complete before next starts)

```rust
// Customer upgrades subscription
// Must be processed sequentially:
// 1. SubscriptionGovernor: Create upgrade order
// 2. BillingGovernor: Calculate proration & charge
// 3. EntitlementGovernor: Enable new features
// 4. QuotaSlaGovernor: Allocate new quota

pub async fn handle_subscription_upgrade(
    orchestrator: &mut Orchestrator,
    event: MarketplaceEvent::SubscriptionUpgrade,
) -> Result<Vec<AuditEntry>> {
    let mut entries = Vec::new();

    // 1. SubscriptionGovernor processes upgrade
    let subscription_response = orchestrator
        .route_event(GovernorType::Subscription, event.clone())
        .await?;
    entries.push(subscription_response.audit_entry);

    // Get proration details from subscription response
    let proration = subscription_response.context["proration"].clone();

    // 2. BillingGovernor processes charge (sequential dependency on step 1)
    let billing_event = MarketplaceEvent::ProcessProrationCharge {
        customer_id: event.customer_id,
        amount: proration["charge_amount"].as_f64().unwrap(),
        proration_details: proration,
    };

    let billing_response = orchestrator
        .route_event(GovernorType::Billing, billing_event)
        .await?;
    entries.push(billing_response.audit_entry);

    // 3. EntitlementGovernor enables new features
    let entitlement_event = MarketplaceEvent::GrantEntitlements {
        customer_id: event.customer_id,
        new_tier: event.new_tier,
    };

    let entitlement_response = orchestrator
        .route_event(GovernorType::Entitlement, entitlement_event)
        .await?;
    entries.push(entitlement_response.audit_entry);

    // 4. QuotaSlaGovernor allocates new quota
    let quota_event = MarketplaceEvent::AllocateQuota {
        customer_id: event.customer_id,
        new_tier: event.new_tier,
    };

    let quota_response = orchestrator
        .route_event(GovernorType::QuotaSla, quota_event)
        .await?;
    entries.push(quota_response.audit_entry);

    Ok(entries)
}
```

### Pattern #2: Parallel Governor Composition

**Use Case**: Multiple governors can process independently (no dependencies)

```rust
// Customer created, need to:
// - Create profile (CustomerAccountGovernor)
// - Create trial subscription (SubscriptionGovernor)
// - Allocate quota (QuotaSlaGovernor)
// - Verify identity (ComplianceGovernor)
// All can happen in parallel

pub async fn handle_customer_signup(
    orchestrator: &mut Orchestrator,
    event: MarketplaceEvent::CustomerSignup,
) -> Result<Vec<AuditEntry>> {
    // Spawn 4 parallel tasks
    let futures = vec![
        // Task 1: Create profile
        orchestrator.route_event_async(
            GovernorType::CustomerAccount,
            MarketplaceEvent::CreateProfile { /* ... */ },
        ),

        // Task 2: Create trial subscription
        orchestrator.route_event_async(
            GovernorType::Subscription,
            MarketplaceEvent::CreateTrialSubscription { /* ... */ },
        ),

        // Task 3: Allocate quota
        orchestrator.route_event_async(
            GovernorType::QuotaSla,
            MarketplaceEvent::AllocateQuota { /* ... */ },
        ),

        // Task 4: Verify identity
        orchestrator.route_event_async(
            GovernorType::Compliance,
            MarketplaceEvent::VerifyIdentity { /* ... */ },
        ),
    ];

    // Wait for all to complete
    let results = futures::future::join_all(futures).await;

    // Collect audit entries
    let entries: Vec<AuditEntry> = results
        .into_iter()
        .filter_map(|r| r.ok())
        .map(|response| response.audit_entry)
        .collect();

    Ok(entries)
}
```

### Pattern #3: Conditional Governor Composition

**Use Case**: Route to different governors based on event/state

```rust
// Handle payment event, route to different governors based on payment status
pub async fn handle_payment_event(
    orchestrator: &mut Orchestrator,
    event: MarketplaceEvent::PaymentProcessed,
) -> Result<Vec<AuditEntry>> {
    let mut entries = Vec::new();

    match event.status {
        PaymentStatus::Success => {
            // Payment succeeded: route to Billing, then Subscription
            let billing_response = orchestrator
                .route_event(GovernorType::Billing, event.clone())
                .await?;
            entries.push(billing_response.audit_entry);

            let renewal_response = orchestrator
                .route_event(GovernorType::Subscription,
                    MarketplaceEvent::RenewalSucceeded { /* ... */ })
                .await?;
            entries.push(renewal_response.audit_entry);
        }

        PaymentStatus::Failed => {
            // Payment failed: route to Billing retry, then Entitlement (suspend)
            let billing_response = orchestrator
                .route_event(GovernorType::Billing, event.clone())
                .await?;
            entries.push(billing_response.audit_entry);

            let suspend_response = orchestrator
                .route_event(GovernorType::Entitlement,
                    MarketplaceEvent::Suspend {
                        reason: "payment_failed".to_string()
                    })
                .await?;
            entries.push(suspend_response.audit_entry);
        }

        PaymentStatus::Disputed => {
            // Disputed: route to Billing (dispute handling) + Compliance (investigation)
            let billing_response = orchestrator
                .route_event(GovernorType::Billing, event.clone())
                .await?;
            entries.push(billing_response.audit_entry);

            let compliance_response = orchestrator
                .route_event(GovernorType::Compliance,
                    MarketplaceEvent::InvestigateDispute { /* ... */ })
                .await?;
            entries.push(compliance_response.audit_entry);
        }
    }

    Ok(entries)
}
```

### Pattern #4: Fan-Out/Fan-In Governor Composition

**Use Case**: One event triggers multiple governors, then aggregate results

```rust
// Noisy neighbor detected: notify affected parties
pub async fn handle_noisy_neighbor(
    orchestrator: &mut Orchestrator,
    event: MarketplaceEvent::NoisyNeighborDetected,
) -> Result<OrchestrationResult> {
    // Fan-out: Notify multiple governors
    let notifications = vec![
        // Notify entitlement (may need to reduce features)
        orchestrator.route_event_async(
            GovernorType::Entitlement,
            MarketplaceEvent::TenantContention { /* ... */ },
        ),

        // Notify billing (may need to adjust SLA credits)
        orchestrator.route_event_async(
            GovernorType::Billing,
            MarketplaceEvent::SLAViolation { /* ... */ },
        ),

        // Notify customer account (send notification)
        orchestrator.route_event_async(
            GovernorType::CustomerAccount,
            MarketplaceEvent::NotifyCustomer { /* ... */ },
        ),
    ];

    // Fan-in: Collect all responses
    let results = futures::future::join_all(notifications).await;

    // Aggregate: Check if any governor failed
    let all_succeeded = results.iter().all(|r| r.is_ok());

    Ok(OrchestrationResult {
        success: all_succeeded,
        entries: results
            .into_iter()
            .filter_map(|r| r.ok().map(|resp| resp.audit_entry))
            .collect(),
    })
}
```

---

## Idempotence Patterns

### Pattern #1: Idempotency Key Deduplication

**Use Case**: Prevent duplicate processing when events are retried

```rust
// ✅ PATTERN: Idempotency key prevents duplicate charge
impl BillingGovernor {
    pub fn process_payment(&mut self, event: &PaymentEvent) -> Result<()> {
        // 1. Check if we've seen this idempotency key
        if let Some(cached_response) = self.idempotency_cache.get(&event.idempotency_key) {
            // Same payment processed before, return cached result
            return Ok(cached_response.clone());
        }

        // 2. Validate payment amount matches expected
        if event.amount != self.expected_amount {
            return Err(BillingError::PaymentAmountMismatch {
                expected: self.expected_amount,
                actual: event.amount,
            });
        }

        // 3. Process payment (idempotency key cache prevents duplicates)
        let result = self.charge_payment(event)?;

        // 4. Cache result (keyed by idempotency key)
        self.idempotency_cache.insert(event.idempotency_key.clone(), result.clone());

        Ok(result)
    }
}

// Consumer side
let payment = PaymentEvent {
    idempotency_key: "pay_abc123",  // Unique per customer+transaction
    amount: 99.00,
    timestamp: Utc::now(),
};

// First attempt
match gov.process_payment(&payment) {
    Ok(result) => println!("Charged: ${}", result.amount),
    Err(e) => println!("Failed: {}", e),
}

// Second attempt (webhook retry) - same idempotency_key
match gov.process_payment(&payment) {
    Ok(result) => println!("Returned cached: ${}", result.amount),
    Err(BillingError::IdempotencyViolation) => println!("Already processed"),
}
```

### Pattern #2: Event ID Deduplication in Orchestrator

**Use Case**: Prevent duplicate event processing at orchestrator level

```rust
// Orchestrator tracks seen events
impl MarketplaceOrchestrator {
    pub async fn handle_event(&mut self, event: MarketplaceEvent) -> Result<Vec<AuditEntry>> {
        let event_id = event.get_id();

        // 1. Check if event already processed
        if let Some(cached_response) = self.processed_events.get(&event_id) {
            // Same event processed before, return cached result
            return Ok(cached_response.clone());
        }

        // 2. Process event (route to appropriate governors)
        let entries = self.route_and_coordinate(&event).await?;

        // 3. Cache result (keyed by event_id)
        self.processed_events.insert(event_id, entries.clone());

        Ok(entries)
    }
}

// Usage: Pub/Sub delivers same event twice?
let event = MarketplaceEvent::SubscriptionRenewal {
    id: "evt_abc123",  // Unique event ID
    customer_id: "cust_xyz",
};

// First delivery
let entries1 = orchestrator.handle_event(event.clone()).await?;
// → Actually processes, routes to governors

// Second delivery (retry) - same event ID
let entries2 = orchestrator.handle_event(event).await?;
// → Returns cached entries, no duplicate processing
assert_eq!(entries1, entries2);
```

### Pattern #3: Invariant-Based Idempotence

**Use Case**: Verify idempotence via invariants rather than explicit keys

```rust
// ✅ PATTERN: Proration calculation is deterministic (idempotent)
impl SubscriptionGovernor {
    pub fn calculate_proration(
        current_tier: FeatureTier,
        new_tier: FeatureTier,
        cycle_day: u32,          // 1-30
        cycle_length: u32,       // 30
    ) -> ProrationResult {
        // Deterministic calculation: same input → same output
        let days_remaining = cycle_length - cycle_day;
        let current_amount = current_tier.monthly_price();
        let new_amount = new_tier.monthly_price();

        // Refund for current tier
        let refund = (days_remaining as f64 / cycle_length as f64) * current_amount;

        // Charge for new tier
        let charge = (days_remaining as f64 / cycle_length as f64) * new_amount;

        // Net charge
        let net = charge - refund;

        // Can be called 1000x with same input, always same result
        // No cache needed because calculation itself is pure
        ProrationResult {
            refund,
            charge,
            net,
        }
    }
}

// Test idempotence through properties
#[test]
fn test_proration_is_idempotent() {
    let tier1 = FeatureTier::Professional;
    let tier2 = FeatureTier::Enterprise;
    let day = 10;
    let cycle = 30;

    let result1 = SubscriptionGovernor::calculate_proration(tier1, tier2, day, cycle);
    let result2 = SubscriptionGovernor::calculate_proration(tier1, tier2, day, cycle);
    let result3 = SubscriptionGovernor::calculate_proration(tier1, tier2, day, cycle);

    assert_eq!(result1, result2);
    assert_eq!(result2, result3);
    // Calling 1000x would give same result
}
```

---

## Compensation & Rollback Patterns

### Pattern #1: Explicit Compensation Transactions

**Use Case**: Undo effects of a failed transaction

```rust
// Subscription upgrade fails at entitlement stage
// Must compensate: refund the proration charge
pub async fn handle_upgrade_failure(
    billing_gov: &mut BillingGovernor,
    entitlement_gov: &mut EntitlementGovernor,
    event: UpgradeFailure,
) -> Result<Vec<AuditEntry>> {
    let mut entries = Vec::new();

    // 1. Identify what succeeded before failure
    // (Proration charge was applied in BillingGovernor)

    // 2. Compensate: Issue refund for proration
    let compensation = BillingEvent::CompensationRefund {
        original_transaction: event.proration_transaction_id,
        reason: "SubscriptionUpgrade_Compensation",
        amount: event.proration_amount,
    };

    let refund_response = billing_gov.transition(compensation)?;
    entries.push(refund_response);

    // 3. Restore previous state in EntitlementGovernor
    // (downgrade features back to previous tier)
    let restoration = EntitlementEvent::RestorePreviousTier {
        customer_id: event.customer_id,
        previous_tier: event.previous_tier,
    };

    let restoration_response = entitlement_gov.transition(restoration)?;
    entries.push(restoration_response);

    // 4. Log compensation in audit trail
    entries.push(AuditEntry {
        event: "CompensationCompleted".to_string(),
        reason: event.failure_reason.clone(),
        // ...
    });

    Ok(entries)
}
```

### Pattern #2: Saga Pattern (Distributed Compensation)

**Use Case**: Multi-step transaction with compensation at each step

```rust
// Subscription cancellation saga:
// 1. Calculate refund due
// 2. Issue refund
// 3. Suspend entitlements
// 4. Archive subscription
// If any step fails, rollback all previous steps

pub async fn cancel_subscription_saga(
    orchestrator: &mut Orchestrator,
    customer_id: &str,
) -> Result<SagaResult> {
    let mut saga_state = SagaState {
        steps_completed: Vec::new(),
        steps_failed: None,
    };

    // Step 1: Calculate refund
    match calculate_refund(orchestrator, customer_id).await {
        Ok(refund_amount) => {
            saga_state.steps_completed.push((
                "calculate_refund".to_string(),
                refund_amount.clone(),
            ));

            // Step 2: Issue refund
            match issue_refund(orchestrator, customer_id, &refund_amount).await {
                Ok(refund_receipt) => {
                    saga_state.steps_completed.push((
                        "issue_refund".to_string(),
                        refund_receipt.clone(),
                    ));

                    // Step 3: Suspend entitlements
                    match suspend_entitlements(orchestrator, customer_id).await {
                        Ok(suspension) => {
                            saga_state.steps_completed.push((
                                "suspend_entitlements".to_string(),
                                suspension.clone(),
                            ));

                            // Step 4: Archive subscription
                            match archive_subscription(orchestrator, customer_id).await {
                                Ok(archive) => {
                                    saga_state.steps_completed.push((
                                        "archive_subscription".to_string(),
                                        archive,
                                    ));
                                    return Ok(SagaResult::Success {
                                        steps: saga_state.steps_completed,
                                    });
                                }
                                Err(e) => {
                                    // Step 4 failed, compensate steps 1-3
                                    saga_state.steps_failed = Some(("archive_subscription", e));
                                }
                            }
                        }
                        Err(e) => {
                            saga_state.steps_failed = Some(("suspend_entitlements", e));
                        }
                    }
                }
                Err(e) => {
                    saga_state.steps_failed = Some(("issue_refund", e));
                }
            }
        }
        Err(e) => {
            saga_state.steps_failed = Some(("calculate_refund", e));
        }
    }

    // Compensate: Rollback all completed steps
    let compensation_entries = rollback_saga(&saga_state).await?;

    Ok(SagaResult::Failure {
        completed_steps: saga_state.steps_completed,
        failed_step: saga_state.steps_failed,
        compensation_entries,
    })
}

// Rollback function
async fn rollback_saga(saga_state: &SagaState) -> Result<Vec<AuditEntry>> {
    let mut entries = Vec::new();

    // Rollback in reverse order
    for (step_name, context) in saga_state.steps_completed.iter().rev() {
        match step_name.as_str() {
            "archive_subscription" => {
                // No rollback: unarchiving is manual
            }
            "suspend_entitlements" => {
                entries.push(restore_entitlements(&context).await?);
            }
            "issue_refund" => {
                entries.push(reverse_refund(&context).await?);
            }
            "calculate_refund" => {
                // No-op: pure calculation
            }
            _ => {}
        }
    }

    Ok(entries)
}
```

### Pattern #3: Circuit Breaker Pattern (Fail-Fast Compensation)

**Use Case**: Detect failure early and skip unnecessary steps

```rust
// Payment processing pipeline with circuit breaker
pub async fn process_payment_with_circuit_breaker(
    billing_gov: &mut BillingGovernor,
    event: PaymentEvent,
) -> Result<PaymentResult> {
    // 1. Check circuit breaker status
    if billing_gov.payment_circuit_breaker.is_open() {
        // Payment processor unavailable, fail fast
        return Err(BillingError::PaymentProcessorUnavailable);
    }

    // 2. Attempt payment
    match billing_gov.charge_payment(&event) {
        Ok(result) => {
            // Success, reset circuit breaker
            billing_gov.payment_circuit_breaker.record_success();
            Ok(result)
        }
        Err(e) => {
            // Failure, increment error count
            billing_gov.payment_circuit_breaker.record_failure();

            // If error count exceeds threshold, open circuit breaker
            if billing_gov.payment_circuit_breaker.error_count() > 10 {
                billing_gov.payment_circuit_breaker.open();

                // Cascading compensation: Don't attempt further charges
                return Err(BillingError::PaymentProcessorCircuitOpen);
            }

            Err(e)
        }
    }
}
```

---

## Timeout & Escalation Patterns

### Pattern #1: Automatic Escalation Chain

**Use Case**: Timeout → escalate to manager → escalate to director

```rust
// Entitlement stuck in pending_approval
impl EntitlementGovernor {
    pub fn check_escalation_deadlines(&mut self) -> Result<Vec<EscalationAction>> {
        let mut actions = Vec::new();

        if self.state != EntitlementState::PendingApproval {
            return Ok(actions);
        }

        let now = Utc::now();
        let elapsed = now - self.created_at;

        // Level 1: 24h timeout → escalate to manager
        if elapsed >= Duration::hours(24) && !self.level1_escalated {
            actions.push(EscalationAction {
                level: 1,
                escalate_to: "manager_review_queue".to_string(),
                deadline: now + Duration::hours(4),
                action: "Manual review of pending approval".to_string(),
            });
            self.level1_escalated = true;
        }

        // Level 2: 48h timeout → escalate to director
        if elapsed >= Duration::hours(48) && !self.level2_escalated {
            actions.push(EscalationAction {
                level: 2,
                escalate_to: "director_review_queue".to_string(),
                deadline: now + Duration::hours(1),
                action: "Escalated: Director review required".to_string(),
            });
            self.level2_escalated = true;
        }

        // Level 3: 72h timeout → auto-approve
        if elapsed >= Duration::hours(72) {
            self.transition(EntitlementEvent::AutoApproveAfterTimeout {
                reason: "72h escalation timeout".to_string(),
            })?;

            actions.push(EscalationAction {
                level: 3,
                escalate_to: "system_auto_action".to_string(),
                deadline: now,
                action: "Auto-approved after 72h timeout".to_string(),
            });
        }

        Ok(actions)
    }
}
```

### Pattern #2: Progressive Timeout with Backoff

**Use Case**: Billing payment retries with increasing timeouts

```rust
// Payment retry with progressive timeouts
impl BillingGovernor {
    pub fn transition_to_retry(&mut self, retry_num: u32) -> Result<()> {
        let timeout = match retry_num {
            1 => Duration::hours(1),      // First retry: 1 hour
            2 => Duration::hours(3),      // Second retry: 3 hours
            3 => Duration::days(1),       // Third retry: 1 day
            4 => Duration::days(7),       // Fourth retry: 7 days (collections)
            _ => return Err(BillingError::MaxRetriesExceeded),
        };

        let deadline = Utc::now() + timeout;

        self.state = match retry_num {
            1 => BillingState::Retry1,
            2 => BillingState::Retry2,
            3 => BillingState::Retry3,
            4 => BillingState::CollectionAgency,
            _ => unreachable!(),
        };

        self.retry_deadline = Some(deadline);

        Ok(())
    }
}
```

### Pattern #3: Deadline-Driven Escalation

**Use Case**: Multiple deadlines in parallel (payment retry AND approval review)

```rust
// Track multiple independent deadlines
pub struct DeadlineTracker {
    deadlines: Vec<Deadline>,
}

pub struct Deadline {
    id: String,
    name: String,
    governor: GovernorType,
    deadline: DateTime<Utc>,
    escalation_level: u32,
    action: String,
}

impl DeadlineTracker {
    pub async fn check_and_escalate(&mut self) -> Result<Vec<EscalationAction>> {
        let now = Utc::now();
        let mut actions = Vec::new();

        for deadline in &mut self.deadlines {
            if now >= deadline.deadline && deadline.escalation_level == 0 {
                // Deadline reached, escalate
                actions.push(EscalationAction {
                    id: deadline.id.clone(),
                    deadline_name: deadline.name.clone(),
                    level: 1,
                    action: deadline.action.clone(),
                });

                deadline.escalation_level = 1;
                deadline.deadline = now + Duration::hours(24);  // New deadline for next escalation
            }
        }

        Ok(actions)
    }
}
```

---

## Error Recovery Patterns

### Pattern #1: Retry with Exponential Backoff

**Use Case**: Transient failures that might succeed if retried

```rust
// Exponential backoff with jitter (deterministic)
pub fn compute_backoff_delay(attempt: u32, max_delay: Duration) -> Duration {
    // Base delay: 100ms * 2^attempt
    let base_ms = 100 * 2_u64.pow(attempt.min(10));  // Cap exponent at 10

    // Deterministic "jitter": include attempt number for variation
    // Same attempt always gets same delay
    let jitter_ms = (attempt as u64 * 17) % 100;  // Deterministic based on attempt
    let total_ms = base_ms + jitter_ms;

    let delay = Duration::from_millis(total_ms);

    // Cap at max_delay
    delay.min(max_delay)
}

#[test]
fn test_backoff_is_deterministic() {
    let max = Duration::from_secs(10);

    // Same attempt number always produces same delay
    assert_eq!(compute_backoff_delay(0, max), compute_backoff_delay(0, max));
    assert_eq!(compute_backoff_delay(1, max), compute_backoff_delay(1, max));
    assert_eq!(compute_backoff_delay(5, max), compute_backoff_delay(5, max));

    // Delays increase exponentially
    assert!(compute_backoff_delay(0, max) < compute_backoff_delay(1, max));
    assert!(compute_backoff_delay(1, max) < compute_backoff_delay(2, max));
}

// Usage in BillingGovernor
impl BillingGovernor {
    pub async fn retry_payment_with_backoff(
        &mut self,
        payment: PaymentEvent,
        max_attempts: u32,
    ) -> Result<PaymentResult> {
        let mut attempt = 0;

        loop {
            attempt += 1;

            match self.charge_payment(&payment) {
                Ok(result) => return Ok(result),
                Err(e) if attempt < max_attempts && e.is_transient() => {
                    let delay = compute_backoff_delay(attempt, Duration::from_secs(60));
                    tokio::time::sleep(delay).await;
                    // Retry
                }
                Err(e) => return Err(e),
            }
        }
    }
}
```

### Pattern #2: Fallback Routing

**Use Case**: Primary service fails, route to backup

```rust
// Payment processor failure? Use backup processor
pub async fn charge_with_fallback(
    event: &PaymentEvent,
    primary_processor: &PaymentProcessor,
    backup_processor: &PaymentProcessor,
) -> Result<PaymentResult> {
    // Try primary
    match primary_processor.charge(event).await {
        Ok(result) => {
            return Ok(result);
        }
        Err(e) if e.is_unavailable() => {
            // Primary unavailable, try backup
            backup_processor.charge(event).await
        }
        Err(e) => Err(e),
    }
}
```

### Pattern #3: Bulkhead Pattern (Isolation)

**Use Case**: Prevent one tenant's failure from affecting others

```rust
// Each tenant has isolated resource bucket (bulkhead)
pub struct TenantBulkhead {
    tenant_id: String,
    max_concurrent_requests: u32,
    current_requests: AtomicU32,
    circuit_breaker: CircuitBreaker,
}

impl TenantBulkhead {
    pub async fn execute<F, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce() -> BoxFuture<'static, Result<T>>,
    {
        // Check circuit breaker
        if self.circuit_breaker.is_open() {
            return Err(Error::CircuitBreakerOpen);
        }

        // Check concurrent request limit
        let current = self.current_requests.fetch_add(1, Ordering::SeqCst);
        if current >= self.max_concurrent_requests {
            self.current_requests.fetch_sub(1, Ordering::SeqCst);
            return Err(Error::QuotaExceeded);
        }

        // Execute in isolation
        let result = f().await;

        // Update circuit breaker based on result
        match &result {
            Ok(_) => self.circuit_breaker.record_success(),
            Err(_) => self.circuit_breaker.record_failure(),
        }

        // Decrement request count
        self.current_requests.fetch_sub(1, Ordering::SeqCst);

        result
    }
}
```

---

## Data Consistency Patterns

### Pattern #1: Optimistic Concurrency Control

**Use Case**: Multiple concurrent updates to same resource

```rust
// Product catalog update with version number
pub struct Product {
    id: String,
    name: String,
    price: f64,
    version: u32,  // ← Version number for optimistic locking
}

impl ProductCatalogGovernor {
    pub fn update_pricing(
        &mut self,
        product_id: &str,
        new_price: f64,
        expected_version: u32,
    ) -> Result<()> {
        let product = self.products.get_mut(product_id)
            .ok_or(ProductError::NotFound)?;

        // Check version matches (no concurrent update)
        if product.version != expected_version {
            return Err(ProductError::ConcurrentUpdateDetected {
                expected: expected_version,
                actual: product.version,
            });
        }

        // Update and increment version
        product.price = new_price;
        product.version += 1;

        Ok(())
    }
}

// Usage
let result = governor.update_pricing("prod_123", 499.0, 5);

// If version mismatch:
// → Err(ConcurrentUpdateDetected { expected: 5, actual: 6 })
// → Caller must retry with correct version
```

### Pattern #2: Event Sourcing for Immutability

**Use Case**: Append-only event log prevents data corruption

```rust
// All changes go through append-only event log
pub struct EventLog {
    events: Vec<AuditEntry>,  // Immutable append-only
}

impl EventLog {
    pub fn append(&mut self, entry: AuditEntry) -> Result<()> {
        // Entry includes hash of previous entry (blockchain-like)
        if !entry.previous_hash.is_empty() {
            let last_hash = sha256(&self.events.last().unwrap().serialize());
            if entry.previous_hash != last_hash {
                return Err(Error::EventChainBroken);
            }
        }

        self.events.push(entry);
        Ok(())
    }

    pub fn verify_integrity(&self) -> Result<()> {
        for i in 1..self.events.len() {
            let prev_hash = sha256(&self.events[i-1].serialize());
            if self.events[i].previous_hash != prev_hash {
                return Err(Error::LogIntegrityViolation { at_index: i });
            }
        }
        Ok(())
    }
}
```

### Pattern #3: Snapshot + Delta for Performance

**Use Case**: Immutability (event log) + performance (snapshots)

```rust
// Snapshot state at intervals, reconstruct from delta if needed
pub struct StateWithSnapshot {
    current_state: SubscriptionState,
    snapshot: Option<SubscriptionSnapshot>,
    snapshot_version: u32,
    delta_events: Vec<AuditEntry>,  // Events since last snapshot
}

impl StateWithSnapshot {
    pub fn apply_event(&mut self, entry: AuditEntry) -> Result<()> {
        // Apply to current state
        self.apply_state_change(&entry)?;

        // Record event in delta
        self.delta_events.push(entry);

        // Create snapshot every 100 events
        if self.delta_events.len() > 100 {
            self.take_snapshot()?;
        }

        Ok(())
    }

    pub fn take_snapshot(&mut self) -> Result<()> {
        self.snapshot = Some(SubscriptionSnapshot {
            state: self.current_state.clone(),
            version: self.snapshot_version,
            timestamp: Utc::now(),
        });
        self.snapshot_version += 1;
        self.delta_events.clear();

        Ok(())
    }

    pub fn reconstruct_state(&self) -> SubscriptionState {
        // Start from snapshot
        let mut state = self.snapshot.as_ref()
            .map(|s| s.state.clone())
            .unwrap_or_default();

        // Apply delta events
        for entry in &self.delta_events {
            let _ = Self::apply_change(&mut state, entry);
        }

        state
    }
}
```

---

## Performance Patterns

### Pattern #1: Async/Await for Concurrency

**Use Case**: Non-blocking I/O, handle many governors concurrently

```rust
// Process multiple events concurrently
pub async fn handle_events_concurrently(
    orchestrator: &mut Orchestrator,
    events: Vec<MarketplaceEvent>,
) -> Result<Vec<Vec<AuditEntry>>> {
    // Spawn async task for each event
    let futures: Vec<_> = events
        .into_iter()
        .map(|event| {
            let orch = orchestrator.clone();
            async move {
                orch.handle_event(event).await
            }
        })
        .collect();

    // Wait for all concurrently
    futures::future::try_join_all(futures).await
}
```

### Pattern #2: Caching with TTL

**Use Case**: Avoid redundant computation (deterministic-friendly)

```rust
// Product catalog cache with TTL
pub struct CachedProductCatalog {
    cache: HashMap<String, (Product, Instant)>,
    ttl: Duration,
}

impl CachedProductCatalog {
    pub fn get(&self, product_id: &str) -> Option<Product> {
        if let Some((product, cached_at)) = self.cache.get(product_id) {
            if cached_at.elapsed() < self.ttl {
                return Some(product.clone());
            }
        }
        None
    }

    pub fn insert(&mut self, product_id: String, product: Product) {
        self.cache.insert(product_id, (product, Instant::now()));
    }
}
```

### Pattern #3: Batch Processing

**Use Case**: Amortize costs over multiple operations

```rust
// Batch billing cycle processing
pub async fn process_billing_batch(
    orchestrator: &mut Orchestrator,
    customers: Vec<String>,
    batch_size: usize,
) -> Result<Vec<AuditEntry>> {
    let mut entries = Vec::new();

    for chunk in customers.chunks(batch_size) {
        let batch_entries = orchestrator
            .process_billing_cycle_batch(chunk.to_vec())
            .await?;

        entries.extend(batch_entries);
    }

    Ok(entries)
}
```

---

**Last Updated**: January 2026
**Next Review**: April 2026
**Owner**: @marketplace-architecture
