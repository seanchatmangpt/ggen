# Marketplace FSM Guide - Complete Governor Reference

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Developer Guide
**Audience**: Rust Developers, Architecture, Product

---

## Table of Contents

1. [FSM Overview](#fsm-overview)
2. [Governor #1: Entitlement](#governor-1-entitlement-fsm)
3. [Governor #2: Billing](#governor-2-billing-fsm)
4. [Governor #3: Product Catalog](#governor-3-product-catalog-fsm)
5. [Governor #4: Subscription](#governor-4-subscription-fsm)
6. [Governor #5: Customer Account](#governor-5-customer-account-fsm)
7. [Governor #6: Quota & SLA](#governor-6-quota--sla-fsm)
8. [Governor #7: Compliance & Audit](#governor-7-compliance--audit-fsm)
9. [Governor #8: Multi-Tenant](#governor-8-multi-tenant-fsm)
10. [Integration Examples](#integration-examples)

---

## FSM Overview

### Core Principles

1. **Type Safety**: All states and transitions are compile-time enforced
2. **Determinism**: Same input → same output, always (no randomness)
3. **Audit Trail**: Every transition generates cryptographic proof
4. **Timeout Escalation**: Automatic escalation when deadlines pass
5. **Idempotence**: Same event processed twice → same result
6. **Graceful Degradation**: System degrades features before full outage

### State Machine Template

```rust
pub enum GovernorState {
    // States define what the system can do
    State1,
    State2,
    State3,
}

pub enum GovernorEvent {
    // Events trigger transitions
    Event1,
    Event2,
    Event3,
}

impl GovernorFSM {
    pub fn transition(&mut self, event: GovernorEvent) -> Result<()> {
        // Type-safe transition logic
        match (self.state, event) {
            (State1, Event1) => { self.state = State2; Ok(()) },
            (State1, Event2) => Err("Invalid transition"),
            _ => Err("Unhandled state/event combination"),
        }
    }
}
```

---

## Governor #1: Entitlement FSM

**Purpose**: Manage customer entitlements (feature access, subscription period)
**Location**: `src/marketplace/entitlement_governor.rs`
**States**: 8
**Transitions**: 12
**Primary Timeline**: 0-72 hours
**Critical Feature**: Feature access control gates

### State Diagram

```
                    ┌──────────────────────────────────┐
                    │     PENDING_APPROVAL             │
                    │  (Awaiting marketplace approval) │
                    └──────────┬───────────────────────┘
                               │
                               │ [24h timeout]
                               │
                ┌──────────────▼──────────────┐
                │         ACTIVE             │
                │ (Customer can use product) │
                └────┬────────────────────┬──┘
                     │                    │
             suspend │                    │ payment_failed (3x)
                     │                    │
              ┌──────▼─────────────────┐  │
              │    SUSPENDED           │  │
              │ (Temp disable/payment) │  │
              └──────┬──────────────┬──┘  │
                     │              │     │
                [72h]│          reinstate  │
               timeout│              │     │
                     │              │ ┌───▼──────────┐
                     │              │ │ REFUND_ISSUED│
                     │              │ └───────────────┘
              ┌──────▼──────┐       │
              │   EXPIRED   │◄──────┘
              │  (Terminal) │
              └─────────────┘

Also reachable:
CANCELLED ──(manual)──> REFUND_ISSUED
ARCHIVED ──(manual)──> Any state
```

### States Explained

| State | Entry Condition | Key Properties | Exit Paths | Timeout |
|-------|-----------------|-----------------|-----------|---------|
| **pending_approval** | New entitlement created | Read-only, locked | active, archived, cancelled | 24h → active |
| **active** | Approval granted | Feature access enabled | suspended, expired, cancelled | None (duration-based) |
| **suspended** | Payment failure or TOS violation | Temporary disable | active (reinstate), expired, refund_issued | 72h → expired |
| **expired** | Entitlement period ends | No access possible | archived (manual) | ∞ (terminal) |
| **cancelled** | Manual cancellation | Pending refund processing | refund_issued | None |
| **refund_issued** | Refund approved/timeout | Final state with proof | None (terminal) | ∞ (terminal) |
| **archived** | Retention policy | Cold storage (read-only) | None (terminal) | ∞ (terminal) |

### Transition Rules

```
pending_approval:
  approve(actor=system, deadline_check=true) → active
    Guard: actor is authorization system
    Guard: 24h deadline not exceeded
    Action: Log approval event
    Action: Emit EntitlementActivated event

  deny(reason) → archived
    Guard: reason is valid (TOS violation, fraud, etc)
    Action: Log denial event
    Action: Emit RefundRequired event (if payment received)

  cancel → cancelled
    Guard: Customer requested cancellation
    Action: Emit CancellationRequested event

  timeout (24h) → active [AUTO-ESCALATE]
    Guard: Current time >= created_at + 24h
    Action: Escalate to manager review
    Action: Emit ApprovalEscalated event

active:
  suspend(reason, duration=72h) → suspended
    Guard: reason in [payment_failed, tos_violation, fraud]
    Action: Revoke feature access
    Action: Emit SuspensionStarted event
    Action: Schedule auto-reinstate in 72h

  expire → expired
    Guard: entitlement period ended
    Guard: Current time >= expiration_date
    Action: Revoke feature access
    Action: Emit EntitlementExpired event

  cancel → cancelled
    Guard: Customer request
    Action: Process cancellation

suspended:
  reinstate → active
    Guard: 72h timeout not yet reached
    Action: Restore feature access
    Action: Emit SuspensionLifted event

  timeout (72h) → expired [AUTO-ESCALATE]
    Guard: 72h suspension period exceeded
    Action: Auto-refund issued
    Action: Emit AutoRefundIssued event
    Action: Transition → refund_issued (skips expired)

cancelled/refund_issued/expired/archived:
  (Terminal states, no outbound transitions)
```

### Integration Examples

#### Example 1: New Customer Subscription

```rust
// 1. Create entitlement in pending_approval state
let event = EntitlementEvent::CreateNew {
    customer_id: "cust_abc123",
    product_id: "prod_xyz789",
    billing_cycle: BillingCycle::Monthly,
    start_date: Utc::now(),
};

// 2. Orchestrator routes to EntitlementGovernor
let response = orchestrator.handle_event(event).await?;
// → State: pending_approval
// → Duration: 24h TTL

// 3. After approval from compliance
let approval = EntitlementEvent::Approve {
    actor: "system:approval-service",
    timestamp: Utc::now(),
};
gov.transition(approval)?;
// → State: active
// → Feature access enabled
// → Emit: EntitlementActivated

// 4. Orchestrator broadcasts EntitlementActivated
// → ProductCatalogGovernor: Enable feature flags
// → QuotaSlaGovernor: Allocate initial quota
// → ComplianceGovernor: Update data residency
// → BillingGovernor: Start billing cycle
```

#### Example 2: Payment Failure Suspension

```rust
// From BillingGovernor's PaymentFailed event
let event = EntitlementEvent::Suspend {
    reason: SuspensionReason::PaymentFailed,
    auto_reinstate_deadline: Utc::now() + Duration::hours(72),
};

gov.transition(event)?;
// → State: suspended
// → Feature access revoked
// → Schedule reinstate in 72h
// → Emit: SuspensionStarted

// If no reinstatement within 72h
orchestrator.check_timeouts()?;
// → Auto-transition: suspended → expired
// → Auto-issue refund
// → Emit: AutoRefundIssued
```

#### Example 3: Idempotent Reactivation

```rust
// Same reinstate event processed twice
let reinstate = EntitlementEvent::Reinstate {
    idempotency_key: "idempotency_key_unique_123",
    timestamp: Utc::now(),
};

// First call
gov.transition(reinstate.clone())?;
// → State: active
// → Emit: SuspensionLifted
// → Cache idempotency_key

// Second call (same idempotency_key)
match gov.transition(reinstate) {
    Ok(_) => unreachable!(), // Should not succeed
    Err(EntitlementError::IdempotencyViolation { .. }) => {
        // Expected: return cached result
        // → State: active (unchanged)
        // → Emit: nothing (already emitted)
    }
}
```

---

## Governor #2: Billing FSM

**Purpose**: Payment processing, invoicing, revenue recognition
**Location**: `src/marketplace/billing_governor.rs`
**States**: 10
**Transitions**: 20
**Primary Timeline**: 7-30 days
**Critical Feature**: Revenue recognition, tax compliance

### State Diagram

```
awaiting_invoice
  │
  │ invoice_generation_time_reached
  ├─────────────────────────────────────┐
  │                                      │
  ▼                                      ▼
invoice_issued                    payment_pending
  │                                      │
  │ customer_received                    │ [payment_received ──→ payment_received]
  │                                      │ [payment_disputed ──→ payment_disputed]
  └─────────────┬───────────────────────┤ [timeout 7d ──→ payment_failed]
                │                       │
                └──────────────┬────────┘
                               │
                    ┌──────────▼────────────┐
                    │  payment_received     │
                    │ (Revenue recognized)  │
                    └──────────┬────────────┘
                               │
                               │ accounting_reconciled
                               │
                    ┌──────────▼────────────┐
                    │     archived          │
                    │   (Terminal: $$$)     │
                    └───────────────────────┘

Failure paths:

payment_failed
  │ [timeout 2d] ──────────────────→ retry_1
  │ [retry_approved] ──────────────→ retry_1
  │ [escalate_to_admin] ───────────→ payment_disputed

retry_1 (1d TTL, backup payment method)
  │ [payment_received] ────────────→ payment_received
  │ [payment_failed | timeout 1d]──→ retry_2

retry_2 (3d TTL, secondary backup method)
  │ [payment_received] ────────────→ payment_received
  │ [payment_failed | timeout 3d]──→ retry_3

retry_3 (7d TTL, last attempt)
  │ [payment_received] ────────────→ payment_received
  │ [payment_failed | timeout 7d]──→ collection_agency

collection_agency
  │ [payment_recovered] ───────────→ archived
  │ [account_written_off] ─────────→ archived
  │ [settlement_agreed] ───────────→ archived

payment_disputed
  │ [dispute_resolved] ────────────→ archived
  │ [refund_issued] ───────────────→ archived
```

### Key Invariants

```rust
// Total processing timeline
max_cycle_days = 7 (payment_pending) + 2 (retry prep) + 1 + 3 + 7 (retries) = 20 days

// Idempotency: Payment amount must match
payment_received:
  amount == invoice.total OR return PaymentAmountMismatch

// Tax compliance
invoice.total = base_amount + tax_calculated
  where tax_calculated uses GCP tax engine

// No duplicate payment
if event.idempotency_key in cache:
  return IdempotencyViolation
cache.insert(event.idempotency_key, payment_id)

// Revenue recognition
PaymentReceived state signals: Safe to record revenue in accounting
```

### Transition Rules

```
awaiting_invoice → invoice_issued
  Guard: Current time >= invoice_generation_time
  Guard: Invoice template valid
  Guard: GCP invoice generation API available
  Action: Generate invoice (PDF, email)
  Action: Log invoice_id to receipt ledger
  Timeout: None (event-driven by billing cycle)

invoice_issued → payment_pending
  Guard: Customer acknowledged invoice
  Timeout: 7 days
  Action: Begin payment window
  Emit: InvoiceDelivered

payment_pending → payment_received
  Guard: payment_amount == invoice.total (idempotency)
  Guard: Payment processor confirmed
  Guard: Idempotency check passes
  Action: Log payment in receipt ledger
  Action: Trigger revenue recognition
  Emit: PaymentReceived
  Note: Skip retry states entirely

payment_pending → payment_failed
  Guard: Payment processor declined
  Timeout: 7 days
  Action: Log failure reason (code, message)
  Emit: PaymentFailed

payment_failed → retry_1
  Guard: Retry allowed (attempt < 3)
  Guard: Alternative payment method exists
  Timeout: 2 days (auto-retry if no manual action)
  Action: Charge backup payment method
  Action: Log retry_1 attempt
  Emit: Retry1Started

retry_1 → retry_2 [similar pattern]
retry_2 → retry_3 [similar pattern]

retry_3 → collection_agency
  Guard: All retries exhausted
  Guard: No manual override
  Timeout: 7 days (escalate to collections)
  Action: Create collections ticket
  Action: Notify customer of delinquency
  Emit: CollectionsEscalated

payment_received → archived
  Guard: payment_received state stable for 1 day
  Guard: accounting_reconciled event received
  Action: Archive invoice + payment record
  Action: Generate final receipt (cryptographic proof)
  Emit: PaymentArchived (terminal)

payment_disputed → archived
  Guard: dispute resolution (refund/chargeback) complete
  Action: Update customer account balance
  Action: Generate dispute resolution record
  Emit: DisputeResolved (terminal)
```

### Integration Examples

#### Example 1: Monthly Billing Cycle

```rust
// BillingGovernor coordinates monthly invoice generation
let event = BillingEvent::CycleBegin {
    billing_month: "2026-01",
    subscriptions: vec![
        Subscription { customer_id: "cust1", amount: 99.0, currency: "USD" },
        Subscription { customer_id: "cust2", amount: 199.0, currency: "USD" },
    ],
};

gov.transition(event)?;
// → State: awaiting_invoice
// → Timeline: Invoice generation scheduled

// Orchestrator triggers invoice generation
let invoice_event = BillingEvent::InvoiceGenerationTimeReached {
    billing_month: "2026-01",
    invoice_count: 2,
};

gov.transition(invoice_event)?;
// → State: invoice_issued
// → Action: PDF generation + email delivery
// → Emit: InvoiceDelivered

// Broadcast to other governors
// → CustomerAccountGovernor: Update invoice status
// → ComplianceGovernor: Log for SOX compliance
```

#### Example 2: Payment Retry Logic

```rust
// Customer's first payment attempt fails
let payment_fail = BillingEvent::PaymentFailed {
    invoice_id: "inv_123",
    failure_code: "card_declined",
    payment_method: "visa_ending_4242",
};

gov.transition(payment_fail)?;
// → State: payment_failed
// → Timeout: 2 days
// → Emit: PaymentFailed

// Orchestrator waits 2 days, then auto-triggers retry
let auto_retry = BillingEvent::RetryApproved {
    invoice_id: "inv_123",
    retry_number: 1,
    payment_method_fallback: "amex_ending_5555",
};

gov.transition(auto_retry)?;
// → State: retry_1
// → Action: Charge backup payment method
// → Timeout: 1 day

// On success
let payment_success = BillingEvent::PaymentReceived {
    invoice_id: "inv_123",
    idempotency_key: "idempotency_xyz",
    amount: 99.00,
    timestamp: Utc::now(),
};

gov.transition(payment_success)?;
// → State: payment_received
// → Emit: PaymentReceived
// → Broadcast: Revenue recognition to accounting
```

#### Example 3: Idempotent Payment Processing

```rust
// Same payment confirmation arrives twice
let payment = BillingEvent::PaymentReceived {
    invoice_id: "inv_123",
    idempotency_key: "pay_idem_abc123",  // Same key
    amount: 99.00,
};

// First attempt
gov.transition(payment.clone())?;
// → State: payment_received
// → Cache: idempotency_key stored
// → Emit: PaymentReceived

// Second attempt (webhook retry)
match gov.transition(payment) {
    Ok(_) => panic!("Should have detected duplicate"),
    Err(BillingGovernorError::IdempotencyViolation { .. }) => {
        // Expected: return cached result
        // State unchanged, no duplicate charge
        // Return original PaymentReceived event
    }
}
```

---

## Governor #3: Product Catalog FSM

**Purpose**: SKU lifecycle, pricing management, feature availability
**Location**: `src/marketplace/product_catalog_governor.rs`
**States**: 7
**Transitions**: 14
**Primary Timeline**: Days to months
**Critical Feature**: Schema validation, feature availability

### State Diagram

```
draft ──submit_for_review──> validation ──success──> update_approved
                                  │
                                  └─ schema_error ──> draft

update_approved ──propagate_successful──> published

published ──────┬──── move_to_featured ──> featured
                │
                ├─ update_pricing ──> validation
                │
                ├─ update_features ──> validation
                │
                └─ deprecate ──────────> deprecated

featured ┌─────┬─────────────────────────┐
         │     │                         │
         │     └─ demote_from_featured──> published
         │
         └─ end_promotion ──────────────> published

deprecated ──continue_support──────────> (no state change)
           ├─ force_archive ──────────→ archived
           └─ resurrect_to_published──> published

archived (terminal, read-only)
```

### States Explained

| State | Purpose | Key Properties | Constraints | Duration |
|-------|---------|-----------------|------------|----------|
| **draft** | Creation & editing | Mutable, not visible to customers | Never purchased | indefinite |
| **validation** | Schema & compliance check | Read-only during validation | Version locked | <5 min |
| **update_approved** | Ready for rollout | Features propagating | Cache invalidation in progress | <1 min |
| **published** | Generally available | Feature gates enabled, visible | Must pass schema | indefinite |
| **featured** | Marketing promotion | High visibility, featured pricing | SLO tracking | days/weeks |
| **deprecated** | Sunset phase | Available but discouraged | Replacement SKU optional | weeks/months |
| **archived** | Historical only | Read-only, immutable | No access changes | ∞ (permanent) |

### Transition Rules

```
draft → validation
  Guard: SubmitForReview event from product owner
  Guard: Required fields populated (name, price, features)
  Action: Lock version
  Action: Trigger SHACL schema validation
  Emit: ValidationStarted

validation → update_approved
  Guard: Schema validation successful
  Guard: All references valid (feature IDs exist)
  Guard: Pricing consistent (no contradictions)
  Action: Unlock version
  Action: Queue propagation to cache/CDN
  Timeout: 5 min (auto-fail if validation hangs)
  Emit: ValidationSucceeded

validation → draft
  Guard: Schema validation failed
  Guard: ValidationError detail available
  Action: Return version to mutable state
  Emit: ValidationFailed

update_approved → published
  Guard: Cache propagation completed
  Guard: Feature flags deployed
  Guard: Pricing in effect
  Action: Make visible in product catalog
  Action: Emit: ProductPublished
  Emit: FeatureFlagsEnabled

published → featured
  Guard: MoveToFeatured event from marketing
  Guard: Featured SLA targets confirmed
  Guard: Pricing valid for featured tier
  Action: Enable featured badge
  Action: Trigger featured_pricing pricing model
  Emit: MovedToFeatured
  Timeout: None (duration-driven, KPI checks)

featured → published
  Guard: DemoteFromFeatured event (poor KPIs or manual)
  Guard: EndPromotion event after deadline
  Action: Remove featured badge
  Action: Revert to regular pricing
  Emit: DemotedFromFeatured

published → deprecated
  Guard: Deprecate event from product owner
  Guard: Optional: Replacement SKU provided
  Action: Mark as deprecated in catalog
  Action: Begin sunset notifications to customers
  Action: Disable new purchases (existing allowed)
  Emit: ProductDeprecated

deprecated → archived
  Guard: ForceArchive event after sunset period
  Guard: All customers migrated/refunded
  Action: Move to read-only archive
  Action: Disable all access
  Emit: ProductArchived (terminal)

deprecated → published
  Guard: ResurrectToPublished event (reverse decision)
  Guard: No later replacement product active
  Action: Restore to published state
  Action: Re-enable new purchases
  Emit: ProductResurrected
```

### Integration Examples

#### Example 1: New Product Launch

```rust
// Product owner creates SKU
let event = ProductEvent::CreateDraft {
    sku_id: "prod_new_2026",
    name: "Enterprise Plus",
    base_price: 2999.0,
    features: vec![
        Feature { id: "feat_sso", name: "SSO" },
        Feature { id: "feat_audit_log", name: "Audit Logs" },
    ],
};

gov.transition(event)?;
// → State: draft
// → Properties: Mutable, hidden from customers

// After review, submit for validation
let submit = ProductEvent::SubmitForReview {
    sku_id: "prod_new_2026",
    actor: "product_owner@company.com",
};

gov.transition(submit)?;
// → State: validation
// → Action: SHACL schema check, feature reference check, pricing check

// Validation succeeds
let validated = ProductEvent::ValidationSucceeded {
    sku_id: "prod_new_2026",
};

gov.transition(validated)?;
// → State: update_approved
// → Action: Queue for cache propagation

// Propagation completes
let propagated = ProductEvent::PropagationSucceeded {
    sku_id: "prod_new_2026",
    deployed_regions: vec!["us-central1", "europe-west1"],
};

gov.transition(propagated)?;
// → State: published
// → Emit: ProductPublished
// → Broadcast to QuotaSlaGovernor, EntitlementGovernor
```

#### Example 2: Price Update

```rust
// Update existing product pricing
let event = ProductEvent::UpdatePricing {
    sku_id: "prod_standard",
    new_price: 499.0,
    effective_date: Utc::now() + Duration::days(7),
};

// From published state, triggers validation
gov.transition(event)?;
// → State: validation
// → Action: Validate new price is consistent
// → Timeout: 5 min

// After validation succeeds
let propagated = ProductEvent::PropagationSucceeded {
    sku_id: "prod_standard",
};

gov.transition(propagated)?;
// → State: published (price updated)
// → Emit: PricingUpdated
// → Broadcast: New pricing effective
```

#### Example 3: Marketing Campaign

```rust
// Promote to featured for Q1 campaign
let event = ProductEvent::MoveToFeatured {
    sku_id: "prod_standard",
    featured_price: 399.0,  // Campaign discount
    campaign_id: "campaign_q1_2026",
    deadline: Utc::now() + Duration::days(90),
};

gov.transition(event)?;
// → State: featured
// → Action: Enable featured badge, featured pricing

// KPI checks trigger automatic demotion
let kpi_check = ProductEvent::KPICheckTriggered {
    sku_id: "prod_standard",
    healthy: false,  // Low conversion
};

gov.transition(kpi_check)?;
// → State: validation (recheck pricing/features)
// → Or: published (automatic demotion)
```

---

## Governor #4: Subscription FSM

**Purpose**: Customer lifecycle management (trial → active → renewal → upgrade → cancel)
**Location**: `src/marketplace/subscription_governor.rs`
**States**: 9
**Transitions**: 20
**Primary Timeline**: Days to years
**Critical Feature**: Proration, churn prevention

### State Diagram

```
trial ──┬──► active      [auto-upgrade at deadline, or manual convert_to_paid]
        │
        └──► cancelled   [manual before expiry]

active ──┬──► renewal_pending  [30 days before renewal date]
         │
         ├──► upgraded          [manual tier upgrade]
         │
         ├──► downgraded        [manual tier downgrade]
         │
         ├──► paused            [manual pause (max 3 months)]
         │
         ├──► cancelled         [manual cancellation]
         │
         └──► expired           [auto after final billing period]

renewal_pending ──┬──► active          [payment received]
                  │
                  └──► renewal_failed  [payment failed, enter retry logic]

renewal_failed ──┬──► active          [payment received after retry]
                 │
                 └──► expired         [retries exhausted, subscription ends]

upgraded ──────────► renewal_pending  [immediately, pro-rata billing]
downgraded ────────► renewal_pending  [immediately, pro-rata billing]
paused ─────────────► active          [manual unpause]

cancelled/expired (terminal states)
```

### Key Invariants

```rust
// Subscription must always have billing cycle
subscription.billing_cycle in [Monthly, Quarterly, Annual]

// Proration: Exact to-the-second calculation
proration = (remaining_days / total_days_in_cycle) * cycle_amount
  where accuracy = to nearest cent (deterministic)

// Trial-to-paid: No billing gap
trial_end_date == paid_start_date (same instant)

// Renewal: Automatic or manual, but not both
if auto_renewal_enabled:
  renewal_pending triggered 30 days before deadline
else:
  manual renewal required (customer action)

// Pause limit: Max 3 months per year
paused_days_this_year <= 90

// No double-charging: Reconciliation on tier change
if upgrade OR downgrade:
  issue refund for overage OR charge for underage (pro-rata)
```

### Transition Rules

```
trial → active
  Guard: Trial period expired (deadline reached)
  Guard: Auto-renewal enabled in settings
  Guard: Payment method valid
  Action: Charge first billing cycle
  Action: Set renewal_date = today + cycle_duration
  Emit: TrialConvertedToPaid
  Emit: BillingCycleStarted

trial → cancelled
  Guard: Customer cancels before deadline
  Action: No charge applied
  Emit: TrialCancelled

active → renewal_pending
  Guard: Current time >= renewal_date - 30 days
  Guard: Auto-renewal enabled
  Action: Begin renewal payment processing
  Timeout: 30 days (reminder window)
  Emit: RenewalPending

active → upgraded
  Guard: Customer requests tier upgrade
  Guard: Upgrade valid (higher tier)
  Action: Calculate proration (credit/charge)
  Action: Issue credit or charge immediately
  Action: Set new tier immediately
  Emit: SubscriptionUpgraded
  Emit: ProrationApplied

active → downgraded
  Guard: Customer requests tier downgrade
  Guard: Downgrade valid (lower tier)
  Action: Calculate proration (refund)
  Action: Issue refund immediately
  Action: Set new tier at next renewal
  Emit: SubscriptionDowngraded
  Emit: RefundIssued

active → paused
  Guard: Customer requests pause
  Guard: paused_days_this_year < 90
  Guard: Pause duration <= 90 days
  Action: Suspend feature access (non-destructive)
  Action: Track pause start date
  Emit: SubscriptionPaused

paused → active
  Guard: Customer unpauses OR pause duration expires
  Guard: Pause < 90 days total this year
  Action: Restore feature access
  Action: Adjust renewal date (shift by pause duration)
  Emit: SubscriptionResumed

active → cancelled
  Guard: Customer requests cancellation
  Action: Mark for cancellation
  Action: Disable auto-renewal
  Action: Set expiration_date = today + cycle_duration
  Emit: SubscriptionCancelled

renewal_pending → active
  Guard: Payment received
  Guard: Amount matches expected renewal
  Guard: Idempotency check passes
  Action: Update renewal_date = today + cycle_duration
  Emit: RenewalSucceeded
  Emit: BillingCycleRestarted

renewal_pending → renewal_failed
  Guard: Payment failed
  Action: Begin retry logic
  Emit: RenewalFailed

renewal_failed → active
  Guard: Payment received after retry
  Action: Update renewal_date
  Emit: RenewalRecovered

renewal_failed → expired
  Guard: Retries exhausted
  Action: Disable further access
  Emit: SubscriptionExpired (terminal)

active → expired
  Guard: Final renewal date passed
  Guard: All billing cycles completed
  Action: Disable all access
  Emit: SubscriptionExpired (terminal)
```

### Integration Examples

#### Example 1: Trial-to-Paid Conversion

```rust
// Customer signs up for trial
let trial_event = SubscriptionEvent::CreateTrial {
    customer_id: "cust_abc",
    tier: FeatureTier::Professional,
    trial_days: 14,
};

gov.transition(trial_event)?;
// → State: trial
// → Duration: 14 days

// After 14 days, auto-convert to paid
let convert = SubscriptionEvent::TrialExpired {
    customer_id: "cust_abc",
    auto_renew: true,
};

gov.transition(convert)?;
// → State: active
// → Charge first month: $199
// → Set renewal_date = today + 30 days
// → Emit: TrialConvertedToPaid
// → Broadcast: BillingGovernor (charge), EntitlementGovernor (enable features)
```

#### Example 2: Proration on Mid-Cycle Upgrade

```rust
// Subscription active, 10 days into 30-day cycle
// Customer upgrades from Professional ($199) to Enterprise ($499)
let upgrade = SubscriptionEvent::Upgrade {
    customer_id: "cust_abc",
    new_tier: FeatureTier::Enterprise,
    timestamp: Utc::now(),
};

gov.transition(upgrade)?;
// → State: upgraded
// → Action: Calculate proration:
//   Remaining days in cycle = 30 - 10 = 20 days
//   Professional refund = 20 / 30 * $199 = $132.67
//   Enterprise charge = 20 / 30 * $499 = $332.67
//   Net charge = $332.67 - $132.67 = $200.00
// → Action: Charge customer $200.00
// → Emit: SubscriptionUpgraded
// → Emit: ProrationApplied (with detailed breakdown)
// → Broadcast: BillingGovernor (process charge), EntitlementGovernor (enable Enterprise features)
```

#### Example 3: Renewal Retry Logic

```rust
// Subscription approaching renewal
let renewal_pending = SubscriptionEvent::RenewalPending {
    customer_id: "cust_abc",
    renewal_amount: 199.00,
};

gov.transition(renewal_pending)?;
// → State: renewal_pending
// → Broadcast: BillingGovernor (attempt payment)

// First attempt fails
let renewal_fail = SubscriptionEvent::RenewalFailed {
    customer_id: "cust_abc",
    reason: PaymentFailureReason::CardDeclined,
};

gov.transition(renewal_fail)?;
// → State: renewal_failed
// → Trigger retry logic (BillingGovernor handles)

// Retry succeeds
let renewal_success = SubscriptionEvent::RenewalSucceeded {
    customer_id: "cust_abc",
    payment_id: "pay_xyz123",
};

gov.transition(renewal_success)?;
// → State: active
// → renewal_date = today + 30 days
// → Emit: RenewalSucceeded
```

---

## Governor #5: Customer Account FSM

**Purpose**: Customer profile, payment methods, communication preferences
**Location**: `src/marketplace/customer_account_governor.rs`
**States**: 7
**Transitions**: 12
**Primary Timeline**: Months to years
**Critical Feature**: Payment method management, data consistency

### State Diagram

```
profile_active ──┬──► payment_method_updated  [update payment method]
                 │
                 ├──► communication_prefs_updated [change notification settings]
                 │
                 ├──► address_updated  [change billing/shipping address]
                 │
                 ├──► identity_verification_pending [KYC re-verification needed]
                 │
                 └──► account_locked  [security event or manual lock]

identity_verification_pending ──┬──► profile_active     [verification passed]
                                │
                                └──► account_locked    [verification failed]

account_locked ──┬──► profile_active     [manual unlock after investigation]
                 │
                 └──► account_archived   [customer requested deletion]

account_archived (terminal, read-only, data retention policy applies)

[All metadata updates are non-blocking state transitions]
```

### States Explained

| State | Purpose | Mutable | Operations Allowed | Exit Condition |
|-------|---------|---------|-------------------|-----------------|
| **profile_active** | Normal operation | Yes | All operations | Payment failure, security event, lock request |
| **payment_method_updated** | New payment method being validated | Temporary | Limited (pending confirmation) | Validation complete (success/failure) |
| **communication_prefs_updated** | Notification settings changing | Temporary | Email/SMS delivery queued | Preference propagation complete |
| **address_updated** | Billing address changing | Temporary | Limited pending validation | Validation complete |
| **identity_verification_pending** | KYC re-verification in progress | Read-only | Minimal (customer support only) | Verification result received |
| **account_locked** | Suspended due to security or fraud | Read-only | No (customer support can unlock) | Manual unlock by admin |
| **account_archived** | Deleted per GDPR/customer request | Read-only | No (historical only) | ∞ (permanent) |

### Transition Rules

```
profile_active → payment_method_updated
  Guard: UpdatePaymentMethod event
  Guard: New payment method valid (passes Stripe validation)
  Guard: Not a duplicate of existing method
  Action: Queue Stripe update
  Action: Store new method in encrypted storage
  Action: Set as primary payment method (if requested)
  Timeout: 5 min (validation timeout)
  Emit: PaymentMethodUpdated

profile_active → communication_prefs_updated
  Guard: UpdateCommunicationPreferences event
  Guard: Preferences in valid set (email, SMS, push, none)
  Action: Update preferences in database
  Action: Queue preference propagation
  Emit: CommunicationPreferencesUpdated

profile_active → identity_verification_pending
  Guard: IdentityVerificationRequired event (auto or manual)
  Guard: Verification deadline has passed (annual re-verification)
  Guard: Or: Fraud score threshold exceeded
  Action: Initiate KYC/AML verification (third-party service)
  Action: Send verification instructions to customer
  Timeout: 30 days (escalate to support)
  Emit: IdentityVerificationStarted

identity_verification_pending → profile_active
  Guard: IdentityVerificationPassed event
  Guard: KYC service confirms valid identity
  Action: Clear verification flags
  Action: Update verification timestamp
  Emit: IdentityVerificationSucceeded

identity_verification_pending → account_locked
  Guard: IdentityVerificationFailed event
  Guard: Customer failed verification 3 times
  Guard: Or: Fraud indicators detected
  Action: Lock account for security
  Action: Notify customer of lock reason
  Action: Escalate to fraud investigation team
  Emit: IdentityVerificationFailed
  Emit: AccountLocked

profile_active → account_locked
  Guard: AccountLockRequested event (manual admin action)
  Guard: Or: Security event detected (unauthorized access attempt)
  Guard: Or: Chargeback filed
  Action: Revoke all access immediately
  Action: Notify customer
  Action: Escalate to security team
  Emit: AccountLocked
  Broadcast: Entitlement (revoke access), Billing (hold collections)

account_locked → profile_active
  Guard: UnlockAccountRequested event (manual admin action)
  Guard: Investigation completed (security clearance)
  Guard: Customer confirms identity (identity re-verification passed)
  Action: Restore account access
  Action: Clear lock reason
  Action: Audit unlock action
  Emit: AccountUnlocked

account_locked → account_archived
  Guard: AccountDeletionRequested event (customer GDPR request)
  Guard: All billing cycles settled
  Guard: No pending disputes
  Action: Anonymize PII (GDPR compliance)
  Action: Archive to cold storage
  Action: Set data retention deadline
  Emit: AccountArchived (terminal)
```

### Integration Examples

#### Example 1: Payment Method Update

```rust
let update = CustomerAccountEvent::UpdatePaymentMethod {
    customer_id: "cust_abc",
    payment_method: PaymentMethod {
        card_token: "tok_stripe_xyz",
        last_4: "4242",
        expiry: "12/2027",
    },
};

gov.transition(update)?;
// → State: payment_method_updated
// → Action: Validate with Stripe
// → Timeout: 5 min

// Validation succeeds
let validated = CustomerAccountEvent::PaymentMethodValidated {
    customer_id: "cust_abc",
    payment_method_id: "pm_stripe_xyz",
};

gov.transition(validated)?;
// → State: profile_active
// → Emit: PaymentMethodUpdated
// → Broadcast: BillingGovernor (new payment method available)
```

#### Example 2: Security Lock

```rust
// Unauthorized access attempt detected
let lock = CustomerAccountEvent::AccountLocked {
    customer_id: "cust_abc",
    reason: LockReason::UnauthorizedAccessAttempt,
    ipv4: "203.0.113.42",
};

gov.transition(lock)?;
// → State: account_locked
// → Emit: AccountLocked
// → Action: Revoke all feature access
// → Broadcast:
//   - EntitlementGovernor: Suspend all entitlements
//   - BillingGovernor: Hold all collections
//   - ComplianceGovernor: Log security event

// After investigation, unlock
let unlock = CustomerAccountEvent::UnlockAccountRequested {
    customer_id: "cust_abc",
    admin_id: "admin_security_team",
};

gov.transition(unlock)?;
// → State: profile_active
// → Emit: AccountUnlocked
```

---

## Governor #6: Quota & SLA FSM

**Purpose**: Resource limits, usage tracking, throttling
**Location**: `src/marketplace/quota_sla_governor.rs`
**States**: 5
**Transitions**: 10
**Primary Timeline**: Seconds to billing cycle
**Critical Feature**: Fair-share allocation, SLO enforcement

### State Diagram

```
healthy ──┬──► warning      [usage > 80% of quota]
          │
          ├──► throttled    [usage > 95% of quota]
          │
          └──► exceeded     [usage > 100% of quota]

warning ──┬──► healthy     [usage drops below 80%]
          │
          └──► throttled   [usage increases to >95%]

throttled ──┬──► healthy    [usage drops below 80%]
            │
            └──► exceeded   [usage exceeds 100%]

exceeded ──┬──► throttled   [usage drops below 100%]
           │
           └──► healthy     [usage drops below 80%]

[All state transitions immediate, no timeouts]
```

### Key Invariants

```rust
// Fair-share allocation by customer tier
enterprise_quota = 40% * total_available_resources
professional_quota = 35% * total_available_resources
starter_quota = 25% * total_available_resources

// Each customer cannot exceed their tier quota
customer_usage <= customer.tier.quota

// SLO targets per customer tier
enterprise_slo = 99.95% uptime, <200ms latency
professional_slo = 99.9% uptime, <500ms latency
starter_slo = 99.0% uptime, <2s latency

// Throttling follows exponential backoff
throttle_delay(n) = min(2^n * 100ms, 10s)
  where n = throttle count this period
```

### Transition Rules

```
healthy → warning
  Guard: usage / quota >= 0.80
  Guard: usage / quota < 0.95
  Guard: Duration > 5 min (noise filtering)
  Action: Send warning notification to customer
  Action: Log quota warning event
  Emit: QuotaWarning

healthy → throttled
  Guard: usage / quota >= 0.95
  Action: Enable request throttling
  Action: Set throttle delay = 100ms
  Action: Send throttle notification
  Emit: QuotaThrottled

healthy → exceeded
  Guard: usage / quota > 1.0
  Guard: Duration > 1 min
  Action: Block all new requests
  Action: Return 429 TooManyRequests
  Action: Escalate to support team
  Emit: QuotaExceeded
  Broadcast: CustomerAccountGovernor (notify), ComplianceGovernor (audit)

warning → healthy
  Guard: usage / quota < 0.80
  Guard: Duration > 5 min (noise filtering)
  Action: Clear warning state
  Emit: QuotaHealthy

warning → throttled
  Guard: usage / quota >= 0.95
  Action: Enable throttling
  Emit: QuotaThrottled

throttled → healthy
  Guard: usage / quota < 0.80
  Action: Disable throttling
  Action: Clear throttle delay
  Emit: QuotaHealthy

throttled → exceeded
  Guard: usage / quota > 1.0
  Action: Block requests (beyond throttling)
  Emit: QuotaExceeded

exceeded → throttled
  Guard: usage / quota >= 0.95 AND < 1.0
  Guard: Duration > 1 min
  Action: Re-enable throttling (instead of blocking)
  Emit: QuotaThrottled

exceeded → healthy
  Guard: usage / quota < 0.80
  Guard: Duration > 5 min
  Action: Clear all throttling/blocking
  Emit: QuotaHealthy
```

### Integration Examples

#### Example 1: Quota Warning

```rust
// Monitor customer quota usage every 10 seconds
let metrics = QuotaSlaEvent::QuotaCheckTriggered {
    customer_id: "cust_abc",
    tier: CustomerTier::Professional,
    cpu_usage: 3500,        // Out of 5000 (70%)
    api_calls_this_hour: 8000, // Out of 10000 (80%)
};

gov.transition(metrics)?;
// If api_calls >= 0.80 * 10000 = 8000
// → State: warning
// → Emit: QuotaWarning
// → Action: Send email to customer: "You're at 80% of API quota"
```

#### Example 2: Quota Exceeded

```rust
// Customer continues exceeding quota
let exceeded = QuotaSlaEvent::QuotaCheckTriggered {
    customer_id: "cust_xyz",
    tier: CustomerTier::Starter,
    cpu_usage: 2600,  // Out of 2500 (104% - EXCEEDED)
    api_calls_this_hour: 10500,  // Out of 10000 (105% - EXCEEDED)
};

gov.transition(exceeded)?;
// → State: exceeded
// → Action: Block new requests (return 429 TooManyRequests)
// → Action: Escalate to support: "Customer exceeding quota"
// → Emit: QuotaExceeded
// → Broadcast: CustomerAccountGovernor (notify), BillingGovernor (consider overage charges)
```

#### Example 3: Quota Recovery

```rust
// Customer's usage drops
let recovery = QuotaSlaEvent::QuotaCheckTriggered {
    customer_id: "cust_xyz",
    tier: CustomerTier::Starter,
    cpu_usage: 1500,   // Out of 2500 (60%)
    api_calls_this_hour: 7000,  // Out of 10000 (70%)
};

gov.transition(recovery)?;
// If state was exceeded and cpu_usage < 0.80 * 2500 AND api_calls < 0.80 * 10000
// Wait 5 min confirmation period
// → State: healthy
// → Action: Unblock requests
// → Emit: QuotaHealthy
// → Action: Clear any throttling
```

---

## Governor #7: Compliance & Audit FSM

**Purpose**: KYC/AML verification, fraud detection, data retention
**Location**: `src/marketplace/compliance_audit_governor.rs`
**States**: 6
**Transitions**: 12
**Primary Timeline**: Minutes to years
**Critical Feature**: Regulatory requirement, audit trail immutability

### State Diagram

```
compliant ──┬──► verification_pending  [Verification deadline reached]
            │
            ├──► fraud_detection_active [Suspicious activity detected]
            │
            └──► investigation  [Violation detected]

verification_pending ──┬──► compliant        [Verification passed]
                       │
                       └──► non_compliant   [Verification failed]

fraud_detection_active ──┬──► compliant              [No fraud confirmed]
                         │
                         └──► investigation        [Fraud confirmed]

investigation ──┬──► compliant      [Investigation cleared]
                │
                └──► non_compliant  [Violation confirmed]

non_compliant ──┬──► compliant      [Remediation completed]
                │
                └──► data_retention [Data retention deadline reached]

data_retention (terminal, read-only)
```

### States Explained

| State | Purpose | Actions | Duration | Next State |
|-------|---------|---------|----------|-----------|
| **compliant** | Passing all regulations | Monitor for violations | ∞ | verification_pending, fraud_detection_active, investigation |
| **verification_pending** | KYC/AML in progress | Request verification from customer | 30 days max | compliant, non_compliant |
| **fraud_detection_active** | Anomalies detected | Monitor transactions, restrict high-risk activities | Hours to days | compliant, investigation |
| **investigation** | Violation under review | Hold account, escalate to compliance team | Days to weeks | compliant, non_compliant |
| **non_compliant** | Regulatory violation confirmed | Implement remediation, notify regulators | Days to months | compliant, data_retention |
| **data_retention** | GDPR/CCPA retention period | Archive data, expire sensitive fields | 7-30 years | (terminal) |

### Transition Rules

```
compliant → verification_pending
  Guard: VerificationDeadlineReached event
  Guard: Annual KYC/AML re-verification (every 365 days)
  Guard: Or: Fraud score threshold exceeded
  Guard: Or: Manual verification requested
  Action: Initiate KYC/AML verification with third-party provider
  Action: Send verification link to customer
  Timeout: 30 days (escalate to support)
  Emit: VerificationRequested
  Broadcast: CustomerAccountGovernor (flag account), BillingGovernor (hold collections if high risk)

verification_pending → compliant
  Guard: VerificationPassed event
  Guard: Third-party KYC service confirms valid identity
  Guard: No fraud indicators
  Action: Clear verification flag
  Action: Update verification_timestamp
  Action: Next verification deadline = today + 365 days
  Emit: VerificationSucceeded

verification_pending → non_compliant
  Guard: VerificationFailed event
  Guard: Or: Verification deadline exceeded (30 days)
  Guard: Or: Fraud indicators detected
  Action: Mark account as non-compliant
  Action: Escalate to compliance team
  Emit: VerificationFailed
  Broadcast: CustomerAccountGovernor (lock account), BillingGovernor (block collections)

compliant → fraud_detection_active
  Guard: AnomalyDetected event
  Guard: Fraud score exceeds threshold (e.g., 7/10)
  Guard: Or: Unusual transaction pattern detected
  Action: Enable enhanced monitoring
  Action: Throttle high-risk transactions
  Action: Flag for manual review
  Emit: FraudDetectionActivated
  Broadcast: BillingGovernor (enable fraud checks)

fraud_detection_active → compliant
  Guard: FraudCheckPassed event (after manual review)
  Guard: Transaction pattern normalized
  Guard: No further suspicious activity for 24 hours
  Action: Disable enhanced monitoring
  Action: Clear fraud flags
  Emit: FraudCheckPassed

fraud_detection_active → investigation
  Guard: FraudConfirmed event
  Guard: Manual reviewer determines fraud
  Guard: Or: Multiple transactions flagged as fraudulent
  Action: Escalate to fraud investigation team
  Action: Block account pending investigation
  Emit: FraudConfirmed
  Broadcast: CustomerAccountGovernor (lock), BillingGovernor (reverse charges)

compliant → investigation
  Guard: ViolationDetected event
  Guard: Or: RegulatoryConcernRaised event
  Guard: Or: Data breach detected
  Action: Initiate formal investigation
  Action: Freeze account pending review
  Action: Create compliance incident ticket
  Timeout: 30 days (escalate to legal)
  Emit: InvestigationStarted
  Broadcast: All governors (audit logging enabled)

investigation → compliant
  Guard: InvestigationCleared event
  Guard: No violations confirmed
  Guard: Remediation not needed
  Action: Clear investigation flags
  Action: Restore account to full function
  Emit: InvestigationCleared

investigation → non_compliant
  Guard: InvestigationViolationConfirmed event
  Guard: Regulatory violation identified
  Action: Implement remediation plan
  Action: Notify regulators (if required)
  Action: Set compliance deadline (e.g., 30 days)
  Emit: ViolationConfirmed
  Broadcast: All governors (enhanced audit logging)

non_compliant → compliant
  Guard: RemediationCompleted event
  Guard: Compliance deadline passed AND all remediation steps done
  Guard: Regulator sign-off (if required)
  Action: Clear non-compliant flags
  Action: Resume normal operations
  Emit: ComplianceRestored

non_compliant → data_retention
  Guard: RetentionPeriodExpired event
  Guard: Or: DataDeletionRequested event (GDPR/CCPA)
  Guard: All disputes, chargebacks, refunds resolved
  Action: Anonymize all PII
  Action: Archive to cold storage
  Action: Set retention expiration date (7-30 years per regulation)
  Emit: DataRetentionInitiated (terminal)
```

### Integration Examples

#### Example 1: KYC/AML Verification

```rust
// Annual verification deadline reached
let kyc_event = ComplianceEvent::VerificationDeadlineReached {
    customer_id: "cust_abc",
    framework: ComplianceFramework::AML,
    last_verified: DateTime::parse_from_rfc3339("2025-01-25T00:00:00Z").unwrap().with_timezone(&Utc),
};

gov.transition(kyc_event)?;
// → State: verification_pending
// → Action: Initiate KYC verification
// → Send: Email with verification link
// → Timeout: 30 days

// Customer completes verification
let verified = ComplianceEvent::VerificationPassed {
    customer_id: "cust_abc",
    framework: ComplianceFramework::AML,
    verifier: "kyc_service_acme",
};

gov.transition(verified)?;
// → State: compliant
// → Emit: VerificationSucceeded
// → Update: next_verification_deadline = today + 365 days
```

#### Example 2: Fraud Detection

```rust
// Unusual transaction pattern detected
let fraud_event = ComplianceEvent::AnomalyDetected {
    customer_id: "cust_xyz",
    fraud_score: 7.5,  // Out of 10
    indicators: vec![
        "Large_transaction_amount",
        "Unusual_geographic_location",
        "Multiple_failed_payments_then_success",
    ],
};

gov.transition(fraud_event)?;
// → State: fraud_detection_active
// → Action: Enable enhanced monitoring
// → Action: Throttle high-risk transactions
// → Action: Flag for manual review

// Manual review concludes fraud
let fraud_confirmed = ComplianceEvent::FraudConfirmed {
    customer_id: "cust_xyz",
    reviewer: "fraud_team_lead",
    fraudulent_transactions: vec!["tx_abc123", "tx_def456"],
};

gov.transition(fraud_confirmed)?;
// → State: investigation
// → Action: Lock account
// → Action: Reverse fraudulent charges
// → Emit: FraudConfirmed
// → Broadcast: BillingGovernor (issue refunds)
```

#### Example 3: GDPR Data Deletion

```rust
// Customer requests GDPR right to deletion
let deletion = ComplianceEvent::DataDeletionRequested {
    customer_id: "cust_123",
    framework: ComplianceFramework::GDPR,
    request_date: Utc::now(),
};

// Account must be in non_compliant state and all disputes resolved
// Then trigger data retention
let retention = ComplianceEvent::RetentionPeriodExpired {
    customer_id: "cust_123",
    framework: ComplianceFramework::GDPR,
};

gov.transition(retention)?;
// → State: data_retention
// → Action: Anonymize all PII (GDPR pseudonymization)
// → Action: Archive to cold storage (7-year retention)
// → Action: Set automatic deletion after 7 years
// → Emit: DataRetentionInitiated (terminal)
// → Broadcast: All governors (customer data no longer accessible)
```

---

## Governor #8: Multi-Tenant FSM

**Purpose**: Isolation verification, fair-share allocation, cascade prevention
**Location**: `src/marketplace/multi_tenant_governance.rs`
**States**: 6
**Transitions**: 12
**Primary Timeline**: Real-time (seconds)
**Critical Feature**: Noisy neighbor detection, resource fair-share

### State Diagram

```
healthy ──┬──► resource_contention  [Noisy neighbor detected: CPU/memory > 75%]
          │
          └──► cascade_prevention   [Cascading failure signs detected]

resource_contention ──┬──► healthy                  [Load rebalance successful]
                      │
                      ├──► load_balancing          [Active rebalancing in progress]
                      │
                      └──► cascade_prevention      [Contention causing cascade]

load_balancing ──┬──► healthy      [Rebalance complete, metrics normal]
                 │
                 └──► cascade_prevention [Rebalance insufficient]

cascade_prevention ──┬──► healthy                  [Emergency measures resolved]
                     │
                     └──► emergency_shutdown      [Full system degradation]

emergency_shutdown ──┬──► recovery                 [Emergency measures activated]
                     │
                     └──► cascade_prevention      [Degradation not helping]

recovery ──┬──► healthy               [System recovered to normal]
           │
           └──► cascade_prevention    [Further issues detected]
```

### Key Invariants

```rust
// Fair-share allocation (tenant tier)
enterprise_weight = 40%
professional_weight = 35%
starter_weight = 25%
  where: sum of weights = 100%

// Noisy neighbor detection threshold
cpu_threshold = 75%  // Alert if any single tenant > this
memory_threshold = 75%
network_threshold = 75%
disk_io_threshold = 75%

// Cascade prevention triggers
min_cpu_reserved = 20%  // Keep 20% CPU free for system
circuit_breaker_error_threshold = 50%  // Block tenant after 50% errors
circuit_breaker_timeout = 1 sec (exponential backoff)

// No cross-tenant data leakage
tenant_isolation = 100%  // Verified via encryption + access control
```

### Transition Rules

```
healthy → resource_contention
  Guard: MetricsSnapshot event
  Guard: Any tenant resource usage >= 75% of allocated
  Guard: Duration > 30 seconds (noise filtering)
  Guard: Impact on other tenants > 5%
  Action: Trigger noisy neighbor detection algorithm
  Action: Identify problematic tenant
  Action: Reduce problem tenant quota to baseline (50% of tier allocation)
  Action: Begin load rebalancing
  Emit: NoisyNeighborDetected
  Broadcast: Multi-tenant metrics, alert to ops team

healthy → cascade_prevention
  Guard: CascadeSignsDetected event
  Guard: Or: Multiple independent failures detected
  Guard: Or: Error rate spike > 10x baseline
  Action: Enable automatic circuit breakers
  Action: Reduce features to essential only (graceful degradation)
  Emit: CascadePreventionActivated

resource_contention → load_balancing
  Guard: LoadRebalancingAuthorized event
  Guard: Or: Auto-trigger after 2 min of contention
  Action: Initiate container migration
  Action: Move workloads to less-loaded nodes
  Action: Monitor rebalance metrics
  Timeout: 5 min (abort if no improvement)
  Emit: LoadRebalancingStarted
  Broadcast: Kubernetes cluster (trigger pod migrations)

load_balancing → healthy
  Guard: MetricsSnapshot shows improvement
  Guard: Peak CPU/memory < 75% on all tenants
  Guard: Duration > 5 min (stability check)
  Action: Restore full quota allocations
  Action: Clear load balancing flags
  Emit: LoadRebalancingSucceeded

load_balancing → cascade_prevention
  Guard: LoadRebalancingTimeout (5 min exceeded)
  Guard: Or: Metrics show further deterioration
  Action: Abort load rebalancing
  Action: Activate full cascade prevention
  Emit: LoadRebalancingFailed
  Broadcast: Escalate to emergency_shutdown

resource_contention → cascade_prevention
  Guard: NoisyNeighborDetectionFailed event
  Guard: Or: Rebalancing not effective after 2 min
  Action: Activate cascade prevention
  Emit: EscalatingToCascadePrevention

cascade_prevention → emergency_shutdown
  Guard: EmergencyShutdownTriggered event
  Guard: Or: System health critical (>25% error rate)
  Guard: Or: CPU usage > 95% sustained
  Action: Enable circuit breakers on all tenants
  Action: Block non-essential requests
  Action: Reduce database connections
  Action: Enable rate limiting (strict)
  Emit: EmergencyShutdownActivated
  Broadcast: All systems (announce maintenance window)

cascade_prevention → healthy
  Guard: MetricsSnapshot shows improvement
  Guard: Error rate drops below 1%
  Guard: Duration > 10 min (stability check)
  Action: Disable cascade prevention measures
  Emit: CascadePreventionEnded
  Broadcast: All systems (service restored)

emergency_shutdown → recovery
  Guard: EmergencyMeasuresActivated event
  Guard: Manual or automatic (after 1 hour)
  Action: Start controlled recovery process
  Action: Gradually increase system load
  Action: Monitor for cascading failure signs
  Timeout: 2 hours (abort and re-enter emergency_shutdown)
  Emit: RecoveryStarted

recovery → healthy
  Guard: MetricsSnapshot shows stable operation
  Guard: All systems responding normally
  Guard: Duration > 30 min (stability check)
  Action: Restore full system capacity
  Action: Clear emergency flags
  Emit: RecoverySucceeded
  Broadcast: All systems (normal operation resumed)

recovery → cascade_prevention
  Guard: SystemHealthDeterioration event during recovery
  Guard: Or: Timeout exceeded (2 hours)
  Action: Re-enter cascade prevention
  Action: Slow down recovery ramp
  Emit: RecoveryInterrupted
  Broadcast: Escalate to on-call engineer
```

### Integration Examples

#### Example 1: Noisy Neighbor Detection

```rust
// Metrics collected every 10 seconds
let metrics = MTGovernorEvent::MetricsSnapshot {
    timestamp: Utc::now(),
    tenants: vec![
        TenantMetrics {
            tenant_id: "tenant_1_enterprise",
            tier: TenantTier::Enterprise,
            cpu_usage: 45,        // 45%
            memory_usage: 60,     // 60%
        },
        TenantMetrics {
            tenant_id: "tenant_2_professional",
            tier: TenantTier::Professional,
            cpu_usage: 88,        // 88% - EXCEEDS THRESHOLD
            memory_usage: 92,     // 92% - EXCEEDS THRESHOLD
        },
        TenantMetrics {
            tenant_id: "tenant_3_starter",
            tier: TenantTier::Starter,
            cpu_usage: 20,        // 20%
            memory_usage: 30,     // 30%
        },
    ],
};

gov.transition(metrics)?;
// → Noisy neighbor detected: tenant_2_professional (88% CPU)
// → State: resource_contention
// → Action: Reduce tenant_2 quota to 50% of Professional allocation
// → Action: Begin load rebalancing
// → Emit: NoisyNeighborDetected
// → Action: Send alert: "Tenant-2 consuming excessive resources"
```

#### Example 2: Load Rebalancing

```rust
// Auto-trigger load rebalancing after 2 min contention
let rebalance = MTGovernorEvent::LoadRebalancingAuthorized {
    problematic_tenant: "tenant_2_professional",
    target_node: "gke-worker-3",  // Less loaded node
};

gov.transition(rebalance)?;
// → State: load_balancing
// → Action: Migrate tenant_2 workloads to gke-worker-3
// → Action: Monitor CPU/memory metrics
// → Timeout: 5 min

// After rebalance succeeds
let rebalance_success = MTGovernorEvent::MetricsSnapshot {
    timestamp: Utc::now(),
    tenants: vec![
        TenantMetrics { tenant_id: "tenant_2_professional", cpu_usage: 42, .. },
        // ...other tenants...
    ],
};

gov.transition(rebalance_success)?;
// → All tenants below 75% threshold
// → Duration confirmed > 5 min stable
// → State: healthy
// → Action: Restore full quota allocations
// → Emit: LoadRebalancingSucceeded
```

#### Example 3: Cascade Prevention

```rust
// System health deteriorating
let cascade = MTGovernorEvent::CascadeSignsDetected {
    error_rate_increase: 12.0,  // 12x baseline
    failed_requests: 1200,
    healthy_requests: 800,
};

gov.transition(cascade)?;
// → State: cascade_prevention
// → Action: Enable circuit breakers on all tenants
// → Action: Block non-essential requests (e.g., analytics, reporting)
// → Action: Reduce database connection pool size
// → Action: Enable strict rate limiting (10 req/sec per tenant)
// → Emit: CascadePreventionActivated
// → Broadcast: Announce "Service operating in degraded mode"

// After 5 min, system stabilizes
let recovery_start = MTGovernorEvent::MetricsSnapshot {
    timestamp: Utc::now(),
    error_rate: 0.5,  // Back to normal
    success_count: 5000,
};

gov.transition(recovery_start)?;
// → State: emergency_shutdown (intermediate)
// → Action: Begin gradual recovery
// → Emit: RecoveryStarted

// After 30+ min of stable operation
let recovered = MTGovernorEvent::MetricsSnapshot {
    timestamp: Utc::now(),
    error_rate: 0.08,  // Baseline
    success_count: 15000,
};

gov.transition(recovered)?;
// → State: healthy
// → Action: Restore full capacity
// → Emit: RecoverySucceeded
```

---

## Integration Examples

### Example A: New Customer Onboarding (All 8 Governors)

```rust
// Customer signs up
let signup = MarketplaceEvent::CustomerSignup {
    customer_id: "cust_new_2026_001",
    email: "alice@company.com",
    tier: FeatureTier::Professional,
    trial_days: 14,
};

// Orchestrator routes to all 8 governors
let coordination = orchestrator.handle_event(signup).await?;

// 1. CustomerAccountGovernor: Create profile
//    → State: profile_active
//    → Action: Create customer record

// 2. SubscriptionGovernor: Create trial subscription
//    → State: trial
//    → Duration: 14 days

// 3. EntitlementGovernor: Create entitlements
//    → State: pending_approval (24h TTL)
//    → Features: API access, dashboard, basic reporting

// 4. QuotaSlaGovernor: Allocate trial quota
//    → State: healthy
//    → Quota: 100k API calls/month, 1000 report runs/month

// 5. ProductCatalogGovernor: Enable product visibility
//    → State: published
//    → Features: Professional tier features visible

// 6. BillingGovernor: Wait for payment method
//    → State: awaiting_invoice
//    → No charge until trial expires

// 7. ComplianceGovernor: Verify KYC/AML
//    → State: verification_pending
//    → Action: Send verification email

// 8. MultiTenantGovernor: Allocate resources
//    → State: healthy
//    → Allocation: 35% of Professional tier resources

// Result: All governors coordinated, customer onboarded, audit trail complete
```

### Example B: Payment Failure Cascade

```rust
// BillingGovernor detects payment failure
let payment_fail = BillingEvent::PaymentFailed {
    invoice_id: "inv_123",
    customer_id: "cust_abc",
};

// 1. BillingGovernor: Enter retry logic
//    → State: payment_failed
//    → Timeout: 2 days

// 2. EntitlementGovernor: Suspend features
//    → Event: Suspend (reason: payment_failed)
//    → State: suspended
//    → Timeout: 72h auto-refund

// 3. QuotaSlaGovernor: Reduce quota
//    → Event: QuotaReduced
//    → State: throttled (rate limiting enabled)
//    → Quota: 50% of allocated

// 4. ComplianceGovernor: Enhanced monitoring
//    → Event: FraudDetection (suspicious pattern)
//    → State: fraud_detection_active

// 5. CustomerAccountGovernor: Notify customer
//    → Event: CommunicationPreferences
//    → Action: Send email: "Payment failed, action required"

// 6. MultiTenantGovernor: Monitor resource impact
//    → Reduced workload due to throttling

// If payment succeeds within 2 days:
//    → All governors reverse actions
//    → Return to normal operation
//    → Issue no refund

// If payment fails after 3 retries:
//    → BillingGovernor: Escalate to collections
//    → EntitlementGovernor: Expire subscription
//    → SubscriptionGovernor: Mark as lapsed
//    → ComplianceGovernor: Investigation
//    → CustomerAccountGovernor: Lock account
```

---

**Last Updated**: January 2026
**Next Review**: April 2026
**Owner**: @marketplace-architecture
**Slack**: #marketplace-fsm-design
