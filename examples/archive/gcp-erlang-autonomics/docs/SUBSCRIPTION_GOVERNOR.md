# Subscription Lifecycle Governor - Design & Implementation Guide

## Overview

The **Subscription Lifecycle Governor** is a production-grade SaaS subscription state machine implementing Erlang's `gen_statem` patterns in Rust. It manages the complete lifecycle of customer subscriptions with type-safe FSM guarantees, deterministic state transitions, and comprehensive audit trails for compliance.

**Core Equation**: $S(t+1) = \mu(S(t), E)$ — Next subscription state is a pure function of current state and event, with no hidden side effects.

## Architecture

### State Machine (8 States)

```
                        ┌──────────────┐
                        │    Trial     │
                        │  (14 days)   │
                        └───────┬──────┘
                                │ customer_purchases OR auto_expire
                        ┌───────▼──────────────┐
                        │   TrialEnded         │
                        │  (decision window)   │
                        └───────┬──────────────┘
                                │ customer_purchases
                        ┌───────▼──────────────┐
        ┌──────────────►│     Active           │◄──────┐
        │               │ (indefinite)         │       │
        │               └───┬──────────────────┘       │
        │                   │                          │
        │       ┌───────────┼──────────┐               │
        │       │           │          │               │
        │  upgrade/       renewal   cancel             │
        │  downgrade   approaching                     │
        │       │           │          │               │
        │       ▼           ▼          ▼               │
        │   Active*   AwaitingRenewal Cancelled        │
        │  (higher)   (payment due)  (30-day window)   │
        │       │           │          │               │
        │       │           │ payment   │ reactivate   │
        │       │           │ failed    │ (30 days)    │
        │       │           ▼          │               │
        │       │    RenewalGrace ─────┘               │
        │       │    (7 days)                          │
        │       │      │                               │
        │       │      │ payment_fixed                 │
        │       └──────┴──────────────────────────────►│
        │                                              │
        │          data_retention_expires (30 days)   │
        │               │                              │
        │               ▼                              │
        │            Lapsed                           │
        │     (archive_for_compliance)                │
        │               │                              │
        │               ▼                              │
        │            Archived (terminal)              │
        │                                              │
        └──────────────────────────────────────────────┘
                  manual_reset (admin only)
```

### Feature Tiers

| Tier | Monthly | Annual | API Calls/mo | Team Members | Data Retention |
|------|---------|--------|--------------|--------------|-----------------|
| Free | $0 | $0 | 10K | 1 | 30 days |
| Starter | $29.99 | $269.91 (10% off) | 1M | 5 | 90 days |
| Professional | $99.99 | $899.91 (10% off) | 10M | 50 | 365 days |
| Enterprise | Custom | Custom | Unlimited | Unlimited | Unlimited |

### Billing Cycles

- **Monthly**: 30 days, 0% discount
- **Annual**: 365 days, 10% discount
- **Two-Year**: 730 days, 20% discount
- **Three-Year**: 1095 days, 30% discount

## Key Features

### 1. State Transitions

Each state has well-defined valid transitions:

```rust
// Example: ACTIVE state transitions
Active + RenewalDateApproaching → AwaitingRenewal
Active + CustomerRequestsUpgrade → Active (pending upgrade approval)
Active + UpgradeApproved → Active (with new tier)
Active + CustomerCancels → Cancelled
Active + PaymentFailed → AwaitingRenewal
```

### 2. Proration Calculation

When a customer upgrades/downgrades mid-cycle, the system calculates fair proration:

```rust
// Given:
// - Old tier price: $99.99/month
// - New tier price: $299.99/month
// - Days elapsed: 10 of 30
// - Days remaining: 20 of 30

// Calculation:
// Daily old rate: $99.99 / 30 = $3.33/day
// Daily new rate: $299.99 / 30 = $9.99/day
// Amount used (old tier): $3.33 × 10 = $33.30
// Remaining old: $3.33 × 20 = $66.60
// Remaining new: $9.99 × 20 = $199.80
// Proration charge: $199.80 - $66.60 = $133.20
```

### 3. Payment Processing

The governor maintains a payment queue for deterministic, fault-tolerant processing:

```rust
pub async fn process_payments(&mut self) -> Result<Vec<(String, i32, bool)>, SubscriptionError> {
    // Returns (subscription_id, amount_cents, success) tuples
    // Processes in FIFO order for consistency
}
```

### 4. Renewal Grace Period

When payment fails, customers get a 7-day grace period to fix their payment method:

```
AwaitingRenewal + PaymentFailed → RenewalGrace
RenewalGrace + CustomerFixesPayment → Active (new period)
RenewalGrace + GracePeriodExpires → Cancelled (no payment received)
```

### 5. Reactivation Window

Cancelled subscriptions can be reactivated within 30 days:

```rust
pub fn can_reactivate(&self) -> bool {
    if let Some(deadline) = self.reactivation_deadline {
        Utc::now() <= deadline
    } else {
        false
    }
}
```

### 6. Churn Prevention

When a customer cancels, the system automatically:
1. Preserves customer data (30-day retention)
2. Sets reactivation window
3. Enables "bring back" offers (future: automatic discounts)
4. Records cancellation reason for analytics

### 7. Audit Trail

Every state transition is recorded with full context:

```rust
pub struct AuditEntry {
    pub id: String,
    pub subscription_id: String,
    pub timestamp: DateTime<Utc>,
    pub event: String,
    pub from_state: String,
    pub to_state: String,
    pub details: String,
}
```

## Implementation Patterns

### Type Safety First

The FSM uses Rust's type system to prevent invalid states:

```rust
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum SubscriptionState {
    Trial,
    TrialEnded,
    Active,
    AwaitingRenewal,
    RenewalGrace,
    Cancelled,
    Lapsed,
    Archived,
}
```

Invalid transitions are caught at **compile time** for static transitions, and at **runtime** for dynamic state logic.

### Pure Functions

State transitions are pure functions with no side effects beyond state mutation:

```rust
pub async fn transition(
    &mut self,
    subscription_id: &str,
    event: SubscriptionEvent,
) -> Result<(SubscriptionState, Option<String>), SubscriptionError>
```

- Input: `(subscription_id, event)`
- Output: `(new_state, optional_action_description)`
- Side effects: Only state modification and audit logging

### Deterministic Behavior

Same subscription + same event = same new state (idempotent):

```rust
// First call
gov.transition(sub_id, event).await → (Active, Some(action))

// Second call with same inputs
gov.transition(sub_id, event).await → (Active, Some(action))  // Same result
```

### Result<T, E> for Errors

All fallible operations return `Result`:

```rust
pub async fn transition(...) -> Result<(SubscriptionState, Option<String>), SubscriptionError>
pub async fn process_payments(&mut self) -> Result<Vec<(String, i32, bool)>, SubscriptionError>
pub async fn create_trial(...) -> Result<Subscription, SubscriptionError>
```

### No unwrap/expect

Production code uses explicit error handling:

```rust
// ✅ Correct
let sub = self.subscriptions.get_mut(id)
    .ok_or(SubscriptionError::SubscriptionNotFound { id: id.to_string() })?;

// ❌ Incorrect (would panic)
let sub = self.subscriptions.get_mut(id).unwrap();
```

## Usage Examples

### Basic Trial Signup

```rust
let mut governor = SubscriptionGovernor::new();

let subscription = governor
    .create_trial("customer-1".to_string(), AccountType::Individual)
    .await?;

assert_eq!(subscription.state, SubscriptionState::Trial);
assert_eq!(subscription.current_tier, FeatureTier::Free);
```

### Purchase Conversion

```rust
let (new_state, action) = governor
    .transition(
        &subscription.id,
        SubscriptionEvent::CustomerPurchases {
            tier: FeatureTier::Professional,
            cycle: BillingCycle::Annual,
        },
    )
    .await?;

assert_eq!(new_state, SubscriptionState::Active);
// Action: "Charge customer X cents"
```

### Mid-Cycle Upgrade

```rust
// Request upgrade
governor.transition(
    &sub_id,
    SubscriptionEvent::CustomerRequestsUpgrade {
        new_tier: FeatureTier::Enterprise,
    },
).await?;

// Approve upgrade (with proration)
let (new_state, action) = governor
    .transition(&sub_id, SubscriptionEvent::UpgradeApproved)
    .await?;

// Action includes charge amount (prorated)
```

### Payment Recovery

```rust
// Payment fails
governor.transition(
    &sub_id,
    SubscriptionEvent::RenewalPaymentFailed {
        reason: "Card declined".to_string(),
    },
).await?;

// Customer fixes payment
let (new_state, _) = governor
    .transition(
        &sub_id,
        SubscriptionEvent::CustomerFixesPayment {
            amount_cents: 99900,
        },
    )
    .await?;

assert_eq!(new_state, SubscriptionState::Active);
```

## Testing Strategy (Chicago TDD)

All tests follow **Arrange-Act-Assert (AAA)** pattern with real collaborators:

```rust
#[tokio::test]
async fn test_trial_to_active_purchase() {
    // Arrange
    let mut governor = SubscriptionGovernor::new();
    let subscription = governor
        .create_trial("cust-001".to_string(), AccountType::Individual)
        .await.unwrap();

    // Act
    let (new_state, action) = governor
        .transition(
            &subscription.id,
            SubscriptionEvent::CustomerPurchases {
                tier: FeatureTier::Starter,
                cycle: BillingCycle::Monthly,
            },
        )
        .await.unwrap();

    // Assert
    assert_eq!(new_state, SubscriptionState::Active);
    assert!(action.is_some());
    assert!(action.unwrap().contains("Charge customer"));
}
```

### Test Categories

1. **Happy Path**: Normal subscription lifecycle
2. **Edge Cases**: Trial expiration, grace period exhaustion, reactivation window expiration
3. **Error Paths**: Invalid transitions, calculation errors
4. **Concurrent Requests**: Multiple state transitions in quick succession
5. **Audit Trails**: Verify all transitions logged correctly

## Compliance & Data Retention

### GDPR Right-to-be-Forgotten

```
Cancelled (30 days) → Lapsed (90 days) → Archived (read-only)
                      └─ ArchiveForCompliance → Read-only historical data
```

Data flow:
1. **Cancelled**: Stop billing, preserve data for 30 days
2. **Lapsed**: Prepare for deletion after 30 days (total 60 days)
3. **Archived**: Final read-only state for compliance (indefinite)

### Audit Trail Immutability

Audit entries are append-only in a `VecDeque`:

```rust
pub async fn emit_receipt(action: &str, result: &str) -> Result<Receipt, ReceiptError> {
    // Hash-chained entries prevent tampering
    // Each entry links to previous via hash
}
```

## Performance Characteristics

- **State transition**: O(1) - direct hashmap lookup
- **Audit trail append**: O(1) - VecDeque push_back
- **Payment processing**: O(n) - queue length (n = pending payments)
- **Proration calculation**: O(1) - arithmetic operations
- **Memory**: Bounded by max subscriptions × average history per subscription

### SLOs

- Transition latency: <1ms (p99)
- Payment queue processing: <100ms for 1000 payments
- Audit trail lookup: <10ms for 1M entries
- Feature tier lookup: <100μs

## Security Considerations

### Invariant Violations

The governor validates all transitions to prevent:
- Invalid tier changes (Enterprise → Free)
- Negative refunds
- Proration calculation errors
- Orphaned subscriptions

### Type Safety

Rust's type system prevents:
- Use-after-free (owned values)
- Race conditions (mutable borrow checker)
- Null pointers (Option<T> instead of nullable references)
- Invalid state transitions (compile-time checks)

### Payment Safety

- All amounts are in cents (integers, not floats)
- No floating-point arithmetic for money
- Charges queued atomically
- Refunds explicit and audited

## Future Enhancements

### Planned Features

1. **Usage-Based Billing**: Charge per API call, storage, etc.
2. **Seat-Based Pricing**: Team accounts with per-seat charges
3. **Dunning Management**: Intelligent retry logic for failed payments
4. **Promotional Codes**: Discount application and validation
5. **Concurrent Requests**: Queue/deduplicate simultaneous upgrade requests
6. **Webhook Events**: Customer-initiated actions
7. **Metrics & Analytics**: Revenue tracking, churn analysis
8. **Custom Billing Cycles**: Quarterly, semi-annual, custom dates

### Integration Points

- **Payment Processor**: Stripe, Braintree, Square
- **Webhook System**: Customer notifications
- **Analytics**: ChartBeat, Segment, Mixpanel
- **CRM**: Salesforce, Hubspot
- **Compliance**: GDPR, SOC2, PCI-DSS

## Comparison with Alternatives

| Feature | Subscription Governor | Chargebee | Stripe Billing | Custom Code |
|---------|----------------------|-----------|-----------------|-------------|
| Type Safety | ✅ (Rust) | ❌ (Dynamic) | ⚠️ (SDK only) | ❌ |
| FSM Clarity | ✅ (Explicit states) | ⚠️ (Implicit) | ⚠️ (API driven) | ❌ |
| Audit Trail | ✅ (Built-in) | ✅ | ⚠️ (Limited) | ❌ |
| Proration | ✅ (Calculated) | ✅ | ✅ | ❌ |
| Testability | ✅ (Pure functions) | ⚠️ (API mocking) | ⚠️ (API mocking) | ❌ |
| Production Ready | ✅ | ✅ | ✅ | ❌ |

## References

- **Erlang gen_statem**: https://www.erlang.org/doc/man/gen_statem.html
- **Chicago TDD**: https://blog.cleancoder.com/uncle-bob/2013/05/27/TheTransformationPriorityPromise.html
- **MAPE-K Loop**: https://en.wikipedia.org/wiki/Autonomic_computing
- **Rust Type System**: https://doc.rust-lang.org/book/ch10-00-generics.html
- **Event Sourcing**: https://martinfowler.com/eaaDev/EventSourcing.html

## Contributing

When extending the subscription governor:

1. **Maintain Invariants**: Ensure all transitions respect business rules
2. **Add Tests First**: Chicago TDD - test before implementation
3. **Document Transitions**: Update state machine diagram
4. **Audit Everything**: Log all state changes
5. **No Unwrap/Expect**: Use Result<T, E> throughout
6. **Deterministic**: Same input = same output, always

---

**Last Updated**: January 2026
**Version**: 1.0.0 (Production Ready)
**Author**: Autonomic System Architecture Team
