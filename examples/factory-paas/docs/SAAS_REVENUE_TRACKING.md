# SaaS Recurring Revenue Tracking - Implementation Guide

## Overview

This module implements **SaaS recurring revenue tracking and commission stacking** for FactoryPaaS using event sourcing and domain-driven design principles.

## Architecture

### Domain Model (Ontology)

#### Entities

1. **SaasProgram** - Affiliate program configuration
   - Commission rate (percentage)
   - Billing cycle (monthly, annual)
   - API integration (Stripe, Paddle, custom)
   - Webhook authentication

2. **Subscription** - Active SaaS subscription
   - MRR (Monthly Recurring Revenue)
   - Subscription status (active, canceled, expired)
   - Billing dates
   - Lifetime commission tracking

3. **RecurringCommission** (Value Object) - Individual commission payment
   - Commission amount
   - Billing period
   - Publisher attribution

#### Commands

1. **TrackSubscription** - Record new or updated subscription
   - Input: program_id, publisher_id, customer_id, plan, mrr_amount
   - Output: SubscriptionCreated event

2. **RecordRecurringCommission** - Monthly commission accrual
   - Input: subscription_id, period_start, period_end
   - Output: RecurringCommissionRecorded event

3. **CancelSubscription** - Mark subscription as canceled
   - Input: subscription_id, cancel_date
   - Output: SubscriptionCanceled event

#### Events

1. **SubscriptionCreated** - New subscription tracked
2. **RecurringCommissionRecorded** - Monthly commission recorded
3. **SubscriptionCanceled** - Subscription canceled

#### Aggregates

1. **RevenueAggregate** - System-wide revenue metrics
   - total_mrr: Total monthly recurring revenue
   - active_subscriptions: Count of active subs
   - lifetime_commissions: Total commissions earned
   - projected_arr: Annual recurring revenue projection (MRR * 12)

2. **PublisherRevenueAggregate** - Per-publisher revenue
   - Publisher-specific MRR
   - Active subscription count
   - Monthly and lifetime commissions

## Event Sourcing

All state changes are captured as immutable events:

```
TrackSubscription → SubscriptionCreated → Update RevenueAggregate
RecordRecurringCommission → RecurringCommissionRecorded → Update Commissions
CancelSubscription → SubscriptionCanceled → Update Metrics
```

### Audit Trail

Every event is persisted with:
- Event ID (UUID)
- Timestamp (ISO 8601)
- Aggregate ID
- Event payload
- Causation ID (command that triggered event)
- Correlation ID (workflow tracking)

## Webhook Integration

### Stripe

Webhook endpoint: `/webhooks/stripe/{program_id}/{publisher_id}`

Supported events:
- `customer.subscription.created` → TrackSubscription
- `invoice.payment_succeeded` → RecordRecurringCommission
- `customer.subscription.deleted` → CancelSubscription

### Paddle

Webhook endpoint: `/webhooks/paddle/{program_id}/{publisher_id}`

Supported events:
- `subscription_created` → TrackSubscription
- `subscription_payment_succeeded` → RecordRecurringCommission
- `subscription_cancelled` → CancelSubscription

## MRR Calculation

```rust
// New subscription
aggregate.total_mrr += subscription.mrr_amount;
aggregate.active_subscriptions += 1;
aggregate.projected_arr = aggregate.total_mrr * 12;

// Commission calculation
let commission = subscription.mrr_amount * program.commission_rate;
aggregate.lifetime_commissions += commission;

// Cancellation
aggregate.total_mrr -= subscription.mrr_amount;
aggregate.active_subscriptions -= 1;
aggregate.canceled_subscriptions += 1;
```

## Commission Stacking

Publishers earn recurring commissions for ALL active subscriptions:

```
Month 1: Sub A ($99) → Commission: $29.70 (30%)
Month 2: Sub A ($99) + Sub B ($199) → Commission: $89.40
Month 3: Sub A + Sub B + Sub C ($299) → Commission: $179.10
```

Total lifetime commission accumulates automatically via event sourcing.

## Testing Strategy (Chicago TDD)

### Unit Tests (State-Based)

All tests follow AAA pattern (Arrange, Act, Assert):

1. **test_track_subscription_increases_mrr** - Verify MRR calculation
2. **test_commission_calculation_applies_rate** - Commission math
3. **test_multiple_subscriptions_stack_mrr** - MRR stacking
4. **test_cancellation_reduces_mrr** - Cancellation logic
5. **test_lifetime_commission_accumulates** - Commission accumulation
6. **test_different_commission_rates** - Multi-program support
7. **test_subscription_status_lifecycle** - State transitions

### Integration Tests

1. **Webhook validation** - Signature verification
2. **Event replay** - Rebuild aggregates from events
3. **Concurrent subscriptions** - Race condition handling

### Property Tests

Using `proptest` for:
- Commission rate boundaries (0.0-1.0)
- MRR calculations (positive decimals)
- Date arithmetic (billing cycles)

## Deployment

### Generated Files

After running `ggen sync`, the following files are generated:

```
world/
├── src/
│   ├── entities.rs       # SaasProgram, Subscription, RecurringCommission
│   ├── commands.rs       # TrackSubscription, RecordRecurringCommission, etc.
│   ├── events.rs         # SubscriptionCreated, RecurringCommissionRecorded, etc.
│   ├── handlers.rs       # Command handlers with business logic
│   ├── aggregates.rs     # RevenueAggregate, PublisherRevenueAggregate
│   └── routes.rs         # Webhook HTTP endpoints
```

### Running the Service

```bash
# From rust-attribution-context directory
cd kernel
cargo run

# Service starts on http://0.0.0.0:8080
# Webhooks: POST /webhooks/stripe/{program_id}/{publisher_id}
# Health: GET /health
```

### Performance SLOs

- Webhook processing: <100ms p99
- MRR calculation: <10ms p99
- Event persistence: <50ms p99
- Aggregate rebuild: <500ms for 10k events

## Security

### Webhook Validation

All webhooks MUST validate signatures:

```rust
// Stripe
let signature = headers.get("stripe-signature")?;
stripe::verify_signature(payload, signature, webhook_secret)?;

// Paddle
let paddle_signature = form.get("p_signature")?;
paddle::verify_signature(form, paddle_signature, public_key)?;
```

### SPARQL Injection Prevention

All ontology queries use parameterized SPARQL (no string interpolation).

## Monitoring

Key metrics (OpenTelemetry):

- `subscriptions.created` (counter)
- `subscriptions.canceled` (counter)
- `commissions.recorded` (counter)
- `mrr.total` (gauge)
- `arr.projected` (gauge)
- `webhook.latency` (histogram)

## Example Usage

### Track New Subscription

```rust
let cmd = TrackSubscription {
    program_id: stripe_program_id,
    publisher_id: affiliate_id,
    customer_id: "cus_stripe123".to_string(),
    plan: "pro".to_string(),
    mrr_amount: Decimal::from(99),
    currency: Some("USD".to_string()),
    start_date: Utc::now(),
};

let event = handle_track_subscription(cmd, &mut revenue_aggregate).await?;
// event: SubscriptionCreated { subscription_id, mrr: 99, ... }
```

### Record Monthly Commission

```rust
let cmd = RecordRecurringCommission {
    subscription_id: sub_id,
    period_start: month_start,
    period_end: month_end,
};

let event = handle_record_recurring_commission(
    cmd,
    &mut revenue_aggregate,
    &subscription,
    &program,
).await?;
// event: RecurringCommissionRecorded { amount: 29.70, ... }
```

## References

- [Ontology: saas.ttl](../ontology/saas.ttl)
- [Commands: saas_commands.ttl](../ontology/saas_commands.ttl)
- [Events: saas_events.ttl](../ontology/saas_events.ttl)
- [Aggregates: saas_aggregates.ttl](../ontology/saas_aggregates.ttl)
- [Tests: saas_revenue_tests.rs](../tests/saas_revenue_tests.rs)

---

**Last Updated**: 2026-01-24
**Version**: 1.0.0
**Status**: Production Ready
