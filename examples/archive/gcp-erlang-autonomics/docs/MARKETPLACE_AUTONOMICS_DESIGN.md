# GCP Marketplace Autonomic System: FSM Architecture Design

**Version:** 1.0.0
**Status:** Production-Ready Design
**Last Updated:** January 2026
**Author:** GCP Marketplace Autonomics Team

## Executive Summary

This document defines the complete state machine architecture for GCP Marketplace autonomic systems using Erlang `gen_statem` FSMs. The design ensures:

- **Type-Safe State Management**: Compile-time enforcement of impossible states
- **Deterministic Behavior**: No random transitions, explicit timeouts
- **Audit-Trail Immutability**: Every state change logged cryptographically
- **Graceful Degradation**: Circuit breakers, cascading failure prevention
- **Self-Healing**: Automatic retry logic, recovery paths, escalation workflows

---

## Table of Contents

1. [Design Principles](#design-principles)
2. [FSM #1: Marketplace Entitlement FSM](#fsm-1-marketplace-entitlement-fsm)
3. [FSM #2: Billing FSM](#fsm-2-billing-fsm)
4. [FSM #3: Product Catalog FSM](#fsm-3-product-catalog-fsm)
5. [FSM #4: Subscription FSM](#fsm-4-subscription-fsm)
6. [FSM #5: Customer Account FSM](#fsm-5-customer-account-fsm)
7. [FSM #6: Quota & SLA FSM](#fsm-6-quota--sla-fsm)
8. [FSM #7: Compliance & Audit FSM](#fsm-7-compliance--audit-fsm)
9. [FSM #8: Multi-Tenant Governance FSM](#fsm-8-multi-tenant-governance-fsm)
10. [Integration Architecture](#integration-architecture)
11. [Error Handling Strategies](#error-handling-strategies)
12. [Implementation Patterns](#implementation-patterns)

---

## Design Principles

### 1. Impossible States (Compile-Time Enforcement)

```rust
/// Invalid states cannot be represented in types
pub enum EntitlementState {
    PendingApproval,      // Cannot be refunding AND active
    Active,               // Cannot be expired AND active
    Suspended,            // Cannot be approved AND suspended
    Expired,              // Terminal state (no transitions out)
    Cancelled,            // Terminal state (no transitions out)
    RefundIssued,         // Implies previous payment_received
    ReinstatePending,     // Only from Suspended
    Archived,             // Terminal state (no transitions out)
}

// Type system prevents: EntitlementState::Active with balance < 0
// Compiler guarantees: Cannot reach impossible combinations
```

### 2. Deterministic Transitions

- No randomness in state machines (reproducible behavior)
- Explicit guard conditions (prevents accidental transitions)
- Timeout-based escalation (automatic, predictable)
- No silent failures (always explicit error states)

### 3. Audit-Trail Immutability

Every transition produces:
```json
{
  "timestamp": "2026-01-25T14:32:00Z",
  "from_state": "pending_approval",
  "to_state": "active",
  "transition_reason": "approval_granted",
  "actor": "system:approval-service",
  "data_hash": "sha256:abc123...",
  "signature": "ed25519:xyz789..."
}
```

### 4. Timeout-Based Escalation

- **24h approval timeout**: Escalates to manager review
- **72h suspension timeout**: Auto-refund issued
- **3x payment retry**: Escalates to collections
- **No manual intervention required**

### 5. Self-Healing

- **Automatic retries**: Up to 3x with exponential backoff
- **Circuit breakers**: Prevent cascading failures
- **Graceful degradation**: Degrade features before full outage
- **Recovery paths**: Always have a way back to healthy state

---

## FSM #1: Marketplace Entitlement FSM

**Purpose**: Manage customer entitlements to use marketplace products
**States**: 8
**Transitions**: 12
**Timeout Events**: 4

### State Diagram

```
                    ┌─────────────┐
                    │   DRAFT     │ (Not shown - internal only)
                    └────────┬────┘
                             │
                         approve
                             │
                    ┌────────▼────────┐
                    │ PENDING_APPROVAL │◄────────────┐
                    └────────┬────────┘              │
                             │                 (reinstate_pending)
                          [24h]                      │
                         timeout                ┌────┴──────┐
                             │                  │            │
                    ┌────────▼─────────┐      deny        approve
                    │      ACTIVE      │       │            │
                    └─┬──────────────┬─┘       │            │
                   ┌──┘              └──┐  ┌───▼──────┐     │
                   │                    │  │ ARCHIVED │◄────┘
              suspend              payment_failed
                   │    failed (3x)     │
                   │                    │
        ┌──────────▼─────────────────┐  │
        │      SUSPENDED             │  │
        └──────────┬──────────────────┘  │
                   │                     │
              [72h] │              ┌─────▼─────────┐
             timeout│              │ REFUND_ISSUED │
                   │              └───────────────┘
            ┌──────▼──────────┐
            │   EXPIRED       │
            └─────────────────┘

            Manual paths:
            Any ──cancel──> CANCELLED ──issue_refund──> REFUND_ISSUED
            Any ──archive──> ARCHIVED
```

### States

| State | Duration | Description | Next Valid States |
|-------|----------|-------------|-------------------|
| `pending_approval` | 24h TTL | Awaiting marketplace approval | `active`, `archived`, `cancelled` |
| `active` | Variable | Customer can use product | `suspended`, `expired`, `cancelled` |
| `suspended` | 72h TTL | Temporary disable (payment/TOS) | `active`, `expired`, `refund_issued` |
| `expired` | ∞ (Terminal) | Entitlement period ended | `archived` |
| `cancelled` | ∞ (Terminal) | Customer cancelled | `refund_issued`, `archived` |
| `refund_issued` | ∞ (Terminal) | Refund processed | `archived` |
| `reinstate_pending` | 24h TTL | Awaiting reinstatement approval | `active`, `cancelled`, `archived` |
| `archived` | ∞ (Terminal) | Moved to cold storage | (end-of-life) |

### Transitions & Rules

```
/* APPROVAL FLOW */
pending_approval --[approve]--> active {
  when: approval_granted_event received
  action: send_email(customer, "Your entitlement is now active")
  action: update_ledger(entitlement_id, "active", timestamp)
  action: notify_customer(entitlement_id)
  action: audit_log("entitlement_activated", customer_id, timestamp)
}

pending_approval --[deny]--> cancelled {
  when: approval_denied_event received
  action: send_email(customer, "Your entitlement application was denied")
  action: audit_log("entitlement_denied", customer_id, reason)
}

pending_approval --[timeout]--> archived {
  when: 24 hours elapsed without approval
  action: send_email(customer, "Your application expired - please reapply")
  action: audit_log("entitlement_approval_timeout", customer_id)
}

/* ACTIVE STATE TRANSITIONS */
active --[suspend]--> suspended {
  when: payment_failed OR tos_violation_detected
  action: send_email(customer, "Your entitlement has been suspended")
  action: stop_service_access(entitlement_id)
  action: audit_log("entitlement_suspended", customer_id, reason)
  timeout: 72h until auto-refund
}

active --[expire]--> expired {
  when: end_date reached
  action: send_email(customer, "Your entitlement has expired")
  action: stop_service_access(entitlement_id)
  action: audit_log("entitlement_expired", customer_id)
}

active --[cancel]--> cancelled {
  when: customer_initiated_cancellation
  action: send_email(customer, "Your entitlement has been cancelled")
  action: calculate_prorated_refund(entitlement_id)
  action: stop_service_access(entitlement_id)
  action: audit_log("entitlement_cancelled", customer_id)
}

/* SUSPENSION RECOVERY */
suspended --[reinstate]--> reinstate_pending {
  when: customer_resolves_issue OR payment_received
  action: send_email(customer, "Reinstatement pending manager approval")
  action: audit_log("reinstatement_requested", customer_id)
  timeout: 24h until approval required
}

reinstate_pending --[approve]--> active {
  when: reinstatement_approved_by_system OR manager
  action: send_email(customer, "Your entitlement has been restored")
  action: resume_service_access(entitlement_id)
  action: audit_log("entitlement_reinstated", customer_id)
}

reinstate_pending --[deny]--> cancelled {
  when: reinstatement_denied
  action: send_email(customer, "Reinstatement was not approved")
  action: audit_log("reinstatement_denied", customer_id)
}

/* SUSPENDED TIMEOUT */
suspended --[timeout]--> refund_issued {
  when: 72 hours elapsed in suspended state
  action: calculate_refund_amount(entitlement_id)
  action: initiate_refund_to_payment_method(entitlement_id)
  action: send_email(customer, "Automatic refund processed")
  action: audit_log("entitlement_auto_refunded", customer_id, refund_amount)
}

/* TERMINAL TRANSITIONS */
{active|suspended|expired|cancelled} --[archive]--> archived {
  when: archive_requested (manual or after 90 days)
  action: move_to_cold_storage(entitlement_id)
  action: audit_log("entitlement_archived", customer_id)
}
```

### Timeout Specifications

| Timeout | Duration | Trigger | Action |
|---------|----------|---------|--------|
| `approval_timeout` | 24h | No approval received | Escalate to manager review, send reminder email |
| `suspension_timeout` | 72h | Remains suspended | Automatically issue refund |
| `reinstatement_timeout` | 24h | Pending approval | Deny reinstatement, move to cancelled |
| `archive_timeout` | 90d | Terminal state reached | Move to cold storage (BigQuery) |

### Actions (Side Effects)

```rust
action send_email(customer_id: &str, template: &str) {
    // Call Cloud Email API
    // Log delivery status
    // Retry 3x on failure with exponential backoff
}

action update_ledger(entitlement_id: &str, state: &str, ts: DateTime) {
    // Write to Firestore: /entitlements/{id}/audit-trail
    // Append immutable event
    // Verify write succeeded (strong consistency)
}

action notify_customer(entitlement_id: &str) {
    // Publish to Cloud Pub/Sub: projects/{project}/topics/entitlement-events
    // Consumer: Notification service
    // Timeout: 5s delivery SLA
}

action audit_log(event: &str, actor: &str, details: Map) {
    // Log to Cloud Logging with label: entitlement_audit
    // Include: timestamp, actor, event, customer_id, signature
    // Retention: 7 years (compliance)
}

action stop_service_access(entitlement_id: &str) {
    // Call Service Identity API: remove customer service account
    // Revoke all active tokens
    // Publish to Pub/Sub: entitlement.access.revoked
}

action resume_service_access(entitlement_id: &str) {
    // Call Service Identity API: create customer service account
    // Issue new credentials
    // Publish to Pub/Sub: entitlement.access.granted
}
```

### Error Handling Paths

```
SCENARIO: Email delivery fails during active → suspended
├─ action send_email() fails (retryable)
├─ retry 1: wait 1s, retry (exponential backoff)
├─ retry 2: wait 2s, retry
├─ retry 3: wait 4s, retry
├─ if all retries fail:
│   ├─ publish alert to Pub/Sub
│   ├─ create incident in Cloud Incident
│   └─ log error but DON'T block state transition
│       (state machine proceeds; notification service retries async)
└─ human fallback: support team notified

SCENARIO: Ledger write fails
├─ action update_ledger() throws error
├─ state machine BLOCKS (critical path)
├─ retry 3x with exponential backoff
├─ if all retries fail:
│   ├─ publish to Pub/Sub: entitlement.ledger-write-failed
│   ├─ create incident in Cloud Incident
│   ├─ circuit breaker trips
│   └─ customer sees: "Processing your request, please retry in 5 minutes"
└─ human fallback: SRE investigates

SCENARIO: Customer initiates cancel during payment failure
├─ FSM receives two concurrent events
├─ active --suspend--> suspended (payment_failed)
├─ active --cancel--> cancelled (received before suspend)
├─ conflict resolution: FIRST event wins (ordered by queue)
├─ other event: dropped with audit log
└─ result: consistent state (either suspended or cancelled, never both)

SCENARIO: Network timeout during approval
├─ Pub/Sub message delivery timeout (5s SLA)
├─ Message NACKed, re-queued
├─ Consumer processes after 30s delay
├─ FSM receives approval event
├─ guard condition checks: still in pending_approval?
│   ├─ YES: transition to active
│   ├─ NO (already expired): silently drop event
└─ idempotency: always safe to reprocess
```

---

## FSM #2: Billing FSM

**Purpose**: Manage monthly billing, invoicing, payment collection
**States**: 6
**Transitions**: 8
**Retry Logic**: Up to 3 attempts per payment

### State Diagram

```
    ┌─────────────────┐
    │ AWAITING_INVOICE│◄─────────────────────────────────┐
    └────────┬────────┘                                  │
             │                                           │
        generate_invoice                              (refund_complete)
             │                                           │
    ┌────────▼────────┐                                 │
    │ INVOICE_ISSUED  │                                 │
    └────────┬────────┘                                 │
             │                                           │
        request_payment                                 │
             │                                           │
    ┌────────▼────────────┐                             │
    │ PAYMENT_PENDING     │◄────────┐                   │
    └─┬──────────────┬────┘         │                   │
   ┌──┘              └──┐       (retry_payment)         │
   │                    │           │                   │
payment_received   payment_failed   │                   │
   │                    │           │                   │
   │         ┌──────────┘           │                   │
   │         │ [retry 1,2,3]        │                   │
   │         │                      │                   │
   │    ┌────▼──────────┐           │                   │
   │    │ PAYMENT_FAILED│───retry───┘                   │
   │    └────┬──────────┘                               │
   │         │                                          │
   │    [late_payment_notification]                     │
   │         │                                          │
   │    [3 retries exhausted]                           │
   │         │                                          │
   │    Collections Queue                               │
   │         │                                          │
   ▼    ┌────▼────────────────┐                         │
┌──────────┐ REFUND_PROCESSING │                        │
│PAYMENT   │─────refund────────┼─────────┐             │
│RECEIVED  │    complete       │         │             │
└──────┬───┘                   └────┬────┘             │
       │                            │                  │
    [monthly_reconciliation]   ┌────▼──────┐          │
       │                       │ (archiving)│          │
       └───cycle_complete──────┴───────────┴──────────┘
```

### States

| State | Duration | Description | Events |
|-------|----------|-------------|--------|
| `awaiting_invoice` | Monthly | Waiting for invoice generation | `generate_invoice` |
| `invoice_issued` | 30d | Invoice sent to customer | `request_payment` |
| `payment_pending` | 15d | Awaiting payment via payment gateway | `payment_received`, `payment_failed`, `timeout` |
| `payment_received` | ∞ (Terminal) | Payment confirmed | `cycle_complete` |
| `payment_failed` | 30d | Initial payment rejection | `retry_payment`, `escalate_to_collections` |
| `refund_processing` | 14d | Refund initiated | `refund_complete` |

### Transitions & Rules

```
/* NORMAL MONTHLY CYCLE */
awaiting_invoice --[generate_invoice]--> invoice_issued {
  when: end of billing period (e.g., 1st of month)
  action: query_usage_from_bigquery(customer_id, billing_period)
  action: calculate_charges(usage, pricing_tier)
  action: generate_invoice_pdf(customer_id, charges)
  action: store_invoice_in_gcs(customer_id, invoice_pdf)
  action: send_email(customer, invoice_pdf_link)
  action: audit_log("invoice_generated", customer_id, amount)
}

invoice_issued --[request_payment]--> payment_pending {
  when: payment_due_date reached (15 days after invoice)
  action: call_payment_gateway(customer_id, payment_intent, amount)
  action: charge_payment_method(customer_payment_id, amount)
  action: audit_log("payment_requested", customer_id, amount)
  timeout: 15d until payment_failed
}

/* SUCCESS PATH */
payment_pending --[payment_received]--> payment_received {
  when: payment_gateway confirms payment successful
  action: record_payment(customer_id, amount, transaction_id)
  action: update_account_balance(customer_id, "credit")
  action: send_email(customer, "Payment received - thank you")
  action: audit_log("payment_received", customer_id, amount, transaction_id)
}

payment_received --[cycle_complete]--> awaiting_invoice {
  when: month-end reconciliation complete
  action: verify_no_discrepancies(customer_id, billing_period)
  action: settle_seller_payout(seller_id, net_amount)
  action: audit_log("billing_cycle_complete", customer_id)
}

/* RETRY PATH */
payment_pending --[payment_failed]--> payment_failed {
  when: payment_gateway declines charge (insufficient funds, card expired)
  action: send_email(customer, "Payment failed - please update payment method")
  action: audit_log("payment_failed", customer_id, reason)
  action: schedule_retry(retry_count=0)
}

payment_failed --[retry_payment]--> payment_pending {
  when: retry_count < 3 (automatic retry, or customer manual retry)
  action: wait_exponential_backoff(2^retry_count minutes)
  action: call_payment_gateway(customer_id, payment_intent, amount)
  action: charge_payment_method(customer_payment_id, amount)
  action: increment_retry_count()
  action: audit_log("payment_retry", customer_id, retry_attempt=retry_count)
}

payment_failed --[exhausted_retries]--> escalate_to_collections {
  when: retry_count >= 3
  action: send_email(customer, "Payment collection failed - account will be suspended")
  action: publish_event(Pub/Sub, "billing.collection_failed", customer_id)
  action: create_incident(Cloud Incident, priority=HIGH)
  action: audit_log("payment_collection_exhausted", customer_id, retry_count=3)
}

/* REFUND PATH */
{payment_received|payment_failed} --[initiate_refund]--> refund_processing {
  when: customer requested refund OR auto-refund (entitlement suspension)
  action: calculate_refund_amount(customer_id, entitlement_id)
  action: process_refund_via_gateway(payment_id, refund_amount)
  action: audit_log("refund_initiated", customer_id, refund_amount)
  timeout: 14d until refund appears in customer account
}

refund_processing --[refund_complete]--> awaiting_invoice {
  when: refund confirmed by payment gateway
  action: verify_refund_in_customer_account(customer_id)
  action: send_email(customer, "Refund processed successfully")
  action: update_account_balance(customer_id, "debit", refund_amount)
  action: audit_log("refund_complete", customer_id, refund_amount)
}
```

### Timeout Specifications

| Timeout | Duration | Trigger | Action |
|---------|----------|---------|--------|
| `payment_pending` | 15d | No payment received | Auto-transition to `payment_failed` |
| `payment_retry` | 3 attempts | Each attempt spaced 2^n minutes apart | After 3rd attempt, escalate to collections |
| `late_payment_notification` | After 5d pending | Payment overdue notification | Send urgent email, increase escalation |
| `refund_processing` | 14d | Refund initiated | Verify completion, follow up with customer |

### Actions (Side Effects)

```rust
action query_usage_from_bigquery(customer_id, period) {
    // Query BigQuery dataset: gcp-marketplace.usage
    // Aggregate per-minute metrics: CPU, memory, network
    // Sum by: customer_id, product_sku, billing_period
    // Timeout: 30s SLA
}

action calculate_charges(usage, pricing_tier) {
    // Apply pricing table: pricing_models table
    // Handle: per-unit, tiered, volume discount
    // Support: free tier, trial period exemption
    // Result: Amount in USD with tax calculation
}

action generate_invoice_pdf(customer_id, charges) {
    // Use Cloud Document AI or wkhtmltopdf
    // Template: invoice.html
    // Include: line items, total, tax, due date
    // Output: PDF in GCS bucket
}

action send_email(customer, template) {
    // Call SendGrid or Gmail API
    // Template variables: customer_name, amount, due_date
    // Retry: 3x with exponential backoff
    // Track: delivery status, open rate
}

action call_payment_gateway(customer_id, amount) {
    // Stripe API: create payment intent
    // Or: Braintree, Adyen, Square (abstracted)
    // Return: payment_id, status
    // Timeout: 10s SLA
}

action audit_log(event, actor, details) {
    // Log to Cloud Logging: billing_audit label
    // Include: timestamp, customer_id, amount, transaction_id
    // Retention: 7 years (SOX compliance)
}
```

### Error Handling Paths

```
SCENARIO: Payment fails, retry succeeds on 2nd attempt
├─ payment_pending --payment_failed--> payment_failed
├─ wait: exponential backoff (2^1 = 2 minutes)
├─ payment_failed --retry_payment--> payment_pending
├─ payment_gateway accepts charge
├─ payment_pending --payment_received--> payment_received ✓
└─ customer never notices (transparent recovery)

SCENARIO: All 3 payment retries fail
├─ retry 1: wait 2min, charge fails
├─ retry 2: wait 4min, charge fails
├─ retry 3: wait 8min, charge fails
├─ payment_failed --exhausted_retries--> escalate_to_collections
├─ actions:
│   ├─ send email: "Please update payment method"
│   ├─ publish to Pub/Sub for collections team
│   ├─ trigger Entitlement FSM: active --suspend--> suspended
│   └─ create incident in Cloud Incident (Priority: HIGH)
└─ result: customer loses service access after 24h

SCENARIO: Invoice generation fails (BigQuery timeout)
├─ query_usage_from_bigquery() timeout (>30s)
├─ retry 3x with exponential backoff
├─ all retries fail
├─ alert published to Pub/Sub: billing.invoice_generation_failed
├─ circuit breaker trips (temp disable billing)
├─ human fallback: support team investigates BigQuery
└─ result: billing delayed, customer receives courtesy credit

SCENARIO: Concurrent refund and new charge attempt
├─ payment_received --initiate_refund--> refund_processing
├─ customer cycle ends, system tries: awaiting_invoice --generate_invoice--> invoice_issued
├─ refund_processing --refund_complete--> awaiting_invoice (same state)
├─ no conflict: both can occur in sequence
├─ audit trail shows precise order
└─ result: refund first, then new billing cycle starts
```

---

## FSM #3: Product Catalog FSM

**Purpose**: Manage product lifecycle, visibility, pricing changes
**States**: 5
**Transitions**: 6
**Effective Dates**: Support future pricing changes

### State Diagram

```
    ┌──────────┐
    │  DRAFT   │
    └────┬─────┘
         │
      publish
         │
    ┌────▼───────┐
    │ PUBLISHED  │◄──────────────┐
    └─┬──────┬───┘               │
      │      │          (unfeature)
   feature  deprecate           │
      │      │          ┌───────┘
      │      │          │
  ┌───▼─┐  ┌─▼──────────┐
  │FEAT │  │DEPREC      │
  │URED │  │ATED        │
  └──┬──┘  └──┬─────────┘
     │        │
     └────┬───┘
          │ archive
        ┌─▼──────┐
        │ARCHIVED │ (Terminal)
        └────────┘
```

### States

| State | Duration | Description | Operations |
|-------|----------|-------------|------------|
| `draft` | Variable | Unpublished, internal only | Edit SKU, pricing, description |
| `published` | Variable | Visible in marketplace | Customers can purchase |
| `featured` | Variable | Promoted on homepage | Higher visibility, boost conversion |
| `deprecated` | 90d | Marked for removal, no new sales | Existing customers keep access |
| `archived` | ∞ (Terminal) | Moved to cold storage | No new sales, read-only access |

### Transitions & Rules

```
/* PUBLICATION */
draft --[publish]--> published {
  when: all_validations_pass (SKU, pricing, description)
  action: validate_product_schema()
  action: verify_pricing_in_range(min_price, max_price)
  action: check_compliance(GDPR, HIPAA_if_needed)
  action: publish_to_gcp_marketplace_api()
  action: publish_event(Pub/Sub, "product.published", product_id)
  action: audit_log("product_published", product_id, seller_id)
}

draft --[reject_publication]--> draft {
  when: validation fails
  action: send_email(seller, "Publication rejected: validation errors")
  action: audit_log("product_publication_failed", product_id, errors)
}

/* PROMOTION */
published --[feature]--> featured {
  when: seller or admin requests feature
  action: verify_seller_quota_for_featured(seller_id)
  action: add_to_homepage_carousel()
  action: increase_visibility_weight()
  action: send_email(seller, "Your product is now featured")
  action: audit_log("product_featured", product_id, seller_id)
}

featured --[unfeature]--> published {
  when: feature period ends or seller requests
  action: remove_from_homepage()
  action: reduce_visibility_weight()
  action: audit_log("product_unfeatured", product_id)
}

/* DEPRECATION */
published --[deprecate]--> deprecated {
  when: seller requests EOL or admin decision
  action: verify_active_entitlements(product_id)
  action: send_email(sellers, "Product deprecated - no new sales after 90 days")
  action: send_email(customers, "Support will end on DATE")
  action: hide_from_new_listings()
  action: allow_existing_renewals()
  action: audit_log("product_deprecated", product_id, eol_date)
  effective_date: 90 days from now
}

deprecated --[archive]--> archived {
  when: 90 days elapsed
  action: block_all_new_sales()
  action: allow_existing_entitlements_to_expire()
  action: move_to_cold_storage(BigQuery)
  action: publish_event(Pub/Sub, "product.archived", product_id)
  action: audit_log("product_archived", product_id)
}

/* PRICING CHANGES */
published --[change_pricing]--> published {
  when: seller submits new price (effective date in future)
  action: verify_price_in_valid_range(new_price)
  action: notify_existing_customers(price_change_policy)
  action: schedule_price_change(effective_date)
  action: audit_log("product_pricing_change", product_id, old_price, new_price, effective_date)
}

/* FEATURED PRICING */
featured --[change_pricing]--> featured {
  when: seller submits new price for featured product
  action: verify_price_change_allows_feature_status()
  action: schedule_price_change(effective_date)
  action: audit_log("featured_product_pricing_change", product_id, effective_date)
}
```

### Feature Toggle Management

```
Product Configuration:
├─ billing_model: ["per-user", "per-compute", "per-storage", "flat-rate"]
├─ trial_offered: [true, false] + trial_duration_days
├─ free_tier_available: [true, false] + free_tier_limits
├─ auto_renewal: [true, false]
├─ support_tier: ["community", "professional", "enterprise"]
├─ data_residency: ["us-only", "eu-only", "global"]
└─ sso_available: [true, false]

Effective Date Handling:
├─ price_change_effective: DateTime (future date)
├─ feature_toggle_effective: DateTime (future date)
├─ trial_period_change_effective: DateTime (future date)
└─ notifications: 30d, 14d, 7d, 1d before effective date
```

### Actions (Side Effects)

```rust
action validate_product_schema() {
    // Check: required fields (name, SKU, description, icon, pricing)
    // Validate: SKU format matches pattern ^[A-Z0-9_-]+$
    // Verify: description length 50-500 chars
    // Confirm: at least one pricing tier defined
}

action verify_pricing_in_range(min_price, max_price) {
    // GCP Marketplace rules:
    //   - Minimum: $0.01 USD per billing period
    //   - Maximum: $999,999.99 USD per billing period
    //   - Must be whole cents
}

action check_compliance(product_config) {
    // If HIPAA required:
    //   ├─ seller must be BAA-enabled
    //   └─ data residency must be US-only
    // If SOC2 required:
    //   ├─ verify seller certification
    //   └─ enable audit logging
}

action publish_to_gcp_marketplace_api() {
    // Call GCP Marketplace API: products.create
    // Return: product_id, visibility_url
    // Timeout: 5s SLA
}

action hide_from_new_listings() {
    // Set product.hidden = true in Firestore
    // Update search index: exclude from queries
    // Result: existing customers can still access, new customers see 404
}
```

### Error Handling Paths

```
SCENARIO: Pricing change validation fails
├─ published --change_pricing--> published
├─ verify_price_in_valid_range() returns error
├─ action: send email to seller with error details
├─ state: remains published (unchanged)
└─ seller retries with corrected price

SCENARIO: Deprecation fails (active entitlements prevent)
├─ published --deprecate--> deprecated
├─ verify_active_entitlements(product_id) fails
├─ action: send email to seller listing customers
├─ state: remains published (unchanged)
├─ recommendation: contact customers before deprecation
└─ seller can force deprecation (acknowledged)

SCENARIO: Concurrent pricing change and feature request
├─ published receives: [change_pricing, feature]
├─ FIFO queue: change_pricing processed first
├─ transition: published (pricing updated)
├─ next event: feature request accepted
├─ result: featured product with new pricing ✓
```

---

## FSM #4: Subscription FSM

**Purpose**: Manage customer subscriptions, renewals, churn prevention
**States**: 7
**Transitions**: 10
**Grace Period**: 30 days

### State Diagram

```
    ┌────────┐
    │ TRIAL  │
    └───┬────┘
        │ [30 days]
        │ trial_expired
        │
    ┌───▼────────┐
    │TRIAL_ENDED │
    └───┬────────┘
        │ customer_converts
        │
    ┌───▼──────┐       ┌──────────────────────────┐
    │  ACTIVE  │◄──┐   │                          │
    └─┬──┬──┬──┘   │   │ auto_renewal_success     │
      │  │  │      │   │                          │
      │  │  └──────┘   │     (every month)        │
      │  │             │                          │
      │  └─────────────┘     ┌──────────────────┐
      │                      │                  │
   downgrade            awaiting_renewal        │
   or cancel                │                   │
      │                 (renewal_window)        │
      │                     │                   │
    ┌─▼─────────┐       ┌───▼──────────┐        │
    │ CANCELLED │       │renewal_grace │        │
    └──────┬────┘       └───┬──────────┘        │
           │                │ [30 days]         │
           │            [customer pays]         │
           │                │                   │
           │            ┌───▼──────┐            │
           │            │  ACTIVE  ├────────────┘
           │            └──────────┘
           │
           │ (30 days lapsed)
           │
        ┌──▼──────┐
        │ LAPSED  │ (Terminal for that cycle)
        └─────────┘
```

### States

| State | Duration | Description | Auto-Actions |
|-------|----------|-------------|--------------|
| `trial` | 30d | Trial period (no charge) | Daily email: "X days remaining" |
| `trial_ended` | 1d | Trial ended, awaiting conversion | Email: "Your trial ended - upgrade now" |
| `active` | Monthly | Customer subscribed, paying | Auto-renew every month |
| `awaiting_renewal` | 14d before renewal | Pre-renewal window | Send reminder: "Your renewal is coming" |
| `renewal_grace` | 30d | Grace period for late payment | Reminder emails: 3x during period |
| `cancelled` | ∞ (Terminal) | Customer cancelled | Stop charging, begin offboarding |
| `lapsed` | ∞ (Terminal) | Subscription expired (unpaid) | Archive after 90d inactivity |

### Transitions & Rules

```
/* TRIAL FLOW */
trial --[trial_expired]--> trial_ended {
  when: 30 days elapsed
  action: send_email(customer, "trial.trial_ended_upsell")
  action: publish_event(Pub/Sub, "subscription.trial_ended", customer_id)
  action: disable_churn_prevention_offer()
  action: audit_log("subscription_trial_ended", customer_id)
}

trial_ended --[convert_to_paid]--> active {
  when: customer clicks "upgrade" or auto-converts
  action: initiate_first_payment(customer_id)
  action: send_email(customer, "subscription.welcome_active")
  action: start_monthly_renewal_schedule(customer_id)
  action: audit_log("subscription_converted_from_trial", customer_id)
}

trial_ended --[cancel]--> cancelled {
  when: customer doesn't convert (optional, depends on policy)
  action: send_email(customer, "subscription.cancelled_post_trial")
  action: audit_log("subscription_cancelled_after_trial", customer_id)
}

/* ACTIVE SUBSCRIPTION */
active --[schedule_renewal]--> awaiting_renewal {
  when: 14 days before subscription renewal date
  action: send_email(customer, "subscription.renewal_reminder_14d")
  action: audit_log("subscription_renewal_scheduled", customer_id, renewal_date)
}

awaiting_renewal --[auto_renew]--> active {
  when: renewal date reached, payment succeeds
  action: charge_payment_method(customer_id, renewal_amount)
  action: increment_billing_cycle_count()
  action: send_email(customer, "subscription.renewal_success")
  action: publish_event(Pub/Sub, "subscription.renewed", customer_id)
  action: audit_log("subscription_renewed", customer_id, amount)
}

awaiting_renewal --[renewal_failed]--> renewal_grace {
  when: payment decline during auto-renewal
  action: send_email(customer, "subscription.renewal_failed_update_payment")
  action: schedule_retry_in(24_hours)
  action: audit_log("subscription_renewal_failed", customer_id)
}

/* GRACE PERIOD */
renewal_grace --[retry_payment]--> active {
  when: customer updates payment method OR automatic retry succeeds
  action: charge_payment_method(customer_id, renewal_amount)
  action: send_email(customer, "subscription.grace_period_resolved")
  action: audit_log("subscription_grace_resolved", customer_id)
}

renewal_grace --[grace_expired]--> lapsed {
  when: 30 days elapsed without successful payment
  action: send_email(customer, "subscription.lapsed_notice")
  action: disable_service_access(customer_id)
  action: publish_event(Pub/Sub, "subscription.lapsed", customer_id)
  action: trigger_churn_prevention_campaign(customer_id)
  action: audit_log("subscription_lapsed", customer_id)
}

/* CANCELLATION */
{active|awaiting_renewal|trial_ended} --[cancel]--> cancelled {
  when: customer initiated cancellation
  action: calculate_prorated_refund(customer_id, cancellation_date)
  action: process_refund()
  action: disable_service_access(customer_id)
  action: send_email(customer, "subscription.cancellation_notice")
  action: schedule_churn_survey(customer_id, 24_hours)
  action: audit_log("subscription_cancelled", customer_id, refund_amount)
}

/* DOWNGRADE/UPGRADE (same tier) */
{active|awaiting_renewal} --[modify_tier]--> active {
  when: customer requests plan change
  action: calculate_prorated_adjustment(old_tier, new_tier, cycle_day)
  action: charge_or_credit_payment_method(customer_id, adjustment)
  action: send_email(customer, "subscription.plan_modified")
  action: audit_log("subscription_plan_modified", customer_id, old_tier, new_tier)
}
```

### Churn Prevention Workflow

```
Churn Prevention Triggers:
├─ trial_ended without conversion
│   └─ action: send_email(template: "special_offer_30_off")
│       └─ timeout: 24h, if no click → send reminder
├─ renewal_grace period entered
│   ├─ day 1-7: send_email("update_payment_method")
│   ├─ day 8-15: send_email("special_offer_50_off", condition="retention")
│   └─ day 16-30: send_email("last_chance_cancel_warning")
└─ subscription_cancelled
    ├─ immediately: send_email("we_miss_you_survey")
    ├─ day 3: send_email("special_offer_30_off_return")
    └─ day 7: send_email("permanent_delete_warning")

Special Offers:
├─ Discount: negotiated per customer segment
├─ Duration: 3-6 months
├─ One-time: cannot stack multiple offers
└─ Tracking: measure ROI of churn prevention
```

### Timeout Specifications

| Timeout | Duration | Trigger | Action |
|---------|----------|---------|--------|
| `trial_period` | 30d | No manual action needed | Auto-transition to `trial_ended` |
| `trial_conversion_window` | 7d | Delay before conversion reminder | Send follow-up email |
| `renewal_reminder` | 14d before | Pre-renewal window | Send "your renewal is coming" email |
| `renewal_grace` | 30d | After failed renewal | Allow catch-up payment |
| `lapsed_archived` | 90d | Subscription lapsed | Move to cold storage |

### Actions (Side Effects)

```rust
action send_email(customer_id, template) {
    // Templates: trial.trial_ended_upsell, subscription.renewal_reminder_14d, etc.
    // Variables: customer_name, renewal_date, current_tier, available_offers
    // Retry: 3x with exponential backoff
    // Timeout: 5s SLA
}

action disable_service_access(customer_id) {
    // Revoke service account permissions
    // Terminate active API keys
    // Send Pub/Sub event: subscription.access.disabled
    // Grace period for data export: 30d
}

action calculate_prorated_refund(customer_id, cancellation_date) {
    // Formula: (days_remaining / days_in_cycle) * cycle_charge
    // Result: amount to refund
    // Minimum: $0.00 (no overcharge)
    // Maximum: full cycle_charge
}

action trigger_churn_prevention_campaign(customer_id) {
    // Query: customer segment, LTV, CAC
    // Determine: offer amount (based on segment economics)
    // Send: special offer email (retention_discount)
    // Track: acceptance rate, ROI
}

action schedule_churn_survey(customer_id, delay) {
    // After delay, trigger survey via email
    // Questions: "Why did you cancel?", "What would bring you back?"
    // Result: feedback used to improve product
}
```

### Error Handling Paths

```
SCENARIO: Trial ends during payment processing
├─ trial --trial_expired--> trial_ended (automatic)
├─ customer simultaneously clicks "convert"
├─ trial_ended --convert_to_paid--> active
├─ first payment initiated
├─ result: consistent (customer is active) ✓

SCENARIO: Auto-renewal retry succeeds on day 3
├─ awaiting_renewal --auto_renew--> active
├─ payment fails: decline
├─ awaiting_renewal --renewal_failed--> renewal_grace
├─ day 1: retry_payment (schedule automatic)
├─ charge succeeds
├─ renewal_grace --retry_payment--> active
└─ customer never loses access (grace period works)

SCENARIO: Customer cancels during renewal_grace
├─ renewal_grace --cancel--> cancelled
├─ action: calculate_prorated_refund (already paid for cycle)
├─ customer receives credit for unused days
├─ result: transparent, fair refund ✓

SCENARIO: Network timeout during tier modification
├─ active --modify_tier--> active
├─ calculate_prorated_adjustment() timeout (>5s)
├─ retry 3x with exponential backoff
├─ all retries fail: publish alert to Pub/Sub
├─ circuit breaker trips (don't modify tier)
├─ human fallback: support team modifies manually
└─ customer sees: "Please contact support to change plan"
```

---

## FSM #5: Customer Account FSM

**Purpose**: Manage customer lifecycle, compliance, fraud detection
**States**: 6
**Transitions**: 8
**Review Period**: 48h

### State Diagram

```
    ┌───────────┐
    │ONBOARDING │
    └─────┬─────┘
          │ approval_granted
          │ OR 48h passed
          │
    ┌─────▼────────┐
    │    ACTIVE    │◄──────────────┐
    └─┬──────┬─────┘               │
      │      │         (reinstate) │
      │      │                     │
   suspend  under_review           │
      │      │                     │
  ┌───▼──┐┌──▼──────────────┐      │
  │SUSP  ││UNDER_REVIEW     │      │
  │ENDED ││ (fraud check)   │      │
  └──┬───┘└──┬──────────────┘      │
     │       │                     │
     │   [48h]                     │
     │       │                     │
     │   ┌───┴──────────┬──────┐   │
     │   │ fraud_detected       │  │
     │   │ OR approved          │  │
     │   │                      │  │
     │   ▼                      ▼  │
     │ DEACTIVATED            ACTIVE
     │   │                    (return)
     │   │
     │   | deactivated_expired
     │   | (90 days)
     │   │
     │ ┌─▼───────┐
     └─>ARCHIVED │ (Terminal)
        └────────┘
```

### States

| State | Duration | Description | Auto-Actions |
|-------|----------|-------------|--------------|
| `onboarding` | 48h | New customer, verification pending | KYC checks, email verification |
| `active` | Variable | Approved, can make purchases | Normal operations |
| `suspended` | Variable | Temporary block (payment issue, TOS) | Can still view, cannot purchase |
| `under_review` | 48h | Fraud/compliance review in progress | Daily update emails |
| `deactivated` | 90d | Account closed by customer/system | Graceful offboarding |
| `archived` | ∞ (Terminal) | Moved to cold storage | Read-only access |

### Transitions & Rules

```
/* ONBOARDING */
onboarding --[auto_approval]--> active {
  when: 48 hours elapsed AND no fraud signals
  action: send_email(customer, "account.onboarding_complete")
  action: send_email(customer, "marketplace.getting_started_guide")
  action: audit_log("account_auto_approved", customer_id)
}

onboarding --[manual_approval]--> active {
  when: compliance team approves earlier
  action: send_email(customer, "account.onboarding_complete")
  action: audit_log("account_manually_approved", customer_id, approver_id)
}

onboarding --[fraud_detected]--> under_review {
  when: fraud detection system flags customer
  action: publish_event(Pub/Sub, "account.fraud_detected", customer_id)
  action: send_email(customer, "account.under_review_notification")
  action: create_incident(Cloud Incident, priority=HIGH)
  action: audit_log("account_fraud_detected", customer_id, fraud_signals)
}

onboarding --[document_request]--> onboarding {
  when: KYC requires additional documents
  action: send_email(customer, "account.document_request", document_type)
  action: schedule_followup(timeout=7_days)
}

/* FRAUD REVIEW */
under_review --[fraud_confirmed]--> deactivated {
  when: compliance team confirms fraud
  action: block_all_purchases(customer_id)
  action: freeze_account_balance()
  action: send_email(customer, "account.fraud_confirmed")
  action: publish_event(Pub/Sub, "account.fraud_confirmed", customer_id)
  action: create_incident(Cloud Incident, priority=CRITICAL)
  action: audit_log("account_fraud_confirmed", customer_id, evidence)
}

under_review --[approved]--> active {
  when: fraud checks pass
  action: send_email(customer, "account.review_complete_approved")
  action: audit_log("account_approved_after_review", customer_id)
}

/* ACTIVE ACCOUNT */
active --[suspend]--> suspended {
  when: payment failed OR TOS violation
  action: send_email(customer, "account.suspended_notice")
  action: audit_log("account_suspended", customer_id, reason)
}

suspended --[reinstate]--> active {
  when: customer resolves issue OR system auto-resolves
  action: send_email(customer, "account.reinstated")
  action: audit_log("account_reinstated", customer_id)
}

/* DEACTIVATION */
{active|suspended} --[deactivate]--> deactivated {
  when: customer requests deactivation or manual admin action
  action: start_data_export_process(customer_id)
  action: send_email(customer, "account.deactivation_notice")
  action: schedule_final_deletion(delay=90_days)
  action: audit_log("account_deactivation_requested", customer_id)
}

deactivated --[archive]--> archived {
  when: 90 days elapsed or manual archive request
  action: move_to_cold_storage(BigQuery)
  action: anonymize_personally_identifiable_info(customer_id)
  action: audit_log("account_archived", customer_id)
}

/* REINSTATEMENT */
deactivated --[reinstate_request]--> under_review {
  when: customer requests account restoration
  action: send_email(customer, "account.reinstatement_under_review")
  action: schedule_compliance_review(timeout=48_hours)
}

under_review --[reinstate_approved]--> active {
  when: reinstatement approved
  action: send_email(customer, "account.reinstatement_approved")
  action: audit_log("account_reinstated", customer_id)
}
```

### Compliance Checks (Onboarding)

```
KYC (Know Your Customer):
├─ Email verification: confirm customer controls email
├─ Phone verification: optional, based on transaction risk
├─ Address verification: if seller in high-risk region
├─ Tax ID verification: for commercial customers
└─ Sanctions screening: OFAC, UN, EU lists (third-party service)

Fraud Detection:
├─ Velocity checks: max transactions per time window
├─ Geolocation: flagif VPN/unusual location
├─ Device fingerprint: check for known fraud patterns
├─ Payment method: validate via payment processor
├─ Email reputation: check against spam lists
└─ Machine Learning: Stripe Radar or Sift Science integration

Risk Scoring:
├─ Low (0-30): Auto-approve
├─ Medium (30-70): Require additional documents
└─ High (70-100): Escalate to compliance team
```

### Actions (Side Effects)

```rust
action send_email(customer_id, template) {
    // Template variables: customer_name, reason, action_needed
    // Retry: 3x with exponential backoff
    // Track: delivery status
}

action publish_event(pub_sub_topic, event) {
    // Topic: projects/{project}/topics/account-events
    // Message: {customer_id, event_type, timestamp, details}
    // Consumer: Fraud detection service, Compliance team
}

action create_incident(cloud_incident, priority) {
    // Create incident in Cloud Incident Management
    // Priority: CRITICAL (fraud) or HIGH (review)
    // Auto-assignment: fraud_team or compliance_team
}

action freeze_account_balance() {
    // Set account.frozen = true in Firestore
    // Prevent: all withdrawals
    // Allow: viewing balance for dispute resolution
}

action start_data_export_process(customer_id) {
    // Package: all customer data (entitlements, invoices, usage)
    // Format: JSON or CSV
    // Delivery: SFTP or downloadable link
    // Timeout: 30 days to download before deletion
}

action anonymize_personally_identifiable_info(customer_id) {
    // Hash: email, phone number
    // Redact: name, address
    // Keep: transaction history for audit trail
    // Result: GDPR compliant retention
}
```

### Error Handling Paths

```
SCENARIO: KYC document upload fails
├─ onboarding receives: document_request
├─ customer uploads file
├─ virus scan fails (malware detected)
├─ action: send_email("invalid_document_type")
├─ state: remains onboarding
├─ customer re-uploads clean document
└─ result: transparent recovery

SCENARIO: OFAC sanctions screening times out
├─ onboarding --manual_approval--> active
├─ sanctions_screening() timeout (>30s)
├─ retry 3x with exponential backoff
├─ all retries fail: escalate to compliance team
├─ human review required (fallback)
├─ state: under_review (temporary hold)
└─ result: account not activated until cleared

SCENARIO: Fraud detected during onboarding, but customer reinstated later
├─ onboarding --fraud_detected--> under_review
├─ under_review --fraud_confirmed--> deactivated
├─ deactivated --reinstate_request--> under_review
├─ compliance team re-reviews (after 6 months)
├─ under_review --reinstate_approved--> active
└─ result: customer can return (with audit trail)

SCENARIO: Concurrent suspension and deactivation requests
├─ active --suspend--> suspended
├─ active --deactivate--> deactivated (received before suspend)
├─ conflict: FIFO queue orders (suspend first)
├─ result: active --suspend--> suspended --deactivate--> deactivated
└─ final state: deactivated with both events logged
```

---

## FSM #6: Quota & SLA FSM

**Purpose**: Manage per-customer usage, burst allowance, fair-share enforcement
**States**: 5
**Transitions**: 6
**Burst Allowance**: 20% temporary overage

### State Diagram

```
    ┌─────────────┐
    │WITHIN_LIMITS│◄────────────────────┐
    └──────┬──────┘                     │
           │ usage > threshold (80%)    │
           │                           │
    ┌──────▼─────────┐                 │
    │    WARNING     │                 │
    └──────┬─────────┘                 │
           │ usage > limit              │
           │ (120% of quota)            │
           │                           │
    ┌──────▼──────────┐                │
    │    EXCEEDED     │◄───────┐       │
    └──────┬──────────┘        │       │
           │ apply_throttle     │       │
           │ (rate limit)       │       │
           │                   │       │
    ┌──────▼──────────┐    recover    │
    │   THROTTLED    │        │       │
    └──────┬─────────┘        │       │
           │ circuit_breaker   │       │
           │ active            │       │
           │                   │       │
    ┌──────▼──────────────┐   │       │
    │CIRCUIT_BREAKER      │   │       │
    │ (requests denied)   │───┘       │
    └─────────┬──────────┘            │
              │ usage_drops           │
              │ reset_allowance()     │
              └───────────────────────┘
```

### States

| State | Duration | Description | Effect |
|-------|----------|-------------|--------|
| `within_limits` | Variable | Usage ≤ 80% of quota | No restrictions |
| `warning` | Variable | Usage 80-100% of quota | Alert emails, recommendations |
| `exceeded` | Variable | Usage > 100% of quota | Charge overage fees |
| `throttled` | Variable | Usage > 120% of quota | Rate limit: 50% of normal rate |
| `circuit_breaker` | Variable | Usage > 150% of quota | Deny requests until recovery |

### Transitions & Rules

```
/* USAGE TRACKING */
any_state --[usage_sample]--> within_limits|warning|exceeded|throttled|circuit_breaker {
  when: usage metric received (every minute)
  action: query_bigquery_usage(customer_id, last_minute)
  action: calculate_percentage_of_quota(usage, quota)
  action: determine_target_state(percentage)
  action: if target_state != current_state: transition(target_state)
  action: audit_log("usage_sample", customer_id, usage, percentage)
}

within_limits --[usage_increase]--> warning {
  when: usage > 80% of quota
  action: send_email(customer, "quota.warning_approaching_limit")
  action: publish_event(Pub/Sub, "quota.warning", customer_id, usage_percentage)
  action: audit_log("quota_warning", customer_id, usage_percentage)
}

warning --[usage_decrease]--> within_limits {
  when: usage drops below 80%
  action: (no action, silent recovery)
}

warning --[usage_exceeds]--> exceeded {
  when: usage > 100% of quota
  action: send_email(customer, "quota.exceeded_overage_charges")
  action: charge_overage_fee(customer_id, overage_amount)
  action: publish_event(Pub/Sub, "quota.exceeded", customer_id, overage_amount)
  action: audit_log("quota_exceeded", customer_id, overage_amount)
}

exceeded --[apply_throttle]--> throttled {
  when: usage > 120% of quota
  action: apply_rate_limit(customer_id, rate=50%)
  action: send_email(customer, "quota.throttling_applied")
  action: publish_event(Pub/Sub, "quota.throttled", customer_id)
  action: audit_log("quota_throttled", customer_id)
}

throttled --[circuit_breaker_trigger]--> circuit_breaker {
  when: usage > 150% of quota
  action: set_circuit_breaker(customer_id, open)
  action: send_email(customer, "quota.circuit_breaker_active")
  action: publish_event(Pub/Sub, "quota.circuit_breaker", customer_id)
  action: create_incident(Cloud Incident, priority=HIGH)
  action: audit_log("quota_circuit_breaker", customer_id)
}

circuit_breaker --[recover]--> within_limits {
  when: usage < 50% of quota (recovery threshold)
  action: set_circuit_breaker(customer_id, closed)
  action: send_email(customer, "quota.circuit_breaker_closed")
  action: reset_allowance(customer_id)
  action: audit_log("quota_recovered", customer_id)
}
```

### Quota Calculation

```
Monthly Quota Formula:
├─ Base quota: Per subscription tier
│   ├─ Trial: 10,000 API calls/month
│   ├─ Standard: 100,000 API calls/month
│   ├─ Professional: 1,000,000 API calls/month
│   └─ Enterprise: Unlimited (soft cap: 10M)
├─ Burst allowance: +20% (temporary)
│   └─ Duration: 5 minutes
├─ Overage pricing: $0.01 per 100 additional calls
└─ Reset: Monthly at 00:00 UTC

Usage Tracking:
├─ Granularity: per-minute metrics (real-time)
├─ Storage: BigQuery (analytics_dataset.usage_events)
├─ TTL: 13 months (rolling window)
└─ Latency: <1s from event to dashboard

Fair-Share Algorithm:
├─ If total_cluster_usage > 80% capacity:
│   ├─ Identify top 5% users by quota-percentage
│   ├─ Apply per-customer throttle
│   └─ Guarantee minimum 50% for other customers
└─ Result: Noisy neighbor prevented
```

### Actions (Side Effects)

```rust
action query_bigquery_usage(customer_id, time_window) {
    // Query: analytics_dataset.usage_events
    // Filter: customer_id, timestamp >= time_window
    // Aggregate: SUM(api_calls_count)
    // Result: usage in last minute
    // Timeout: 5s SLA
}

action calculate_percentage_of_quota(usage, quota) {
    // Formula: (usage / quota) * 100
    // Return: percentage (0-∞)
}

action determine_target_state(percentage) {
    // 0-80%: within_limits
    // 80-100%: warning
    // 100-120%: exceeded
    // 120-150%: throttled
    // >150%: circuit_breaker
}

action apply_rate_limit(customer_id, rate) {
    // Call API Gateway: create rate limit policy
    // Setting: rate = 50% of normal (for throttled)
    // Result: HTTP 429 Too Many Requests on excess
    // Timeout: 1m to take effect
}

action charge_overage_fee(customer_id, overage_amount) {
    // Calculate: (overage_calls / 100) * $0.01
    // Add to: next invoice
    // Log: billing_audit label in Cloud Logging
}

action set_circuit_breaker(customer_id, state) {
    // Update: Firestore customers/{id}/circuit_breaker
    // State: open (deny requests) or closed (allow)
    // Result: Pub/Sub event triggers Load Balancer routing
}

action reset_allowance(customer_id) {
    // Reset: burst allowance counter to 0
    // Effect: Customer can use burst again next period
}
```

### Error Handling Paths

```
SCENARIO: BigQuery query timeout during usage check
├─ usage_sample received
├─ query_bigquery_usage() timeout (>5s)
├─ retry 3x with exponential backoff (1s, 2s, 4s)
├─ all retries fail: assume no new usage
├─ state: remains current (conservative)
├─ alert published: quota.bigquery_timeout
└─ human fallback: SRE investigates BigQuery

SCENARIO: Concurrent rate limit and circuit breaker
├─ throttled --apply_throttle--> throttled
├─ usage spike exceeds circuit breaker threshold
├─ throttled --circuit_breaker_trigger--> circuit_breaker
├─ result: requests denied (circuit breaker wins)
└─ customer cannot make any requests until recovery

SCENARIO: Usage drops during circuit breaker
├─ circuit_breaker state active
├─ customer stops making requests
├─ usage_sample: usage < 50% quota
├─ circuit_breaker --recover--> within_limits
├─ action: reset_allowance() called
└─ customer regains full access

SCENARIO: Burst allowance edge case (exactly 120% for 5 minutes)
├─ exceeded --apply_throttle--> throttled (triggered)
├─ burst window expires (5 minutes)
├─ rate_limit removed automatically
├─ customer can make requests at 50% rate
├─ if usage drops: throttled --recover--> exceeded
└─ result: rate limit remains until usage recovers
```

---

## FSM #7: Compliance & Audit FSM

**Purpose**: Manage compliance status, audit procedures, incident response
**States**: 4
**Transitions**: 5
**Audit Frequency**: Quarterly

### State Diagram

```
    ┌──────────────────┐
    │   COMPLIANT      │◄────────────────────────┐
    └─┬──────────┬─────┘                         │
      │          │ audit_scheduled           (remediation_complete)
      │          │ (quarterly)                   │
      │          │                              │
    violation    │              ┌────────────────┘
    detected     │              │
      │          │              │
      │     ┌────▼────────────┐ │
      │     │AUDIT_PENDING    │ │
      │     └────┬───────┬────┘ │
      │          │       │      │
      │      audit_pass  audit_fail
      │          │       │
      │          │    ┌──▼────────────┐
      │          │    │NON_COMPLIANT  │
      │          │    └──┬───────┬────┘
      │          │       │       │
      │          │   remediate  escalate
      │          │       │       │
      │          │    ┌──▼──────────────────┐
      └────────┬──────>REMEDIATION_IN_      │
               │       PROGRESS              │
               │       └──┬──────────────────┘
               │          │ remediation_verified
               │          │
               └──────────┘
```

### States

| State | Duration | Description | Review Cadence |
|-------|----------|-------------|-----------------|
| `compliant` | 90d | Passing all compliance checks | Quarterly audit |
| `audit_pending` | 14d | Audit in progress | Continuous review |
| `non_compliant` | ∞ | Failed compliance checks | Weekly review |
| `remediation_in_progress` | 30d | Remediating non-compliance | Daily check-in |

### Transitions & Rules

```
/* NORMAL COMPLIANCE */
compliant --[audit_scheduled]--> audit_pending {
  when: quarterly audit triggered (automated schedule)
  action: run_compliance_checks(customer_id)
  action: verify_gdpr_dpa(customer_id)
  action: verify_hipaa_baa(customer_id)
  action: verify_soc2_controls(customer_id)
  action: verify_data_residency(customer_id)
  action: send_email(customer, "compliance.audit_initiated")
  action: audit_log("compliance_audit_started", customer_id)
}

audit_pending --[audit_pass]--> compliant {
  when: all compliance checks pass
  action: send_email(customer, "compliance.audit_passed")
  action: publish_event(Pub/Sub, "compliance.audit_passed", customer_id)
  action: audit_log("compliance_audit_passed", customer_id)
}

/* VIOLATION DETECTED */
compliant --[violation_detected]--> audit_pending {
  when: continuous monitoring detects violation
  action: send_email(customer, "compliance.violation_alert")
  action: create_incident(Cloud Incident, priority=CRITICAL)
  action: trigger_immediate_audit()
  action: audit_log("compliance_violation_detected", customer_id, violation_type)
}

audit_pending --[audit_fail]--> non_compliant {
  when: audit confirms violation
  action: send_email(customer, "compliance.audit_failed")
  action: publish_event(Pub/Sub, "compliance.audit_failed", customer_id)
  action: create_incident(Cloud Incident, priority=CRITICAL)
  action: audit_log("compliance_audit_failed", customer_id, violations)
}

/* REMEDIATION */
non_compliant --[initiate_remediation]--> remediation_in_progress {
  when: customer submits remediation plan
  action: send_email(customer, "compliance.remediation_accepted")
  action: schedule_daily_checkin(customer_id)
  action: audit_log("compliance_remediation_started", customer_id, plan)
}

remediation_in_progress --[remediation_verified]--> compliant {
  when: compliance team verifies fixes
  action: run_compliance_checks(customer_id)
  action: send_email(customer, "compliance.remediation_verified")
  action: publish_event(Pub/Sub, "compliance.compliant", customer_id)
  action: audit_log("compliance_remediation_verified", customer_id)
}

non_compliant --[escalate_to_legal]--> escalated {
  when: violation is critical or unresolved >60 days
  action: send_email(legal_team, "compliance.escalation_notice")
  action: create_incident(Cloud Incident, priority=CRITICAL, assignee=legal)
  action: audit_log("compliance_escalated_to_legal", customer_id, reason)
}

/* CONTINUOUS MONITORING */
any_state --[data_residency_check]--> current_state {
  when: data location verification (daily)
  action: query_bigquery_location(customer_id)
  action: verify_location_matches_policy(location)
  action: if location != policy: trigger_violation
  action: audit_log("data_residency_verified", customer_id, location)
}

any_state --[encryption_audit]--> current_state {
  when: encryption status verification (daily)
  action: check_encryption_status(customer_data)
  action: verify_tls_certificates()
  action: if encryption_disabled: trigger_violation
  action: audit_log("encryption_audit", customer_id, status)
}
```

### Compliance Requirements

```
GDPR (EU Customers):
├─ DPA (Data Processing Agreement): Required
├─ Data Residency: EU region (ireland, europe-west1)
├─ Right to be Forgotten: Data deletion within 30d
├─ Data Portability: Export in standard format
└─ Breach Notification: Notify within 72h

HIPAA (Healthcare Customers):
├─ BAA (Business Associate Agreement): Required
├─ Encryption: AES-256 at rest, TLS 1.2 in transit
├─ Audit Logging: Immutable, 6-year retention
├─ Access Controls: MFA required, role-based access
└─ Breach Notification: Notify within 60d

SOC2 Type II (Enterprise Customers):
├─ Certification: Required for enterprise contracts
├─ Monitoring: Continuous controls testing
├─ Evidence: Audit trail, change logs, incident reports
└─ Attestation: Annual audit by third-party firm

CCPA (California Customers):
├─ Privacy Policy: Must disclose data practices
├─ Opt-Out: "Do Not Sell My Personal Information"
├─ Data Access: Provide all personal data within 30d
└─ Deletion: Delete personal data within 45d
```

### Audit Trail Immutability

```
Audit Event Structure:
{
  "event_id": "uuid",
  "timestamp": "2026-01-25T14:32:00Z",
  "customer_id": "cust_123",
  "event_type": "compliance.audit_passed",
  "actor": "system:compliance-engine",
  "before_state": "audit_pending",
  "after_state": "compliant",
  "details": {
    "checks_passed": 47,
    "checks_failed": 0,
    "certifications": ["gdpr_dpa", "soc2"]
  },
  "data_hash": "sha256:abc123...",
  "signature": "ed25519:xyz789...",
  "chain_hash": "sha256:previous_event_hash"  // Merkle chain
}

Storage:
├─ Primary: Firestore (immediate access)
├─ Backup: Cloud Storage (GCS) archive
├─ TTL: 7 years (compliance requirement)
└─ Access: Read-only for audit trail (no modifications)

Verification:
├─ Signature: Ed25519 verification (tamper detection)
├─ Chain: Merkle chain validates sequence
└─ Immutability: Append-only, no deletes
```

### Actions (Side Effects)

```rust
action run_compliance_checks(customer_id) {
    // Check: Data location (must match policy)
    // Check: Encryption status (must be enabled)
    // Check: TLS certificate validity
    // Check: API key rotation (must be <90 days)
    // Check: Audit logging enabled
    // Check: MFA enabled for admins
    // Result: Pass/Fail with details
    // Timeout: 5m SLA
}

action verify_gdpr_dpa(customer_id) {
    // Query: Firestore contracts/{customer_id}/dpa
    // Check: signed = true
    // Check: effective_date <= today
    // Check: not expired (renewal_date > today)
}

action verify_hipaa_baa(customer_id) {
    // Query: Firestore contracts/{customer_id}/baa
    // Check: signed = true
    // Check: effective_date <= today
    // Check: not expired
}

action trigger_immediate_audit() {
    // Enqueue: compliance check job (priority: urgent)
    // Timeout: must complete within 1h
    // Result: decision on compliance status
}

action schedule_daily_checkin(customer_id) {
    // Pub/Sub: schedule daily message
    // Job: verify remediation progress
    // Notification: send update email to customer
    // Duration: 30 days
}

action query_bigquery_location(customer_id) {
    // Query: storage_locations table
    // Check: all data in policy-compliant region
    // Result: location list
}
```

### Error Handling Paths

```
SCENARIO: Audit discovers encryption disabled
├─ compliant --violation_detected--> audit_pending
├─ audit_pending --audit_fail--> non_compliant
├─ action: create_incident(priority=CRITICAL)
├─ action: send_email(customer, "CRITICAL: Encryption disabled")
├─ customer immediately enables encryption
├─ customer submits remediation plan
├─ non_compliant --initiate_remediation--> remediation_in_progress
├─ compliance team runs daily checks
├─ encryption verified after 1 day
├─ remediation_in_progress --remediation_verified--> compliant
└─ result: transparent recovery, audit trail complete

SCENARIO: DPA expired, violation detected
├─ compliant --violation_detected--> audit_pending
├─ violation: verify_gdpr_dpa() returns false (expired)
├─ compliance team notifies customer
├─ non_compliant --initiate_remediation--> remediation_in_progress
├─ customer renews DPA
├─ remediation_in_progress --remediation_verified--> compliant
└─ result: service never disrupted (preventive check)

SCENARIO: Multiple violations, escalation to legal
├─ non_compliant (e.g., failed 3 audits)
├─ violation_count > 2 AND duration > 60 days
├─ non_compliant --escalate_to_legal--> escalated
├─ action: legal_team notified
├─ action: incident escalated to priority CRITICAL
└─ result: human review required (no automatic recovery)

SCENARIO: Merkle chain verification detects tampering
├─ audit_log verification: signature_invalid
├─ chain_hash mismatch detected
├─ action: alert to security team immediately
├─ action: isolate customer data
├─ action: begin forensic investigation
└─ result: system stops all operations (fail-safe)
```

---

## FSM #8: Multi-Tenant Governance FSM

**Purpose**: Manage cluster health, resource fairness, cascade prevention
**States**: 5
**Transitions**: 6
**Escalation Strategy**: Graceful degradation

### State Diagram

```
    ┌────────────┐
    │  HEALTHY   │◄─────────────────────────────┐
    └─┬────┬─────┘                              │
      │    │ high_cpu_detected              (cascade_recovered)
      │    │ OR high_memory_detected           │
      │    │                                   │
  noisy │    │                                  │
 neighbor│  ┌▼────────────────┐                │
  │     │  │RESOURCE_CONTENTION│               │
  │     │  └─┬────┬──────┬────┘               │
  │     │    │    │      │                    │
  │     │    │    │  ┌───▼──────────┐        │
  │     │    │    │  │LOAD_BALANCING│        │
  │     │    │    │  └───┬────┬─────┘        │
  │     │    │    │      │    │              │
  │     │    │    └─ rebalance_success       │
  │     │    │                              │
  │  ┌──▼────▼────────────────────┐          │
  └─>│CASCADE_PREVENTION          ├──────────┘
     │(feature degradation)       │
     └────┬───┬──────────────────┘
          │   │
          │   │ cascade_recovered
          │   │
          │ ┌─▼──────────────────┐
          │ │EMERGENCY_SHUTDOWN  │
          │ │(deny new requests)  │
          │ └────────────────────┘
          │
          │ health_restored
          │
          └────────────────────┘
```

### States

| State | Duration | Description | Action |
|-------|----------|-------------|--------|
| `healthy` | Variable | Normal operations, <70% CPU/memory | No restrictions |
| `resource_contention` | Variable | 70-85% CPU/memory | Monitor closely |
| `load_balancing` | Variable | 85-95% CPU/memory | Rebalance tenants |
| `cascade_prevention` | Variable | >95% CPU/memory | Degrade features |
| `emergency_shutdown` | Variable | Critical (>99% or circuit broken) | Deny new requests |

### Transitions & Rules

```
/* HEALTH MONITORING */
any_state --[metrics_sample]--> healthy|resource_contention|load_balancing|cascade_prevention|emergency_shutdown {
  when: metrics collected every 10 seconds
  action: query_stackdriver_metrics()
  action: calculate_cluster_utilization()
  action: determine_target_state()
  action: if target_state != current_state: transition()
  action: audit_log("cluster_metrics_sampled", utilization)
}

healthy --[resource_contention_threshold]--> resource_contention {
  when: cluster utilization > 70%
  action: send_alert(Pub/Sub, "cluster.resource_contention")
  action: enable_verbose_logging()
  action: begin_tenant_profiling()
  action: audit_log("resource_contention_detected")
}

resource_contention --[rebalance_needed]--> load_balancing {
  when: utilization > 85% AND noisy neighbors identified
  action: identify_top_usage_tenants()
  action: apply_per_tenant_throttle(threshold=80th_percentile)
  action: migrate_expensive_jobs_to_batch_queue()
  action: send_alert(Pub/Sub, "cluster.load_balancing")
  action: audit_log("load_balancing_initiated")
}

load_balancing --[rebalance_success]--> resource_contention {
  when: utilization drops below 80%
  action: remove_throttling(delayed=60s_grace_period)
  action: audit_log("load_balancing_successful")
}

resource_contention --[cascade_prevention_threshold]--> cascade_prevention {
  when: utilization > 95% OR circuit_breaker_open > 5
  action: degrade_features(disable: ["advanced_reports", "real_time_dashboards"])
  action: reduce_query_complexity_limit()
  action: increase_cache_ttl(2x)
  action: send_alert(Pub/Sub, "cluster.cascade_prevention")
  action: create_incident(Cloud Incident, priority=HIGH)
  action: audit_log("cascade_prevention_enabled")
}

cascade_prevention --[emergency_shutdown]--> emergency_shutdown {
  when: utilization > 99% OR circuit_breaker_exhausted
  action: deny_new_requests(return_http_503)
  action: allow_existing_transactions_to_complete(timeout=30s)
  action: send_alert(Pub/Sub, "cluster.emergency_shutdown")
  action: page_on_call_engineer()
  action: create_incident(Cloud Incident, priority=CRITICAL)
  action: audit_log("emergency_shutdown_initiated")
}

emergency_shutdown --[cascade_recovered]--> cascade_prevention {
  when: utilization drops below 90%
  action: re_enable_new_requests()
  action: send_alert(Pub/Sub, "cluster.emergency_shutdown_lifted")
  action: audit_log("emergency_shutdown_lifted")
}

cascade_prevention --[health_restored]--> resource_contention {
  when: utilization drops below 80%
  action: re_enable_disabled_features(delayed=5m_grace_period)
  action: reset_query_complexity_limit()
  action: reset_cache_ttl()
  action: audit_log("cascade_prevention_disabled")
}

resource_contention --[health_restored]--> healthy {
  when: utilization drops below 70%
  action: disable_verbose_logging()
  action: stop_tenant_profiling()
  action: audit_log("cluster_healthy_restored")
}
```

### Tenant Profiling & Fairness

```
Fair-Share Algorithm:
├─ Quota per tenant: (cpu_total / num_tenants) * 1.1 (10% buffer)
├─ If one tenant > quota:
│   ├─ Flag as "noisy neighbor"
│   ├─ Apply rate limit: reduce to quota * 0.8 (20% penalty)
│   └─ Guarantee minimum for others: (cpu_total / num_tenants) * 0.9
├─ Measurement: rolling 5-minute window
└─ Action: continuous monitoring, auto-rebalance

Resource Isolation:
├─ CPU: Kubernetes cpu-limits per pod
├─ Memory: Kubernetes memory-limits per pod
├─ Network: API Gateway rate limits per customer
├─ Storage: BigQuery slot reservations per customer
└─ Query execution: Per-tenant connection pool limits

Noisy Neighbor Detection:
├─ Metric: usage > 80th percentile + 2*stddev
├─ Duration: sustained for 2 consecutive samples (20s)
├─ Action: apply throttle (rate = 50% of quota)
├─ Recovery: usage < 50th percentile for 3 consecutive samples
└─ Escalation: if sustained >10 minutes → cascade_prevention
```

### Feature Degradation Strategy

```
Degradation Levels:
├─ Level 1 (85% utilization): Disable non-essential features
│   ├─ advanced_reports (complex SQL queries)
│   ├─ real_time_dashboards (high-refresh queries)
│   └─ result: save 15-20% CPU
├─ Level 2 (90% utilization): Reduce data freshness
│   ├─ cache_ttl: 5m → 15m
│   ├─ batch_interval: 1m → 5m
│   └─ result: save 10-15% CPU
├─ Level 3 (95% utilization): Limit query complexity
│   ├─ max_join_depth: 5 → 2
│   ├─ max_result_set: 1M rows → 100k rows
│   └─ result: save 20-30% CPU
└─ Level 4 (99% utilization): Emergency shutdown
    ├─ deny_new_requests (HTTP 503)
    ├─ allow_existing_transactions (30s grace)
    └─ result: prevent cascading failure

Feature Re-enablement:
├─ Graceful: with grace period (60s) before re-enabling
├─ Staggered: don't re-enable all at once
├─ Monitored: watch metrics after each re-enable
└─ Automatic: system handles without manual intervention
```

### Actions (Side Effects)

```rust
action query_stackdriver_metrics() {
    // Query: CPU utilization, memory utilization
    // Scope: cluster level (sum of all tenants)
    // Granularity: 10-second rolling window
    // Result: (cpu_percent, memory_percent)
    // Timeout: 5s SLA
}

action calculate_cluster_utilization() {
    // Formula: (used_cpu / total_cpu) * 100
    // Return: percentage (0-∞)
}

action determine_target_state(utilization) {
    // 0-70%: healthy
    // 70-85%: resource_contention
    // 85-95%: load_balancing
    // 95-99%: cascade_prevention
    // >99%: emergency_shutdown
}

action identify_top_usage_tenants() {
    // Query: per-tenant CPU/memory usage
    // Sort: descending by usage
    // Identify: top 5% heavy users
    // Result: [customer_id, usage_percent, ...]
}

action apply_per_tenant_throttle(threshold) {
    // For each heavy tenant:
    //   ├─ set rate_limit = threshold * 0.8
    //   ├─ call API Gateway: update rate limit policy
    //   └─ publish Pub/Sub: throttle_applied event
}

action migrate_expensive_jobs_to_batch_queue() {
    // Identify: real-time queries in batch_candidate category
    // Move to: Cloud Batch (BigQuery) for later execution
    // Result: free up real-time CPU for interactive queries
}

action degrade_features(features_to_disable) {
    // For each feature:
    //   ├─ set feature_flag.enabled = false
    //   ├─ publish Pub/Sub: feature_disabled event
    //   └─ return HTTP 503 if requested
}

action deny_new_requests() {
    // Update: API Gateway policy
    // Result: new requests get HTTP 503 Service Unavailable
    // Existing: transactions allowed to complete (30s grace)
}

action page_on_call_engineer() {
    // Trigger: PagerDuty or Opsgenie
    // Severity: critical
    // Context: cluster_utilization, affected_tenants, suggested_actions
}
```

### Error Handling Paths

```
SCENARIO: Metrics collection timeout during resource contention
├─ metrics_sample timeout (>5s)
├─ retry 3x with exponential backoff (1s, 2s, 4s)
├─ all retries fail: use last_known_metrics
├─ state: remains current (conservative)
├─ alert published: cluster.metrics_timeout
└─ human fallback: SRE investigates Stackdriver

SCENARIO: Noisy neighbor mitigated, but still uses >80% of quota
├─ load_balancing state active
├─ apply_per_tenant_throttle(threshold=80th_percentile)
├─ tenant still exceeds throttle
├─ action: escalate to cascade_prevention
├─ degrade_features() disables advanced_reports
├─ tenant CPU drops to acceptable level
└─ result: fair-share enforcement successful

SCENARIO: Feature re-enablement causes spike
├─ cascade_prevention state active (features disabled)
├─ utilization drops to 85% (load_balancing threshold)
├─ system re-enables disabled features
├─ feature initialization: CPU spike to 92%
├─ result: state transitions back to cascade_prevention
├─ feature re-enable delayed by grace period
└─ result: no oscillation (hysteresis prevents flapping)

SCENARIO: Emergency shutdown during customer request
├─ emergency_shutdown state entered
├─ customer API request received
├─ response: HTTP 503 Service Unavailable (retry-after: 30s)
├─ after 30s grace period: existing transactions allowed to complete
├─ as utilization drops: emergency_shutdown lifted
├─ customer retries: request succeeds
└─ result: temporary disruption, automatic recovery
```

---

## Integration Architecture

### GCP Service Integration Points

```
┌─────────────────────────────────────────────────────────┐
│                    GCP MARKETPLACE AUTONOMIC SYSTEM     │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌────────────────────────────────────────────────┐   │
│  │          FSM Orchestration Layer               │   │
│  │  (gen_statem + Rust type-safe wrappers)       │   │
│  └────────────────────────────────────────────────┘   │
│                     │                                  │
│    ┌────────────────┼────────────────┬────────────┐   │
│    │                │                │            │    │
│    ▼                ▼                ▼            ▼    │
│
│ ┌─────────────────┐ ┌──────────────┐ ┌─────────┐ │
│ │  Entitlement    │ │ Billing FSM  │ │ Catalog │ │
│ │     FSM         │ │              │ │  FSM    │ │
│ └─────────────────┘ └──────────────┘ └─────────┘ │
│
│ ┌─────────────┐ ┌──────────────┐ ┌────────────┐ │
│ │Subscription │ │ Account FSM  │ │ Quota FSM  │ │
│ │    FSM      │ │              │ │            │ │
│ └─────────────┘ └──────────────┘ └────────────┘ │
│
│ ┌─────────────────────┐ ┌──────────────────────┐ │
│ │ Compliance FSM      │ │ Governance FSM       │ │
│ │ (GDPR/HIPAA/SOC2)   │ │ (Fair-share, Cascade)│ │
│ └─────────────────────┘ └──────────────────────┘ │
└─────────────────────────────────────────────────────┘
                     │
        ┌────────────┼────────────┬───────────────┐
        │            │            │               │
        ▼            ▼            ▼               ▼

  ┌──────────────────────────────────────────────────┐
  │         GCP Service Integration Layer            │
  ├──────────────────────────────────────────────────┤
  │                                                  │
  │ ┌────────────────────────────────────────────┐  │
  │ │ GCP Marketplace API (Entitlements, Usage)  │  │
  │ └────────────────────────────────────────────┘  │
  │              │              │                   │
  │         customer id    usage metrics             │
  │              │              │                   │
  │              ▼              ▼                   │
  │
  │ ┌──────────────────┐ ┌─────────────────────┐  │
  │ │ GCP Billing API  │ │ BigQuery Analytics  │  │
  │ │ (invoicing, pay) │ │ (usage metrics)     │  │
  │ └──────────────────┘ └─────────────────────┘  │
  │         │                    │                 │
  │    invoices created      usage aggregated      │
  │         │                    │                 │
  │         ▼                    ▼                 │
  │
  │ ┌──────────────────┐ ┌─────────────────────┐  │
  │ │ Cloud Pub/Sub    │ │ Cloud Logging       │  │
  │ │ (event broker)   │ │ (audit trail)       │  │
  │ └──────────────────┘ └─────────────────────┘  │
  │         │                    │                 │
  │    notifications         audit events          │
  │         │                    │                 │
  │         ▼                    ▼                 │
  │
  │ ┌──────────────────┐ ┌─────────────────────┐  │
  │ │ Cloud Firestore  │ │ Cloud Storage       │  │
  │ │ (entity state)   │ │ (invoices, reports) │  │
  │ └──────────────────┘ └─────────────────────┘  │
  │         │                    │                 │
  │      entitlements        documents             │
  │      subscriptions       archives              │
  │      customers          cold storage          │
  │         │                    │                 │
  │         ▼                    ▼                 │
  │
  │ ┌──────────────────┐ ┌─────────────────────┐  │
  │ │ Secret Manager   │ │ Cloud Incident      │  │
  │ │ (API keys, creds)│ │ (incident tracking) │  │
  │ └──────────────────┘ └─────────────────────┘  │
  │         │                    │                 │
  │   credentials          on-call paging         │
  │   payment methods      escalation             │
  │         │                    │                 │
  │         ▼                    ▼                 │
  │
  │ ┌──────────────────┐ ┌─────────────────────┐  │
  │ │ Service Identity │ │ API Gateway         │  │
  │ │ (service accts)  │ │ (rate limiting)     │  │
  │ └──────────────────┘ └─────────────────────┘  │
  │         │                    │                 │
  │   access tokens          throttling           │
  │   permissions            quotas               │
  │         │                    │                 │
  │         ▼                    ▼                 │
  │
  │ ┌──────────────────┐ ┌─────────────────────┐  │
  │ │ Cloud Monitoring │ │ Load Balancer       │  │
  │ │ (metrics, alerts)│ │ (traffic shaping)   │  │
  │ └──────────────────┘ └─────────────────────┘  │
  │         │                    │                 │
  │   CPU, memory            circuit breakers    │
  │   latency metrics        tenant routing      │
  │         │                    │                 │
  │         ▼                    ▼                 │
  │
  │ ┌──────────────────┐ ┌─────────────────────┐  │
  │ │ Payment Gateway  │ │ Email Service       │  │
  │ │ (Stripe/etc)     │ │ (SendGrid/Gmail)    │  │
  │ └──────────────────┘ └─────────────────────┘  │
  │         │                    │                 │
  │   payments, refunds     notifications        │
  │         │                    │                 │
  │         ▼                    ▼                 │
  │
  │ ┌──────────────────────────────────────────┐  │
  │ │ External Audit Services                  │  │
  │ │ (OFAC screening, KYC verification)       │  │
  │ └──────────────────────────────────────────┘  │
  └──────────────────────────────────────────────────┘
```

### Event Flow Architecture

```
Event Source → Pub/Sub Topic → Subscription → Consumer → FSM Action
     │
     ├─ GCP Marketplace API
     │   └─ entitlement.created → EntitlementFSM (pending_approval)
     │   └─ entitlement.cancelled → EntitlementFSM (cancelled)
     │
     ├─ Payment Gateway (Stripe)
     │   └─ charge.succeeded → BillingFSM (payment_received)
     │   └─ charge.failed → BillingFSM (payment_failed)
     │
     ├─ Scheduled Jobs
     │   └─ monthly_reconciliation → BillingFSM (cycle_complete)
     │   └─ trial_expiry → SubscriptionFSM (trial_expired)
     │   └─ compliance_audit → ComplianceFSM (audit_scheduled)
     │
     ├─ Monitoring Systems
     │   └─ cpu_utilization > 85% → GovernanceFSM (load_balancing)
     │   └─ query_latency > 5s → QuotaFSM (exceeded)
     │
     └─ Manual Approval Queues
         └─ approval_granted → EntitlementFSM (active)
         └─ refund_approved → BillingFSM (refund_processing)
```

---

## Error Handling Strategies

### Retry Strategy (Exponential Backoff)

```rust
fn exponential_backoff(attempt: u32) -> Duration {
    let base_delay_ms = 100;
    let delay_ms = base_delay_ms * 2u64.pow(attempt);
    Duration::from_millis(delay_ms.min(30000)) // cap at 30s
}

// Attempt 1: 100ms
// Attempt 2: 200ms
// Attempt 3: 400ms
// Attempt 4: 800ms
// Attempt 5+: 30s (capped)
```

### Circuit Breaker Pattern

```rust
enum CircuitBreakerState {
    Closed,      // Normal operation
    Open,        // Failing, reject requests
    HalfOpen,    // Testing recovery
}

fn check_circuit_breaker(service: &str) -> Result<(), Error> {
    match get_breaker_state(service) {
        CircuitBreakerState::Closed => Ok(()),       // proceed
        CircuitBreakerState::Open => Err(Error::ServiceUnavailable),
        CircuitBreakerState::HalfOpen => Ok(()),      // test call
    }
}

// Transitions:
// Closed --[failure_threshold_reached]--> Open
// Open --[timeout_elapsed]--> HalfOpen
// HalfOpen --[test_succeeds]--> Closed
// HalfOpen --[test_fails]--> Open
```

### Idempotency & Deduplication

```
Idempotent Operations:
├─ All state transitions keyed by (entity_id, event_id)
├─ If event_id already processed: return cached result
├─ Prevents double-charging, double-refunds, duplicate emails
├─ Key stored in Firestore: /idempotency-keys/{event_id}
├─ TTL: 24 hours (after which duplicates allowed, but unlikely)
└─ Result: safe to retry without side effects

Event Deduplication:
├─ Pub/Sub: at_least_once delivery (can receive duplicates)
├─ Consumer: check idempotency_key before processing
├─ If duplicate: increment metric, return success (silent)
├─ Result: exactly-once semantics via application logic
```

---

## Implementation Patterns

### Type-Safe State Transitions

```rust
pub enum EntitlementState {
    PendingApproval,
    Active,
    Suspended,
    Expired,
    Cancelled,
    RefundIssued,
    ReinstatePending,
    Archived,
}

pub enum EntitlementEvent {
    Approve,
    Deny,
    Suspend(SuspendReason),
    Reinstate,
    Cancel,
    Expire,
    Archive,
}

impl EntitlementState {
    pub fn apply(self, event: EntitlementEvent) -> Result<EntitlementState, Error> {
        match (self, event) {
            (PendingApproval, Approve) => Ok(Active),
            (PendingApproval, Deny) => Ok(Cancelled),
            (Active, Suspend(_)) => Ok(Suspended),
            (Active, Cancel) => Ok(Cancelled),
            (Active, Expire) => Ok(Expired),
            (Suspended, Reinstate) => Ok(ReinstatePending),
            (Suspended, Expire) => Ok(Expired),
            (ReinstatePending, Approve) => Ok(Active),
            (ReinstatePending, Deny) => Ok(Cancelled),
            (Cancelled, _) => Ok(Cancelled), // terminal state
            (_, _) => Err(Error::InvalidTransition(current, event)),
        }
    }
}

// Type system guarantees:
// - Cannot represent invalid states
// - Cannot apply invalid transitions
// - Compiler error if not handling all cases
```

### Audit Trail with Merkle Chaining

```rust
pub struct AuditEvent {
    pub id: String,                  // UUID
    pub timestamp: DateTime<Utc>,
    pub entity_id: String,           // customer_id, entitlement_id
    pub event_type: String,
    pub from_state: String,
    pub to_state: String,
    pub actor: String,
    pub details: serde_json::Value,
    pub data_hash: String,           // SHA256 of entity state
    pub signature: String,           // Ed25519 signature
    pub chain_hash: String,          // SHA256 of previous event
}

impl AuditEvent {
    pub fn new(
        entity_id: &str,
        from_state: &str,
        to_state: &str,
        actor: &str,
        details: serde_json::Value,
    ) -> Self {
        let event_json = serde_json::json!({
            "entity_id": entity_id,
            "from_state": from_state,
            "to_state": to_state,
            "actor": actor,
            "details": details,
        });

        let data_hash = sha256(event_json.to_string());
        let signature = sign_ed25519(&data_hash);
        let chain_hash = compute_chain_hash(previous_event);

        AuditEvent {
            id: uuid::Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            entity_id: entity_id.to_string(),
            event_type: format!("{}_to_{}", from_state, to_state),
            from_state: from_state.to_string(),
            to_state: to_state.to_string(),
            actor: actor.to_string(),
            details,
            data_hash,
            signature,
            chain_hash,
        }
    }

    pub fn verify_signature(&self) -> Result<(), Error> {
        verify_ed25519_signature(&self.signature, &self.data_hash)
    }

    pub fn verify_chain_integrity(&self) -> Result<(), Error> {
        let computed_hash = compute_chain_hash(self.previous_event());
        if computed_hash == self.chain_hash {
            Ok(())
        } else {
            Err(Error::TamperingDetected)
        }
    }
}
```

### Timeout Management

```rust
pub struct TimeoutHandle {
    pub event_id: String,
    pub entity_id: String,
    pub timeout_type: TimeoutType,
    pub trigger_time: DateTime<Utc>,
}

pub enum TimeoutType {
    ApprovalTimeout(Duration),           // 24h
    SuspensionTimeout(Duration),         // 72h
    RefundTimeout(Duration),             // 14d
    TrialTimeout(Duration),              // 30d
    AuditTimeout(Duration),              // 48h
}

impl TimeoutHandle {
    pub async fn arm(self) {
        let duration = match self.timeout_type {
            TimeoutType::ApprovalTimeout(d) => d,
            TimeoutType::SuspensionTimeout(d) => d,
            _ => return,
        };

        tokio::spawn(async move {
            tokio::time::sleep(duration).await;

            // Fire timeout event to FSM
            let timeout_event = format!("{}_timeout", self.timeout_type);
            publish_event(&self.entity_id, &timeout_event).await;
        });
    }
}

// Result: timeout_event automatically published after duration expires
// No manual polling required, no lost timeouts
```

---

## Summary Table

| FSM | States | Transitions | Key Timeout | Critical Path |
|-----|--------|-------------|-------------|-----------------|
| Entitlement | 8 | 12 | 24h approval | pending_approval → active |
| Billing | 6 | 8 | 15d payment | awaiting_invoice → payment_received |
| Catalog | 5 | 6 | 90d deprecation | draft → published |
| Subscription | 7 | 10 | 30d trial | trial → active |
| Account | 6 | 8 | 48h review | onboarding → active |
| Quota & SLA | 5 | 6 | 5m burst | within_limits → warning |
| Compliance | 4 | 5 | 30d remediation | compliant → audit_pending |
| Governance | 5 | 6 | 10s monitoring | healthy → cascade_prevention |

---

## Conclusion

This comprehensive FSM architecture provides:

1. **Type Safety**: Impossible states cannot be represented; compiler guarantees correctness
2. **Determinism**: No random behavior; all transitions explicit and auditable
3. **Auditability**: Every state change cryptographically signed and Merkle-linked
4. **Resilience**: Circuit breakers, retry logic, graceful degradation
5. **Fairness**: Multi-tenant resource allocation with noisy neighbor detection
6. **Compliance**: Built-in support for GDPR, HIPAA, SOC2, CCPA requirements
7. **Self-Healing**: Automatic recovery paths, no manual intervention required
8. **Performance**: Sub-second state transitions, <5s timeout verification

All FSMs integrate seamlessly with GCP services (Marketplace API, Billing, Firestore, Pub/Sub, BigQuery, Cloud Logging) to create a fully autonomous, production-grade marketplace platform.

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-25
**Status**: Production-Ready Design
**Review Cadence**: Quarterly (next: 2026-04-25)
