# Product Catalog Governor - SKU Lifecycle Management

## Overview

The **Product Catalog Governor** is a Rust implementation of an Erlang gen_statem-inspired FSM that manages the complete lifecycle of products (SKUs) in a GCP Marketplace.

**Location**: `/examples/gcp-erlang-autonomics/src/marketplace/product_catalog_governor.rs`

**Role in MAPE-K**: Implements the **Plan** phase for marketplace product coordination, determining state transitions and coordinating downstream actions.

## Architecture

### Core Concept

The governor models product lifecycle as a deterministic state machine with:
- **7 States**: Draft, Published, Featured, Deprecated, Archived, Validation, UpdateApproved
- **Event-driven transitions**: Product events trigger state changes
- **Invariant validation**: SKU schema validation at every transition
- **Audit trail**: Complete history of state changes with before/after values
- **Optimistic locking**: Version numbers for concurrent update handling

### State Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                     PRODUCT CATALOG FSM                          │
└─────────────────────────────────────────────────────────────────┘

                          ┌──────────┐
                          │  DRAFT   │
                          └────┬─────┘
                               │
                      SubmitForReview
                               │
                               ▼
                    ┌──────────────────┐
                    │   PUBLISHED      │◄──────────────────────┐
                    └────┬────────┬────┘                        │
                         │        │                            │
         ┌────────────────┼────────┼────────────────┐          │
         │                │        │                │          │
         │                │        │                │    ResurrectToPublished
         │                │        │                │          │
         │    UpdatePricing│ UpdateFeatures│ MoveToFeatured │
         │                │        │                │          │
         ▼                ▼        ▼                ▼          │
    ┌────────────────────────────────────┐         ┌──────────┘
    │       VALIDATION                   │         │
    │  (schema + compliance checks)      │         │
    └────┬───────────────────────────────┘    ┌────┴──────────┐
         │                                     │               │
    ValidationSucceeded │            DemoteFromFeatured    EndPromotion
         │                                     │               │
         ▼                                     │               │
    ┌──────────────────┐                       │               │
    │ UPDATE_APPROVED  │                       │               │
    └────┬─────────────┘                       │               │
         │                                     │               │
    PropagationSucceeded                       │               │
         │                                     │               │
         └─────────────────────────────────────┴───────────────┘
                                               │
                                               ▼
                                    ┌──────────────────┐
                                    │    FEATURED      │
                                    └────┬──────┬──────┘
                                         │      │
                                    UpdateWhile  KPICheckTriggered
                                    Featured     (healthy=false)
                                         │      │
                                    ┌────▼──────▼─────┐
                                    │  [goes back to  │
                                    │  VALIDATION or  │
                                    │  PUBLISHED]     │
                                    └─────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│ PUBLISHED/FEATURED state can transition to:                     │
│                                                                  │
│   Deprecate(Option<replacement_sku_id>) ─────────▶ DEPRECATED  │
└─────────────────────────────────────────────────────────────────┘

        ┌──────────────┐
        │  DEPRECATED  │
        └────┬──────┬──┘
             │      │
      ContinueSupport ForceArchive
             │      │
      (async,│      │
       no    │      ▼
      state  │   ┌──────────┐
      change)│   │ ARCHIVED │ ◄────── Terminal State
             │   └──────────┘         (read-only)
             │
      ResurrectToPublished
             │
             ▼
        PUBLISHED
```

## States

### Draft
**Purpose**: New SKU in development, not yet on marketplace

**Entry conditions**: New governor creation

**Valid events**:
- `SubmitForReview` → Published

**Actions**:
- `StoreDraftSKU` - Store in draft bucket
- `ValidateSKUSchema` - Validate name, description, price, tier

**Timeouts**: None (indefinite until submission)

**Invariants**:
- SKU name: 1-200 characters
- Description: 1-5000 characters
- Price > 0 and < 999,999,999 cents
- Tier in [starter, professional, enterprise, custom]

### Published
**Purpose**: Live on Marketplace, available for purchase

**Entry conditions**: Submit from Draft, Resurrect from Deprecated, Demote from Featured

**Valid events**:
- `UpdatePricing(PricingChange)` → Validation
- `UpdateFeatures(FeatureUpdate)` → Validation
- `FeatureRequest(String)` → Published (async, no state change)
- `MoveToFeatured` → Featured
- `Deprecate(Option<sku_id>)` → Deprecated
- `ComplianceCheckTriggered` → Published (periodic check)

**Actions**:
- `PublishToMarketplace` (from Draft)
- `UpdateMarketplacePricing` (pricing changes)
- `UpdateMarketplaceFeatures` (feature changes)
- `NotifyCustomers` (for significant changes)

**Timeouts**: Weekly compliance check

**Monitoring**:
- Track demand (KPIs)
- Monitor conversion metrics
- Update catalog index

### Featured
**Purpose**: Promoted on Marketplace home page with increased visibility

**Entry conditions**: Move from Published

**Valid events**:
- `UpdatePricing(PricingChange)` → Validation
- `UpdateFeatures(FeatureUpdate)` → Validation
- `DemoteFromFeatured` → Published
- `EndPromotion` → Published
- `KPICheckTriggered(bool)` → Published (if KPIs declined) or stays Featured (if healthy)

**Actions**:
- `IncreaseVisibility` (boost on homepage)
- Track conversion metrics
- A/B testing integration

**Timeouts**: Monthly KPI check (auto-demote if KPIs decline >20%)

### Deprecated
**Purpose**: Being phased out, no new purchases allowed, existing customers supported

**Entry conditions**: Deprecate from Published or Featured

**Valid events**:
- `ResurrectToPublished` → Published
- `ForceArchive` → Archived
- `ContinueSupport` → Deprecated (async, no state change)

**Actions**:
- `RedirectToReplacement` (if replacement provided)
- `NotifyCustomers` (existing customers can continue using)

**Timeouts**: 6 months auto-archive

**Constraints**:
- No new purchases allowed
- Existing customers unaffected
- Support continues

### Archived
**Purpose**: Final state - product is end-of-life, read-only, historical data preserved

**Entry conditions**: Force from Deprecated or auto-archive after 6 months

**Valid transitions**: NONE (terminal state)

**Actions**:
- `ArchiveHistoricalData` - Preserve complete history for compliance

### Validation (Intermediate)
**Purpose**: Awaiting approval for pricing/feature changes

**Entry conditions**: UpdatePricing or UpdateFeatures from Published/Featured

**Valid events**:
- `ValidationSucceeded` → UpdateApproved
- `ValidationFailed(reason)` → Error (revert to previous state)

**Actions**:
- `ValidateSKUSchema` - Pre-flight checks

### UpdateApproved (Intermediate)
**Purpose**: Update approved, propagating to marketplace

**Entry conditions**: ValidationSucceeded from Validation

**Valid events**:
- `PropagationSucceeded` → Published or Featured (return to previous state)

## Event Types

### ProductEvent Enum

```rust
pub enum ProductEvent {
    SubmitForReview,
    UpdatePricing(PricingChange),
    UpdateFeatures(FeatureUpdate),
    FeatureRequest(String),
    MoveToFeatured,
    DemoteFromFeatured,
    EndPromotion,
    Deprecate(Option<String>),  // Option<replacement_sku_id>
    ContinueSupport,
    ResurrectToPublished,
    ForceArchive,
    ValidationSucceeded,
    ValidationFailed(String),
    UpdateApproved,
    PropagationSucceeded,
    ComplianceCheckTriggered,
    KPICheckTriggered(bool),    // healthy?
}
```

## Pricing Change Workflow

### PricingChange Validation

```rust
pub struct PricingChange {
    pub current_price_cents: u64,
    pub new_price_cents: u64,
    pub effective_date: DateTime<Utc>,
    pub tier: String,
}
```

**Compliance checks**:
1. **Price increase bound**: Increases >10% require ≥30 days notice (GDPR compliance)
2. **Price bounds**:
   - Minimum: 1 cent (> 0)
   - Maximum: 999,999,999 cents
3. **Effective date**: Must be in future
4. **Tier validation**: Must match SKU tier

**Workflow**:
1. `Published` state + `UpdatePricing` event
2. → `Validation` state
3. → Approval chain validates pricing compliance
4. → `ValidationSucceeded` → `UpdateApproved`
5. → `PropagationSucceeded` → Back to `Published`
6. Metadata updated with new price and version incremented

### Example: Safe Price Increase
```
Old price: $99.99 (9999 cents)
New price: $104.99 (10499 cents) — 5% increase
Effective date: Today + 60 days
Status: ✓ Compliant (5% < 10% threshold)
Notice: ✓ 60 days > 30 days required
```

### Example: Problematic Price Increase
```
Old price: $100.00 (10000 cents)
New price: $115.00 (11500 cents) — 15% increase
Effective date: Today + 10 days
Status: ✗ REJECTED
Reason: 15% increase requires 30+ days notice (only 10 days provided)
```

## Feature Update Workflow

### FeatureUpdate Validation

```rust
pub struct FeatureUpdate {
    pub feature_name: String,
    pub description: String,
    pub is_breaking_change: bool,
    pub min_version: Option<String>,
    pub dependencies: Vec<String>,
}
```

**Validation checks**:
1. **Feature name**: 1-100 characters, no special characters
2. **Dependencies**: All must exist (or be self-referencing)
3. **Circular dependencies**: Detect self-dependencies
4. **Breaking changes**: Can be tracked for beta customer testing

**Workflow**:
1. `Published`/`Featured` state + `UpdateFeatures` event
2. → `Validation` state
3. → Dependency resolution validates feature can be added
4. → Beta testing if breaking change
5. → `ValidationSucceeded` → `UpdateApproved`
6. → `PropagationSucceeded` → Back to previous state
7. Metadata features list updated, version incremented

### Example: Valid Feature Update
```
Feature: "advanced-reporting"
Dependencies: ["analytics"] ← Already exists in SKU
Breaking change: No
Status: ✓ Compliant
```

### Example: Failed Feature Update
```
Feature: "advanced-reporting"
Dependencies: ["machine-learning"] ← MISSING!
Status: ✗ REJECTED
Reason: Required dependency 'machine-learning' not found
```

## Concurrent Update Handling

### Optimistic Locking

Each SKUMetadata has:
- `version: u32` - Incremented on each update
- `revision_hash: String` - Content hash for conflict detection

**Workflow**:
1. Read SKU version N
2. Apply changes (pricing, features)
3. Attempt to write version N+1
4. If version changed to N+2 meanwhile → Conflict
5. Retry with latest version

**Implementation**:
```rust
pub fn compute_revision_hash(&self) -> String {
    // Hash-based: sku_id, name, price_cents, features, version
    // Used to detect concurrent modifications
}
```

## Audit Trail

Each state transition records an **AuditEntry**:

```rust
pub struct AuditEntry {
    pub timestamp: DateTime<Utc>,
    pub from_state: String,
    pub to_state: String,
    pub event: String,
    pub before_value: Option<String>,
    pub after_value: Option<String>,
    pub affected_customer_count: u32,
    pub approval_required: bool,
}
```

**Captured data**:
- Timeline of all state changes
- Pricing change before/after
- Feature additions/removals
- Approval chain (if manual review required)
- Customer impact (how many customers affected)

**Use cases**:
- Compliance reporting (GDPR, terms changes)
- Root cause analysis (why did product get archived?)
- Customer communication (when were prices changed?)
- Chargeback defense (prove price changes were communicated)

## Customer Notification System

### Notification Triggers

**Automatic notifications**:
- Price increase (>5%) - Email + in-app alert
- Feature deprecation (breaking change) - Email + 30-day notice
- Product deprecation - Email + redirection info
- Product archival - Email + migration options

**Async (no state change)**:
- Feature requests
- Continue support (deprecated products)

**Manual notification** via `NotifyCustomers` action:
- Email template rendering
- In-app dashboard alert
- Webhook to customer integrations

### Example Notification
```
Subject: Important: [Product] Pricing Update

Dear Customer,

We're updating [Product] pricing effective [Date]:
- Current: $99.99/month
- New: $104.99/month
- Change: 5% increase
- Your plan: No impact until next renewal

You can:
1. Continue on current price until renewal
2. Switch to different tier
3. Cancel anytime

Questions? Reply to this email.

---
Changes approved: [Timestamp]
Audit trail: [Compliance Link]
```

## Integration with GCP Marketplace

### Marketplace Sync

**Actions that trigger marketplace updates**:
1. `PublishToMarketplace` - Create SKU in GCP Marketplace
2. `UpdateMarketplacePricing` - Update price tiers
3. `UpdateMarketplaceFeatures` - Update feature list
4. `IncreaseVisibility` / `DecreaseVisibility` - Featured toggle
5. `RedirectToReplacement` - Deprecation redirect
6. `ArchiveHistoricalData` - Historical record retention

**Eventual consistency model**:
- Local state changes immediately
- Marketplace updates propagate asynchronously
- `PropagationSucceeded` event confirms completion
- Retries on failure with exponential backoff

## Code Structure

### File Organization

```
src/marketplace/
├── mod.rs                        # Module exports
├── product_catalog_governor.rs   # This file (2000+ lines)
│   ├── Errors (ProductCatalogError enum)
│   ├── States (ProductState enum)
│   ├── Events (ProductEvent enum)
│   ├── Types (SKUMetadata, PricingChange, etc.)
│   ├── Coordinator (ProductCatalogGovernor struct)
│   └── Tests (Chicago TDD, 30+ test cases)
└── [other governors]
```

### Key Types

| Type | Purpose |
|------|---------|
| `ProductCatalogError` | Error types for all failure modes |
| `ProductState` | Current state (Draft, Published, etc.) |
| `ProductEvent` | State machine events |
| `SKUMetadata` | Product data (name, price, features, limits) |
| `PricingChange` | Pricing change request with compliance validation |
| `FeatureUpdate` | Feature change request with dependency resolution |
| `CatalogAction` | Downstream actions (publish, notify, archive) |
| `AuditEntry` | State transition history with before/after |
| `ProductCatalogGovernor` | Main coordinator struct |

## Testing (Chicago TDD)

### Test Coverage (35+ tests)

**Draft state** (2 tests):
- `test_draft_to_published_flow` - Normal submission path
- `test_draft_sku_invalid_*` - Schema validation

**Published state** (8 tests):
- `test_published_pricing_change_workflow` - Pricing change initiation
- `test_published_pricing_change_excessive_increase` - GDPR compliance
- `test_published_feature_update_*` - Feature validation
- `test_published_move_to_featured` - Visibility increase
- `test_published_deprecate_*` - Deprecation with replacement
- `test_published_compliance_check` - Periodic checks

**Featured state** (4 tests):
- `test_featured_demote_to_published` - Demote
- `test_featured_kpi_check_decline_auto_demotes` - KPI-driven demotion
- `test_featured_kpi_check_healthy_stays_featured` - KPI-driven retention

**Deprecated state** (2 tests):
- `test_deprecated_resurrect_to_published` - Bring back product
- `test_deprecated_force_archive` - End-of-life archival

**Validation state** (2 tests):
- `test_validation_success_updates_metadata` - Metadata commit
- `test_validation_failure_returns_error` - Failure handling

**Archived state** (1 test):
- `test_archived_is_terminal` - No transitions allowed

**Advanced tests** (6+ tests):
- `test_concurrent_updates_buffered` - Optimistic locking
- `test_audit_trail_records_transitions` - Audit verification
- `test_time_in_state_tracking` - Duration tracking
- `test_sku_revision_hash_computation` - Hash stability
- `test_sku_tier_validation` - Schema validation
- `test_pricing_change_validation_*` - Compliance checks
- `test_feature_update_*` - Dependency resolution

### Test Pattern (AAA)

All tests follow Chicago TDD pattern:

```rust
#[tokio::test]
async fn test_published_pricing_change_workflow() {
    // Arrange: Set up initial state
    let sku = make_sku("sku-001", "Starter Plan", 9999);
    let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
    governor.transition(ProductEvent::SubmitForReview).await.unwrap();

    // Act: Perform the action
    let pricing_change = PricingChange {
        current_price_cents: 9999,
        new_price_cents: 10499,  // 5% increase
        effective_date: Utc::now() + Duration::days(60),
        tier: "professional".to_string(),
    };
    let (new_state, action) = governor
        .transition(ProductEvent::UpdatePricing(pricing_change))
        .await
        .unwrap();

    // Assert: Verify observable outcomes
    assert_eq!(new_state, ProductState::Validation);
    assert!(matches!(action, Some(CatalogAction::ValidateSKUSchema(_))));
    assert!(governor.has_pending_changes());
}
```

## Error Handling

### ProductCatalogError Types

```rust
pub enum ProductCatalogError {
    InvalidTransition { from: String, to: String, event: String },
    InvariantViolation(String),
    PricingComplianceError(String),
    FeatureValidationError(String),
    SKUValidationError(String),
    ConcurrentUpdateConflict { sku_id: String },
    ApprovalChainFailed { reason: String },
    MarketplaceSyncFailed { reason: String },
    NotificationFailed { reason: String },
    InvalidActionInState { state: String },
}
```

**All errors use Result<T, E>**: No panics, no unwrap() in production code

## Performance Characteristics

- **State transition**: O(1) - match on state + event
- **Invariant validation**: O(n) where n = feature count (typically < 20)
- **Audit trail append**: O(1) amortized
- **Concurrent updates**: O(1) with version checking

**SLO targets**:
- Transition latency: < 10ms (p99)
- Validation latency: < 50ms (pricing) + < 100ms (features)
- Marketplace sync: < 2s (via background job)

## Example Usage

### Create New SKU

```rust
use gcp_erlang_autonomics::*;

// 1. Create SKU metadata
let sku = SKUMetadata {
    sku_id: "starter-plan-v2".to_string(),
    name: "Starter Plan".to_string(),
    description: "Entry-level SaaS plan".to_string(),
    price_cents: 9999,  // $99.99
    tier: "starter".to_string(),
    features: vec!["api".to_string(), "integrations".to_string()],
    limits: [(
        "max_api_calls".to_string(),
        1_000_000_u64,
    )].iter().cloned().collect(),
    version: 1,
    last_updated: chrono::Utc::now(),
    revision_hash: "abc123".to_string(),
};

// 2. Create governor
let mut governor = ProductCatalogGovernor::new(
    "starter-plan-v2".to_string(),
    sku,
)?;

// 3. Submit for marketplace review
let (new_state, action) = governor
    .transition(ProductEvent::SubmitForReview)
    .await?;

assert_eq!(new_state, ProductState::Published);
println!("SKU published: {:?}", action);
```

### Update Pricing with Compliance Check

```rust
// 1. Propose price increase
let pricing = PricingChange {
    current_price_cents: 9999,
    new_price_cents: 10999,     // 10% increase
    effective_date: chrono::Utc::now() + chrono::Duration::days(60),
    tier: "starter".to_string(),
};

// 2. Validate (automatic via transition)
let (new_state, action) = governor
    .transition(ProductEvent::UpdatePricing(pricing))
    .await?;

// ✓ Compliant: 10% increase ≤ 10% threshold, 60 days > 30 days notice
assert_eq!(new_state, ProductState::Validation);

// 3. Approve (from approval chain)
let (_state, _) = governor
    .transition(ProductEvent::ValidationSucceeded)
    .await?;

// 4. Propagate to marketplace (async background job)
let (_state, _) = governor
    .transition(ProductEvent::PropagationSucceeded)
    .await?;

assert_eq!(governor.metadata().price_cents, 10999);
assert_eq!(governor.metadata().version, 2);
```

## Compliance Features

### GDPR Pricing Rules
- Price increases >10% require 30+ days notice
- All price changes tracked in audit trail
- Customers notified before effective date

### Data Retention
- Archived products preserved indefinitely
- Complete audit trail for compliance reporting
- Enables dispute resolution / chargeback defense

### Audit Trail
- Immutable history of all state changes
- Before/after values for compliance
- Approval chain tracking
- Customer impact metrics

## Future Enhancements

1. **A/B Testing Integration**: Track conversion metrics for featured SKUs
2. **Churn Prevention**: Detect declining KPIs, offer incentives
3. **Regional Pricing**: Support different prices by geography
4. **Multi-currency**: Support multiple currencies per tier
5. **Entitlement Sync**: Link to customer subscription lifecycle
6. **Metered Billing**: Track usage-based pricing triggers
7. **Marketplace Multi-tenancy**: Separate catalogs by region/partner
8. **Feature Flags**: Gradual feature rollout with gates

## Related Components

- **SubscriptionGovernor**: Manages individual customer subscriptions (linked via SKU)
- **MarketplaceOrchestrator**: Coordinates all marketplace governors
- **MTGovernor**: Multi-tenant isolation and fair-share allocation
- **BillingGovernor**: Proration and charge calculation
- **EntitlementGovernor**: Customer entitlement validation
- **ComplianceAuditGovernor**: Compliance rule enforcement

## References

- **MAPE-K Loop**: Monitor → Analyze → Plan → Execute → Knowledge
- **Gen_statem**: Erlang's generic state machine framework (inspiration)
- **DfLSS**: Design for Lean Six Sigma (prevents defects early)
- **Chicago TDD**: State-based testing with real collaborators
- **Poka-Yoke**: Error-proofing design patterns
