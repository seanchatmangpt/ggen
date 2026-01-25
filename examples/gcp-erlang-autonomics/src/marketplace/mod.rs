//! Marketplace governance module
//!
//! Provides comprehensive SaaS marketplace governance including:
//! - Multi-tenant resource governance with fair-share allocation and noisy neighbor detection
//! - Subscription lifecycle management (trial → active → renewal → archived)
//! - Feature tier management with soft/hard limits
//! - Proration calculation for mid-cycle changes
//! - Product lifecycle management (SKU state machine)
//! - Quota and SLA enforcement
//! - Compliance auditing and data retention
//!
//! ## Modules
//! - **entitlement_governor**: GCP Marketplace entitlement lifecycle (8-state FSM)
//! - **multi_tenant_governance**: Multi-tenant isolation, fair-share allocation, cascade prevention (6-state FSM)
//! - **subscription_governor**: SaaS subscription lifecycle management
//! - **billing_governor**: Payment and billing FSM
//! - **marketplace_orchestrator**: Top-level coordinator for marketplace governors
//! - **product_catalog_governor**: SKU lifecycle management
//! - **quota_sla_governor**: Quota and SLA enforcement

pub mod entitlement_governor;
pub mod multi_tenant_governance;
pub mod subscription_governor;
pub mod billing_governor;
pub mod marketplace_orchestrator;
pub mod product_catalog_governor;
pub mod quota_sla_governor;
pub mod compliance_audit_governor;

// Entitlement governance (GCP Marketplace)
pub use entitlement_governor::{
    EntitlementGovernor, EntitlementState, EntitlementEvent, EntitlementConfig,
    MarketplaceError, Entitlement, StateChangeReceipt,
};

// Multi-tenant governance (primary focus)
pub use multi_tenant_governance::{
    MTGovernor, MTGovernorState, MTGovernorEvent, MTGovernorError, TenantMetrics, TenantTier,
    CascadeIndicator, LoadBalancingStrategy, AuditEvent,
};

// Subscription governance
pub use subscription_governor::{
    SubscriptionGovernor, Subscription, SubscriptionState, SubscriptionEvent, SubscriptionError,
    FeatureTier, BillingCycle, AccountType, UsageMetrics, ProratedCharge,
};

// Billing governance
pub use billing_governor::{
    BillingGovernor, BillingState, BillingEvent, BillingAction,
    BillingGovernorError, PaymentMethod, PaymentInfo, DisputeOutcome,
    ReceiptFormat, generate_idempotency_key,
};

// Marketplace orchestration
pub use marketplace_orchestrator::{
    MarketplaceOrchestrator, OrchestratorState, MarketplaceEvent, GovernorType,
    GovernorResponse, GovernorResponseStatus, MarketplaceOrchestratorError, OrchestratorStats,
};

// Product catalog governance
pub use product_catalog_governor::{
    ProductCatalogGovernor, ProductState, ProductEvent, ProductCatalogError,
    SKUMetadata, PricingChange, FeatureUpdate, CatalogAction,
};

// Quota and SLA governance
pub use quota_sla_governor::{
    QuotaSlaGovernor, QuotaSlaState, QuotaSlaEvent, QuotaSlaError, CustomerTier,
    QuotaType, QuotaMetric, SlaMetrics, QuotaSlaAction,
};

// Compliance & Audit governance
pub use compliance_audit_governor::{
    ComplianceGovernor, ComplianceState, ComplianceEvent, ComplianceError,
    ComplianceFramework, AuditTrailEntry, AuditResult, Violation,
    DataResidency, BreachIncident, BreachPhase,
};
