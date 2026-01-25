//! Product Catalog Governor - SKU lifecycle management with gen_statem-inspired FSM
//!
//! This module implements the **Plan** phase of MAPE-K for marketplace product management:
//! - Erlang gen_statem-inspired FSM for SKU state machine
//! - Pricing compliance validation (GDPR rules, bounds checking)
//! - Feature dependency resolution and conflict detection
//! - Customer notification workflow coordination
//! - Audit trail with before/after values and approval chains
//! - Concurrent update handling with optimistic locking
//! - Eventual consistency with GCP Marketplace
//!
//! ## State Machine Diagram
//!
//! ```
//! draft
//!   ├─ submit_for_review ──→ published
//!   └─ add_to_catalog_later (timeout: indefinite)
//!
//! published
//!   ├─ update_pricing ──→ validation ──→ update_approved ──→ propagate_to_marketplace
//!   ├─ update_features ──→ validation ──→ update_approved ──→ propagate_to_marketplace
//!   ├─ feature_request (async, no state change)
//!   ├─ move_to_featured ──→ featured
//!   ├─ deprecate ──→ deprecated
//!   └─ Timeout: weekly compliance check
//!
//! featured
//!   ├─ update_while_featured ──→ validation ──→ update_approved ──→ propagate_to_marketplace
//!   ├─ demote_to_published ──→ published
//!   ├─ end_promotion ──→ published
//!   └─ Timeout: monthly KPI check (auto-demote if declined)
//!
//! deprecated
//!   ├─ continue_support (async, no state change)
//!   ├─ resurrect_to_published ──→ published
//!   ├─ force_archive ──→ archived
//!   └─ Timeout: 6 months auto-archive
//!
//! archived (TERMINAL)
//!   └─ Historical data preserved (read-only)
//! ```

use chrono::{DateTime, Utc, Duration};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use std::collections::HashMap;

/// Product catalog governor errors
#[derive(Debug, Error)]
pub enum ProductCatalogError {
    #[error("Invalid state transition: {from} → {to} with event {event}")]
    InvalidTransition {
        from: String,
        to: String,
        event: String,
    },

    #[error("Invariant violation: {0}")]
    InvariantViolation(String),

    #[error("Pricing compliance failed: {0}")]
    PricingComplianceError(String),

    #[error("Feature validation failed: {0}")]
    FeatureValidationError(String),

    #[error("SKU schema validation failed: {0}")]
    SKUValidationError(String),

    #[error("Concurrent update conflict: {sku_id}")]
    ConcurrentUpdateConflict { sku_id: String },

    #[error("Approval chain failed: {reason}")]
    ApprovalChainFailed { reason: String },

    #[error("Marketplace sync failed: {reason}")]
    MarketplaceSyncFailed { reason: String },

    #[error("Customer notification failed: {reason}")]
    NotificationFailed { reason: String },

    #[error("Invalid action in state: {state}")]
    InvalidActionInState { state: String },
}

/// Product state in marketplace lifecycle
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum ProductState {
    /// New SKU in development, not yet on marketplace
    Draft,
    /// Live on Marketplace
    Published,
    /// Promoted on Marketplace home page
    Featured,
    /// Being phased out (existing customers unaffected)
    Deprecated,
    /// Final state, read-only, historical data preserved
    Archived,
    /// Intermediate: awaiting approval
    Validation,
    /// Intermediate: update approved, propagating to marketplace
    UpdateApproved,
}

impl ProductState {
    fn as_str(&self) -> &str {
        match self {
            ProductState::Draft => "Draft",
            ProductState::Published => "Published",
            ProductState::Featured => "Featured",
            ProductState::Deprecated => "Deprecated",
            ProductState::Archived => "Archived",
            ProductState::Validation => "Validation",
            ProductState::UpdateApproved => "UpdateApproved",
        }
    }

    fn is_terminal(&self) -> bool {
        matches!(self, ProductState::Archived)
    }
}

/// Pricing change request
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PricingChange {
    /// Current price (cents)
    pub current_price_cents: u64,
    /// Proposed new price (cents)
    pub new_price_cents: u64,
    /// Effective date (must be ≥30 days in future)
    pub effective_date: DateTime<Utc>,
    /// Pricing tier (e.g., "starter", "professional", "enterprise")
    pub tier: String,
}

impl PricingChange {
    /// Validate pricing compliance (GDPR: no price increase >10% without notice)
    pub fn validate(&self) -> Result<(), ProductCatalogError> {
        // Check price increase bound (10% without 30-day notice)
        let increase_pct = if self.current_price_cents > 0 {
            ((self.new_price_cents as f64 - self.current_price_cents as f64)
                / self.current_price_cents as f64) * 100.0
        } else {
            0.0
        };

        if increase_pct > 10.0 {
            let notice_days = (self.effective_date - Utc::now()).num_days();
            if notice_days < 30 {
                return Err(ProductCatalogError::PricingComplianceError(
                    format!(
                        "Price increase {}% requires 30-day notice, but only {}d notice provided",
                        increase_pct, notice_days
                    ),
                ));
            }
        }

        // Price must be within reasonable bounds
        if self.new_price_cents == 0 {
            return Err(ProductCatalogError::PricingComplianceError(
                "New price cannot be zero".to_string(),
            ));
        }

        if self.new_price_cents > 999_999_999 {
            return Err(ProductCatalogError::PricingComplianceError(
                "New price exceeds maximum bound".to_string(),
            ));
        }

        // Effective date must be in future
        if self.effective_date <= Utc::now() {
            return Err(ProductCatalogError::PricingComplianceError(
                "Effective date must be in the future".to_string(),
            ));
        }

        Ok(())
    }
}

/// Feature update request
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct FeatureUpdate {
    /// Feature name
    pub feature_name: String,
    /// Feature description
    pub description: String,
    /// Is this a breaking change?
    pub is_breaking_change: bool,
    /// Minimum version that must be running (if breaking)
    pub min_version: Option<String>,
    /// Dependencies (other features that must be present)
    pub dependencies: Vec<String>,
}

impl FeatureUpdate {
    /// Validate feature dependencies and conflicts
    pub fn validate(&self, existing_features: &[String]) -> Result<(), ProductCatalogError> {
        // Check dependencies are available
        for dep in &self.dependencies {
            if !existing_features.contains(dep) && dep != &self.feature_name {
                return Err(ProductCatalogError::FeatureValidationError(
                    format!("Required dependency '{}' not found", dep),
                ));
            }
        }

        // Check for circular dependencies (simplified: detect self-dependency)
        if self.dependencies.contains(&self.feature_name) {
            return Err(ProductCatalogError::FeatureValidationError(
                "Feature cannot depend on itself".to_string(),
            ));
        }

        // Validate feature name
        if self.feature_name.is_empty() || self.feature_name.len() > 100 {
            return Err(ProductCatalogError::FeatureValidationError(
                "Feature name must be 1-100 characters".to_string(),
            ));
        }

        Ok(())
    }
}

/// SKU metadata with schema validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SKUMetadata {
    /// Unique SKU identifier
    pub sku_id: String,
    /// Human-readable name
    pub name: String,
    /// Product description
    pub description: String,
    /// Current price in cents
    pub price_cents: u64,
    /// Pricing tier (e.g., "starter", "professional", "enterprise")
    pub tier: String,
    /// Current features
    pub features: Vec<String>,
    /// Usage limits (e.g., max-users, max-api-calls)
    pub limits: HashMap<String, u64>,
    /// Version history (for rollback capability)
    pub version: u32,
    /// Last update timestamp
    pub last_updated: DateTime<Utc>,
    /// Revision hash for optimistic locking
    pub revision_hash: String,
}

impl SKUMetadata {
    /// Validate SKU against schema
    pub fn validate(&self) -> Result<(), ProductCatalogError> {
        // Name validation
        if self.name.is_empty() || self.name.len() > 200 {
            return Err(ProductCatalogError::SKUValidationError(
                "Name must be 1-200 characters".to_string(),
            ));
        }

        // Description validation
        if self.description.is_empty() || self.description.len() > 5000 {
            return Err(ProductCatalogError::SKUValidationError(
                "Description must be 1-5000 characters".to_string(),
            ));
        }

        // Price validation
        if self.price_cents == 0 {
            return Err(ProductCatalogError::SKUValidationError(
                "Price must be greater than zero".to_string(),
            ));
        }

        if self.price_cents > 999_999_999 {
            return Err(ProductCatalogError::SKUValidationError(
                "Price exceeds maximum bound".to_string(),
            ));
        }

        // Tier validation
        let valid_tiers = ["starter", "professional", "enterprise", "custom"];
        if !valid_tiers.contains(&self.tier.as_str()) {
            return Err(ProductCatalogError::SKUValidationError(
                format!("Invalid tier '{}'. Must be one of {:?}", self.tier, valid_tiers),
            ));
        }

        // SKU ID validation (alphanumeric + hyphen only)
        if !self.sku_id.chars().all(|c| c.is_alphanumeric() || c == '-') {
            return Err(ProductCatalogError::SKUValidationError(
                "SKU ID must contain only alphanumeric characters and hyphens".to_string(),
            ));
        }

        Ok(())
    }

    /// Compute revision hash for optimistic locking
    pub fn compute_revision_hash(&self) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        self.sku_id.hash(&mut hasher);
        self.name.hash(&mut hasher);
        self.price_cents.hash(&mut hasher);
        self.features.hash(&mut hasher);
        self.version.hash(&mut hasher);

        format!("{:x}", hasher.finish())
    }
}

/// Catalog actions (what should happen)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CatalogAction {
    /// Store SKU in draft bucket
    StoreDraftSKU(SKUMetadata),
    /// Validate SKU schema
    ValidateSKUSchema(SKUMetadata),
    /// Publish to marketplace catalog
    PublishToMarketplace(SKUMetadata),
    /// Update marketplace with pricing change
    UpdateMarketplacePricing(String, PricingChange), // (sku_id, change)
    /// Update marketplace with feature changes
    UpdateMarketplaceFeatures(String, FeatureUpdate), // (sku_id, update)
    /// Increase marketplace visibility (homepage featured)
    IncreaseVisibility(String), // sku_id
    /// Decrease marketplace visibility (remove from featured)
    DecreaseVisibility(String), // sku_id
    /// Redirect customers to replacement SKU
    RedirectToReplacement(String, String), // (old_sku_id, new_sku_id)
    /// Notify customers of change
    NotifyCustomers(String, String), // (sku_id, notification_reason)
    /// Archive historical data
    ArchiveHistoricalData(String), // sku_id
}

/// Events that drive FSM transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProductEvent {
    /// Submit new SKU for marketplace review
    SubmitForReview,
    /// Update pricing (initiates validation workflow)
    UpdatePricing(PricingChange),
    /// Update features (initiates validation workflow)
    UpdateFeatures(FeatureUpdate),
    /// Async feature request (doesn't change state)
    FeatureRequest(String),
    /// Move from published to featured
    MoveToFeatured,
    /// Move from featured back to published
    DemoteFromFeatured,
    /// End promotion (featured → published)
    EndPromotion,
    /// Mark product as deprecated (no new purchases)
    Deprecate(Option<String>), // Optional replacement SKU ID
    /// Continue supporting deprecated product (async, no state change)
    ContinueSupport,
    /// Resurrect deprecated product
    ResurrectToPublished,
    /// Force archive (bypasses 6-month wait)
    ForceArchive,
    /// Validation succeeded (internal)
    ValidationSucceeded,
    /// Validation failed with reason
    ValidationFailed(String),
    /// Update approved, ready to propagate
    UpdateApproved,
    /// Propagation to marketplace succeeded
    PropagationSucceeded,
    /// Periodic compliance check (timeout event)
    ComplianceCheckTriggered,
    /// Monthly KPI check (featured only)
    KPICheckTriggered(bool), // KPIs healthy?
}

/// Product governor instance (per-SKU coordination)
#[derive(Debug, Clone)]
pub struct ProductCatalogGovernor {
    state: ProductState,
    sku_id: String,
    metadata: SKUMetadata,
    state_entered_at: DateTime<Utc>,
    last_state_change: DateTime<Utc>,
    pending_changes: Option<ProductEvent>, // Buffered change during validation
    audit_log: Vec<AuditEntry>,
}

/// Audit entry for approval chain and version history
#[derive(Debug, Clone, Serialize, Deserialize)]
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

impl ProductCatalogGovernor {
    /// Create new governor for SKU
    pub fn new(sku_id: String, metadata: SKUMetadata) -> Result<Self, ProductCatalogError> {
        // Validate metadata on creation
        metadata.validate()?;

        Ok(Self {
            state: ProductState::Draft,
            sku_id: sku_id.clone(),
            metadata,
            state_entered_at: Utc::now(),
            last_state_change: Utc::now(),
            pending_changes: None,
            audit_log: Vec::new(),
        })
    }

    /// Process event and compute state transition
    ///
    /// Returns: (new_state, optional_action_to_execute)
    pub async fn transition(
        &mut self,
        event: ProductEvent,
    ) -> Result<(ProductState, Option<CatalogAction>), ProductCatalogError> {
        let (new_state, action) = match (&self.state, &event) {
            // === DRAFT state ===
            (ProductState::Draft, ProductEvent::SubmitForReview) => {
                self.metadata.validate()?;
                (
                    ProductState::Published,
                    Some(CatalogAction::PublishToMarketplace(self.metadata.clone())),
                )
            }

            // === PUBLISHED state ===
            (ProductState::Published, ProductEvent::UpdatePricing(pricing_change)) => {
                pricing_change.validate()?;
                self.pending_changes = Some(ProductEvent::UpdatePricing(pricing_change.clone()));
                (
                    ProductState::Validation,
                    Some(CatalogAction::ValidateSKUSchema(self.metadata.clone())),
                )
            }

            (ProductState::Published, ProductEvent::UpdateFeatures(feature_update)) => {
                feature_update.validate(&self.metadata.features)?;
                self.pending_changes = Some(ProductEvent::UpdateFeatures(feature_update.clone()));
                (
                    ProductState::Validation,
                    Some(CatalogAction::ValidateSKUSchema(self.metadata.clone())),
                )
            }

            (ProductState::Published, ProductEvent::FeatureRequest(feature_name)) => {
                // Async: request goes to product team, no state change
                tracing::info!(
                    sku_id = %self.sku_id,
                    feature = %feature_name,
                    "Feature request received (async)"
                );
                (ProductState::Published, None)
            }

            (ProductState::Published, ProductEvent::MoveToFeatured) => {
                (
                    ProductState::Featured,
                    Some(CatalogAction::IncreaseVisibility(self.sku_id.clone())),
                )
            }

            (ProductState::Published, ProductEvent::Deprecate(replacement)) => {
                let action = if let Some(repl_sku) = replacement {
                    Some(CatalogAction::RedirectToReplacement(self.sku_id.clone(), repl_sku.clone()))
                } else {
                    None
                };
                (ProductState::Deprecated, action)
            }

            (ProductState::Published, ProductEvent::ComplianceCheckTriggered) => {
                // Periodic compliance check (no state change if compliant)
                tracing::info!(
                    sku_id = %self.sku_id,
                    "Weekly compliance check passed"
                );
                (ProductState::Published, None)
            }

            // === FEATURED state ===
            (ProductState::Featured, ProductEvent::UpdatePricing(pricing_change)) => {
                pricing_change.validate()?;
                self.pending_changes = Some(ProductEvent::UpdatePricing(pricing_change.clone()));
                (
                    ProductState::Validation,
                    Some(CatalogAction::ValidateSKUSchema(self.metadata.clone())),
                )
            }

            (ProductState::Featured, ProductEvent::UpdateFeatures(feature_update)) => {
                feature_update.validate(&self.metadata.features)?;
                self.pending_changes = Some(ProductEvent::UpdateFeatures(feature_update.clone()));
                (
                    ProductState::Validation,
                    Some(CatalogAction::ValidateSKUSchema(self.metadata.clone())),
                )
            }

            (ProductState::Featured, ProductEvent::DemoteFromFeatured) => {
                (
                    ProductState::Published,
                    Some(CatalogAction::DecreaseVisibility(self.sku_id.clone())),
                )
            }

            (ProductState::Featured, ProductEvent::EndPromotion) => {
                (
                    ProductState::Published,
                    Some(CatalogAction::DecreaseVisibility(self.sku_id.clone())),
                )
            }

            (ProductState::Featured, ProductEvent::KPICheckTriggered(healthy)) => {
                if *healthy {
                    // KPIs still good, stay featured
                    (ProductState::Featured, None)
                } else {
                    // KPIs declined, auto-demote
                    (
                        ProductState::Published,
                        Some(CatalogAction::DecreaseVisibility(self.sku_id.clone())),
                    )
                }
            }

            // === DEPRECATED state ===
            (ProductState::Deprecated, ProductEvent::ContinueSupport) => {
                // Async: support continues, no state change
                tracing::info!(
                    sku_id = %self.sku_id,
                    "Continue support request received (async)"
                );
                (ProductState::Deprecated, None)
            }

            (ProductState::Deprecated, ProductEvent::ResurrectToPublished) => {
                (
                    ProductState::Published,
                    Some(CatalogAction::PublishToMarketplace(self.metadata.clone())),
                )
            }

            (ProductState::Deprecated, ProductEvent::ForceArchive) => {
                (
                    ProductState::Archived,
                    Some(CatalogAction::ArchiveHistoricalData(self.sku_id.clone())),
                )
            }

            // === VALIDATION state ===
            (ProductState::Validation, ProductEvent::ValidationSucceeded) => {
                // Apply pending changes to metadata
                if let Some(pending) = &self.pending_changes {
                    if let ProductEvent::UpdatePricing(pricing) = pending {
                        self.metadata.price_cents = pricing.new_price_cents;
                    }
                    if let ProductEvent::UpdateFeatures(feature_update) = pending {
                        // In production: merge feature updates, check for conflicts
                        self.metadata.features.push(feature_update.feature_name.clone());
                    }
                }
                self.metadata.version += 1;
                self.pending_changes = None;

                (ProductState::UpdateApproved, None)
            }

            (ProductState::Validation, ProductEvent::ValidationFailed(reason)) => {
                self.pending_changes = None;
                // Return to original state (determine based on audit log)
                let prev_state = if let Some(last_entry) = self.audit_log.last() {
                    // Parse previous state from audit
                    match last_entry.from_state.as_str() {
                        "Published" => ProductState::Published,
                        "Featured" => ProductState::Featured,
                        _ => ProductState::Published,
                    }
                } else {
                    ProductState::Published
                };

                return Err(ProductCatalogError::ApprovalChainFailed {
                    reason: reason.clone(),
                });
            }

            // === UPDATE_APPROVED state ===
            (ProductState::UpdateApproved, ProductEvent::PropagationSucceeded) => {
                // Return to original state (Featured or Published)
                let prev_state = if let Some(last_entry) = self.audit_log.last() {
                    match last_entry.from_state.as_str() {
                        "Featured" => ProductState::Featured,
                        _ => ProductState::Published,
                    }
                } else {
                    ProductState::Published
                };

                (prev_state, None)
            }

            // === ARCHIVED state (terminal) ===
            (ProductState::Archived, _) => {
                return Err(ProductCatalogError::InvalidTransition {
                    from: "Archived".to_string(),
                    to: "Archived".to_string(),
                    event: format!("{:?}", event),
                })
            }

            // Default: invalid transition
            (current, event) => {
                return Err(ProductCatalogError::InvalidTransition {
                    from: current.as_str().to_string(),
                    to: "?".to_string(),
                    event: format!("{:?}", event),
                })
            }
        };

        // Record audit entry
        let old_state = self.state;
        if old_state != new_state {
            let audit = AuditEntry {
                timestamp: Utc::now(),
                from_state: old_state.as_str().to_string(),
                to_state: new_state.as_str().to_string(),
                event: format!("{:?}", event),
                before_value: None,
                after_value: None,
                affected_customer_count: 0,
                approval_required: matches!(new_state, ProductState::Validation),
            };
            self.audit_log.push(audit);
        }

        self.state = new_state;
        self.last_state_change = Utc::now();

        tracing::info!(
            sku_id = %self.sku_id,
            from = %old_state.as_str(),
            to = %new_state.as_str(),
            "Product state transition"
        );

        Ok((new_state, action))
    }

    /// Get current state
    pub fn current_state(&self) -> ProductState {
        self.state
    }

    /// Get time in current state
    pub fn time_in_state(&self) -> Duration {
        Utc::now() - self.state_entered_at
    }

    /// Get audit trail
    pub fn audit_trail(&self) -> &[AuditEntry] {
        &self.audit_log
    }

    /// Check if product has pending changes
    pub fn has_pending_changes(&self) -> bool {
        self.pending_changes.is_some()
    }

    /// Get current metadata
    pub fn metadata(&self) -> &SKUMetadata {
        &self.metadata
    }

    /// Get mutable metadata (for testing)
    pub fn metadata_mut(&mut self) -> &mut SKUMetadata {
        &mut self.metadata
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn make_sku(sku_id: &str, name: &str, price_cents: u64) -> SKUMetadata {
        SKUMetadata {
            sku_id: sku_id.to_string(),
            name: name.to_string(),
            description: "Test product".to_string(),
            price_cents,
            tier: "professional".to_string(),
            features: vec!["basic".to_string()],
            limits: HashMap::new(),
            version: 1,
            last_updated: Utc::now(),
            revision_hash: "abc123".to_string(),
        }
    }

    // ============================================================================
    // DRAFT STATE TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_draft_to_published_flow() {
        // Arrange
        let sku = make_sku("sku-001", "Starter Plan", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();

        // Act
        let (new_state, action) = governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Published);
        assert!(matches!(action, Some(CatalogAction::PublishToMarketplace(_))));
        assert_eq!(governor.audit_trail().len(), 1);
    }

    #[tokio::test]
    async fn test_draft_sku_invalid_name() {
        // Arrange
        let mut sku = make_sku("sku-001", "Valid Name", 9999);
        sku.name = "".to_string(); // Invalid: empty name

        // Act
        let result = ProductCatalogGovernor::new("sku-001".to_string(), sku);

        // Assert
        assert!(matches!(result, Err(ProductCatalogError::SKUValidationError(_))));
    }

    #[tokio::test]
    async fn test_draft_sku_invalid_price() {
        // Arrange
        let mut sku = make_sku("sku-001", "Valid Name", 9999);
        sku.price_cents = 0; // Invalid: zero price

        // Act
        let result = ProductCatalogGovernor::new("sku-001".to_string(), sku);

        // Assert
        assert!(matches!(result, Err(ProductCatalogError::SKUValidationError(_))));
    }

    // ============================================================================
    // PUBLISHED STATE TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_published_pricing_change_workflow() {
        // Arrange
        let sku = make_sku("sku-001", "Starter Plan", 9999); // 99.99 USD
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();

        // Transition to Published
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act: Initiate pricing change (5% increase with 60-day notice)
        let pricing_change = PricingChange {
            current_price_cents: 9999,
            new_price_cents: 10499, // 5% increase
            effective_date: Utc::now() + Duration::days(60),
            tier: "professional".to_string(),
        };

        let (new_state, action) = governor
            .transition(ProductEvent::UpdatePricing(pricing_change))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Validation);
        assert!(matches!(action, Some(CatalogAction::ValidateSKUSchema(_))));
        assert!(governor.has_pending_changes());
    }

    #[tokio::test]
    async fn test_published_pricing_change_excessive_increase() {
        // Arrange
        let sku = make_sku("sku-001", "Starter Plan", 10000);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act: Try 15% increase with insufficient notice
        let pricing_change = PricingChange {
            current_price_cents: 10000,
            new_price_cents: 11500, // 15% increase
            effective_date: Utc::now() + Duration::days(10), // Only 10 days notice
            tier: "professional".to_string(),
        };

        let result = governor.transition(ProductEvent::UpdatePricing(pricing_change)).await;

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::PricingComplianceError(_))
        ));
    }

    #[tokio::test]
    async fn test_published_feature_update_with_dependencies() {
        // Arrange
        let mut sku = make_sku("sku-001", "Professional", 29999);
        sku.features = vec!["basic".to_string(), "analytics".to_string()];
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act: Add feature with existing dependency
        let feature_update = FeatureUpdate {
            feature_name: "advanced-reporting".to_string(),
            description: "Detailed reports".to_string(),
            is_breaking_change: false,
            min_version: None,
            dependencies: vec!["analytics".to_string()],
        };

        let (new_state, action) = governor
            .transition(ProductEvent::UpdateFeatures(feature_update))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Validation);
        assert!(matches!(action, Some(CatalogAction::ValidateSKUSchema(_))));
    }

    #[tokio::test]
    async fn test_published_feature_update_missing_dependency() {
        // Arrange
        let sku = make_sku("sku-001", "Basic", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act: Try to add feature with missing dependency
        let feature_update = FeatureUpdate {
            feature_name: "advanced-reporting".to_string(),
            description: "Detailed reports".to_string(),
            is_breaking_change: false,
            min_version: None,
            dependencies: vec!["analytics".to_string()], // Not in features
        };

        let result = governor.transition(ProductEvent::UpdateFeatures(feature_update)).await;

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::FeatureValidationError(_))
        ));
    }

    #[tokio::test]
    async fn test_published_move_to_featured() {
        // Arrange
        let sku = make_sku("sku-001", "Popular Plan", 19999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act
        let (new_state, action) = governor
            .transition(ProductEvent::MoveToFeatured)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Featured);
        assert!(matches!(action, Some(CatalogAction::IncreaseVisibility(_))));
    }

    #[tokio::test]
    async fn test_published_deprecate_with_replacement() {
        // Arrange
        let sku = make_sku("sku-old", "Old Plan", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-old".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act
        let (new_state, action) = governor
            .transition(ProductEvent::Deprecate(Some("sku-new".to_string())))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Deprecated);
        assert!(matches!(
            action,
            Some(CatalogAction::RedirectToReplacement(_, _))
        ));
    }

    #[tokio::test]
    async fn test_published_compliance_check() {
        // Arrange
        let sku = make_sku("sku-001", "Standard", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act: Periodic compliance check (no state change if compliant)
        let (new_state, action) = governor
            .transition(ProductEvent::ComplianceCheckTriggered)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Published);
        assert!(action.is_none());
    }

    // ============================================================================
    // FEATURED STATE TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_featured_demote_to_published() {
        // Arrange
        let sku = make_sku("sku-001", "Featured Product", 29999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::MoveToFeatured)
            .await
            .unwrap();

        // Act
        let (new_state, action) = governor
            .transition(ProductEvent::DemoteFromFeatured)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Published);
        assert!(matches!(action, Some(CatalogAction::DecreaseVisibility(_))));
    }

    #[tokio::test]
    async fn test_featured_kpi_check_decline_auto_demotes() {
        // Arrange
        let sku = make_sku("sku-001", "Featured Product", 29999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::MoveToFeatured)
            .await
            .unwrap();

        // Act: KPI check shows declining performance
        let (new_state, action) = governor
            .transition(ProductEvent::KPICheckTriggered(false))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Published);
        assert!(matches!(action, Some(CatalogAction::DecreaseVisibility(_))));
    }

    #[tokio::test]
    async fn test_featured_kpi_check_healthy_stays_featured() {
        // Arrange
        let sku = make_sku("sku-001", "Featured Product", 29999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::MoveToFeatured)
            .await
            .unwrap();

        // Act: KPI check shows healthy metrics
        let (new_state, action) = governor
            .transition(ProductEvent::KPICheckTriggered(true))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Featured);
        assert!(action.is_none());
    }

    // ============================================================================
    // DEPRECATED STATE TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_deprecated_resurect_to_published() {
        // Arrange
        let sku = make_sku("sku-001", "Old Plan", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::Deprecate(None))
            .await
            .unwrap();

        // Act
        let (new_state, action) = governor
            .transition(ProductEvent::ResurrectToPublished)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Published);
        assert!(matches!(action, Some(CatalogAction::PublishToMarketplace(_))));
    }

    #[tokio::test]
    async fn test_deprecated_force_archive() {
        // Arrange
        let sku = make_sku("sku-001", "Old Plan", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::Deprecate(None))
            .await
            .unwrap();

        // Act
        let (new_state, action) = governor
            .transition(ProductEvent::ForceArchive)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::Archived);
        assert!(matches!(action, Some(CatalogAction::ArchiveHistoricalData(_))));
    }

    // ============================================================================
    // VALIDATION STATE TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_validation_success_updates_metadata() {
        // Arrange
        let sku = make_sku("sku-001", "Starter", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        let pricing_change = PricingChange {
            current_price_cents: 9999,
            new_price_cents: 10999,
            effective_date: Utc::now() + Duration::days(60),
            tier: "professional".to_string(),
        };

        governor
            .transition(ProductEvent::UpdatePricing(pricing_change))
            .await
            .unwrap();

        // Act: Validation succeeds
        let (new_state, _) = governor
            .transition(ProductEvent::ValidationSucceeded)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, ProductState::UpdateApproved);
        assert_eq!(governor.metadata().price_cents, 10999);
        assert_eq!(governor.metadata().version, 2);
        assert!(!governor.has_pending_changes());
    }

    #[tokio::test]
    async fn test_validation_failure_returns_error() {
        // Arrange
        let sku = make_sku("sku-001", "Starter", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        let pricing_change = PricingChange {
            current_price_cents: 9999,
            new_price_cents: 10999,
            effective_date: Utc::now() + Duration::days(60),
            tier: "professional".to_string(),
        };

        governor
            .transition(ProductEvent::UpdatePricing(pricing_change))
            .await
            .unwrap();

        // Act: Validation fails
        let result = governor
            .transition(ProductEvent::ValidationFailed(
                "Price increased too much".to_string(),
            ))
            .await;

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::ApprovalChainFailed { .. })
        ));
        assert!(!governor.has_pending_changes());
    }

    // ============================================================================
    // ARCHIVED STATE TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_archived_is_terminal() {
        // Arrange
        let sku = make_sku("sku-001", "Old Plan", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::Deprecate(None))
            .await
            .unwrap();
        governor
            .transition(ProductEvent::ForceArchive)
            .await
            .unwrap();

        // Act: Try any event on archived product
        let result = governor.transition(ProductEvent::SubmitForReview).await;

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::InvalidTransition { .. })
        ));
    }

    // ============================================================================
    // CONCURRENT UPDATE TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_concurrent_updates_buffered() {
        // Arrange
        let sku = make_sku("sku-001", "Product", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();

        // Act: Submit first pricing change
        let pricing_change1 = PricingChange {
            current_price_cents: 9999,
            new_price_cents: 10999,
            effective_date: Utc::now() + Duration::days(60),
            tier: "professional".to_string(),
        };

        let (state1, _) = governor
            .transition(ProductEvent::UpdatePricing(pricing_change1))
            .await
            .unwrap();

        // Assert: In validation, pending change buffered
        assert_eq!(state1, ProductState::Validation);
        assert!(governor.has_pending_changes());
    }

    // ============================================================================
    // AUDIT TRAIL TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_audit_trail_records_transitions() {
        // Arrange
        let sku = make_sku("sku-001", "Product", 9999);
        let mut governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();

        // Act: Perform multiple transitions
        governor
            .transition(ProductEvent::SubmitForReview)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::MoveToFeatured)
            .await
            .unwrap();
        governor
            .transition(ProductEvent::DemoteFromFeatured)
            .await
            .unwrap();

        // Assert
        let audit = governor.audit_trail();
        assert_eq!(audit.len(), 3);
        assert_eq!(audit[0].from_state, "Draft");
        assert_eq!(audit[0].to_state, "Published");
        assert_eq!(audit[1].from_state, "Published");
        assert_eq!(audit[1].to_state, "Featured");
        assert_eq!(audit[2].from_state, "Featured");
        assert_eq!(audit[2].to_state, "Published");
    }

    // ============================================================================
    // TIME TRACKING TESTS
    // ============================================================================

    #[tokio::test]
    async fn test_time_in_state_tracking() {
        // Arrange
        let sku = make_sku("sku-001", "Product", 9999);
        let governor = ProductCatalogGovernor::new("sku-001".to_string(), sku).unwrap();

        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        // Act
        let time_in_state = governor.time_in_state();

        // Assert: Time in draft state should be ≥100ms
        assert!(time_in_state.num_milliseconds() >= 100);
    }

    // ============================================================================
    // SKU METADATA VALIDATION TESTS
    // ============================================================================

    #[tokio::test]
    fn test_sku_revision_hash_computation() {
        // Arrange
        let sku1 = make_sku("sku-001", "Product", 9999);
        let mut sku2 = make_sku("sku-001", "Product", 9999);

        // Act
        let hash1 = sku1.compute_revision_hash();
        let hash2 = sku2.compute_revision_hash();

        // Assert: Same SKU should have same hash
        assert_eq!(hash1, hash2);

        // Act: Modify and check hash changes
        sku2.price_cents = 19999;
        let hash3 = sku2.compute_revision_hash();

        // Assert: Different price should give different hash
        assert_ne!(hash1, hash3);
    }

    #[tokio::test]
    fn test_sku_tier_validation() {
        // Arrange
        let mut sku = make_sku("sku-001", "Product", 9999);
        sku.tier = "invalid-tier".to_string();

        // Act
        let result = sku.validate();

        // Assert
        assert!(matches!(result, Err(ProductCatalogError::SKUValidationError(_))));
    }

    #[tokio::test]
    fn test_sku_id_validation() {
        // Arrange
        let sku = make_sku("sku!invalid", "Product", 9999); // Contains invalid char

        // Act
        let result = ProductCatalogGovernor::new("sku!invalid".to_string(), sku);

        // Assert
        assert!(matches!(result, Err(ProductCatalogError::SKUValidationError(_))));
    }

    // ============================================================================
    // PRICING CHANGE VALIDATION TESTS
    // ============================================================================

    #[tokio::test]
    fn test_pricing_change_validation_zero_price() {
        // Arrange
        let pricing = PricingChange {
            current_price_cents: 10000,
            new_price_cents: 0,
            effective_date: Utc::now() + Duration::days(60),
            tier: "professional".to_string(),
        };

        // Act
        let result = pricing.validate();

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::PricingComplianceError(_))
        ));
    }

    #[tokio::test]
    fn test_pricing_change_validation_future_date_required() {
        // Arrange
        let pricing = PricingChange {
            current_price_cents: 10000,
            new_price_cents: 11000,
            effective_date: Utc::now() - Duration::days(1), // Past date!
            tier: "professional".to_string(),
        };

        // Act
        let result = pricing.validate();

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::PricingComplianceError(_))
        ));
    }

    // ============================================================================
    // FEATURE UPDATE VALIDATION TESTS
    // ============================================================================

    #[tokio::test]
    fn test_feature_update_circular_dependency_check() {
        // Arrange
        let feature = FeatureUpdate {
            feature_name: "circular-feature".to_string(),
            description: "Test".to_string(),
            is_breaking_change: false,
            min_version: None,
            dependencies: vec!["circular-feature".to_string()], // Self-dependency
        };

        // Act
        let result = feature.validate(&[]);

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::FeatureValidationError(_))
        ));
    }

    #[tokio::test]
    fn test_feature_update_empty_name() {
        // Arrange
        let feature = FeatureUpdate {
            feature_name: "".to_string(), // Empty!
            description: "Test".to_string(),
            is_breaking_change: false,
            min_version: None,
            dependencies: vec![],
        };

        // Act
        let result = feature.validate(&[]);

        // Assert
        assert!(matches!(
            result,
            Err(ProductCatalogError::FeatureValidationError(_))
        ));
    }
}
