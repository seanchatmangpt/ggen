//! Revenue attribution and commission calculations

use super::{RouteSlug, SubscriptionTier};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Revenue event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RevenueEvent {
    /// Event ID
    pub id: Uuid,
    /// Affiliate ID
    pub affiliate_id: Uuid,
    /// Route slug
    pub route_slug: RouteSlug,
    /// Click ID
    pub click_id: Uuid,
    /// Revenue amount in cents
    pub amount_cents: u64,
    /// Commission amount in cents
    pub commission_cents: u64,
    /// Subscription tier at time of event
    pub tier: SubscriptionTier,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
}

impl RevenueEvent {
    /// Create a new revenue event with commission calculation
    pub fn new(
        affiliate_id: Uuid, route_slug: RouteSlug, click_id: Uuid, amount_cents: u64,
        tier: SubscriptionTier,
    ) -> Self {
        let commission_rate = tier.commission_rate();
        let commission_cents = ((amount_cents as f64) * commission_rate) as u64;

        Self {
            id: Uuid::new_v4(),
            affiliate_id,
            route_slug,
            click_id,
            amount_cents,
            commission_cents,
            tier,
            timestamp: Utc::now(),
        }
    }
}

/// Revenue attribution engine
pub struct RevenueAttribution {
    /// Revenue events storage
    events: HashMap<Uuid, RevenueEvent>,
}

impl RevenueAttribution {
    /// Create a new revenue attribution engine
    pub fn new() -> Self {
        Self {
            events: HashMap::new(),
        }
    }

    /// Record a revenue event
    pub fn record_revenue(
        &mut self, affiliate_id: Uuid, route_slug: RouteSlug, click_id: Uuid, amount_cents: u64,
        tier: SubscriptionTier,
    ) -> RevenueEvent {
        let event = RevenueEvent::new(affiliate_id, route_slug, click_id, amount_cents, tier);
        self.events.insert(event.id, event.clone());
        event
    }

    /// Get total revenue for affiliate
    pub fn total_revenue_by_affiliate(&self, affiliate_id: &Uuid) -> u64 {
        self.events
            .values()
            .filter(|event| &event.affiliate_id == affiliate_id)
            .map(|event| event.amount_cents)
            .sum()
    }

    /// Get total commission for affiliate
    pub fn total_commission_by_affiliate(&self, affiliate_id: &Uuid) -> u64 {
        self.events
            .values()
            .filter(|event| &event.affiliate_id == affiliate_id)
            .map(|event| event.commission_cents)
            .sum()
    }

    /// Get revenue by route
    pub fn revenue_by_route(&self, route_slug: &RouteSlug) -> u64 {
        self.events
            .values()
            .filter(|event| &event.route_slug == route_slug)
            .map(|event| event.amount_cents)
            .sum()
    }

    /// Get commission by route
    pub fn commission_by_route(&self, route_slug: &RouteSlug) -> u64 {
        self.events
            .values()
            .filter(|event| &event.route_slug == route_slug)
            .map(|event| event.commission_cents)
            .sum()
    }

    /// Calculate average commission rate for affiliate
    pub fn avg_commission_rate(&self, affiliate_id: &Uuid) -> f64 {
        let events: Vec<_> = self
            .events
            .values()
            .filter(|event| &event.affiliate_id == affiliate_id)
            .collect();

        if events.is_empty() {
            return 0.0;
        }

        let total_revenue: u64 = events.iter().map(|e| e.amount_cents).sum();
        let total_commission: u64 = events.iter().map(|e| e.commission_cents).sum();

        if total_revenue == 0 {
            return 0.0;
        }

        (total_commission as f64) / (total_revenue as f64)
    }

    /// Get all events for affiliate
    pub fn events_by_affiliate(&self, affiliate_id: &Uuid) -> Vec<&RevenueEvent> {
        self.events
            .values()
            .filter(|event| &event.affiliate_id == affiliate_id)
            .collect()
    }

    /// Verify commission calculation accuracy
    pub fn verify_commission(&self, event_id: &Uuid) -> Result<bool, AttributionError> {
        let event = self
            .events
            .get(event_id)
            .ok_or(AttributionError::EventNotFound)?;

        let expected_commission =
            ((event.amount_cents as f64) * event.tier.commission_rate()) as u64;
        Ok(event.commission_cents == expected_commission)
    }
}

impl Default for RevenueAttribution {
    fn default() -> Self {
        Self::new()
    }
}

/// Attribution errors
#[derive(Debug, Clone, thiserror::Error)]
pub enum AttributionError {
    #[error("Revenue event not found")]
    EventNotFound,
    #[error("Invalid commission calculation")]
    InvalidCommission,
}
