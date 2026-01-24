//! Click tracking and receipt generation

use super::{ClickReceipt, RouteSlug};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Click event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClickEvent {
    /// Click ID
    pub id: Uuid,
    /// Route slug
    pub route_slug: RouteSlug,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
    /// User agent
    pub user_agent: Option<String>,
    /// IP address
    pub ip_address: Option<String>,
    /// Referrer
    pub referrer: Option<String>,
}

impl ClickEvent {
    /// Create a new click event
    pub fn new(route_slug: RouteSlug) -> Self {
        Self {
            id: Uuid::new_v4(),
            route_slug,
            timestamp: Utc::now(),
            user_agent: None,
            ip_address: None,
            referrer: None,
        }
    }

    /// Set user agent
    pub fn with_user_agent(mut self, user_agent: String) -> Self {
        self.user_agent = Some(user_agent);
        self
    }

    /// Set IP address
    pub fn with_ip_address(mut self, ip_address: String) -> Self {
        self.ip_address = Some(ip_address);
        self
    }

    /// Set referrer
    pub fn with_referrer(mut self, referrer: String) -> Self {
        self.referrer = Some(referrer);
        self
    }
}

/// Click tracker with receipt generation
pub struct ClickTracker {
    /// Click events storage
    clicks: HashMap<Uuid, ClickEvent>,
    /// Click receipts storage
    receipts: HashMap<Uuid, ClickReceipt>,
    /// Last receipt hash for chaining
    last_receipt_hash: Option<String>,
}

impl ClickTracker {
    /// Create a new click tracker
    pub fn new() -> Self {
        Self {
            clicks: HashMap::new(),
            receipts: HashMap::new(),
            last_receipt_hash: None,
        }
    }

    /// Track a click event and generate receipt
    pub fn track_click(&mut self, event: ClickEvent) -> Result<ClickReceipt, TrackingError> {
        let click_id = event.id;
        let route_slug = event.route_slug.clone();

        // Store click event
        self.clicks.insert(click_id, event);

        // Generate receipt with blockchain-like chaining
        let receipt = ClickReceipt::new(
            click_id,
            route_slug,
            self.last_receipt_hash.clone(),
        );

        // Verify receipt integrity
        if !receipt.verify() {
            return Err(TrackingError::InvalidReceipt);
        }

        // Update last receipt hash
        self.last_receipt_hash = Some(receipt.hash.clone());

        // Store receipt
        self.receipts.insert(receipt.id, receipt.clone());

        Ok(receipt)
    }

    /// Get click event by ID
    pub fn get_click(&self, click_id: &Uuid) -> Option<&ClickEvent> {
        self.clicks.get(click_id)
    }

    /// Get receipt by ID
    pub fn get_receipt(&self, receipt_id: &Uuid) -> Option<&ClickReceipt> {
        self.receipts.get(receipt_id)
    }

    /// Get all receipts for a route
    pub fn get_receipts_by_route(&self, route_slug: &RouteSlug) -> Vec<&ClickReceipt> {
        self.receipts
            .values()
            .filter(|receipt| &receipt.route_slug == route_slug)
            .collect()
    }

    /// Verify receipt chain integrity
    pub fn verify_chain(&self) -> bool {
        let mut receipts: Vec<_> = self.receipts.values().collect();
        receipts.sort_by_key(|r| r.timestamp);

        let mut prev_hash: Option<String> = None;
        for receipt in receipts {
            // Verify receipt hash
            if !receipt.verify() {
                return false;
            }

            // Verify chain linkage
            if receipt.prev_hash != prev_hash {
                return false;
            }

            prev_hash = Some(receipt.hash.clone());
        }

        true
    }

    /// Get total click count
    pub fn total_clicks(&self) -> usize {
        self.clicks.len()
    }

    /// Get click count by route
    pub fn clicks_by_route(&self, route_slug: &RouteSlug) -> usize {
        self.clicks
            .values()
            .filter(|click| &click.route_slug == route_slug)
            .count()
    }
}

impl Default for ClickTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Click tracking errors
#[derive(Debug, Clone, thiserror::Error)]
pub enum TrackingError {
    #[error("Invalid receipt generated")]
    InvalidReceipt,
    #[error("Click not found")]
    ClickNotFound,
    #[error("Receipt not found")]
    ReceiptNotFound,
}
