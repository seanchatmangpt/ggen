//! FactoryPaaS Domain Models
//!
//! Affiliate marketing and content publishing platform domain models.
//! Handles affiliate link routing, click tracking, revenue attribution,
//! and content publishing workflows.

pub mod affiliate;
pub mod click_tracking;
pub mod content;
pub mod revenue;
pub mod subscription;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

/// Route slug for affiliate links (e.g., /r/{route_slug})
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RouteSlug(String);

impl RouteSlug {
    /// Create a new route slug with validation
    pub fn new(slug: String) -> Result<Self, InvalidRouteSlug> {
        if slug.is_empty() {
            return Err(InvalidRouteSlug::Empty);
        }
        if !slug.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_') {
            return Err(InvalidRouteSlug::InvalidCharacters);
        }
        Ok(Self(slug))
    }

    /// Get the slug as a string slice
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for RouteSlug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Errors that can occur when creating a route slug
#[derive(Debug, Clone, thiserror::Error)]
pub enum InvalidRouteSlug {
    #[error("Route slug cannot be empty")]
    Empty,
    #[error("Route slug contains invalid characters (only alphanumeric, dash, underscore allowed)")]
    InvalidCharacters,
}

/// Click receipt with cryptographic verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClickReceipt {
    /// Unique receipt ID
    pub id: Uuid,
    /// Click event ID
    pub click_id: Uuid,
    /// Route slug
    pub route_slug: RouteSlug,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
    /// SHA-256 hash of receipt data
    pub hash: String,
    /// Previous receipt hash (for blockchain-like verification)
    pub prev_hash: Option<String>,
}

impl ClickReceipt {
    /// Verify receipt integrity
    pub fn verify(&self) -> bool {
        let computed_hash = self.compute_hash();
        computed_hash == self.hash
    }

    /// Compute SHA-256 hash of receipt data
    fn compute_hash(&self) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(self.id.to_string().as_bytes());
        hasher.update(self.click_id.to_string().as_bytes());
        hasher.update(self.route_slug.as_str().as_bytes());
        hasher.update(self.timestamp.to_rfc3339().as_bytes());
        if let Some(ref prev) = self.prev_hash {
            hasher.update(prev.as_bytes());
        }
        format!("{:x}", hasher.finalize())
    }

    /// Create a new click receipt
    pub fn new(
        click_id: Uuid,
        route_slug: RouteSlug,
        prev_hash: Option<String>,
    ) -> Self {
        let id = Uuid::new_v4();
        let timestamp = Utc::now();
        let mut receipt = Self {
            id,
            click_id,
            route_slug,
            timestamp,
            hash: String::new(),
            prev_hash,
        };
        receipt.hash = receipt.compute_hash();
        receipt
    }
}

/// Subscription tier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SubscriptionTier {
    Free,
    Starter,
    Professional,
    Enterprise,
}

impl SubscriptionTier {
    /// Get monthly quota for clicks
    pub fn click_quota(&self) -> Option<u64> {
        match self {
            Self::Free => Some(100),
            Self::Starter => Some(10_000),
            Self::Professional => Some(100_000),
            Self::Enterprise => None, // Unlimited
        }
    }

    /// Get commission rate (0.0 to 1.0)
    pub fn commission_rate(&self) -> f64 {
        match self {
            Self::Free => 0.05,
            Self::Starter => 0.10,
            Self::Professional => 0.15,
            Self::Enterprise => 0.20,
        }
    }
}

/// Content publication status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PublicationStatus {
    Draft,
    Scheduled,
    Published,
    Archived,
}
