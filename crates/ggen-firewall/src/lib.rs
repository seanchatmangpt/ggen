//! Life Firewall: 3 Ingress Channels Only
//!
//! Three channels control all ingress:
//! - Batch: Scheduled bulk operations with rate limits
//! - Scheduled: Time-windowed planned operations
//! - Emergency: Circuit-breaker bypass for critical operations
//!
//! Everything else gets refused with cryptographic receipt.

pub mod admission;
pub mod channels;
pub mod refusal;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;
use uuid::Uuid;

/// The three permitted ingress channels
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IngressChannel {
    /// Batch operations: scheduled bulk work with rate limits
    Batch,
    /// Scheduled operations: time-windowed planned work
    Scheduled,
    /// Emergency operations: circuit-breaker bypass for critical work
    Emergency,
}

impl fmt::Display for IngressChannel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Batch => write!(f, "Batch"),
            Self::Scheduled => write!(f, "Scheduled"),
            Self::Emergency => write!(f, "Emergency"),
        }
    }
}

/// Request entering the firewall
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IngressRequest {
    pub id: Uuid,
    pub channel: IngressChannel,
    pub payload: Vec<u8>,
    pub timestamp: DateTime<Utc>,
    pub metadata: serde_json::Value,
}

impl IngressRequest {
    pub fn new(channel: IngressChannel, payload: Vec<u8>) -> Self {
        Self {
            id: Uuid::new_v4(),
            channel,
            payload,
            timestamp: Utc::now(),
            metadata: serde_json::json!({}),
        }
    }

    pub fn with_metadata(mut self, metadata: serde_json::Value) -> Self {
        self.metadata = metadata;
        self
    }
}

/// Response from firewall admission control
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AdmissionResponse {
    /// Request admitted through channel
    Admitted {
        request_id: Uuid,
        channel: IngressChannel,
        admitted_at: DateTime<Utc>,
    },
    /// Request refused with cryptographic receipt
    Refused {
        request_id: Uuid,
        reason: String,
        receipt: refusal::RefusalReceipt,
    },
}

/// Firewall errors
#[derive(Error, Debug)]
pub enum FirewallError {
    #[error("Channel {0} is not available")]
    ChannelUnavailable(IngressChannel),

    #[error("Rate limit exceeded for channel {0}")]
    RateLimitExceeded(IngressChannel),

    #[error("Invalid request: {0}")]
    InvalidRequest(String),

    #[error("Admission denied: {0}")]
    AdmissionDenied(String),

    #[error("Internal error: {0}")]
    Internal(String),
}

pub type Result<T> = std::result::Result<T, FirewallError>;

/// Channel trait for ingress channel implementations
#[async_trait]
pub trait Channel: Send + Sync {
    /// Check if request can be admitted through this channel
    async fn can_admit(&self, request: &IngressRequest) -> Result<bool>;

    /// Admit request through channel
    async fn admit(&mut self, request: IngressRequest) -> Result<AdmissionResponse>;

    /// Get channel type
    fn channel_type(&self) -> IngressChannel;
}

/// Life Firewall: Controls all ingress through 3 channels only
pub struct Firewall {
    batch: Box<dyn Channel>,
    scheduled: Box<dyn Channel>,
    emergency: Box<dyn Channel>,
}

impl Firewall {
    /// Create new firewall with channel implementations
    pub fn new(
        batch: Box<dyn Channel>,
        scheduled: Box<dyn Channel>,
        emergency: Box<dyn Channel>,
    ) -> Self {
        Self {
            batch,
            scheduled,
            emergency,
        }
    }

    /// Create firewall with default channel implementations
    pub fn with_defaults() -> Self {
        Self {
            batch: Box::new(channels::BatchChannel::new(100, 10)),
            scheduled: Box::new(channels::ScheduledChannel::new()),
            emergency: Box::new(channels::EmergencyChannel::new()),
        }
    }

    /// Process ingress request through appropriate channel
    pub async fn process(&mut self, request: IngressRequest) -> AdmissionResponse {
        let channel = match request.channel {
            IngressChannel::Batch => &mut self.batch,
            IngressChannel::Scheduled => &mut self.scheduled,
            IngressChannel::Emergency => &mut self.emergency,
        };

        // Attempt admission through channel
        match channel.admit(request.clone()).await {
            Ok(response) => response,
            Err(e) => {
                // Generate refusal receipt
                let receipt = refusal::RefusalReceipt::new(
                    request.id,
                    request.channel,
                    e.to_string(),
                    serde_json::to_vec(&request).unwrap_or_default(),
                );

                AdmissionResponse::Refused {
                    request_id: request.id,
                    reason: e.to_string(),
                    receipt,
                }
            }
        }
    }

    /// Check if request would be admitted (dry-run)
    pub async fn check_admission(&self, request: &IngressRequest) -> Result<bool> {
        let channel = match request.channel {
            IngressChannel::Batch => &self.batch,
            IngressChannel::Scheduled => &self.scheduled,
            IngressChannel::Emergency => &self.emergency,
        };

        channel.can_admit(request).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_firewall_creation() {
        let firewall = Firewall::with_defaults();
        assert!(std::mem::size_of_val(&firewall) > 0);
    }

    #[tokio::test]
    async fn test_ingress_request_creation() {
        let request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);
        assert_eq!(request.channel, IngressChannel::Batch);
        assert_eq!(request.payload, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn test_channel_display() {
        assert_eq!(format!("{}", IngressChannel::Batch), "Batch");
        assert_eq!(format!("{}", IngressChannel::Scheduled), "Scheduled");
        assert_eq!(format!("{}", IngressChannel::Emergency), "Emergency");
    }
}
