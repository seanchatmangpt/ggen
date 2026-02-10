//! Admission control at firewall boundary
//!
//! Enforces rules for all incoming requests before channel-specific logic.

use crate::{FirewallError, IngressRequest, Result};
use serde::{Deserialize, Serialize};

/// Maximum payload size (10MB)
const MAX_PAYLOAD_SIZE: usize = 10 * 1024 * 1024;

/// Admission controller validates requests at boundary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdmissionController {
    max_payload_size: usize,
}

impl AdmissionController {
    pub fn new() -> Self {
        Self {
            max_payload_size: MAX_PAYLOAD_SIZE,
        }
    }

    pub fn with_max_payload_size(max_payload_size: usize) -> Self {
        Self { max_payload_size }
    }

    /// Validate request at boundary before channel admission
    pub fn validate_request(&self, request: &IngressRequest) -> Result<()> {
        // Check payload size
        if request.payload.len() > self.max_payload_size {
            return Err(FirewallError::InvalidRequest(format!(
                "Payload size {} exceeds maximum {}",
                request.payload.len(),
                self.max_payload_size
            )));
        }

        // Check payload not empty
        if request.payload.is_empty() {
            return Err(FirewallError::InvalidRequest(
                "Payload cannot be empty".to_string(),
            ));
        }

        // Check timestamp not in future
        let now = chrono::Utc::now();
        if request.timestamp > now {
            return Err(FirewallError::InvalidRequest(
                "Request timestamp cannot be in the future".to_string(),
            ));
        }

        // Check timestamp not too old (more than 24 hours)
        let max_age = chrono::Duration::hours(24);
        if now - request.timestamp > max_age {
            return Err(FirewallError::InvalidRequest(
                "Request timestamp too old (>24 hours)".to_string(),
            ));
        }

        Ok(())
    }
}

impl Default for AdmissionController {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::IngressChannel;
    use chrono::{Duration, Utc};

    #[test]
    fn test_valid_request() {
        let controller = AdmissionController::new();
        let request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);

        assert!(controller.validate_request(&request).is_ok());
    }

    #[test]
    fn test_empty_payload_rejected() {
        let controller = AdmissionController::new();
        let request = IngressRequest::new(IngressChannel::Batch, vec![]);

        let result = controller.validate_request(&request);
        assert!(matches!(result, Err(FirewallError::InvalidRequest(_))));
    }

    #[test]
    fn test_oversized_payload_rejected() {
        let controller = AdmissionController::with_max_payload_size(100);
        let request = IngressRequest::new(IngressChannel::Batch, vec![0; 101]);

        let result = controller.validate_request(&request);
        assert!(matches!(result, Err(FirewallError::InvalidRequest(_))));
    }

    #[test]
    fn test_future_timestamp_rejected() {
        let controller = AdmissionController::new();
        let mut request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);
        request.timestamp = Utc::now() + Duration::hours(1);

        let result = controller.validate_request(&request);
        assert!(matches!(result, Err(FirewallError::InvalidRequest(_))));
    }

    #[test]
    fn test_old_timestamp_rejected() {
        let controller = AdmissionController::new();
        let mut request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);
        request.timestamp = Utc::now() - Duration::hours(25);

        let result = controller.validate_request(&request);
        assert!(matches!(result, Err(FirewallError::InvalidRequest(_))));
    }

    #[test]
    fn test_boundary_timestamp_accepted() {
        let controller = AdmissionController::new();
        let mut request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);
        request.timestamp = Utc::now() - Duration::hours(23);

        assert!(controller.validate_request(&request).is_ok());
    }
}
