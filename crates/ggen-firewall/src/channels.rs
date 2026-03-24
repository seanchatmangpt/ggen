//! Channel implementations for the three ingress channels

use crate::{
    admission::AdmissionController, AdmissionResponse, Channel, FirewallError, IngressChannel,
    IngressRequest, Result,
};
use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use std::collections::VecDeque;

/// Batch Channel: Rate-limited bulk operations
pub struct BatchChannel {
    max_batch_size: usize,
    rate_limit_per_minute: usize,
    window_start: DateTime<Utc>,
    requests_in_window: usize,
    queue: VecDeque<IngressRequest>,
    controller: AdmissionController,
}

impl BatchChannel {
    pub fn new(max_batch_size: usize, rate_limit_per_minute: usize) -> Self {
        Self {
            max_batch_size,
            rate_limit_per_minute,
            window_start: Utc::now(),
            requests_in_window: 0,
            queue: VecDeque::new(),
            controller: AdmissionController::new(),
        }
    }

    fn reset_window_if_needed(&mut self) {
        let now = Utc::now();
        if now - self.window_start >= Duration::minutes(1) {
            self.window_start = now;
            self.requests_in_window = 0;
        }
    }

    fn can_accept_batch(&mut self) -> bool {
        self.reset_window_if_needed();
        self.requests_in_window < self.rate_limit_per_minute
            && self.queue.len() < self.max_batch_size
    }
}

#[async_trait]
impl Channel for BatchChannel {
    async fn can_admit(&self, request: &IngressRequest) -> Result<bool> {
        if request.channel != IngressChannel::Batch {
            return Ok(false);
        }

        self.controller.validate_request(request)?;
        Ok(true)
    }

    async fn admit(&mut self, request: IngressRequest) -> Result<AdmissionResponse> {
        // Validate channel
        if request.channel != IngressChannel::Batch {
            return Err(FirewallError::InvalidRequest(format!(
                "Request channel {} does not match Batch channel",
                request.channel
            )));
        }

        // Check admission rules
        self.controller.validate_request(&request)?;

        // Check rate limits
        if !self.can_accept_batch() {
            return Err(FirewallError::RateLimitExceeded(IngressChannel::Batch));
        }

        // Admit request
        self.requests_in_window += 1;
        let request_id = request.id;
        let admitted_at = Utc::now();

        self.queue.push_back(request);

        Ok(AdmissionResponse::Admitted {
            request_id,
            channel: IngressChannel::Batch,
            admitted_at,
        })
    }

    fn channel_type(&self) -> IngressChannel {
        IngressChannel::Batch
    }
}

/// Scheduled Channel: Time-windowed planned operations
pub struct ScheduledChannel {
    scheduled_windows: Vec<(DateTime<Utc>, DateTime<Utc>)>,
    controller: AdmissionController,
}

impl ScheduledChannel {
    pub fn new() -> Self {
        Self {
            scheduled_windows: Vec::new(),
            controller: AdmissionController::new(),
        }
    }

    pub fn add_window(&mut self, start: DateTime<Utc>, end: DateTime<Utc>) {
        self.scheduled_windows.push((start, end));
    }

    fn is_within_window(&self, time: DateTime<Utc>) -> bool {
        if self.scheduled_windows.is_empty() {
            return true; // No windows defined = always open
        }

        self.scheduled_windows
            .iter()
            .any(|(start, end)| time >= *start && time <= *end)
    }
}

impl Default for ScheduledChannel {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Channel for ScheduledChannel {
    async fn can_admit(&self, request: &IngressRequest) -> Result<bool> {
        if request.channel != IngressChannel::Scheduled {
            return Ok(false);
        }

        self.controller.validate_request(request)?;

        Ok(self.is_within_window(request.timestamp))
    }

    async fn admit(&mut self, request: IngressRequest) -> Result<AdmissionResponse> {
        // Validate channel
        if request.channel != IngressChannel::Scheduled {
            return Err(FirewallError::InvalidRequest(format!(
                "Request channel {} does not match Scheduled channel",
                request.channel
            )));
        }

        // Check admission rules
        self.controller.validate_request(&request)?;

        // Check time window
        if !self.is_within_window(request.timestamp) {
            return Err(FirewallError::AdmissionDenied(
                "Request outside scheduled window".to_string(),
            ));
        }

        Ok(AdmissionResponse::Admitted {
            request_id: request.id,
            channel: IngressChannel::Scheduled,
            admitted_at: Utc::now(),
        })
    }

    fn channel_type(&self) -> IngressChannel {
        IngressChannel::Scheduled
    }
}

/// Emergency Channel: Circuit-breaker bypass for critical operations
pub struct EmergencyChannel {
    enabled: bool,
    emergency_count: usize,
    controller: AdmissionController,
}

impl EmergencyChannel {
    pub fn new() -> Self {
        Self {
            enabled: true,
            emergency_count: 0,
            controller: AdmissionController::new(),
        }
    }

    pub fn enable(&mut self) {
        self.enabled = true;
    }

    pub fn disable(&mut self) {
        self.enabled = false;
    }

    pub fn emergency_count(&self) -> usize {
        self.emergency_count
    }
}

impl Default for EmergencyChannel {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Channel for EmergencyChannel {
    async fn can_admit(&self, request: &IngressRequest) -> Result<bool> {
        if request.channel != IngressChannel::Emergency {
            return Ok(false);
        }

        if !self.enabled {
            return Ok(false);
        }

        self.controller.validate_request(request)?;
        Ok(true)
    }

    async fn admit(&mut self, request: IngressRequest) -> Result<AdmissionResponse> {
        // Validate channel
        if request.channel != IngressChannel::Emergency {
            return Err(FirewallError::InvalidRequest(format!(
                "Request channel {} does not match Emergency channel",
                request.channel
            )));
        }

        // Check if emergency channel is enabled
        if !self.enabled {
            return Err(FirewallError::ChannelUnavailable(
                IngressChannel::Emergency,
            ));
        }

        // Check admission rules
        self.controller.validate_request(&request)?;

        // Admit emergency request (always admitted if enabled)
        self.emergency_count += 1;

        Ok(AdmissionResponse::Admitted {
            request_id: request.id,
            channel: IngressChannel::Emergency,
            admitted_at: Utc::now(),
        })
    }

    fn channel_type(&self) -> IngressChannel {
        IngressChannel::Emergency
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_batch_channel_rate_limit() {
        let mut channel = BatchChannel::new(100, 5);

        // Should admit up to rate limit
        for i in 0..5 {
            let request = IngressRequest::new(IngressChannel::Batch, vec![i]);
            let response = channel.admit(request).await;
            assert!(response.is_ok());
        }

        // Should refuse when rate limit exceeded
        let request = IngressRequest::new(IngressChannel::Batch, vec![6]);
        let response = channel.admit(request).await;
        assert!(matches!(response, Err(FirewallError::RateLimitExceeded(_))));
    }

    #[tokio::test]
    async fn test_scheduled_channel_window() {
        let mut channel = ScheduledChannel::new();
        let now = Utc::now();
        let future = now + Duration::hours(1);

        channel.add_window(now, future);

        let request = IngressRequest::new(IngressChannel::Scheduled, vec![1, 2, 3]);
        let response = channel.admit(request).await;
        assert!(response.is_ok());
    }

    #[tokio::test]
    async fn test_emergency_channel_enabled() {
        let mut channel = EmergencyChannel::new();

        let request = IngressRequest::new(IngressChannel::Emergency, vec![1, 2, 3]);
        let response = channel.admit(request).await;
        assert!(response.is_ok());
        assert_eq!(channel.emergency_count(), 1);
    }

    #[tokio::test]
    async fn test_emergency_channel_disabled() {
        let mut channel = EmergencyChannel::new();
        channel.disable();

        let request = IngressRequest::new(IngressChannel::Emergency, vec![1, 2, 3]);
        let response = channel.admit(request).await;
        assert!(matches!(response, Err(FirewallError::ChannelUnavailable(_))));
    }

    #[tokio::test]
    async fn test_channel_type_validation() {
        let mut batch = BatchChannel::new(100, 10);
        let wrong_request = IngressRequest::new(IngressChannel::Scheduled, vec![1]);

        let response = batch.admit(wrong_request).await;
        assert!(matches!(response, Err(FirewallError::InvalidRequest(_))));
    }
}
