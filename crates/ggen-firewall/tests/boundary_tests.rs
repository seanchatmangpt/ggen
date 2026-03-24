//! Boundary enforcement tests for Life Firewall
//!
//! Verifies that only 3 channels admit requests and everything else is refused.

use ggen_firewall::{
    AdmissionResponse, Firewall, IngressChannel, IngressRequest,
};
use chrono::{Duration, Utc};

#[tokio::test]
async fn test_only_three_channels_exist() {
    let firewall = Firewall::with_defaults();

    // Verify we can create requests for all 3 channels
    let batch_req = IngressRequest::new(IngressChannel::Batch, vec![1]);
    let scheduled_req = IngressRequest::new(IngressChannel::Scheduled, vec![2]);
    let emergency_req = IngressRequest::new(IngressChannel::Emergency, vec![3]);

    assert_eq!(batch_req.channel, IngressChannel::Batch);
    assert_eq!(scheduled_req.channel, IngressChannel::Scheduled);
    assert_eq!(emergency_req.channel, IngressChannel::Emergency);

    // No other channels should exist (enum has only 3 variants)
    let _proof: () = match IngressChannel::Batch {
        IngressChannel::Batch => (),
        IngressChannel::Scheduled => (),
        IngressChannel::Emergency => (),
        // Compiler enforces: no other variants possible
    };

    // Satisfy clippy
    drop(firewall);
}

#[tokio::test]
async fn test_batch_channel_admits_valid_requests() {
    let mut firewall = Firewall::with_defaults();

    let request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);
    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Admitted { channel, .. } => {
            assert_eq!(channel, IngressChannel::Batch);
        }
        AdmissionResponse::Refused { .. } => {
            panic!("Valid batch request should be admitted");
        }
    }
}

#[tokio::test]
async fn test_scheduled_channel_admits_valid_requests() {
    let mut firewall = Firewall::with_defaults();

    let request = IngressRequest::new(IngressChannel::Scheduled, vec![1, 2, 3]);
    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Admitted { channel, .. } => {
            assert_eq!(channel, IngressChannel::Scheduled);
        }
        AdmissionResponse::Refused { .. } => {
            panic!("Valid scheduled request should be admitted");
        }
    }
}

#[tokio::test]
async fn test_emergency_channel_admits_valid_requests() {
    let mut firewall = Firewall::with_defaults();

    let request = IngressRequest::new(IngressChannel::Emergency, vec![1, 2, 3]);
    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Admitted { channel, .. } => {
            assert_eq!(channel, IngressChannel::Emergency);
        }
        AdmissionResponse::Refused { .. } => {
            panic!("Valid emergency request should be admitted");
        }
    }
}

#[tokio::test]
async fn test_batch_channel_refuses_rate_limited_requests() {
    let mut firewall = Firewall::with_defaults();

    // Flood the batch channel
    for i in 0..15 {
        let request = IngressRequest::new(IngressChannel::Batch, vec![i]);
        let _response = firewall.process(request).await;
    }

    // Next request should be refused
    let request = IngressRequest::new(IngressChannel::Batch, vec![99]);
    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Refused { receipt, .. } => {
            assert!(receipt.verify(), "Refusal receipt should be valid");
            assert_eq!(receipt.channel, IngressChannel::Batch);
        }
        AdmissionResponse::Admitted { .. } => {
            panic!("Rate-limited request should be refused");
        }
    }
}

#[tokio::test]
async fn test_invalid_requests_refused_with_receipt() {
    let mut firewall = Firewall::with_defaults();

    // Empty payload is invalid
    let request = IngressRequest::new(IngressChannel::Batch, vec![]);
    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Refused { receipt, reason, .. } => {
            assert!(receipt.verify(), "Refusal receipt should be valid");
            assert!(reason.contains("empty"), "Reason should mention empty payload");
        }
        AdmissionResponse::Admitted { .. } => {
            panic!("Invalid request should be refused");
        }
    }
}

#[tokio::test]
async fn test_future_timestamp_refused() {
    let mut firewall = Firewall::with_defaults();

    let mut request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);
    request.timestamp = Utc::now() + Duration::hours(1);

    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Refused { receipt, reason, .. } => {
            assert!(receipt.verify());
            assert!(reason.contains("future"));
        }
        AdmissionResponse::Admitted { .. } => {
            panic!("Future timestamp should be refused");
        }
    }
}

#[tokio::test]
async fn test_old_timestamp_refused() {
    let mut firewall = Firewall::with_defaults();

    let mut request = IngressRequest::new(IngressChannel::Scheduled, vec![1, 2, 3]);
    request.timestamp = Utc::now() - Duration::hours(25);

    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Refused { receipt, reason, .. } => {
            assert!(receipt.verify());
            assert!(reason.contains("old"));
        }
        AdmissionResponse::Admitted { .. } => {
            panic!("Old timestamp should be refused");
        }
    }
}

#[tokio::test]
async fn test_refusal_receipt_has_all_fields() {
    let mut firewall = Firewall::with_defaults();

    let request = IngressRequest::new(IngressChannel::Batch, vec![]);
    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Refused { receipt, .. } => {
            // Verify all receipt fields are populated
            assert!(!receipt.request_id.is_nil());
            assert!(!receipt.reason.is_empty());
            assert!(!receipt.payload_hash.is_empty());
            assert!(!receipt.signature.is_empty());
            assert!(!receipt.receipt_id.is_nil());

            // Verify signature is valid
            assert!(receipt.verify());
        }
        AdmissionResponse::Admitted { .. } => {
            panic!("Should have been refused");
        }
    }
}

#[tokio::test]
async fn test_receipt_json_serialization() {
    let mut firewall = Firewall::with_defaults();

    let request = IngressRequest::new(IngressChannel::Emergency, vec![]);
    let response = firewall.process(request).await;

    match response {
        AdmissionResponse::Refused { receipt, .. } => {
            // Test JSON roundtrip
            let json = receipt.to_json().expect("Should serialize to JSON");
            assert!(!json.is_empty());

            let restored = ggen_firewall::refusal::RefusalReceipt::from_json(&json)
                .expect("Should deserialize from JSON");

            assert_eq!(receipt.request_id, restored.request_id);
            assert_eq!(receipt.signature, restored.signature);
            assert!(restored.verify());
        }
        AdmissionResponse::Admitted { .. } => {
            panic!("Should have been refused");
        }
    }
}

#[tokio::test]
async fn test_check_admission_dry_run() {
    let firewall = Firewall::with_defaults();

    let valid_request = IngressRequest::new(IngressChannel::Batch, vec![1, 2, 3]);
    let can_admit = firewall.check_admission(&valid_request).await;
    assert!(can_admit.is_ok());

    let invalid_request = IngressRequest::new(IngressChannel::Scheduled, vec![]);
    let can_admit = firewall.check_admission(&invalid_request).await;
    assert!(can_admit.is_err());
}

#[tokio::test]
async fn test_firewall_enforces_boundary() {
    let mut firewall = Firewall::with_defaults();

    // Test all 3 channels can receive valid requests
    let channels = [
        IngressChannel::Batch,
        IngressChannel::Scheduled,
        IngressChannel::Emergency,
    ];

    for channel in channels {
        let request = IngressRequest::new(channel, vec![1, 2, 3]);
        let response = firewall.process(request).await;

        assert!(
            matches!(response, AdmissionResponse::Admitted { .. }),
            "Channel {:?} should admit valid requests",
            channel
        );
    }
}
