//! E2E tests for firewall invalid channel refusal
//!
//! Tests:
//! - Three channel ingress enforcement
//! - Invalid channel request refusal with receipt
//! - Cryptographic refusal receipt generation
//! - Channel-specific admission rules
//! - Emergency channel bypass behavior

use ggen_firewall::{
    channels::{BatchChannel, EmergencyChannel, ScheduledChannel},
    AdmissionResponse, Firewall, IngressChannel, IngressRequest,
};

/// Test three channel ingress enforcement
#[tokio::test]
async fn test_three_channel_enforcement() {
    // Arrange
    let mut firewall = Firewall::with_defaults();

    // Act & Assert - Batch channel
    let batch_request = IngressRequest::new(IngressChannel::Batch, b"batch work".to_vec());
    let response = firewall.process(batch_request).await;
    match response {
        AdmissionResponse::Admitted { channel, .. } => {
            assert_eq!(channel, IngressChannel::Batch);
        }
        AdmissionResponse::Refused { .. } => panic!("Batch request should be admitted"),
    }

    // Act & Assert - Scheduled channel
    let scheduled_request =
        IngressRequest::new(IngressChannel::Scheduled, b"scheduled work".to_vec());
    let response = firewall.process(scheduled_request).await;
    match response {
        AdmissionResponse::Admitted { channel, .. } => {
            assert_eq!(channel, IngressChannel::Scheduled);
        }
        AdmissionResponse::Refused { .. } => panic!("Scheduled request should be admitted"),
    }

    // Act & Assert - Emergency channel
    let emergency_request =
        IngressRequest::new(IngressChannel::Emergency, b"emergency work".to_vec());
    let response = firewall.process(emergency_request).await;
    match response {
        AdmissionResponse::Admitted { channel, .. } => {
            assert_eq!(channel, IngressChannel::Emergency);
        }
        AdmissionResponse::Refused { .. } => panic!("Emergency request should be admitted"),
    }
}

/// Test batch channel rate limiting
#[tokio::test]
async fn test_batch_channel_rate_limiting() {
    // Arrange - Batch channel with capacity 3, rate 1/sec
    let batch = Box::new(BatchChannel::new(3, 1));
    let scheduled = Box::new(ScheduledChannel::new());
    let emergency = Box::new(EmergencyChannel::new());

    let mut firewall = Firewall::new(batch, scheduled, emergency);

    // Act - Send requests up to capacity
    let mut admitted = 0;
    let mut refused = 0;

    for i in 0..10 {
        let request = IngressRequest::new(
            IngressChannel::Batch,
            format!("batch-{}", i).into_bytes(),
        );
        let response = firewall.process(request).await;

        match response {
            AdmissionResponse::Admitted { .. } => admitted += 1,
            AdmissionResponse::Refused { .. } => refused += 1,
        }
    }

    // Assert - Should admit up to capacity, refuse rest
    assert!(admitted > 0, "Should admit some requests");
    assert!(refused > 0, "Should refuse excess requests");
    assert_eq!(admitted + refused, 10);
    assert!(admitted <= 3, "Should not exceed capacity");
}

/// Test refusal receipt generation
#[tokio::test]
async fn test_refusal_receipt_generation() {
    // Arrange - Batch channel with very low capacity
    let batch = Box::new(BatchChannel::new(1, 1));
    let scheduled = Box::new(ScheduledChannel::new());
    let emergency = Box::new(EmergencyChannel::new());

    let mut firewall = Firewall::new(batch, scheduled, emergency);

    // Fill capacity
    let request1 = IngressRequest::new(IngressChannel::Batch, b"first".to_vec());
    let _ = firewall.process(request1).await;

    // Act - Send request that will be refused
    let request2 = IngressRequest::new(IngressChannel::Batch, b"second".to_vec());
    let request_id = request2.id;
    let response = firewall.process(request2).await;

    // Assert - Should be refused with receipt
    match response {
        AdmissionResponse::Refused {
            request_id: refused_id,
            reason,
            receipt,
        } => {
            assert_eq!(refused_id, request_id);
            assert!(!reason.is_empty());
            assert_eq!(receipt.request_id, request_id);
            assert_eq!(receipt.channel, IngressChannel::Batch);
            assert!(!receipt.payload_hash.is_empty());
            assert!(!receipt.signature.is_empty());
        }
        AdmissionResponse::Admitted { .. } => panic!("Request should be refused"),
    }
}

/// Test cryptographic refusal receipt
#[tokio::test]
async fn test_cryptographic_refusal_receipt() {
    // Arrange
    let batch = Box::new(BatchChannel::new(0, 1)); // Zero capacity - always refuses
    let scheduled = Box::new(ScheduledChannel::new());
    let emergency = Box::new(EmergencyChannel::new());

    let mut firewall = Firewall::new(batch, scheduled, emergency);

    // Act - Send request
    let request = IngressRequest::new(IngressChannel::Batch, b"test".to_vec());
    let response = firewall.process(request).await;

    // Assert - Verify receipt properties
    match response {
        AdmissionResponse::Refused { receipt, .. } => {
            // Receipt should have cryptographic signature
            assert!(!receipt.signature.is_empty());
            assert_eq!(receipt.signature.len(), 64); // SHA-256 hash hex-encoded

            // Receipt should have hash
            assert!(!receipt.payload_hash.is_empty());
            assert_eq!(receipt.payload_hash.len(), 64); // SHA-256 hash hex-encoded

            // Receipt should have timestamp
            assert!(receipt.refused_at > chrono::DateTime::UNIX_EPOCH);

            // Receipt should have receipt ID
            assert!(!receipt.receipt_id.is_nil());
        }
        AdmissionResponse::Admitted { .. } => panic!("Should be refused"),
    }
}

/// Test channel-specific admission rules
#[tokio::test]
async fn test_channel_specific_rules() {
    // Arrange - Different configurations per channel
    let batch = Box::new(BatchChannel::new(5, 2)); // Capacity 5, rate 2/sec
    let scheduled = Box::new(ScheduledChannel::new()); // Time-based
    let emergency = Box::new(EmergencyChannel::new()); // Always admits

    let mut firewall = Firewall::new(batch, scheduled, emergency);

    // Act & Assert - Emergency always admits (even in burst)
    let mut emergency_results = Vec::new();
    for i in 0..20 {
        let request = IngressRequest::new(
            IngressChannel::Emergency,
            format!("emergency-{}", i).into_bytes(),
        );
        let response = firewall.process(request).await;
        emergency_results.push(response);
    }

    let emergency_admitted = emergency_results
        .iter()
        .filter(|r| matches!(r, AdmissionResponse::Admitted { .. }))
        .count();

    assert_eq!(
        emergency_admitted, 20,
        "Emergency channel should admit all requests"
    );
}

/// Test emergency channel bypass behavior
#[tokio::test]
async fn test_emergency_bypass() {
    // Arrange
    let mut firewall = Firewall::with_defaults();

    // Act - Send emergency requests in rapid succession
    let mut handles = Vec::new();

    for i in 0..10 {
        let request = IngressRequest::new(
            IngressChannel::Emergency,
            format!("critical-{}", i).into_bytes(),
        )
        .with_metadata(serde_json::json!({
            "severity": "critical",
            "priority": i
        }));

        // Process synchronously to test bypass
        let response = firewall.process(request).await;
        handles.push(response);
    }

    // Assert - All emergency requests admitted
    let admitted_count = handles
        .iter()
        .filter(|r| matches!(r, AdmissionResponse::Admitted { .. }))
        .count();

    assert_eq!(admitted_count, 10, "All emergency requests should bypass");
}

/// Test concurrent channel processing
#[tokio::test]
async fn test_concurrent_channel_processing() {
    // Arrange
    let firewall = std::sync::Arc::new(tokio::sync::Mutex::new(Firewall::with_defaults()));
    let num_requests = 30;

    // Act - Send requests across all channels concurrently
    let mut handles = Vec::new();

    for i in 0..num_requests {
        let firewall = firewall.clone();
        let channel = match i % 3 {
            0 => IngressChannel::Batch,
            1 => IngressChannel::Scheduled,
            _ => IngressChannel::Emergency,
        };

        let handle = tokio::spawn(async move {
            let request = IngressRequest::new(channel, format!("req-{}", i).into_bytes());
            let mut fw = firewall.lock().await;
            fw.process(request).await
        });

        handles.push(handle);
    }

    // Wait for all
    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.expect("Task panicked"))
        .collect();

    // Assert - Verify responses per channel
    let batch_responses: Vec<_> = results
        .iter()
        .filter(|r| match r {
            AdmissionResponse::Admitted { channel, .. } => *channel == IngressChannel::Batch,
            AdmissionResponse::Refused { receipt, .. } => receipt.channel == IngressChannel::Batch,
        })
        .collect();

    let emergency_responses: Vec<_> = results
        .iter()
        .filter(|r| match r {
            AdmissionResponse::Admitted { channel, .. } => *channel == IngressChannel::Emergency,
            AdmissionResponse::Refused { receipt, .. } => {
                receipt.channel == IngressChannel::Emergency
            }
        })
        .collect();

    assert!(!batch_responses.is_empty());
    assert!(!emergency_responses.is_empty());
}

/// Test admission check without processing
#[tokio::test]
async fn test_dry_run_admission_check() {
    // Arrange
    let batch = Box::new(BatchChannel::new(2, 1));
    let scheduled = Box::new(ScheduledChannel::new());
    let emergency = Box::new(EmergencyChannel::new());

    let firewall = Firewall::new(batch, scheduled, emergency);

    // Act - Check admission without processing
    let request = IngressRequest::new(IngressChannel::Batch, b"test".to_vec());

    let can_admit = firewall
        .check_admission(&request)
        .await
        .expect("Check failed");

    // Assert - Should indicate admission possibility
    assert!(can_admit, "Should be able to admit under capacity");
}

/// Test refusal receipt uniqueness
#[tokio::test]
async fn test_refusal_receipt_uniqueness() {
    // Arrange
    let batch = Box::new(BatchChannel::new(0, 1)); // Always refuses
    let scheduled = Box::new(ScheduledChannel::new());
    let emergency = Box::new(EmergencyChannel::new());

    let mut firewall = Firewall::new(batch, scheduled, emergency);

    // Act - Generate multiple refusal receipts
    let mut receipts = Vec::new();

    for i in 0..5 {
        let request = IngressRequest::new(IngressChannel::Batch, format!("req-{}", i).into_bytes());
        let response = firewall.process(request).await;

        if let AdmissionResponse::Refused { receipt, .. } = response {
            receipts.push(receipt);
        }
    }

    // Assert - All receipts should be unique
    assert_eq!(receipts.len(), 5);

    for i in 0..receipts.len() {
        for j in (i + 1)..receipts.len() {
            assert_ne!(
                receipts[i].receipt_id, receipts[j].receipt_id,
                "Receipts {} and {} should have different receipt IDs",
                i, j
            );
            assert_ne!(
                receipts[i].signature, receipts[j].signature,
                "Receipts {} and {} should have different signatures",
                i, j
            );
        }
    }
}

/// Test metadata preservation through firewall
#[tokio::test]
async fn test_metadata_preservation() {
    // Arrange
    let mut firewall = Firewall::with_defaults();
    let metadata = serde_json::json!({
        "priority": "high",
        "source": "api-gateway",
        "correlation_id": "12345"
    });

    // Act
    let request = IngressRequest::new(IngressChannel::Emergency, b"data".to_vec())
        .with_metadata(metadata.clone());

    let request_id = request.id;
    let response = firewall.process(request).await;

    // Assert - Request ID preserved
    match response {
        AdmissionResponse::Admitted {
            request_id: resp_id,
            ..
        } => {
            assert_eq!(resp_id, request_id);
        }
        AdmissionResponse::Refused { .. } => panic!("Emergency should not be refused"),
    }
}
