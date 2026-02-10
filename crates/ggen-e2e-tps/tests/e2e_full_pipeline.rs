//! E2E tests for complete A2A/TPS pipeline
//!
//! Tests the full flow: Firewall → Packet → A2A → Receipt
//!
//! Pipeline stages:
//! 1. Firewall: Ingress admission control (3 channels)
//! 2. Packet: Work order validation and routing
//! 3. A2A: Agent-to-agent communication protocol
//! 4. Receipt: Cryptographic receipt generation and chaining
//!
//! Tests:
//! - Complete pipeline success path
//! - Pipeline failure propagation
//! - Receipt chain across pipeline stages
//! - Backpressure integration with firewall
//! - Jidoka integration with pipeline

use ed25519_dalek::SigningKey;
use ggen_backpressure::{limiter::RateLimiter, limiter::RateLimiterConfig, AdmissionController};
use ggen_firewall::{
    channels::{BatchChannel, EmergencyChannel, ScheduledChannel},
    AdmissionResponse, Firewall, IngressChannel, IngressRequest,
};
use ggen_jidoka::{AndonSignal, Gate, ProductionLine, Signal};
use ggen_packet::{Priority, WorkOrder};
use ggen_receipt::{generate_keypair, hash_data, Receipt};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Pipeline context holding state across stages
struct PipelineContext {
    receipts: Vec<Receipt>,
    work_orders: Vec<WorkOrder>,
    signing_key: SigningKey,
}

impl PipelineContext {
    fn new(signing_key: SigningKey) -> Self {
        Self {
            receipts: Vec::new(),
            work_orders: Vec::new(),
            signing_key,
        }
    }

    fn add_receipt(&mut self, receipt: Receipt) {
        self.receipts.push(receipt);
    }

    fn add_work_order(&mut self, wo: WorkOrder) {
        self.work_orders.push(wo);
    }

    fn last_receipt(&self) -> Option<&Receipt> {
        self.receipts.last()
    }
}

/// Test complete pipeline success path
#[tokio::test]
async fn test_complete_pipeline_success() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let mut context = PipelineContext::new(signing_key);
    let mut firewall = Firewall::with_defaults();

    // Stage 1: Firewall - Submit batch request
    let payload = serde_json::to_vec(&serde_json::json!({
        "objective": "Process payment",
        "owner": "payment-service"
    }))
    .expect("Failed to serialize");

    let request = IngressRequest::new(IngressChannel::Batch, payload.clone());
    let request_id = request.id;

    let firewall_response = firewall.process(request).await;

    // Assert Stage 1 - Admitted
    match firewall_response {
        AdmissionResponse::Admitted { request_id: rid, .. } => {
            assert_eq!(rid, request_id);

            // Generate receipt for admission
            let receipt = Receipt::new(
                format!("firewall-admit-{}", rid),
                vec![hash_data(&payload)],
                vec![hash_data(b"admitted")],
                None,
            )
            .sign(&context.signing_key)
            .expect("Failed to sign");

            context.add_receipt(receipt);
        }
        AdmissionResponse::Refused { .. } => panic!("Request should be admitted"),
    }

    // Stage 2: Packet - Create work order
    let work_order = WorkOrder::new(
        "Process payment".to_string(),
        "payment-service".to_string(),
    )
    .expect("Failed to create work order")
    .with_priority(Priority::High)
    .expect("Failed to set priority");

    context.add_work_order(work_order.clone());

    // Generate receipt for work order creation
    let wo_bytes = serde_json::to_vec(&work_order).expect("Failed to serialize");
    let receipt = Receipt::new(
        format!("workorder-{}", work_order.id),
        vec![hash_data(b"admitted")],
        vec![hash_data(&wo_bytes)],
        None,
    )
    .chain(context.last_receipt().unwrap())
    .expect("Failed to chain")
    .sign(&context.signing_key)
    .expect("Failed to sign");

    context.add_receipt(receipt);

    // Stage 3: A2A - Simulate agent processing (placeholder)
    let agent_result = serde_json::json!({
        "status": "completed",
        "transaction_id": "txn-12345"
    });

    let agent_bytes = serde_json::to_vec(&agent_result).expect("Failed to serialize");
    let receipt = Receipt::new(
        "agent-processing".to_string(),
        vec![hash_data(&wo_bytes)],
        vec![hash_data(&agent_bytes)],
        None,
    )
    .chain(context.last_receipt().unwrap())
    .expect("Failed to chain")
    .sign(&context.signing_key)
    .expect("Failed to sign");

    context.add_receipt(receipt);

    // Stage 4: Final receipt
    let final_receipt = Receipt::new(
        "pipeline-complete".to_string(),
        vec![hash_data(&agent_bytes)],
        vec![hash_data(b"success")],
        None,
    )
    .chain(context.last_receipt().unwrap())
    .expect("Failed to chain")
    .sign(&context.signing_key)
    .expect("Failed to sign");

    context.add_receipt(final_receipt);

    // Assert - Complete chain verification
    assert_eq!(context.receipts.len(), 4);

    // Verify all signatures
    for (i, receipt) in context.receipts.iter().enumerate() {
        assert!(
            receipt.verify(&verifying_key).is_ok(),
            "Receipt {} signature failed",
            i
        );
    }

    // Verify chain integrity
    for i in 1..context.receipts.len() {
        let prev_hash = context.receipts[i - 1].hash().expect("Failed to hash");
        assert_eq!(
            context.receipts[i].previous_receipt_hash,
            Some(prev_hash),
            "Receipt {} not properly chained",
            i
        );
    }
}

/// Test pipeline failure propagation
#[tokio::test]
async fn test_pipeline_failure_propagation() {
    // Arrange - Firewall with zero capacity (always refuses)
    let batch = Box::new(BatchChannel::new(0, 1));
    let scheduled = Box::new(ScheduledChannel::new());
    let emergency = Box::new(EmergencyChannel::new());

    let mut firewall = Firewall::new(batch, scheduled, emergency);
    let (signing_key, _) = generate_keypair();
    let mut context = PipelineContext::new(signing_key);

    // Act - Submit request
    let request = IngressRequest::new(IngressChannel::Batch, b"test".to_vec());
    let response = firewall.process(request).await;

    // Assert - Pipeline halts at firewall
    match response {
        AdmissionResponse::Refused {
            receipt: refusal_receipt,
            ..
        } => {
            // Generate failure receipt
            let receipt = Receipt::new(
                "firewall-refused".to_string(),
                vec![hash_data(b"test")],
                vec![hash_data(refusal_receipt.payload_hash.as_bytes())],
                None,
            )
            .sign(&context.signing_key)
            .expect("Failed to sign");

            context.add_receipt(receipt);

            // Verify failure recorded
            assert_eq!(context.receipts.len(), 1);
            assert_eq!(context.work_orders.len(), 0); // No work orders created
        }
        AdmissionResponse::Admitted { .. } => panic!("Should be refused"),
    }
}

/// Test receipt chain across pipeline stages
#[tokio::test]
async fn test_receipt_chain_across_stages() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let mut receipts = Vec::new();

    // Stage 1: Firewall admission
    let stage1 = Receipt::new(
        "stage-firewall".to_string(),
        vec![hash_data(b"request-data")],
        vec![hash_data(b"admitted")],
        None,
    )
    .sign(&signing_key)
    .expect("Failed to sign");
    receipts.push(stage1);

    // Stage 2: Packet validation
    let stage2 = Receipt::new(
        "stage-packet".to_string(),
        vec![hash_data(b"admitted")],
        vec![hash_data(b"validated")],
        None,
    )
    .chain(&receipts[0])
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");
    receipts.push(stage2);

    // Stage 3: A2A processing
    let stage3 = Receipt::new(
        "stage-a2a".to_string(),
        vec![hash_data(b"validated")],
        vec![hash_data(b"processed")],
        None,
    )
    .chain(&receipts[1])
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");
    receipts.push(stage3);

    // Stage 4: Final receipt
    let stage4 = Receipt::new(
        "stage-complete".to_string(),
        vec![hash_data(b"processed")],
        vec![hash_data(b"success")],
        None,
    )
    .chain(&receipts[2])
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");
    receipts.push(stage4);

    // Assert - Verify full chain
    assert_eq!(receipts.len(), 4);

    for (i, receipt) in receipts.iter().enumerate() {
        assert!(
            receipt.verify(&verifying_key).is_ok(),
            "Stage {} signature failed",
            i
        );

        if i > 0 {
            let prev_hash = receipts[i - 1].hash().expect("Failed to hash");
            assert_eq!(
                receipt.previous_receipt_hash,
                Some(prev_hash),
                "Stage {} not chained",
                i
            );
        } else {
            assert!(receipt.previous_receipt_hash.is_none(), "Stage 0 should be genesis");
        }
    }
}

/// Test backpressure integration with firewall
#[tokio::test]
async fn test_backpressure_firewall_integration() {
    // Arrange - Limited capacity in both firewall and rate limiter
    let batch = Box::new(BatchChannel::new(2, 1));
    let scheduled = Box::new(ScheduledChannel::new());
    let emergency = Box::new(EmergencyChannel::new());

    let mut firewall = Firewall::new(batch, scheduled, emergency);

    let limiter_config = RateLimiterConfig {
        max_rps: 100.0,
        max_concurrent: 3,
        burst_size: 3,
    };
    let limiter = RateLimiter::new(limiter_config);

    // Act - Process requests with backpressure
    let mut admitted_count = 0;
    let mut refused_count = 0;
    let mut backpressure_count = 0;

    for i in 0..10 {
        // Check backpressure first - may get error or None
        match limiter.try_acquire() {
            Ok(Some(_token)) => {
                // Got token, continue to firewall
            }
            Ok(None) | Err(_) => {
                backpressure_count += 1;
                continue;
            }
        }

        // Try firewall
        let request = IngressRequest::new(
            IngressChannel::Batch,
            format!("req-{}", i).into_bytes(),
        );
        let response = firewall.process(request).await;

        match response {
            AdmissionResponse::Admitted { .. } => admitted_count += 1,
            AdmissionResponse::Refused { .. } => refused_count += 1,
        }
    }

    // Assert - Backpressure and firewall both limiting
    assert!(backpressure_count > 0, "Backpressure should trigger");
    assert!(admitted_count > 0, "Some should be admitted");
    assert_eq!(
        admitted_count + refused_count + backpressure_count,
        10,
        "All requests accounted for"
    );
}

/// Test Jidoka integration with pipeline
#[tokio::test]
async fn test_jidoka_pipeline_integration() {
    // Arrange - Mock gate for pipeline stage
    struct PipelineGate {
        name: String,
        should_pass: bool,
    }

    #[async_trait::async_trait]
    impl ggen_jidoka::Signal for PipelineGate {
        async fn check(&self) -> ggen_jidoka::Result<AndonSignal> {
            Ok(if self.should_pass {
                AndonSignal::Green
            } else {
                AndonSignal::Red
            })
        }

        fn name(&self) -> &str {
            &self.name
        }

        fn description(&self) -> &str {
            "Pipeline validation gate"
        }
    }

    #[async_trait::async_trait]
    impl Gate for PipelineGate {
        async fn execute(&self) -> ggen_jidoka::Result<AndonSignal> {
            self.check().await
        }
    }

    // Create production line with gates
    let mut line = ProductionLine::new();
    line.add_gate(Arc::new(PipelineGate {
        name: "firewall-check".to_string(),
        should_pass: true,
    }))
    .add_gate(Arc::new(PipelineGate {
        name: "packet-validation".to_string(),
        should_pass: true,
    }))
    .add_gate(Arc::new(PipelineGate {
        name: "a2a-processing".to_string(),
        should_pass: false, // This gate fails
    }));

    // Act
    let result = line.run().await;

    // Assert - Line should halt at failed gate
    assert!(result.is_err(), "Pipeline should halt on quality issue");
}

/// Test concurrent pipeline execution
#[tokio::test]
async fn test_concurrent_pipeline_execution() {
    // Arrange
    let firewall = Arc::new(RwLock::new(Firewall::with_defaults()));
    let (signing_key, verifying_key) = generate_keypair();
    let signing_key = Arc::new(signing_key);
    let num_pipelines = 10;

    // Act - Execute multiple pipelines concurrently
    let mut handles = Vec::new();

    for pipeline_id in 0..num_pipelines {
        let firewall = firewall.clone();
        let signing_key = signing_key.clone();

        let handle = tokio::spawn(async move {
            let mut receipts = Vec::new();

            // Firewall stage
            let request = IngressRequest::new(
                IngressChannel::Emergency, // Use emergency to ensure admission
                format!("pipeline-{}", pipeline_id).into_bytes(),
            );

            let response = firewall.write().await.process(request).await;

            match response {
                AdmissionResponse::Admitted { request_id, .. } => {
                    // Generate receipt
                    let receipt = Receipt::new(
                        format!("pipeline-{}-firewall", pipeline_id),
                        vec![hash_data(format!("pipeline-{}", pipeline_id).as_bytes())],
                        vec![hash_data(b"admitted")],
                        None,
                    )
                    .sign(&signing_key)
                    .expect("Failed to sign");

                    receipts.push(receipt);

                    // Packet stage
                    let wo_receipt = Receipt::new(
                        format!("pipeline-{}-packet", pipeline_id),
                        vec![hash_data(b"admitted")],
                        vec![hash_data(b"validated")],
                        None,
                    )
                    .chain(&receipts[0])
                    .expect("Failed to chain")
                    .sign(&signing_key)
                    .expect("Failed to sign");

                    receipts.push(wo_receipt);

                    Ok((request_id, receipts))
                }
                AdmissionResponse::Refused { .. } => Err("Refused"),
            }
        });

        handles.push(handle);
    }

    // Wait for all pipelines
    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.expect("Task panicked"))
        .collect();

    // Assert - All pipelines succeeded
    let successful = results.iter().filter(|r| r.is_ok()).count();
    assert_eq!(successful, num_pipelines, "All pipelines should succeed");

    // Verify receipt chains
    for result in results.iter() {
        if let Ok((_, receipts)) = result {
            assert_eq!(receipts.len(), 2);

            // Verify signatures
            for receipt in receipts {
                assert!(receipt.verify(&verifying_key).is_ok());
            }

            // Verify chain
            let prev_hash = receipts[0].hash().expect("Failed to hash");
            assert_eq!(receipts[1].previous_receipt_hash, Some(prev_hash));
        }
    }
}

/// Test end-to-end pipeline with all components
#[tokio::test]
async fn test_end_to_end_full_stack() {
    // Arrange - Full stack setup
    let (signing_key, verifying_key) = generate_keypair();
    let mut firewall = Firewall::with_defaults();
    let limiter_config = RateLimiterConfig {
        max_rps: 100.0,
        max_concurrent: 10,
        burst_size: 10,
    };
    let limiter = RateLimiter::new(limiter_config);

    // Act - Process request through all stages

    // 1. Backpressure check
    let _token = limiter
        .acquire()
        .await
        .expect("Failed to acquire backpressure token");

    // 2. Firewall admission
    let payload = serde_json::to_vec(&serde_json::json!({
        "task": "deploy-service",
        "environment": "production"
    }))
    .expect("Failed to serialize");

    let request = IngressRequest::new(IngressChannel::Emergency, payload.clone());
    let firewall_response = firewall.process(request).await;

    let mut receipts = Vec::new();

    match firewall_response {
        AdmissionResponse::Admitted { .. } => {
            let receipt = Receipt::new(
                "e2e-firewall".to_string(),
                vec![hash_data(&payload)],
                vec![hash_data(b"admitted")],
                None,
            )
            .sign(&signing_key)
            .expect("Failed to sign");
            receipts.push(receipt);
        }
        AdmissionResponse::Refused { .. } => panic!("Should be admitted"),
    }

    // 3. Work order creation
    let work_order = WorkOrder::new(
        "Deploy service to production".to_string(),
        "devops-team".to_string(),
    )
    .expect("Failed to create work order")
    .with_priority(Priority::Critical)
    .expect("Failed to set priority");

    let wo_bytes = serde_json::to_vec(&work_order).expect("Failed to serialize");
    let receipt = Receipt::new(
        "e2e-packet".to_string(),
        vec![hash_data(b"admitted")],
        vec![hash_data(&wo_bytes)],
        None,
    )
    .chain(&receipts[0])
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");
    receipts.push(receipt);

    // 4. Jidoka quality gates
    struct QualityGate;

    #[async_trait::async_trait]
    impl ggen_jidoka::Signal for QualityGate {
        async fn check(&self) -> ggen_jidoka::Result<AndonSignal> {
            Ok(AndonSignal::Green)
        }

        fn name(&self) -> &str {
            "quality-gate"
        }

        fn description(&self) -> &str {
            "Quality validation"
        }
    }

    #[async_trait::async_trait]
    impl Gate for QualityGate {
        async fn execute(&self) -> ggen_jidoka::Result<AndonSignal> {
            self.check().await
        }
    }

    let mut line = ProductionLine::new();
    line.add_gate(Arc::new(QualityGate));

    let gate_result = line.run().await;
    assert!(gate_result.is_ok(), "Quality gates should pass");

    let receipt = Receipt::new(
        "e2e-jidoka".to_string(),
        vec![hash_data(&wo_bytes)],
        vec![hash_data(b"quality-passed")],
        None,
    )
    .chain(&receipts[1])
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");
    receipts.push(receipt);

    // 5. Final completion receipt
    let final_receipt = Receipt::new(
        "e2e-complete".to_string(),
        vec![hash_data(b"quality-passed")],
        vec![hash_data(b"deployment-success")],
        None,
    )
    .chain(&receipts[2])
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");
    receipts.push(final_receipt);

    // Assert - Verify complete chain
    assert_eq!(receipts.len(), 4);

    // Verify all signatures
    for (i, receipt) in receipts.iter().enumerate() {
        assert!(
            receipt.verify(&verifying_key).is_ok(),
            "Receipt {} signature failed",
            i
        );
    }

    // Verify chain integrity
    for i in 1..receipts.len() {
        let prev_hash = receipts[i - 1].hash().expect("Failed to hash");
        assert_eq!(
            receipts[i].previous_receipt_hash,
            Some(prev_hash),
            "Receipt {} not chained",
            i
        );
    }

    // Verify operation sequence
    assert_eq!(receipts[0].operation_id, "e2e-firewall");
    assert_eq!(receipts[1].operation_id, "e2e-packet");
    assert_eq!(receipts[2].operation_id, "e2e-jidoka");
    assert_eq!(receipts[3].operation_id, "e2e-complete");
}
