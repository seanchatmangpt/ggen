//! End-to-End Integration Example: ETL + KGC-4D + Workflow Engine
//!
//! This example demonstrates the complete holographic orchestration pipeline:
//! 1. ETL produces RDF triples with receipts
//! 2. Orchestrator injects KGC-4D temporal context
//! 3. Variables are aggregated into workflow triggers
//! 4. Workflow engine receives execution commands
//!
//! Run with: cargo run --example etl-kgc-workflow-integration

use knhk_orchestrator::{
    EtlTripleEvent, Orchestrator, OrchestratorConfig, ProcessInstanceEvent, ProcessInstanceState,
    WorkflowTriggerEvent,
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    println!("═══════════════════════════════════════════════════════════════");
    println!("ETL-KGC-4D-Workflow Engine Integration Example");
    println!("Holographic Orchestration: A = μ(O)");
    println!("═══════════════════════════════════════════════════════════════\n");

    // Create orchestrator with configuration
    let config = OrchestratorConfig {
        workflow_id: "order-processing-workflow".to_string(),
        batch_size: 5,
        git_reference: "abc123def456abc123def456abc123def456abc1".to_string(), // Valid SHA-1
        enable_kgc_snapshots: true,
        enable_span_correlation: true,
        dedup_cache_size: 100,
        andon_overflow_threshold: 1000,
    };

    let orchestrator = Orchestrator::new(config);

    println!("STAGE 1: ETL Event Generation");
    println!("─────────────────────────────────────────────────────────────");

    // Simulate ETL output: RDF triples from data processing
    let etl_events = vec![
        EtlTripleEvent::new(
            "http://example.com/order/123".to_string(),
            "http://example.com/hasCustomer".to_string(),
            "http://example.com/customer/456".to_string(),
            1704787200000000000, // January 9, 2024
        )
        .with_graph("orders".to_string()),
        EtlTripleEvent::new(
            "http://example.com/order/123".to_string(),
            "http://example.com/hasStatus".to_string(),
            "\"PENDING\"".to_string(),
            1704787201000000000,
        ),
        EtlTripleEvent::new(
            "http://example.com/order/123".to_string(),
            "http://example.com/hasTotal".to_string(),
            "\"99.99\"".to_string(),
            1704787202000000000,
        ),
        EtlTripleEvent::new(
            "http://example.com/customer/456".to_string(),
            "http://example.com/hasEmail".to_string(),
            "\"alice@example.com\"".to_string(),
            1704787203000000000,
        ),
        EtlTripleEvent::new(
            "http://example.com/customer/456".to_string(),
            "http://example.com/hasRegion".to_string(),
            "\"us-west-2\"".to_string(),
            1704787204000000000,
        ),
    ];

    for (i, event) in etl_events.iter().enumerate() {
        println!(
            "  Event {}: {} → {}",
            i + 1,
            event.subject.split('/').last().unwrap_or("?"),
            event.object
        );
    }

    println!("\nSTAGE 2: Process Batch Through Orchestrator");
    println!("─────────────────────────────────────────────────────────────");

    // Process batch through orchestrator pipeline
    let trigger = orchestrator.process_batch(etl_events.clone()).await?;

    println!("  ✓ Trigger generated: {}\n", trigger.process_instance_id);
    println!("  Workflow: {}", trigger.workflow_id);
    println!("  Events aggregated: {}", trigger.event_count);
    println!("  Variables extracted: {}", trigger.process_variables.len());

    println!("\nSTAGE 3: Process Variables");
    println!("─────────────────────────────────────────────────────────────");

    for (key, value) in &trigger.process_variables {
        println!("  {}: {}", key, value);
    }

    println!("\nSTAGE 4: Workflow Trigger Submission");
    println!("─────────────────────────────────────────────────────────────");

    // Simulate workflow engine receiving the trigger
    println!(
        "  Submitting trigger to workflow-engine: {}",
        trigger.workflow_id
    );
    println!("  Correlation ID: {}", trigger.correlation_id);
    println!("  Span ID: {}", trigger.span_id);
    println!("  ✓ Workflow execution initiated\n");

    // Simulate workflow engine returning process instance event
    let process_event = ProcessInstanceEvent::new(
        trigger.process_instance_id.clone(),
        trigger.workflow_id.clone(),
        ProcessInstanceState::Running,
    )
    .with_state_var("orderId".to_string(), serde_json::json!("123"))
    .with_state_var("customerId".to_string(), serde_json::json!("456"))
    .with_state_var("status".to_string(), serde_json::json!("PROCESSING"));

    println!("STAGE 5: Workflow Execution & Feedback");
    println!("─────────────────────────────────────────────────────────────");

    println!("  Process Instance: {}", process_event.process_instance_id);
    println!("  State: {:?}", process_event.workflow_state);
    println!("  Timestamp: {}", process_event.state_timestamp);

    println!("\nSTAGE 6: Metrics & Andon Signals");
    println!("─────────────────────────────────────────────────────────────");

    // Collect metrics
    let metrics = orchestrator.metrics().await;
    let andon = orchestrator.check_andon_signal().await;

    println!(
        "  Total Events Processed: {}",
        metrics.total_events_processed
    );
    println!("  Aggregation Ratio: {:.2}", metrics.aggregation_ratio);
    println!("  Pending ETL Events: {}", metrics.pending_etl_events);

    match andon {
        knhk_orchestrator::AndonSignal::Green { message } => {
            println!("  🟢 Andon Signal: {}", message);
        }
        knhk_orchestrator::AndonSignal::Yellow { message } => {
            println!("  🟡 Andon Signal: {}", message);
        }
        knhk_orchestrator::AndonSignal::Red { message } => {
            println!("  🔴 Andon Signal: {}", message);
        }
    }

    println!("\n═══════════════════════════════════════════════════════════════");
    println!("Integration Summary");
    println!("═══════════════════════════════════════════════════════════════");

    println!(
        "\n✓ ETL → Orchestrator: {} events processed",
        etl_events.len()
    );
    println!("✓ KGC-4D Context: Temporal coordinates injected");
    println!(
        "✓ Variable Aggregation: {} variables extracted",
        trigger.process_variables.len()
    );
    println!(
        "✓ Workflow Trigger: {} submitted",
        trigger.process_instance_id
    );
    println!("✓ Workflow Execution: Process instance created");
    println!("✓ Span Correlation: End-to-end tracing enabled");

    println!("\nThe holographic projection (A = μ(O)) succeeded!");
    println!("Integration closure achieved. ✓");

    Ok(())
}
