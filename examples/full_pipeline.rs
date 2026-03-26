//! Full Pipeline Example
//!
//! Complete end-to-end demonstration integrating all A2A/TPS components:
//! - Firewall (3-channel ingress)
//! - Backpressure (kanban flow control)
//! - Jidoka (quality gates)
//! - A2A Tasks (state machine)
//! - Receipt Chain (cryptographic audit trail)

use ggen_a2a::{
    state_machine::{StateTransition, TaskStateMachine},
    Artifact, ArtifactType, Task, TaskState,
};
use ggen_backpressure::{KanbanBoard, KanbanConfig, Stage};
use ggen_firewall::{AdmissionResponse, Firewall, IngressChannel, IngressRequest};
use ggen_jidoka::{AndonSignal, Gate, ProductionLine, Result as JidokaResult};
use ggen_receipt::{generate_keypair, hash_data, Receipt, ReceiptChain};
use std::sync::Arc;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("╔════════════════════════════════════════════════════════════╗");
    println!("║       A2A/TPS FULL PIPELINE DEMONSTRATION                  ║");
    println!("╚════════════════════════════════════════════════════════════╝\n");

    // Initialize components
    let (signing_key, verifying_key) = generate_keypair();
    let mut firewall = Firewall::with_defaults();
    let kanban = KanbanBoard::new(KanbanConfig {
        ready_limit: 2,
        in_progress_limit: 1,
        review_limit: 1,
    });

    println!("✓ Initialized: Firewall, Kanban, Receipt system\n");

    // PHASE 1: INGRESS CONTROL
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│ PHASE 1: Ingress Control (Firewall)                    │");
    println!("╰─────────────────────────────────────────────────────────╯\n");

    let work_request = IngressRequest::new(
        IngressChannel::Batch,
        "Generate REST API from OpenAPI spec".as_bytes().to_vec(),
    );

    let request_id = match firewall.process(work_request).await {
        AdmissionResponse::Admitted {
            request_id,
            channel,
            admitted_at,
        } => {
            println!("✓ Request admitted through {:?}", channel);
            println!("  ID: {}", request_id);
            println!("  Time: {}", admitted_at);
            request_id
        }
        AdmissionResponse::Refused { reason, .. } => {
            panic!("✗ Request refused: {}", reason);
        }
    };
    println!();

    // PHASE 2: KANBAN FLOW CONTROL
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│ PHASE 2: Flow Control (Kanban Board)                   │");
    println!("╰─────────────────────────────────────────────────────────╯\n");

    let task_id = format!("WO-{}", &request_id.to_string()[..8]);
    kanban.add_to_backlog(task_id.clone()).await?;
    println!("✓ Work order {} added to backlog", task_id);

    println!("\nPulling through kanban stages:");
    let _ready_token = kanban.pull(&task_id).await?;
    println!(
        "  → Ready (WIP: {}/{})",
        kanban.count(Stage::Ready).await,
        kanban.wip_limit(Stage::Ready).await
    );

    let _progress_token = kanban.pull(&task_id).await?;
    println!(
        "  → In Progress (WIP: {}/{})",
        kanban.count(Stage::InProgress).await,
        kanban.wip_limit(Stage::InProgress).await
    );
    println!();

    // PHASE 3: TASK STATE MACHINE
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│ PHASE 3: Task Lifecycle (A2A State Machine)            │");
    println!("╰─────────────────────────────────────────────────────────╯\n");

    let mut task = Task::new(
        "Generate REST API from spec".to_string(),
        "system".to_string(),
    )
    .with_assignment("codegen-agent".to_string());

    println!("✓ Task created: {}", task.id);
    println!("  State: {:?}", task.state);

    let start = StateTransition::new(TaskState::Running, "codegen-agent".to_string());
    TaskStateMachine::transition(&mut task, start)?;
    println!("  → Running");

    // Add artifacts
    let spec = Artifact::text(
        "api-spec.yaml".to_string(),
        ArtifactType::Input,
        "openapi: 3.0.0\ninfo:\n  title: Sample API".to_string(),
    );
    task.artifacts.insert("spec".to_string(), spec);
    println!("  + Added input artifact: api-spec.yaml");
    println!();

    // PHASE 4: QUALITY GATES
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│ PHASE 4: Quality Control (Jidoka Gates)                │");
    println!("╰─────────────────────────────────────────────────────────╯\n");

    let mut line = ProductionLine::new();
    line.add_gate(Arc::new(MockGate::new(
        "Spec Validation",
        AndonSignal::Green,
    )))
    .add_gate(Arc::new(MockGate::new(
        "Code Generation",
        AndonSignal::Green,
    )))
    .add_gate(Arc::new(MockGate::new("Syntax Check", AndonSignal::Green)))
    .add_gate(Arc::new(MockGate::new("Type Check", AndonSignal::Green)));

    let gate_results = line.run().await?;
    println!(
        "✓ All quality gates passed ({}/{})",
        gate_results.len(),
        line.gate_count()
    );

    for result in gate_results {
        println!("  ✓ {}: {}", result.gate_name, result.signal);
    }
    println!();

    // Complete task
    let complete = StateTransition::new(TaskState::Completed, "codegen-agent".to_string());
    TaskStateMachine::transition(&mut task, complete)?;

    let output = Artifact::file(
        "api.rs".to_string(),
        ArtifactType::Output,
        std::path::PathBuf::from("target/api.rs"),
    )
    .with_hash("abc123".to_string());
    task.artifacts.insert("output".to_string(), output);

    println!("✓ Task completed");
    println!("  + Generated artifact: api.rs\n");

    // PHASE 5: RECEIPT CHAIN
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│ PHASE 5: Audit Trail (Receipt Chain)                   │");
    println!("╰─────────────────────────────────────────────────────────╯\n");

    // Create receipt chain for audit trail
    let genesis = Receipt::new(
        "admit-request".to_string(),
        vec![hash_data(request_id.as_bytes())],
        vec![hash_data(task_id.as_bytes())],
        None,
    )
    .sign(&signing_key)?;

    let mut chain = ReceiptChain::from_genesis(genesis.clone())?;
    println!("✓ Genesis receipt created");
    println!("  Operation: admit-request");
    println!("  Hash: {}...", &genesis.hash()?[..16]);

    let execute = Receipt::new(
        "execute-task".to_string(),
        vec![hash_data(task_id.as_bytes())],
        vec![hash_data(b"api.rs")],
        None,
    )
    .chain(&genesis)?
    .sign(&signing_key)?;

    chain.append(execute.clone())?;
    println!("\n✓ Execution receipt appended");
    println!("  Operation: execute-task");
    println!("  Hash: {}...", &execute.hash()?[..16]);

    let validate = Receipt::new(
        "quality-gates".to_string(),
        vec![hash_data(b"api.rs")],
        vec![hash_data(b"validated")],
        None,
    )
    .chain(&execute)?
    .sign(&signing_key)?;

    chain.append(validate)?;
    println!("\n✓ Validation receipt appended");
    println!("  Operation: quality-gates");

    // Verify chain
    chain.verify(&verifying_key)?;
    println!("\n✓ Complete chain verified");
    println!("  Length: {} receipts", chain.len());
    println!();

    // PHASE 6: FLOW COMPLETION
    println!("╭─────────────────────────────────────────────────────────╮");
    println!("│ PHASE 6: Flow Completion                               │");
    println!("╰─────────────────────────────────────────────────────────╯\n");

    let _review_token = kanban.pull(&task_id).await?;
    println!("  → Review");

    let _done_token = kanban.pull(&task_id).await?;
    println!("  → Done");
    println!();

    // FINAL SUMMARY
    println!("╔════════════════════════════════════════════════════════════╗");
    println!("║                    PIPELINE SUMMARY                        ║");
    println!("╚════════════════════════════════════════════════════════════╝\n");

    println!("Work Order: {}", task_id);
    println!("Request ID: {}", request_id);
    println!();

    println!("Pipeline Stages:");
    println!("  ✓ Firewall: Admitted via Batch channel");
    println!("  ✓ Kanban: Pulled through 4 stages");
    println!("  ✓ Task: Created → Running → Completed");
    println!("  ✓ Jidoka: Passed 4 quality gates");
    println!("  ✓ Receipts: 3-link chain verified");
    println!();

    println!("Quality Metrics:");
    println!("  Andon signals: 🟢 All green");
    println!("  WIP limits: Respected");
    println!("  State transitions: Valid");
    println!("  Cryptographic audit: Complete");
    println!();

    println!("Kanban Board State:");
    println!("  Backlog: {}", kanban.count(Stage::Backlog).await);
    println!(
        "  Ready: {}/{}",
        kanban.count(Stage::Ready).await,
        kanban.wip_limit(Stage::Ready).await
    );
    println!(
        "  In Progress: {}/{}",
        kanban.count(Stage::InProgress).await,
        kanban.wip_limit(Stage::InProgress).await
    );
    println!(
        "  Review: {}/{}",
        kanban.count(Stage::Review).await,
        kanban.wip_limit(Stage::Review).await
    );
    println!("  Done: {}", kanban.count(Stage::Done).await);
    println!();

    if let Some(duration) = task.duration() {
        println!("Task duration: {:?}", duration);
    }

    println!("\n╔════════════════════════════════════════════════════════════╗");
    println!("║  ✓ FULL PIPELINE COMPLETED SUCCESSFULLY                    ║");
    println!("╚════════════════════════════════════════════════════════════╝");

    Ok(())
}

// Mock gate for demonstration
#[derive(Debug)]
struct MockGate {
    name: String,
    signal: AndonSignal,
}

impl MockGate {
    fn new(name: impl Into<String>, signal: AndonSignal) -> Self {
        Self {
            name: name.into(),
            signal,
        }
    }
}

#[async_trait::async_trait]
impl ggen_jidoka::Signal for MockGate {
    async fn check(&self) -> JidokaResult<AndonSignal> {
        sleep(Duration::from_millis(50)).await;
        Ok(self.signal)
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn description(&self) -> &str {
        "Mock quality gate"
    }
}

#[async_trait::async_trait]
impl Gate for MockGate {
    async fn execute(&self) -> JidokaResult<AndonSignal> {
        self.check().await
    }
}
