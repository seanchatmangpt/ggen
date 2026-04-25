//! # 7-Agent Validation System Demo
//!
//! Demonstrates the closed-loop validation system:
//! 1. ggen generates code (μ₁-μ₅ pipeline)
//! 2. 7 agents validate independently (parallel execution)
//! 3. Consensus layer aggregates results (PBFT 5-of-7 quorum)
//! 4. Results feed back into ggen's improvement cycle
//!
//! ## Usage
//!
//! ```bash
//! cargo run --example 7-agent-validation
//! ```

use validation_system::ValidationSystem;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== 7-Agent Validation System Demo ===\n");
    println!("A novel validation approach combining:");
    println!("  • ggen's quality gates (jidoka)");
    println!("  • Byzantine consensus (5-of-7 quorum)");
    println!("  • Armstrong supervision (let-it-crash)");
    println!("  • OpenTelemetry integration");
    println!();

    // Create validation system
    println!("📦 Initializing validation system...");
    let system = ValidationSystem::new().await?;

    println!("✓ 7 agents registered and supervised");
    println!("  - Agent 1: Compiler Gate");
    println!("  - Agent 2: Test Gate");
    println!("  - Agent 3: Lint Gate");
    println!("  - Agent 4: SHACL Gate");
    println!("  - Agent 5: OTEL Gate");
    println!("  - Agent 6: Security Gate");
    println!("  - Agent 7: Performance Gate");
    println!();

    // Simulate a ggen code generation
    println!("🔧 Simulating ggen code generation...");
    let test_package = "/tmp/test-package";
    let mock_receipt = "blake3-receipt-12345";

    println!("✓ Generated code: {}", test_package);
    println!("✓ BLAKE3 receipt: {}", mock_receipt);
    println!();

    // Run validation
    println!("🔍 Running 7-agent validation...");
    let decision = system.validate_generation(test_package, mock_receipt).await?;

    println!();
    println!("=== Validation Result ===");

    match decision {
        validation_system::consensus::ConsensusDecision::Approve {
            green_votes,
            yellow_votes,
            red_votes,
        } => {
            println!("✅ APPROVED - Code passes validation");
            println!("   GREEN votes: {} / 7", green_votes);
            println!("   YELLOW votes: {} / 7", yellow_votes);
            println!("   RED votes: {} / 7", red_votes);
        }
        validation_system::consensus::ConsensusDecision::Reject {
            green_votes,
            yellow_votes,
            red_votes,
            reasons,
        } => {
            println!("❌ REJECTED - Code fails validation");
            println!("   GREEN votes: {} / 7", green_votes);
            println!("   YELLOW votes: {} / 7", yellow_votes);
            println!("   RED votes: {} / 7", red_votes);
            println!();
            println!("   Reasons:");
            for reason in reasons {
                println!("     - {}", reason);
            }
        }
        validation_system::consensus::ConsensusDecision::Defer {
            green_votes,
            yellow_votes,
            red_votes,
            suggestion,
        } => {
            println!("⏸️  DEFERRED - Unable to reach consensus");
            println!("   GREEN votes: {} / 7", green_votes);
            println!("   YELLOW votes: {} / 7", yellow_votes);
            println!("   RED votes: {} / 7", red_votes);
            println!();
            println!("   Suggestion: {}", suggestion);
        }
    }

    println!();
    println!("=== Key Innovations ===");
    println!("1. Autonomous Fault Tolerance");
    println!("   - Each agent supervised (Armstrong principles)");
    println!("   - Let-it-crash: Fail fast, restart cleanly");
    println!("   - No shared state: Message passing only");
    println!();
    println!("2. Byzantine Consensus (PBFT)");
    println!("   - 5-of-7 quorum prevents malicious/failed agents from blocking");
    println!("   - Priority: RED (safety) > GREEN (progress) > YELLOW (warning)");
    println!();
    println!("3. Closed-Loop Feedback");
    println!("   - Validation results feed back into ggen's kaizen cycle");
    println!("   - Continuous improvement (PDCA: Plan-Do-Check-Act)");
    println!();
    println!("4. OpenTelemetry Integration");
    println!("   - All validation decisions emit OTEL spans");
    println!("   - Traceable in Jaeger UI (http://localhost:16686)");
    println!();

    Ok(())
}
