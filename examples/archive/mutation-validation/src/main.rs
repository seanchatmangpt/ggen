//! # Mutation-Based Validation Demo
//!
//! Demonstrates 7-agent mutation-based validation:
//! 1. Agents 1-3: Inject mutations (syntax, logic, boundary)
//! 2. Agents 4-5: Run tests to detect mutations
//! 3. Agent 6: Calculate mutation score
//! 4. Agent 7: Suggest test improvements
//!
//! **Key Innovation**: Active adversarial testing (agents try to BREAK code)

use mutation_validation::MutationValidationSystem;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Mutation-Based Validation System Demo ===\n");
    println!("A novel validation approach where 7 agents perform ADVERSARIAL testing:");
    println!("  (Active testing: agents try to BREAK the code)\n");
    println!("PHASE 1: Inject Mutations (Agents 1-3)");
    println!("  Agent 1: Syntax mutations    - Change operators, delete statements");
    println!("  Agent 2: Logic mutations     - Flip conditionals, negate expressions");
    println!("  Agent 3: Boundary mutations  - Off-by-one, loop bounds");
    println!();
    println!("PHASE 2: Detect and Analyze (Agents 4-7)");
    println!("  Agent 4: Unit tests         - Run unit tests to detect mutations");
    println!("  Agent 5: Integration tests  - Run integration tests to detect mutations");
    println!("  Agent 6: Score calculator    - Calculate mutation score (killed/survived)");
    println!("  Agent 7: Test improver       - Suggest test improvements for gaps");
    println!();

    // Create validation system
    println!("📦 Initializing mutation-based validation system...");
    let system = MutationValidationSystem::new().await?;

    println!("✓ 7 mutation agents registered");
    println!();

    // Simulate ggen-generated code
    println!("🔧 Simulating ggen code generation...");
    let test_package = "/tmp/test-mutation-package";
    let test_code = r#"
fn calculate_discount(price: i32, customer_type: &str) -> f64 {
    let mut discount = 0.0;

    if customer_type == "premium" {
        discount = 0.2;
    } else if customer_type == "standard" {
        discount = 0.1;
    }

    price as f64 * (1.0 - discount)
}

fn main() {
    let prices = vec![100, 200, 300];
    for i in 0..prices.len() {
        let final_price = calculate_discount(prices[i], "standard");
        println!("Price: {}", final_price);
    }
}
"#;

    println!("✓ Generated code: {}", test_package);
    println!("✓ Original code: {} lines", test_code.lines().count());
    println!();

    // Run mutation testing
    println!("🔍 Running mutation testing...");
    let report = system
        .run_mutation_testing(test_package, test_code, "cargo test")
        .await?;

    println!();
    println!("=== Mutation Testing Result ===");
    println!();
    println!("📊 Mutation Score: {:.1}%", report.result.mutation_score * 100.0);
    println!("   Total mutations: {}", report.result.total_mutations);
    println!("   Killed (detected): {}", report.result.killed);
    println!("   Survived (missed): {}", report.result.survived);
    println!("   Errors: {}", report.result.errors);
    println!();
    println!("📋 Assessment: {}", report.assessment);
    println!();

    // Show surviving mutations (test gaps)
    if !report.result.surviving_mutations().is_empty() {
        println!("⚠️  Surviving Mutations (Test Gaps):");
        for mutation in report.result.surviving_mutations() {
            println!("   Line {}: '{}' → '{}'",
                mutation.line + 1,
                mutation.original_code.trim(),
                mutation.mutated_code.trim()
            );
        }
        println!();
    }

    // Show test suggestions
    if !report.suggestions.is_empty() {
        println!("💡 Test Improvement Suggestions:");
        for (i, suggestion) in report.suggestions.iter().enumerate() {
            println!("   {}. [{}] {}",
                i + 1,
                suggestion.priority,
                suggestion.suggested_test
            );
            println!("      Rationale: {}", suggestion.rationale);
        }
        println!();
    }

    println!("=== Key Innovations ===");
    println!("1. Active Adversarial Testing");
    println!("   - Agents try to BREAK the code (not just validate it)");
    println!("   - Mutations simulate bugs, typos, and edge cases");
    println!();
    println!("2. Mutation Score");
    println!("   - Quantitative measure of test quality");
    println!("   - Percentage of mutations detected by tests");
    println!("   - Higher score = stronger test suite");
    println!();
    println!("3. Test Gap Detection");
    println!("   - Surviving mutations reveal test weaknesses");
    println!("   - Agent 7 suggests specific tests to add");
    println!("   - Targeted improvements, not generic advice");
    println!();
    println!("4. Two-Phase Workflow");
    println!("   - PHASE 1: Inject mutations (Agents 1-3)");
    println!("   - PHASE 2: Detect and analyze (Agents 4-7)");
    println!("   - Clear separation: mutation vs. detection");
    println!();

    println!("=== Comparison: Consensus vs. Property-Based vs. Mutation ===");
    println!();
    println!("Consensus (7-Agent Validation):");
    println!("  - All agents test the SAME thing");
    println!("  - Vote to reach agreement (5-of-7 quorum)");
    println!("  - Goal: Fault tolerance (Byzantine)");
    println!();
    println!("Property-Based (7-Property Validation):");
    println!("  - Each agent tests a DIFFERENT property");
    println!("  - All must pass (7-of-7)");
    println!("  - Goal: Comprehensive coverage (invariants)");
    println!();
    println!("Mutation-Based (This System):");
    println!("  - Agents actively try to BREAK the code");
    println!("  - Tests compete against mutations");
    println!("  - Goal: Test quality (adversarial)");
    println!();

    Ok(())
}
