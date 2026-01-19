//! Example: Testing with DummyLM
//!
//! Demonstrates comprehensive testing patterns using DummyLM:
//! 1. Sequential mode - Responses in order
//! 2. Query-based mode - Conditional responses
//! 3. Example-following mode - Demonstration matching
//! 4. Test assertions and verification
//! 5. Integration testing patterns
//!
//! Run with: cargo run --example testing_demo

use ggen_ai::dspy::{
    field::{InputField, OutputField},
    module::Module,
    signature::Signature,
    testing::DummyLM,
};
use serde_json::json;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Testing with DummyLM Demo ===\n");

    // Part 1: Sequential Mode
    println!("--- Part 1: Sequential Mode ---\n");
    println!("Returns responses in order, cycles when exhausted");

    let responses = vec![
        HashMap::from([("answer".to_string(), json!("Paris"))]),
        HashMap::from([("answer".to_string(), json!("London"))]),
        HashMap::from([("answer".to_string(), json!("Berlin"))]),
    ];

    let dummy = DummyLM::sequential(responses);

    let test_questions = vec![
        "What is the capital of France?",
        "What is the capital of UK?",
        "What is the capital of Germany?",
        "What is the capital of Spain?", // Will cycle back to first
    ];

    for (i, question) in test_questions.iter().enumerate() {
        let inputs = HashMap::from([("prompt".to_string(), json!(question))]);

        let result = dummy.forward(inputs).await?;
        let answer = result.get("answer").and_then(|v| v.as_str()).unwrap_or("N/A");

        println!("Q{}: {}", i + 1, question);
        println!("A{}: {}", i + 1, answer);
        println!();
    }

    println!("Total calls: {}", dummy.call_count());
    println!(
        "Expected: 4 calls, cycles through 3 responses, 4th repeats 1st\n"
    );

    // Part 2: Query-Based Mode
    println!("--- Part 2: Query-Based Mode ---\n");
    println!("Returns responses based on prompt content matching");

    let mut query_map = HashMap::new();
    query_map.insert(
        "France".to_string(),
        HashMap::from([("answer".to_string(), json!("Paris, the City of Light"))]),
    );
    query_map.insert(
        "Python".to_string(),
        HashMap::from([("answer".to_string(), json!("A high-level programming language"))]),
    );
    query_map.insert(
        "AI".to_string(),
        HashMap::from([("answer".to_string(), json!("Artificial Intelligence"))]),
    );

    let dummy = DummyLM::query_based(query_map);

    let queries = vec![
        "Tell me about France",
        "What is Python?",
        "Explain AI to me",
        "What is JavaScript?", // No match, will use default
    ];

    for query in queries {
        let inputs = HashMap::from([("prompt".to_string(), json!(query))]);

        let result = dummy.forward(inputs).await?;
        let answer = result
            .get("answer")
            .or_else(|| result.get("response"))
            .and_then(|v| v.as_str())
            .unwrap_or("N/A");

        println!("Query: {}", query);
        println!("Response: {}", answer);
        println!();
    }

    // Part 3: Example-Following Mode
    println!("--- Part 3: Example-Following Mode ---\n");
    println!("Matches input to demonstrations and returns corresponding output");

    let demonstrations = vec![
        (
            HashMap::from([("problem".to_string(), json!("2 + 2"))]),
            HashMap::from([("solution".to_string(), json!("4"))]),
        ),
        (
            HashMap::from([("problem".to_string(), json!("5 * 3"))]),
            HashMap::from([("solution".to_string(), json!("15"))]),
        ),
        (
            HashMap::from([("problem".to_string(), json!("10 - 3"))]),
            HashMap::from([("solution".to_string(), json!("7"))]),
        ),
    ];

    let dummy = DummyLM::example_following(demonstrations);

    let test_problems = vec!["problem: 2 + 2", "problem: 5 * 3", "problem: 10 - 3"];

    for problem_text in test_problems {
        let inputs = HashMap::from([("prompt".to_string(), json!(problem_text))]);

        let result = dummy.forward(inputs).await?;
        let solution = result.get("solution").and_then(|v| v.as_str()).unwrap_or("N/A");

        println!("Problem: {}", problem_text);
        println!("Solution: {}", solution);
        println!();
    }

    // Part 4: Call History and Verification
    println!("--- Part 4: Call History and Verification ---\n");

    let responses = vec![
        HashMap::from([("status".to_string(), json!("success"))]),
        HashMap::from([("status".to_string(), json!("pending"))]),
    ];

    let dummy = DummyLM::sequential(responses);

    // Make some calls
    for i in 1..=3 {
        let inputs = HashMap::from([("prompt".to_string(), json!(format!("Request {}", i)))]);
        let _ = dummy.forward(inputs).await?;
    }

    println!("Total calls made: {}", dummy.call_count());

    let history = dummy.history();
    println!("History entries: {}", history.len());

    println!("\nFull history:");
    for (i, entry) in history.iter().enumerate() {
        println!("  Call {}: {}", i + 1, entry.prompt);
    }

    let last_2 = dummy.last_n_calls(2);
    println!("\nLast 2 calls (most recent first):");
    for entry in last_2 {
        println!("  - {}", entry.prompt);
    }

    // Reset and verify
    dummy.reset();
    println!("\nAfter reset:");
    println!("  Call count: {}", dummy.call_count());
    println!("  History length: {}", dummy.history().len());

    // Part 5: Integration Testing Pattern
    println!("\n--- Part 5: Integration Testing Pattern ---\n");

    // Define a signature
    let signature = Signature::new("QA", "Answer questions accurately")
        .with_input(InputField::new("question", "Question", "String"))
        .with_output(OutputField::new("answer", "Answer", "String"));

    // Create DummyLM with expected responses
    let test_responses = vec![
        HashMap::from([("answer".to_string(), json!("Rust is a systems programming language"))]),
        HashMap::from([("answer".to_string(), json!("DSPy is a framework for LLM programming"))]),
    ];

    let dummy = DummyLM::sequential(test_responses).with_signature(signature);

    // Test cases
    struct TestCase {
        question: &'static str,
        expected_contains: &'static str,
    }

    let test_cases = vec![
        TestCase {
            question: "What is Rust?",
            expected_contains: "Rust",
        },
        TestCase {
            question: "What is DSPy?",
            expected_contains: "DSPy",
        },
    ];

    println!("Running integration tests...\n");

    let mut passed = 0;
    let mut failed = 0;

    for (i, test_case) in test_cases.iter().enumerate() {
        let inputs = HashMap::from([("question".to_string(), json!(test_case.question))]);

        let result = dummy.forward(inputs).await?;
        let answer = result.get("answer").and_then(|v| v.as_str()).unwrap_or("");

        let test_passed = answer.contains(test_case.expected_contains);

        println!(
            "Test {}: {} - {}",
            i + 1,
            if test_passed { "✓ PASS" } else { "✗ FAIL" },
            test_case.question
        );

        if !test_passed {
            println!("  Expected to contain: {}", test_case.expected_contains);
            println!("  Got: {}", answer);
            failed += 1;
        } else {
            passed += 1;
        }
    }

    println!("\nTest Results: {} passed, {} failed", passed, failed);

    // Part 6: Testing Best Practices
    println!("\n--- Part 6: Testing Best Practices ---\n");

    println!("✓ Use Sequential mode for:");
    println!("  - Simple unit tests with known outputs");
    println!("  - Testing module composition and flow");
    println!("  - Verifying call counts and order");

    println!("\n✓ Use Query-Based mode for:");
    println!("  - Testing conditional logic");
    println!("  - Simulating different LLM responses");
    println!("  - Testing error handling paths");

    println!("\n✓ Use Example-Following mode for:");
    println!("  - Testing optimization algorithms");
    println!("  - Validating demonstration matching");
    println!("  - Testing few-shot learning patterns");

    println!("\n✓ Always verify:");
    println!("  - Call count matches expectations");
    println!("  - History contains expected prompts");
    println!("  - Outputs have required fields");
    println!("  - Edge cases handle correctly");

    println!("\n✓ DummyLM advantages:");
    println!("  - Fast: No API calls");
    println!("  - Deterministic: Predictable outputs");
    println!("  - Offline: No network required");
    println!("  - Trackable: Full call history");

    println!("\n=== Example Complete ===");
    println!(
        "\nDummyLM is essential for:\n\
         - Unit testing DSPy modules without API costs\n\
         - Integration testing pipelines deterministically\n\
         - Development and debugging without LLM dependency\n\
         - CI/CD pipelines that need fast, reliable tests\n\
         - Regression testing to prevent breaking changes"
    );

    Ok(())
}
