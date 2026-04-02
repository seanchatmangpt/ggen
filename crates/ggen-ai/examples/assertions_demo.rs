//! Example: Assertions and Validation System
//!
//! Demonstrates DSPy's assertion system for runtime constraints:
//! 1. Basic assertions (hard requirements)
//! 2. Suggestions (soft constraints)
//! 3. Backtracking and retry logic
//! 4. Custom validators
//! 5. Validator composition
//!
//! Run with: cargo run --example assertions_demo
//!
//! Prerequisites:
//! - Set GGEN_LLM_MODEL environment variable
//! - Set appropriate API key

use ggen_ai::dspy::{
    assertions::*,
    field::{InputField, OutputField},
    module::Module,
    predictor::Predictor,
    signature::Signature,
};
use serde_json::json;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Assertions and Validation Demo ===\n");

    // Define a signature for generating summaries
    let signature = Signature::new(
        "TextSummarization",
        "Generate concise, informative summaries",
    )
    .with_input(InputField::new("text", "Text to summarize", "String"))
    .with_output(OutputField::new("summary", "Concise summary", "String"))
    .with_instructions(
        "Summarize the given text in 2-3 sentences. \
         Be concise but capture the main points.",
    );

    let base_predictor = Predictor::new(signature);

    // Sample text to summarize
    let sample_text = "Rust is a systems programming language that runs blazingly fast, \
                       prevents segfaults, and guarantees thread safety. It accomplishes \
                       these goals by being memory safe without using garbage collection. \
                       Rust uses ownership with a set of rules that the compiler checks at \
                       compile time. The ownership system enables Rust to make memory safety \
                       guarantees without needing a garbage collector.";

    let inputs = HashMap::from([("text".to_string(), json!(sample_text))]);

    // Part 1: Basic Assertions (Hard Requirements)
    println!("--- Part 1: Basic Assertions ---\n");

    // Assert: Summary must be 50-200 characters
    let length_assertion = Assertion::assert(LengthValidator::between(50, 200))
        .with_feedback(
            "Summary must be between 50 and 200 characters. \
             Previous output was too short or too long.",
        )
        .max_retries(3);

    // Assert: Must not be empty
    let not_empty_assertion = Assertion::assert(NotEmptyValidator)
        .with_feedback("Summary cannot be empty. Please provide content.")
        .max_retries(2);

    let mut executor = BacktrackExecutor::new(vec![length_assertion, not_empty_assertion]);

    println!("Running predictor with length and non-empty assertions...");

    match executor.execute(&base_predictor, inputs.clone()).await {
        Ok(result) => {
            println!(
                "✓ Success! Summary: {}",
                result
                    .get("summary")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );
            println!("  Attempts: {}", executor.total_attempts());
        }
        Err(e) => {
            eprintln!("✗ Failed after retries: {}", e);
            if std::env::var("GGEN_LLM_MODEL").is_err() {
                eprintln!("\nNote: Set GGEN_LLM_MODEL and appropriate API key");
                return Ok(());
            }
        }
    }

    // Part 2: Suggestions (Soft Constraints)
    println!("\n--- Part 2: Suggestions (Soft Constraints) ---\n");

    // Suggest: Should contain key technical terms
    let contains_suggestion = Assertion::suggest(ContainsValidator::new("Rust"))
        .with_feedback("Summary should mention 'Rust' explicitly.")
        .max_retries(2);

    // Suggest: Should be detailed (but won't fail if not)
    let detail_suggestion = Assertion::suggest(LengthValidator::min(100))
        .with_feedback("Summary could be more detailed (at least 100 chars).")
        .max_retries(1);

    let mut executor = BacktrackExecutor::new(vec![contains_suggestion, detail_suggestion]);

    println!("Running predictor with suggestions...");

    match executor.execute(&base_predictor, inputs.clone()).await {
        Ok(result) => {
            println!(
                "✓ Completed! Summary: {}",
                result
                    .get("summary")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );

            let warnings = executor.warnings();
            if !warnings.is_empty() {
                println!("\n⚠ Warnings ({}):", warnings.len());
                for warning in warnings {
                    println!(
                        "  - {} (after {} attempts)",
                        warning.feedback, warning.attempts
                    );
                }
            } else {
                println!("  No warnings - all suggestions satisfied!");
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }

    // Part 3: Pattern Validation
    println!("\n--- Part 3: Pattern Validation ---\n");

    // Create a signature for email extraction
    let email_sig = Signature::new("EmailExtraction", "Extract email from text")
        .with_input(InputField::new("text", "Text containing email", "String"))
        .with_output(OutputField::new("email", "Extracted email", "String"))
        .with_instructions("Extract the email address from the given text.");

    let email_predictor = Predictor::new(email_sig);

    // Validate email format with regex
    let email_pattern = PatternValidator::new(r"^[\w\.\-]+@[\w\.\-]+\.\w+$")?;

    let email_assertion = Assertion::assert(email_pattern)
        .with_feedback(
            "Email must be in valid format: user@domain.com. \
             Previous output was not a valid email.",
        )
        .max_retries(3);

    let mut executor = BacktrackExecutor::new(vec![email_assertion]);

    let email_text = "Contact me at john.doe@example.com for more information.";
    let email_inputs = HashMap::from([("text".to_string(), json!(email_text))]);

    println!("Extracting email with format validation...");

    match executor.execute(&email_predictor, email_inputs).await {
        Ok(result) => {
            println!(
                "✓ Valid email extracted: {}",
                result
                    .get("email")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );
        }
        Err(e) => {
            eprintln!("✗ Failed: {}", e);
        }
    }

    // Part 4: Custom Function Validators
    println!("\n--- Part 4: Custom Function Validators ---\n");

    // Custom validator: Check word count
    let word_count_validator = FnValidator::new(
        |value| {
            if let Some(s) = value.as_str() {
                let word_count = s.split_whitespace().count();
                if word_count >= 10 && word_count <= 30 {
                    ValidationResult::valid()
                } else {
                    ValidationResult::invalid(format!(
                        "Summary has {} words, need 10-30",
                        word_count
                    ))
                }
            } else {
                ValidationResult::invalid("Must be a string")
            }
        },
        "Word count between 10-30",
    );

    let word_count_assertion = Assertion::assert(word_count_validator)
        .with_feedback("Summary must have 10-30 words. Adjust length accordingly.")
        .max_retries(3);

    let mut executor = BacktrackExecutor::new(vec![word_count_assertion]);

    println!("Running predictor with custom word count validator...");

    match executor.execute(&base_predictor, inputs.clone()).await {
        Ok(result) => {
            let summary = result
                .get("summary")
                .and_then(|v| v.as_str())
                .unwrap_or("N/A");
            let word_count = summary.split_whitespace().count();
            println!("✓ Valid summary ({} words): {}", word_count, summary);
        }
        Err(e) => {
            eprintln!("✗ Failed: {}", e);
        }
    }

    // Part 5: Validator Composition (AND/OR/NOT)
    println!("\n--- Part 5: Validator Composition ---\n");

    // ALL: Must satisfy all validators
    let all_validators: Vec<BoxedValidator> = vec![
        Box::new(NotEmptyValidator),
        Box::new(LengthValidator::between(50, 200)),
        Box::new(ContainsValidator::new("memory")),
    ];

    let all_validator = AllValidator::new(all_validators);

    let all_assertion = Assertion::assert(all_validator)
        .with_feedback("Summary must: (1) not be empty, (2) be 50-200 chars, (3) mention 'memory'")
        .max_retries(3);

    println!("Testing ALL validator (must pass all checks)...");

    let mut executor = BacktrackExecutor::new(vec![all_assertion]);

    match executor.execute(&base_predictor, inputs.clone()).await {
        Ok(result) => {
            println!(
                "✓ All checks passed! Summary: {}",
                result
                    .get("summary")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );
        }
        Err(e) => {
            eprintln!("✗ Failed: {}", e);
        }
    }

    // ANY: Must satisfy at least one validator
    let any_validators: Vec<BoxedValidator> = vec![
        Box::new(ContainsValidator::new("Rust")),
        Box::new(ContainsValidator::new("systems")),
        Box::new(ContainsValidator::new("programming")),
    ];

    let any_validator = AnyValidator::new(any_validators);

    let any_assertion = Assertion::assert(any_validator)
        .with_feedback("Summary should mention at least one: 'Rust', 'systems', or 'programming'")
        .max_retries(2);

    println!("\nTesting ANY validator (pass if any check succeeds)...");

    let mut executor = BacktrackExecutor::new(vec![any_assertion]);

    match executor.execute(&base_predictor, inputs.clone()).await {
        Ok(result) => {
            println!(
                "✓ At least one check passed! Summary: {}",
                result
                    .get("summary")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );
        }
        Err(e) => {
            eprintln!("✗ Failed: {}", e);
        }
    }

    // NOT: Must NOT satisfy validator
    let not_validator = NotValidator::new(Box::new(ContainsValidator::new("garbage")));

    let not_assertion = Assertion::assert(not_validator)
        .with_feedback("Summary must not mention 'garbage'")
        .max_retries(2);

    println!("\nTesting NOT validator (must not contain word)...");

    let mut executor = BacktrackExecutor::new(vec![not_assertion]);

    match executor.execute(&base_predictor, inputs).await {
        Ok(result) => {
            println!(
                "✓ Passed! Summary: {}",
                result
                    .get("summary")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );
        }
        Err(e) => {
            eprintln!("✗ Failed: {}", e);
        }
    }

    println!("\n=== Example Complete ===");
    println!(
        "\nAssertion System Features:\n\
         1. Hard Assertions - Fail task if not satisfied\n\
         2. Soft Suggestions - Log warnings but don't fail\n\
         3. Automatic Retries - Backtracking with feedback\n\
         4. Pattern Matching - Regex validation\n\
         5. Custom Validators - Function-based logic\n\
         6. Composition - AND/OR/NOT combinators\n\n\
         Use Cases:\n\
         - Length constraints (min/max characters)\n\
         - Format validation (email, phone, dates)\n\
         - Content requirements (must/must not contain)\n\
         - Quality checks (word count, readability)\n\
         - Business rules (custom validation logic)"
    );

    Ok(())
}
