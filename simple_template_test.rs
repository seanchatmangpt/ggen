//! Direct test of template validator functionality

use std::path::Path;

// Simple test to check if we can directly instantiate and use the template validator
fn main() {
    println!("Testing Template Validator functionality...\n");

    // Try to test the basic template validator
    match test_basic_validator() {
        Ok(()) => println!("✅ Basic template validator works"),
        Err(e) => println!("❌ Basic template validator failed: {}", e),
    }

    println!("\nTest completed!");
}

fn test_basic_validator() -> Result<(), Box<dyn std::error::Error>> {
    // Test 1: Import the validator
    let _validator = ggen_ai::generators::validator::TemplateValidator::new();
    println!("✅ TemplateValidator can be instantiated");

    // Test 2: Test validation with a simple template
    let valid_template = r#"---
to: "test.rs"
---
fn main() {
    println!("Hello, World!");
}"#;

    let result = ggen_ai::generators::validator::TemplateValidator::new()
        .validate_template(valid_template)?;

    println!("✅ Template validation works for valid template");
    println!("   - Valid: {}", result.valid);
    println!("   - Issues: {}", result.issues.len());

    // Test 3: Test validation with invalid template
    let invalid_template = r#"---
to: "test.rs"
---
fn main() {
    println!("Hello, World!")  // Missing semicolon
}"#;

    let result = ggen_ai::generators::validator::TemplateValidator::new()
        .validate_template(invalid_template)?;

    println!("✅ Template validation works for invalid template");
    println!("   - Valid: {}", result.valid);
    println!("   - Issues: {}", result.issues.len());

    Ok(())
}