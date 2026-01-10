//! Signature Validation Example
//!
//! This example demonstrates:
//! 1. Creating signatures with constraints
//! 2. Validating input against signatures
//! 3. Detailed error reporting
//! 4. Type checking and constraint enforcement
//!
//! Run with: cargo run --example signature_validation --package ggen-ai

use serde_json::json;
use std::collections::HashMap;

/// Simplified Signature and Constraint types for demonstration
#[derive(Debug, Clone)]
pub struct Signature {
    name: String,
    description: String,
    inputs: Vec<Field>,
    outputs: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    type_annotation: String,
    description: String,
    required: bool,
    constraints: Vec<Constraint>,
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Pattern(String),
    MinLength(usize),
    MaxLength(usize),
    MinValue(f64),
    MaxValue(f64),
}

impl Signature {
    fn new(name: &str, description: &str) -> Self {
        Self {
            name: name.to_string(),
            description: description.to_string(),
            inputs: Vec::new(),
            outputs: Vec::new(),
        }
    }

    fn with_input(mut self, field: Field) -> Self {
        self.inputs.push(field);
        self
    }

    fn with_output(mut self, field: Field) -> Self {
        self.outputs.push(field);
        self
    }
}

impl Field {
    fn new(name: &str, type_annotation: &str) -> Self {
        Self {
            name: name.to_string(),
            type_annotation: type_annotation.to_string(),
            description: String::new(),
            required: false,
            constraints: Vec::new(),
        }
    }

    fn with_description(mut self, desc: &str) -> Self {
        self.description = desc.to_string();
        self
    }

    fn required(mut self) -> Self {
        self.required = true;
        self
    }

    fn with_constraint(mut self, constraint: Constraint) -> Self {
        self.constraints.push(constraint);
        self
    }
}

/// Validator for input data against signature
struct SignatureValidator {
    signature: Signature,
}

#[derive(Debug)]
struct ValidationResult {
    valid: bool,
    errors: Vec<String>,
}

impl SignatureValidator {
    fn new(signature: Signature) -> Self {
        Self { signature }
    }

    fn validate(&self, input: &serde_json::Value) -> ValidationResult {
        let mut errors = Vec::new();

        for field in &self.signature.inputs {
            // Check required fields
            if field.required && !input.get(&field.name).is_some() {
                errors.push(format!("Missing required field: {}", field.name));
                continue;
            }

            if let Some(value) = input.get(&field.name) {
                // Validate type
                if !self.check_type(value, &field.type_annotation) {
                    errors.push(format!(
                        "Type mismatch for field '{}': expected {}, got {}",
                        field.name,
                        field.type_annotation,
                        self.value_type(value)
                    ));
                    continue;
                }

                // Validate constraints
                for constraint in &field.constraints {
                    if let Err(e) = self.validate_constraint(value, constraint, &field.name) {
                        errors.push(e);
                    }
                }
            }
        }

        ValidationResult {
            valid: errors.is_empty(),
            errors,
        }
    }

    fn check_type(&self, value: &serde_json::Value, expected_type: &str) -> bool {
        match expected_type {
            "String" => value.is_string(),
            "i32" | "i64" | "integer" => value.is_i64(),
            "f32" | "f64" | "float" => value.is_f64() || value.is_i64(),
            "bool" => value.is_boolean(),
            _ => true, // Accept unknown types
        }
    }

    fn value_type(&self, value: &serde_json::Value) -> &'static str {
        if value.is_string() {
            "String"
        } else if value.is_i64() {
            "integer"
        } else if value.is_f64() {
            "float"
        } else if value.is_boolean() {
            "bool"
        } else if value.is_array() {
            "array"
        } else if value.is_object() {
            "object"
        } else if value.is_null() {
            "null"
        } else {
            "unknown"
        }
    }

    fn validate_constraint(
        &self,
        value: &serde_json::Value,
        constraint: &Constraint,
        field_name: &str,
    ) -> Result<(), String> {
        match constraint {
            Constraint::Pattern(pattern) => {
                if let Some(s) = value.as_str() {
                    let re = regex::Regex::new(pattern).map_err(|_| "Invalid regex".to_string())?;
                    if !re.is_match(s) {
                        return Err(format!(
                            "Pattern mismatch for field '{}': expected {}, got '{}'",
                            field_name, pattern, s
                        ));
                    }
                }
                Ok(())
            }
            Constraint::MinLength(min) => {
                if let Some(s) = value.as_str() {
                    if s.len() < *min {
                        return Err(format!(
                            "String too short for field '{}': minimum {} chars, got {}",
                            field_name,
                            min,
                            s.len()
                        ));
                    }
                }
                Ok(())
            }
            Constraint::MaxLength(max) => {
                if let Some(s) = value.as_str() {
                    if s.len() > *max {
                        return Err(format!(
                            "String too long for field '{}': maximum {} chars, got {}",
                            field_name,
                            max,
                            s.len()
                        ));
                    }
                }
                Ok(())
            }
            Constraint::MinValue(min) => {
                if let Some(n) = value.as_f64() {
                    if n < *min {
                        return Err(format!(
                            "Value too small for field '{}': minimum {}, got {}",
                            field_name, min, n
                        ));
                    }
                }
                Ok(())
            }
            Constraint::MaxValue(max) => {
                if let Some(n) = value.as_f64() {
                    if n > *max {
                        return Err(format!(
                            "Value too large for field '{}': maximum {}, got {}",
                            field_name, max, n
                        ));
                    }
                }
                Ok(())
            }
        }
    }
}

fn main() {
    println!("=== Signature Validation Example ===\n");

    // Step 1: Create signature with constraints
    println!("Step 1: Create Signature with Constraints");
    let ticker_field = Field::new("ticker_symbol", "String")
        .with_description("Stock ticker symbol")
        .required()
        .with_constraint(Constraint::Pattern("^[A-Z]{1,5}$".to_string()))
        .with_constraint(Constraint::MinLength(1))
        .with_constraint(Constraint::MaxLength(5));

    let period_field = Field::new("period_days", "i32")
        .with_description("Analysis period in days")
        .required()
        .with_constraint(Constraint::MinValue(1.0))
        .with_constraint(Constraint::MaxValue(365.0));

    let recommendation_field = Field::new("recommendation", "String")
        .with_description("Investment recommendation");

    let signature = Signature::new(
        "FinancialAnalyzer",
        "Analyzes stock data and provides recommendations",
    )
    .with_input(ticker_field)
    .with_input(period_field)
    .with_output(recommendation_field);

    println!("  - Created signature: {}", signature.name);
    println!("  - Input fields:");
    for field in &signature.inputs {
        println!("    * {}: {} ({} constraints)", field.name, field.type_annotation, field.constraints.len());
    }
    println!();

    // Step 2: Create validator
    println!("Step 2: Create Validator");
    let validator = SignatureValidator::new(signature);
    println!("  - Validator created\n");

    // Step 3: Test valid input
    println!("Step 3: Validate Valid Input");
    let valid_input = json!({
        "ticker_symbol": "AAPL",
        "period_days": 90
    });
    println!("  - Input: {}", valid_input);

    let result = validator.validate(&valid_input);
    if result.valid {
        println!("  - Validation: PASSED ✓");
    } else {
        println!("  - Validation: FAILED");
        for error in &result.errors {
            println!("    - {}", error);
        }
    }
    println!();

    // Step 4: Test invalid ticker (lowercase)
    println!("Step 4: Validate Invalid Ticker (lowercase)");
    let invalid_ticker = json!({
        "ticker_symbol": "aapl",
        "period_days": 90
    });
    println!("  - Input: {}", invalid_ticker);

    let result = validator.validate(&invalid_ticker);
    if result.valid {
        println!("  - Validation: PASSED ✓");
    } else {
        println!("  - Validation: FAILED");
        for error in &result.errors {
            println!("    - {}", error);
        }
    }
    println!();

    // Step 5: Test invalid ticker (contains numbers)
    println!("Step 5: Validate Invalid Ticker (contains numbers)");
    let invalid_ticker2 = json!({
        "ticker_symbol": "AAPL123",
        "period_days": 90
    });
    println!("  - Input: {}", invalid_ticker2);

    let result = validator.validate(&invalid_ticker2);
    if result.valid {
        println!("  - Validation: PASSED ✓");
    } else {
        println!("  - Validation: FAILED");
        for error in &result.errors {
            println!("    - {}", error);
        }
    }
    println!();

    // Step 6: Test invalid period (too large)
    println!("Step 6: Validate Invalid Period (too large)");
    let invalid_period = json!({
        "ticker_symbol": "MSFT",
        "period_days": 400
    });
    println!("  - Input: {}", invalid_period);

    let result = validator.validate(&invalid_period);
    if result.valid {
        println!("  - Validation: PASSED ✓");
    } else {
        println!("  - Validation: FAILED");
        for error in &result.errors {
            println!("    - {}", error);
        }
    }
    println!();

    // Step 7: Test missing required field
    println!("Step 7: Validate Missing Required Field");
    let missing_field = json!({
        "period_days": 90
    });
    println!("  - Input: {}", missing_field);

    let result = validator.validate(&missing_field);
    if result.valid {
        println!("  - Validation: PASSED ✓");
    } else {
        println!("  - Validation: FAILED");
        for error in &result.errors {
            println!("    - {}", error);
        }
    }
    println!();

    // Step 8: Test type mismatch
    println!("Step 8: Validate Type Mismatch");
    let type_mismatch = json!({
        "ticker_symbol": "AAPL",
        "period_days": "not_a_number"
    });
    println!("  - Input: {}", type_mismatch);

    let result = validator.validate(&type_mismatch);
    if result.valid {
        println!("  - Validation: PASSED ✓");
    } else {
        println!("  - Validation: FAILED");
        for error in &result.errors {
            println!("    - {}", error);
        }
    }
    println!();

    println!("=== Example completed successfully ===");
}
