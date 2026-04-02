//! Advanced DSPy assertion example with ChainOfThought integration
//!
//! Demonstrates:
//! - Custom validators
//! - Pattern matching
//! - Context injection for self-correction
//! - Integration with more complex modules
//!
//! Run with: cargo run --example assertions_advanced

use ggen_ai::dspy::{
    assertions::{
        AnyValidator, AssertableModule, Assertion, BacktrackExecutor, BoxedValidator,
        ContainsValidator, FnValidator, LengthValidator, NotEmptyValidator, PatternValidator,
        ValidationResult,
    },
    field::{InputField, OutputField},
    Module, ModuleError, Signature,
};
use serde_json::Value;
use std::collections::HashMap;

/// Code generation module with quality improvement on retry
struct CodeGenModule {
    sig: Signature,
    attempt_count: std::sync::Arc<std::sync::Mutex<usize>>,
}

impl CodeGenModule {
    fn new() -> Self {
        let sig = Signature::new("CodeGen", "Rust code generator")
            .with_input(InputField::new("task", "Code generation task", "String"))
            .with_output(OutputField::new("code", "Generated Rust code", "String"));

        Self {
            sig,
            attempt_count: std::sync::Arc::new(std::sync::Mutex::new(0)),
        }
    }

    fn get_code(&self, attempt: usize) -> &'static str {
        match attempt {
            0 => "fn hello() { }",  // No documentation
            1 => "/// Says hello\nfn hello() { }",  // Has doc, but no implementation
            2 => "/// Says hello\nfn hello() {\n    println!(\"Hello\");\n}",  // Missing return type
            _ => "/// Says hello to the world\n/// # Returns\n/// Unit type\nfn hello() {\n    println!(\"Hello, world!\");\n}",
        }
    }
}

#[async_trait::async_trait]
impl Module for CodeGenModule {
    fn signature(&self) -> &Signature {
        &self.sig
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        let mut count = self.attempt_count.lock().unwrap();
        let attempt = *count;
        *count += 1;

        let _task = inputs
            .get("task")
            .and_then(|v| v.as_str())
            .unwrap_or("Unknown task");

        // Check if retry context is present
        let has_retry_context = inputs.contains_key("__retry_context");
        if has_retry_context {
            println!(
                "  Attempt {} (with feedback): Improving code...",
                attempt + 1
            );
        } else {
            println!("  Attempt {}: Generating code...", attempt + 1);
        }

        let code = self.get_code(attempt);

        let mut outputs = HashMap::new();
        outputs.insert("code".to_string(), Value::String(code.to_string()));
        Ok(outputs)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== DSPy Assertion System - Advanced Example ===\n");

    // Example 1: Custom function-based validator
    println!("1. Custom Validator - Code must have documentation:");
    {
        let module = CodeGenModule::new();

        let doc_validator = FnValidator::new(
            |value| {
                if let Some(code) = value.as_str() {
                    if code.contains("///") {
                        ValidationResult::valid()
                    } else {
                        ValidationResult::invalid("Code must include documentation comments (///)")
                    }
                } else {
                    ValidationResult::invalid("Output must be a string")
                }
            },
            "Has documentation comments",
        );

        let assertion = Assertion::assert(doc_validator)
            .with_feedback("Generated code must include documentation comments")
            .max_retries(3);

        let asserted_module = module.with_assertion(assertion);

        let mut inputs = HashMap::new();
        inputs.insert(
            "task".to_string(),
            Value::String("Create a hello function".to_string()),
        );

        match asserted_module.forward(inputs).await {
            Ok(output) => {
                let code = output.get("code").unwrap();
                println!("   Success!\n   Generated code:\n{}", code);
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    // Example 2: Pattern matching for code quality
    println!("\n2. Pattern Validator - Function must have implementation:");
    {
        let module = CodeGenModule::new();

        // Regex to check for actual function body content (not just braces)
        let impl_validator = FnValidator::new(
            |value| {
                if let Some(code) = value.as_str() {
                    // Check if function has actual code (contains println! or similar)
                    if code.contains("println!") || code.contains("return") || code.len() > 50 {
                        ValidationResult::valid()
                    } else {
                        ValidationResult::invalid(
                            "Function must have actual implementation (not just empty braces)",
                        )
                    }
                } else {
                    ValidationResult::invalid("Output must be a string")
                }
            },
            "Has implementation",
        );

        let assertion = Assertion::assert(impl_validator)
            .with_feedback("Function needs actual implementation code")
            .max_retries(4);

        let asserted_module = module.with_assertion(assertion);

        let mut inputs = HashMap::new();
        inputs.insert(
            "task".to_string(),
            Value::String("Create a hello function".to_string()),
        );

        match asserted_module.forward(inputs).await {
            Ok(output) => {
                let code = output.get("code").unwrap();
                println!("   Success!\n   Generated code:\n{}", code);
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    // Example 3: Multiple assertions with mixed severity
    println!("\n3. Mixed Assertions - Required docs + Suggested formatting:");
    {
        let module = CodeGenModule::new();

        let assertions = vec![
            // Hard requirement: must have documentation
            Assertion::assert(ContainsValidator::new("///"))
                .with_feedback("Code must include documentation")
                .max_retries(3),
            // Hard requirement: must have implementation
            Assertion::assert(ContainsValidator::new("println!"))
                .with_feedback("Function must have println! statement")
                .max_retries(4),
            // Soft suggestion: prefer detailed documentation
            Assertion::suggest(FnValidator::new(
                |value| {
                    if let Some(code) = value.as_str() {
                        let doc_lines = code.lines().filter(|l| l.contains("///")).count();
                        if doc_lines >= 3 {
                            ValidationResult::valid()
                        } else {
                            ValidationResult::invalid(format!(
                                "Only {} doc lines, prefer 3+",
                                doc_lines
                            ))
                        }
                    } else {
                        ValidationResult::invalid("Output must be a string")
                    }
                },
                "Detailed documentation (3+ lines)",
            ))
            .with_feedback("Consider adding more detailed documentation")
            .max_retries(2),
        ];

        let asserted_module = module.with_assertions(assertions);

        let mut inputs = HashMap::new();
        inputs.insert(
            "task".to_string(),
            Value::String("Create a hello function".to_string()),
        );

        match asserted_module.forward(inputs).await {
            Ok(output) => {
                let code = output.get("code").unwrap();
                println!("   Success!\n   Generated code:\n{}", code);
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    // Example 4: BacktrackExecutor with context injection
    println!("\n4. Context Injection - Self-correction with retry feedback:");
    {
        let module = CodeGenModule::new();

        let assertions = vec![
            Assertion::assert(ContainsValidator::new("///"))
                .with_feedback("MUST include documentation comments")
                .max_retries(5),
            Assertion::assert(LengthValidator::min(80))
                .with_feedback("Code should be comprehensive (80+ chars)")
                .max_retries(5),
        ];

        // Use execute_with_context to inject retry feedback
        let mut executor = BacktrackExecutor::new(assertions);

        let mut inputs = HashMap::new();
        inputs.insert(
            "task".to_string(),
            Value::String("Create a hello function".to_string()),
        );

        match executor.execute_with_context(&module, inputs).await {
            Ok(output) => {
                let code = output.get("code").unwrap();
                println!("   Success with context injection!");
                println!("   Generated code:\n{}", code);
                println!("\n   Warnings: {}", executor.warnings().len());
                for warning in executor.warnings() {
                    println!(
                        "     - {} (after {} attempts)",
                        warning.feedback, warning.attempts
                    );
                }
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    // Example 5: OR logic with AnyValidator
    println!("\n5. AnyValidator - Either short OR has detailed docs:");
    {
        let module = CodeGenModule::new();

        // Accept either:
        // - Very short code (< 30 chars) OR
        // - Detailed documentation (3+ doc lines)
        let validators: Vec<BoxedValidator> = vec![
            Box::new(LengthValidator::max(30)),
            Box::new(FnValidator::new(
                |value| {
                    if let Some(code) = value.as_str() {
                        let doc_lines = code.lines().filter(|l| l.contains("///")).count();
                        if doc_lines >= 3 {
                            ValidationResult::valid()
                        } else {
                            ValidationResult::invalid("Not enough documentation")
                        }
                    } else {
                        ValidationResult::invalid("Must be string")
                    }
                },
                "Has 3+ doc lines",
            )),
        ];

        let any_validator = AnyValidator::new(validators);

        let assertion = Assertion::assert(any_validator)
            .with_feedback("Code should be either very short OR have detailed documentation")
            .max_retries(5);

        let asserted_module = module.with_assertion(assertion);

        let mut inputs = HashMap::new();
        inputs.insert(
            "task".to_string(),
            Value::String("Create a hello function".to_string()),
        );

        match asserted_module.forward(inputs).await {
            Ok(output) => {
                let code = output.get("code").unwrap();
                println!("   Success!\n   Generated code:\n{}", code);
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    println!("\n=== Example Complete ===\n");
    Ok(())
}
