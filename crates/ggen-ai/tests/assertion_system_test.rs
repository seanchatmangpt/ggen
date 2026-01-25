//! Comprehensive integration test for DSPy assertion system
//!
//! Verifies all components work together:
//! - Validators (all built-in types)
//! - Assertions (Assert and Suggest)
//! - BacktrackExecutor (retry logic)
//! - AssertedModule (module wrapper)
//! - Context injection
//! - Warning collection

use ggen_ai::dspy::{
    assertions::{
        AllValidator, AnyValidator, AssertableModule, Assertion, AssertionLevel, BacktrackExecutor,
        BoxedValidator, ContainsValidator, FnValidator, ItemCountValidator, LengthValidator,
        NotEmptyValidator, NotValidator, PatternValidator, UniqueItemsValidator, ValidationResult,
    },
    field::{InputField, OutputField},
    Module, ModuleError, Signature,
};
use serde_json::{json, Value};
use std::collections::HashMap;

// Test module that simulates improving on retry
struct TestModule {
    sig: Signature,
    responses: Vec<String>,
    call_count: std::sync::Arc<std::sync::Mutex<usize>>,
}

impl TestModule {
    fn new(responses: Vec<String>) -> Self {
        let sig = Signature::new("Test", "Test module")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        Self {
            sig,
            responses,
            call_count: std::sync::Arc::new(std::sync::Mutex::new(0)),
        }
    }

    fn calls(&self) -> usize {
        *self.call_count.lock().unwrap()
    }
}

#[async_trait::async_trait]
impl Module for TestModule {
    fn signature(&self) -> &Signature {
        &self.sig
    }

    async fn forward(
        &self, _inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        let mut count = self.call_count.lock().unwrap();
        let idx = *count;
        *count += 1;

        let response = self
            .responses
            .get(idx)
            .unwrap_or_else(|| self.responses.last().unwrap());

        let mut outputs = HashMap::new();
        outputs.insert("output".to_string(), Value::String(response.clone()));
        Ok(outputs)
    }
}

#[tokio::test]
async fn test_assertion_success_first_try() {
    let module = TestModule::new(vec!["hello world".to_string()]);

    let assertion = Assertion::assert(LengthValidator::min(5))
        .with_feedback("Too short")
        .max_retries(3);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_assertion_retry_then_success() {
    let module = TestModule::new(vec![
        "hi".to_string(),          // Too short - retry
        "hello".to_string(),       // Too short - retry
        "hello world".to_string(), // Success
    ]);

    let assertion = Assertion::assert(LengthValidator::min(10))
        .with_feedback("Too short")
        .max_retries(3);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
    assert_eq!(module.calls(), 3); // Called 3 times
}

#[tokio::test]
async fn test_assertion_max_retries_exceeded() {
    let module = TestModule::new(vec!["hi".to_string()]); // Always too short

    let assertion = Assertion::assert(LengthValidator::min(10))
        .with_feedback("Too short")
        .max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_err());
    assert_eq!(module.calls(), 2); // Initial + 1 retry = 2 total
}

#[tokio::test]
async fn test_suggestion_warns_but_succeeds() {
    let module = TestModule::new(vec!["hi".to_string()]);

    let suggestion = Assertion::suggest(LengthValidator::min(10))
        .with_feedback("Should be longer")
        .max_retries(2);

    let mut executor = BacktrackExecutor::new(vec![suggestion]);

    let result = executor.execute(&module, HashMap::new()).await;
    assert!(result.is_ok());
    assert_eq!(executor.warnings().len(), 1);
}

#[tokio::test]
async fn test_multiple_assertions() {
    let module = TestModule::new(vec![
        "hi".to_string(),           // Too short
        "hello".to_string(),        // Better, but no exclamation
        "hello world!".to_string(), // Success
    ]);

    let assertions = vec![
        Assertion::assert(LengthValidator::min(10))
            .with_feedback("Too short")
            .max_retries(3),
        Assertion::assert(ContainsValidator::new("!"))
            .with_feedback("Must be exciting!")
            .max_retries(3),
    ];

    let asserted = module.with_assertions(assertions);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_all_validator() {
    let module = TestModule::new(vec!["hello world!".to_string()]);

    let validators: Vec<BoxedValidator> = vec![
        Box::new(NotEmptyValidator),
        Box::new(LengthValidator::between(5, 50)),
        Box::new(ContainsValidator::new("hello")),
    ];

    let all = AllValidator::new(validators);

    let assertion = Assertion::assert(all).max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_any_validator() {
    let module = TestModule::new(vec!["hi".to_string()]);

    // Either very short OR very long
    let validators: Vec<BoxedValidator> = vec![
        Box::new(LengthValidator::max(5)),
        Box::new(LengthValidator::min(20)),
    ];

    let any = AnyValidator::new(validators);

    let assertion = Assertion::assert(any).max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok()); // "hi" is <= 5, so passes
}

#[tokio::test]
async fn test_not_validator() {
    let module = TestModule::new(vec!["good content".to_string()]);

    let not = NotValidator::new(Box::new(ContainsValidator::new("bad")));

    let assertion = Assertion::assert(not)
        .with_feedback("Must not contain bad words")
        .max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_fn_validator() {
    let module = TestModule::new(vec![
        "one two".to_string(),            // 2 words - fail
        "one two three four".to_string(), // 4 words - success
    ]);

    let validator = FnValidator::new(
        |value| {
            if let Some(s) = value.as_str() {
                if s.split_whitespace().count() >= 3 {
                    ValidationResult::valid()
                } else {
                    ValidationResult::invalid("Need at least 3 words")
                }
            } else {
                ValidationResult::invalid("Must be string")
            }
        },
        "Word count >= 3",
    );

    let assertion = Assertion::assert(validator).max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_pattern_validator() {
    let module = TestModule::new(vec![
        "invalid-email".to_string(),
        "user@example.com".to_string(),
    ]);

    let pattern = PatternValidator::new(r"^[\w\.\-]+@[\w\.\-]+\.\w+$").expect("Valid regex");

    let assertion = Assertion::assert(pattern)
        .with_feedback("Must be valid email")
        .max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_context_injection() {
    let module = TestModule::new(vec![
        "bad".to_string(),
        "better".to_string(),
        "best response".to_string(),
    ]);

    let assertion = Assertion::assert(LengthValidator::min(10))
        .with_feedback("Response too short")
        .max_retries(3);

    let mut executor = BacktrackExecutor::new(vec![assertion]);

    // execute_with_context injects past outputs and feedback
    let result = executor.execute_with_context(&module, HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_assertion_level() {
    let assert_assertion = Assertion::assert(NotEmptyValidator);
    assert_eq!(assert_assertion.level(), AssertionLevel::Assert);

    let suggest_assertion = Assertion::suggest(NotEmptyValidator);
    assert_eq!(suggest_assertion.level(), AssertionLevel::Suggest);
}

#[tokio::test]
async fn test_max_attempts_configuration() {
    let assertion = Assertion::assert(LengthValidator::min(5)).max_retries(10);

    assert_eq!(assertion.max_attempts(), 10);
}

#[tokio::test]
async fn test_custom_feedback() {
    let module = TestModule::new(vec!["hi".to_string()]);

    let assertion = Assertion::assert(LengthValidator::min(10))
        .with_feedback("CUSTOM: Must be at least 10 characters!")
        .max_retries(1);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert!(err.to_string().contains("CUSTOM"));
}

#[tokio::test]
async fn test_item_count_validator() {
    // Create module that returns arrays
    let sig = Signature::new("Test", "Test")
        .with_input(InputField::new("input", "Input", "String"))
        .with_output(OutputField::new("output", "Output", "Array"));

    struct ArrayModule {
        sig: Signature,
        items: Vec<Value>,
    }

    #[async_trait::async_trait]
    impl Module for ArrayModule {
        fn signature(&self) -> &Signature {
            &self.sig
        }

        async fn forward(
            &self, _: HashMap<String, Value>,
        ) -> Result<HashMap<String, Value>, ModuleError> {
            let mut outputs = HashMap::new();
            outputs.insert("output".to_string(), Value::Array(self.items.clone()));
            Ok(outputs)
        }
    }

    let module = ArrayModule {
        sig,
        items: vec![json!("a"), json!("b"), json!("c")],
    };

    let assertion = Assertion::assert(ItemCountValidator::between(2, 5)).max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_unique_items_validator() {
    let sig = Signature::new("Test", "Test")
        .with_input(InputField::new("input", "Input", "String"))
        .with_output(OutputField::new("output", "Output", "Array"));

    struct ArrayModule {
        sig: Signature,
        items: Vec<Value>,
    }

    #[async_trait::async_trait]
    impl Module for ArrayModule {
        fn signature(&self) -> &Signature {
            &self.sig
        }

        async fn forward(
            &self, _: HashMap<String, Value>,
        ) -> Result<HashMap<String, Value>, ModuleError> {
            let mut outputs = HashMap::new();
            outputs.insert("output".to_string(), Value::Array(self.items.clone()));
            Ok(outputs)
        }
    }

    let module = ArrayModule {
        sig,
        items: vec![json!("a"), json!("b"), json!("c")], // All unique
    };

    let assertion = Assertion::assert(UniqueItemsValidator)
        .with_feedback("Items must be unique")
        .max_retries(2);

    let asserted = module.with_assertion(assertion);

    let result = asserted.forward(HashMap::new()).await;
    assert!(result.is_ok());
}
