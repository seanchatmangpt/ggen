//! AssertableModule trait for integrating assertions with DSPy modules

use super::executor::{Assertion, AssertionResult, BacktrackExecutor};
use crate::dspy::{Module, ModuleError};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

/// Trait for modules that support assertions
///
/// Extends the base `Module` trait with assertion capabilities.
/// Modules implementing this trait can have assertions attached and
/// will automatically use backtracking retry logic on validation failures.
pub trait AssertableModule: Module {
    /// Add an assertion to this module
    fn with_assertion(self, assertion: Assertion) -> AssertedModule<Self>
    where
        Self: Sized,
    {
        AssertedModule::new(self).with_assertion(assertion)
    }

    /// Add multiple assertions to this module
    fn with_assertions(self, assertions: Vec<Assertion>) -> AssertedModule<Self>
    where
        Self: Sized,
    {
        AssertedModule::new(self).with_assertions(assertions)
    }
}

/// Blanket implementation - all Modules can be assertable
impl<T: Module> AssertableModule for T {}

/// Module wrapper that includes assertions
///
/// Wraps a base module and executes it with assertion validation
/// and automatic retry logic.
pub struct AssertedModule<M: Module> {
    /// Inner module
    inner: M,

    /// Assertions to validate
    assertions: Vec<Assertion>,

    /// Whether to use retry context injection
    use_context_injection: bool,
}

impl<M: Module> AssertedModule<M> {
    /// Create new asserted module
    pub fn new(inner: M) -> Self {
        Self {
            inner,
            assertions: Vec::new(),
            use_context_injection: true,
        }
    }

    /// Add an assertion
    pub fn with_assertion(mut self, assertion: Assertion) -> Self {
        self.assertions.push(assertion);
        self
    }

    /// Add multiple assertions
    pub fn with_assertions(mut self, assertions: Vec<Assertion>) -> Self {
        self.assertions.extend(assertions);
        self
    }

    /// Enable or disable retry context injection
    ///
    /// When enabled (default), retry attempts will include past outputs
    /// and feedback in the input to help the model self-correct.
    pub fn with_context_injection(mut self, enabled: bool) -> Self {
        self.use_context_injection = enabled;
        self
    }

    /// Get reference to inner module
    pub fn inner(&self) -> &M {
        &self.inner
    }

    /// Get mutable reference to inner module
    pub fn inner_mut(&mut self) -> &mut M {
        &mut self.inner
    }

    /// Unwrap to get inner module
    pub fn into_inner(self) -> M {
        self.inner
    }

    /// Get assertions
    pub fn assertions(&self) -> &[Assertion] {
        &self.assertions
    }

    /// Execute with assertions using backtrack executor
    async fn execute_with_assertions(
        &self, inputs: HashMap<String, Value>,
    ) -> AssertionResult<HashMap<String, Value>> {
        let mut executor = BacktrackExecutor::new(self.assertions.clone());

        if self.use_context_injection {
            executor.execute_with_context(&self.inner, inputs).await
        } else {
            executor.execute(&self.inner, inputs).await
        }
    }
}

#[async_trait::async_trait]
impl<M: Module + Send + Sync + 'static> Module for AssertedModule<M> {
    fn signature(&self) -> &crate::dspy::Signature {
        self.inner.signature()
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        if self.assertions.is_empty() {
            // No assertions - just forward to inner module
            return self.inner.forward(inputs).await;
        }

        // Execute with assertions
        self.execute_with_assertions(inputs)
            .await
            .map_err(|e| ModuleError::Other(e.to_string()))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

/// Helper function to create an asserted module
///
/// # Example
/// ```ignore
/// use ggen_ai::dspy::assertions::*;
///
/// let module = create_asserted_module(
///     my_module,
///     vec![
///         Assertion::assert(LengthValidator::min(10)).max_retries(3),
///         Assertion::suggest(NotEmptyValidator).max_retries(2),
///     ]
/// );
/// ```
pub fn create_asserted_module<M: Module>(
    module: M, assertions: Vec<Assertion>,
) -> AssertedModule<M> {
    AssertedModule::new(module).with_assertions(assertions)
}

/// Helper to wrap module in Arc for use as teacher in optimizers
///
/// # Example
/// ```ignore
/// use ggen_ai::dspy::assertions::*;
/// use ggen_ai::dspy::optimizer::BootstrapFewShot;
///
/// let asserted_module = my_module.with_assertion(
///     Assertion::assert(validator).max_retries(3)
/// );
///
/// let teacher = arc_asserted_module(asserted_module);
///
/// let optimizer = BootstrapFewShot::new(metric)
///     .with_teacher(teacher);
/// ```
pub fn arc_asserted_module<M: Module + Send + Sync + 'static>(
    module: AssertedModule<M>,
) -> Arc<dyn Module> {
    Arc::new(module)
}

#[cfg(test)]
mod tests {
    use super::super::validator::*;
    use super::*;
    use crate::dspy::{field::*, Signature};

    // Mock module for testing
    struct TestModule {
        sig: Signature,
        output: String,
    }

    impl TestModule {
        fn new(output: String) -> Self {
            let sig = Signature::new("Test", "Test")
                .with_input(InputField::new("input", "Input", "String"))
                .with_output(OutputField::new("output", "Output", "String"));

            Self { sig, output }
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
            let mut outputs = HashMap::new();
            outputs.insert("output".to_string(), Value::String(self.output.clone()));
            Ok(outputs)
        }
    }

    #[tokio::test]
    async fn test_asserted_module_creation() {
        let module = TestModule::new("hello".to_string());
        let assertion = Assertion::assert(LengthValidator::min(5));

        let asserted = module.with_assertion(assertion);

        assert_eq!(asserted.assertions().len(), 1);
    }

    #[tokio::test]
    async fn test_asserted_module_multiple_assertions() {
        let module = TestModule::new("hello".to_string());

        let assertions = vec![
            Assertion::assert(NotEmptyValidator),
            Assertion::assert(LengthValidator::between(3, 10)),
        ];

        let asserted = AssertedModule::new(module).with_assertions(assertions);

        assert_eq!(asserted.assertions().len(), 2);
    }

    #[tokio::test]
    async fn test_asserted_module_execute_success() {
        let module = TestModule::new("hello world".to_string());

        let assertion = Assertion::assert(LengthValidator::between(5, 20)).max_retries(2);

        let asserted = module.with_assertion(assertion);

        let inputs = HashMap::new();
        let result = asserted.forward(inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_asserted_module_execute_failure() {
        let module = TestModule::new("hi".to_string());

        let assertion = Assertion::assert(LengthValidator::min(10)).max_retries(2);

        let asserted = module.with_assertion(assertion);

        let inputs = HashMap::new();
        let result = asserted.forward(inputs).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_asserted_module_no_assertions() {
        let module = TestModule::new("hello".to_string());
        let asserted = AssertedModule::new(module);

        let inputs = HashMap::new();
        let result = asserted.forward(inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_asserted_module_inner_access() {
        let module = TestModule::new("hello".to_string());
        let mut asserted = AssertedModule::new(module);

        assert_eq!(asserted.inner().output, "hello");
        asserted.inner_mut().output = "world".to_string();
        assert_eq!(asserted.inner().output, "world");

        let inner = asserted.into_inner();
        assert_eq!(inner.output, "world");
    }

    #[tokio::test]
    async fn test_asserted_module_context_injection() {
        let module = TestModule::new("hello".to_string());

        let assertion = Assertion::assert(LengthValidator::min(5));

        let asserted = module
            .with_assertion(assertion)
            .with_context_injection(true);

        let inputs = HashMap::new();
        let result = asserted.forward(inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_create_asserted_module_helper() {
        let module = TestModule::new("hello world".to_string());

        let assertions = vec![
            Assertion::assert(NotEmptyValidator).max_retries(2),
            Assertion::assert(LengthValidator::min(5)).max_retries(2),
        ];

        let asserted = create_asserted_module(module, assertions);

        let inputs = HashMap::new();
        let result = asserted.forward(inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_arc_asserted_module_helper() {
        let module = TestModule::new("hello world".to_string());

        let assertion = Assertion::assert(LengthValidator::min(5));
        let asserted = module.with_assertion(assertion);

        let arc_module = arc_asserted_module(asserted);

        let inputs = HashMap::new();
        let result = arc_module.forward(inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_asserted_module_signature() {
        let module = TestModule::new("hello".to_string());
        let asserted = module.with_assertion(Assertion::assert(NotEmptyValidator));

        assert_eq!(asserted.signature().name, "Test");
    }
}
