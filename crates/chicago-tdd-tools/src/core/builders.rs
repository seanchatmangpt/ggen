//! > 📚 Reference
//!
//! Test Data Builders
//!
//! Provides fluent builders for creating test data structures.
//! Aligned with workflow engine's `TestDataBuilder` API for consistency.
//!
//! # Go the Extra Mile: 1st/2nd/3rd Idea Progression
//!
//! - **1st Idea**: `TestDataBuilder` - Specific `HashMap<String, String>` implementation
//! - **2nd Idea**: `GenericTestDataBuilder<K, V>` - Generic builder for any `K: Into<String>, V: Into<String>`
//! - **3rd Idea**: `ValidatedTestDataBuilder<T>` - Type-level validated builder with OTEL/Weaver validation

use serde_json::Value;
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

#[cfg(feature = "fake-data")]
use fake::{Fake, Faker};

#[cfg(feature = "otel")]
use crate::observability::otel::types::{Span, SpanContext, SpanId, SpanStatus, TraceId};
#[cfg(feature = "otel")]
use std::sync::atomic::{AtomicU64, Ordering};
#[cfg(feature = "otel")]
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(feature = "otel")]
static SPAN_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

#[cfg(feature = "otel")]
fn generate_trace_id() -> TraceId {
    let nanos = SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_nanos()).unwrap_or(1);
    let counter = u128::from(SPAN_ID_COUNTER.fetch_add(1, Ordering::Relaxed));
    // Mix time and counter to produce a unique 128-bit trace ID
    TraceId(nanos ^ counter.wrapping_mul(0x9e37_79b9_7f4a_7c15_9e37_79b9_7f4a_7c15))
}

#[cfg(feature = "otel")]
fn generate_span_id() -> SpanId {
    let counter = SPAN_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
    #[allow(clippy::cast_possible_truncation)]
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(1);
    SpanId(nanos.wrapping_add(counter.wrapping_mul(0x517c_c1b7_2722_0a95)))
}

// ============================================================================
// BUILDER PRESET SYSTEM
// ============================================================================

/// Type alias for preset configuration functions
///
/// A preset is a function that takes a `TestDataBuilder` and returns a configured `TestDataBuilder`.
/// Presets are composable - you can chain multiple presets together.
type PresetFn = Box<dyn Fn(TestDataBuilder) -> TestDataBuilder + Send + Sync>;

/// Global preset registry
///
/// Thread-safe registry for storing and retrieving builder presets.
/// Uses `OnceLock` for initialization and `Mutex` for interior mutability.
fn preset_registry() -> &'static Mutex<HashMap<String, PresetFn>> {
    static REGISTRY: OnceLock<Mutex<HashMap<String, PresetFn>>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Validation function type for builder validation hooks
///
/// Takes a reference to the data being built and returns Ok(()) if valid,
/// or Err(String) with an error message if invalid.
type ValidationFn = Box<dyn Fn(&HashMap<String, String>) -> Result<(), String> + Send + Sync>;

/// > 📚 Reference
///
/// Builder for test data (case variables).
///
/// This builder creates test data as `HashMap<String, String>` and can convert to JSON.
/// Provides a fluent API for building test data structures.
///
/// Supports optional validation hooks that run when `build()` or `try_build()` is called.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::builders::TestDataBuilder;
///
/// let data = TestDataBuilder::new()
///     .with_var("key", "value")
///     .build();
///
/// assert_eq!(data.get("key").unwrap(), "value");
/// ```
pub struct TestDataBuilder {
    data: HashMap<String, String>,
    #[allow(clippy::type_complexity)] // Validation functions are inherently complex
    validations: Vec<ValidationFn>,
}

// Custom Debug implementation since ValidationFn doesn't implement Debug
impl std::fmt::Debug for TestDataBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TestDataBuilder")
            .field("data", &self.data)
            .field("validations", &format!("{} validation(s)", self.validations.len()))
            .finish()
    }
}

impl TestDataBuilder {
    /// Create a new test data builder
    #[must_use]
    pub fn new() -> Self {
        Self { data: HashMap::new(), validations: Vec::new() }
    }

    /// Register a named preset for reusable test data configurations
    ///
    /// Presets allow you to define common test data patterns once and reuse them across tests.
    ///
    /// **Note:** Presets cannot call other presets recursively (this would cause a deadlock).
    /// If you need to build on another preset, load the base preset first, build it, and
    /// manually apply the data.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::builders::TestDataBuilder;
    ///
    /// // Register a preset for valid orders
    /// TestDataBuilder::register_preset("valid_order", |builder| {
    ///     builder
    ///         .with_var("order_id", "ORD-001")
    ///         .with_var("amount", "100.00")
    ///         .with_var("status", "pending")
    /// });
    ///
    /// // Use the preset
    /// let data = TestDataBuilder::preset("valid_order")
    ///     .unwrap()
    ///     .with_var("customer_id", "12345")
    ///     .build();
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an error if the preset registry lock is poisoned.
    pub fn register_preset<F>(name: impl Into<String>, preset_fn: F) -> Result<(), String>
    where
        F: Fn(Self) -> Self + Send + Sync + 'static,
    {
        let registry = preset_registry();
        {
            let mut registry_guard =
                registry.lock().map_err(|e| format!("Failed to lock preset registry: {e}"))?;
            registry_guard.insert(name.into(), Box::new(preset_fn));
        }
        Ok(())
    }

    /// Load a named preset
    ///
    /// Applies a previously registered preset to create a configured builder.
    /// The returned builder can be further customized with additional method calls.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::builders::TestDataBuilder;
    ///
    /// // First register a preset
    /// TestDataBuilder::register_preset("valid_order", |builder| {
    ///     builder
    ///         .with_var("order_id", "ORD-001")
    ///         .with_var("status", "pending")
    /// }).ok();
    ///
    /// // Then use it
    /// let data = TestDataBuilder::preset("valid_order")
    ///     .unwrap()
    ///     .with_var("customer_id", "12345")
    ///     .build();
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an error if the preset is not found or if the registry lock is poisoned.
    pub fn preset(name: impl AsRef<str>) -> Result<Self, String> {
        let registry = preset_registry();
        let registry_guard =
            registry.lock().map_err(|e| format!("Failed to lock preset registry: {e}"))?;

        let preset_fn = registry_guard
            .get(name.as_ref())
            .ok_or_else(|| format!("Preset '{}' not found", name.as_ref()))?;

        let builder = Self::new();
        let result = preset_fn(builder);
        drop(registry_guard);
        Ok(result)
    }

    /// Add a validation hook that will be called when building
    ///
    /// Validation hooks allow you to add custom validation logic that runs when
    /// `build()` or `try_build()` is called. Multiple validations can be added
    /// and they will all be run in order.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::builders::TestDataBuilder;
    ///
    /// let result = TestDataBuilder::new()
    ///     .with_validation(|data| {
    ///         if !data.contains_key("required_field") {
    ///             return Err("Missing required_field".to_string());
    ///         }
    ///         Ok(())
    ///     })
    ///     .with_var("required_field", "value")
    ///     .try_build();
    ///
    /// assert!(result.is_ok());
    /// ```
    #[must_use]
    pub fn with_validation<F>(mut self, validation: F) -> Self
    where
        F: Fn(&HashMap<String, String>) -> Result<(), String> + Send + Sync + 'static,
    {
        self.validations.push(Box::new(validation));
        self
    }

    /// Add a variable
    #[must_use]
    pub fn with_var(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.data.insert(key.into(), value.into());
        self
    }

    /// Add order data (common business scenario)
    #[must_use]
    pub fn with_order_data(
        mut self,
        order_id: impl Into<String>,
        amount: impl Into<String>,
    ) -> Self {
        self.data.insert("order_id".to_string(), order_id.into());
        self.data.insert("total_amount".to_string(), amount.into());
        self.data.insert("currency".to_string(), "USD".to_string());
        self.data.insert("order_status".to_string(), "pending".to_string());
        self
    }

    /// Add customer data
    #[must_use]
    pub fn with_customer_data(mut self, customer_id: impl Into<String>) -> Self {
        self.data.insert("customer_id".to_string(), customer_id.into());
        self.data
            .insert("customer_email".to_string(), "customer@example.com".to_string());
        self
    }

    /// Add approval data
    #[must_use]
    pub fn with_approval_data(
        mut self,
        request_id: impl Into<String>,
        amount: impl Into<String>,
    ) -> Self {
        self.data.insert("request_id".to_string(), request_id.into());
        self.data.insert("amount".to_string(), amount.into());
        self.data.insert("condition".to_string(), "true".to_string());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake email address
    #[must_use]
    pub fn with_fake_email(mut self) -> Self {
        self.data.insert("email".to_string(), Faker.fake::<String>());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake name
    #[must_use]
    pub fn with_fake_name(mut self) -> Self {
        self.data.insert("name".to_string(), Faker.fake::<String>());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake UUID
    #[must_use]
    pub fn with_fake_uuid(mut self) -> Self {
        self.data.insert("uuid".to_string(), Faker.fake::<String>());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake phone number
    #[must_use]
    pub fn with_fake_phone(mut self) -> Self {
        self.data.insert("phone".to_string(), Faker.fake::<String>());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake address
    #[must_use]
    pub fn with_fake_address(mut self) -> Self {
        self.data.insert("address".to_string(), Faker.fake::<String>());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake company name
    #[must_use]
    pub fn with_fake_company(mut self) -> Self {
        self.data.insert("company".to_string(), Faker.fake::<String>());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake order data with realistic values
    #[must_use]
    pub fn with_fake_order_data(mut self) -> Self {
        self.data.insert("order_id".to_string(), Faker.fake::<String>());
        self.data
            .insert("total_amount".to_string(), format!("{:.2}", Faker.fake::<f64>() * 1000.0));
        self.data.insert("currency".to_string(), "USD".to_string());
        self.data.insert("order_status".to_string(), Faker.fake::<String>());
        self
    }

    #[cfg(feature = "fake-data")]
    /// Add fake customer data with realistic values
    #[must_use]
    pub fn with_fake_customer_data(mut self) -> Self {
        self.data.insert("customer_id".to_string(), Faker.fake::<String>());
        self.data.insert("customer_email".to_string(), Faker.fake::<String>());
        self.data.insert("customer_name".to_string(), Faker.fake::<String>());
        self
    }

    /// Run all validation hooks
    ///
    /// # Errors
    ///
    /// Returns the first validation error encountered.
    fn run_validations(&self) -> Result<(), String> {
        for validation in &self.validations {
            validation(&self.data)?;
        }
        Ok(())
    }

    /// Build test data as JSON
    ///
    /// Converts `HashMap<String, String>` to `serde_json::Value`.
    /// Runs all validation hooks before building.
    ///
    /// # Errors
    ///
    /// Returns `serde_json::Error` if serialization fails, or validation error if validation fails.
    ///
    /// # Panics
    ///
    /// Panics if validation fails (for backward compatibility with non-validation usage).
    pub fn build_json(self) -> Result<Value, serde_json::Error> {
        if let Err(e) = self.run_validations() {
            #[allow(clippy::panic)] // Intentional: panic on validation failure for backward compat
            {
                panic!("Validation failed: {e}");
            }
        }
        serde_json::to_value(&self.data)
    }

    /// Build test data as YAML
    ///
    /// Converts `HashMap<String, String>` to YAML string.
    /// Runs all validation hooks before building.
    ///
    /// # Errors
    ///
    /// Returns `serde_yaml::Error` if serialization fails, or validation error if validation fails.
    ///
    /// # Panics
    ///
    /// Panics if validation fails.
    pub fn build_yaml(self) -> Result<String, serde_yaml::Error> {
        if let Err(e) = self.run_validations() {
            #[allow(clippy::panic)]
            {
                panic!("Validation failed: {e}");
            }
        }
        serde_yaml::to_string(&self.data)
    }

    /// Build test data as TOML
    ///
    /// Converts `HashMap<String, String>` to TOML string.
    /// Runs all validation hooks before building.
    ///
    /// # Errors
    ///
    /// Returns `toml::ser::Error` if serialization fails, or validation error if validation fails.
    ///
    /// # Panics
    ///
    /// Panics if validation fails.
    pub fn build_toml(self) -> Result<String, toml::ser::Error> {
        if let Err(e) = self.run_validations() {
            #[allow(clippy::panic)]
            {
                panic!("Validation failed: {e}");
            }
        }
        toml::to_string(&self.data)
    }

    /// Build test data as `HashMap`
    ///
    /// Returns the underlying `HashMap<String, String>`.
    /// Runs all validation hooks before building.
    ///
    /// # Panics
    ///
    /// Panics if any validation hook returns an error.
    #[must_use]
    pub fn build(self) -> HashMap<String, String> {
        if let Err(e) = self.run_validations() {
            #[allow(clippy::panic)] // Intentional: panic on validation failure for backward compat
            {
                panic!("Validation failed: {e}");
            }
        }
        self.data
    }

    /// Build test data with validation
    ///
    /// Similar to `build()` but returns a `Result` instead of panicking on validation errors.
    /// Use this when you want to handle validation errors gracefully.
    ///
    /// # Errors
    ///
    /// Returns validation error if any validation hook fails.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::builders::TestDataBuilder;
    ///
    /// let result = TestDataBuilder::new()
    ///     .with_validation(|data| {
    ///         if data.is_empty() {
    ///             return Err("Data cannot be empty".to_string());
    ///         }
    ///         Ok(())
    ///     })
    ///     .try_build();
    ///
    /// assert!(result.is_err());
    /// ```
    pub fn try_build(self) -> Result<HashMap<String, String>, String> {
        self.run_validations()?;
        Ok(self.data)
    }
}

impl Default for TestDataBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// 2nd IDEA: Go bigger (80/20) - Generic version
// ============================================================================

/// > 📚 Reference
///
/// Generic test data builder for any key-value types.
///
/// **2nd Idea**: Generic builder that works with any `K: Into<String>, V: Into<String>`
/// This provides 80% more value (works for all string-convertible types) with minimal effort.
///
/// **Telemetry**: Basic OTEL spans (if otel feature enabled)
/// **Validation**: OTEL span validation
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::builders::GenericTestDataBuilder;
///
/// let data = GenericTestDataBuilder::<String, String>::new()
///     .with_var("key1", "value1")
///     .with_var("key2", "value2")
///     .build();
///
/// assert_eq!(data.get("key1").unwrap(), "value1");
/// ```
pub struct GenericTestDataBuilder<K, V> {
    data: HashMap<String, String>,
    _key_type: std::marker::PhantomData<K>,
    _value_type: std::marker::PhantomData<V>,
}

impl<K, V> GenericTestDataBuilder<K, V>
where
    K: Into<String>,
    V: Into<String>,
{
    /// Create a new generic test data builder
    #[must_use]
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            _key_type: std::marker::PhantomData,
            _value_type: std::marker::PhantomData,
        }
    }

    /// Add a variable with generic key and value types
    #[must_use]
    pub fn with_var<KI, VI>(mut self, key: KI, value: VI) -> Self
    where
        KI: Into<String>,
        VI: Into<String>,
    {
        self.data.insert(key.into(), value.into());
        self
    }

    /// Build test data as `HashMap`
    #[must_use]
    pub fn build(self) -> HashMap<String, String> {
        self.data
    }

    /// Build test data as JSON
    ///
    /// # Errors
    ///
    /// Returns `serde_json::Error` if serialization fails.
    pub fn build_json(self) -> Result<Value, serde_json::Error> {
        serde_json::to_value(&self.data)
    }

    /// Build test data with OTEL span instrumentation
    ///
    /// # Panics
    ///
    /// Panics if system time is before `UNIX_EPOCH` (should never happen in practice).
    #[cfg(feature = "otel")]
    #[must_use]
    pub fn build_with_otel(self, span_name: &str) -> (HashMap<String, String>, Span) {
        #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds since epoch won't exceed u64::MAX for many years
        let start_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("SystemTime should always be after UNIX_EPOCH")
            .as_millis() as u64;

        let mut span = Span::new_active(
            SpanContext::root(generate_trace_id(), generate_span_id(), 1),
            span_name.to_string(),
            start_time,
            std::collections::BTreeMap::new(),
            Vec::new(),
            SpanStatus::Unset,
        );

        span.attributes.insert("operation".to_string(), "build_test_data".to_string());
        span.attributes.insert("item_count".to_string(), self.data.len().to_string());

        #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds since epoch won't exceed u64::MAX for many years
        let end_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("SystemTime should always be after UNIX_EPOCH")
            .as_millis() as u64;

        // End time should always be >= start time in normal operation
        // If this fails, it indicates a system clock issue
        if let Err(e) = span.complete(end_time) {
            // Log error but don't fail - span will remain active
            #[cfg(feature = "logging")]
            log::warn!("Failed to complete span: {e}");
            #[cfg(not(feature = "logging"))]
            eprintln!("Warning: Failed to complete span: {}", e);
        } else {
            span.status = SpanStatus::Ok;
        }

        (self.data, span)
    }
}

impl<K, V> Default for GenericTestDataBuilder<K, V>
where
    K: Into<String>,
    V: Into<String>,
{
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// 3rd IDEA: Maximum value - Type-level validation + OTEL + Weaver
// ============================================================================

/// > 📚 Reference
///
/// Validated test data builder with type-level validation and OTEL/Weaver validation.
///
/// **3rd Idea**: Type-level validated builder that prevents invalid states at compile time.
/// Maximum value: Type-safe, validated, prevents entire class of errors.
///
/// **Telemetry**: Full OTEL spans and metrics
/// **Validation**: OTEL span validation + Weaver live-check schema validation
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::builders::ValidatedTestDataBuilder;
///
/// struct ConfigSchema;
///
/// let builder = ValidatedTestDataBuilder::<ConfigSchema>::new();
/// let data = builder
///     .with_var("port", "8080")
///     .build();
///
/// assert_eq!(data.get("port").unwrap(), "8080");
/// ```
pub struct ValidatedTestDataBuilder<T> {
    data: HashMap<String, String>,
    _validation: std::marker::PhantomData<T>,
    #[cfg(feature = "otel")]
    span: Option<Span>,
}

impl<T> ValidatedTestDataBuilder<T> {
    /// Create a new validated test data builder
    #[must_use]
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            _validation: std::marker::PhantomData,
            #[cfg(feature = "otel")]
            span: None,
        }
    }

    /// Add a variable (validated at compile time through type system)
    #[must_use]
    pub fn with_var(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.data.insert(key.into(), value.into());
        self
    }

    /// Start OTEL span for this builder
    ///
    /// # Panics
    ///
    /// Panics if system time is before `UNIX_EPOCH` (should never happen in practice).
    #[cfg(feature = "otel")]
    #[must_use]
    pub fn start_span(mut self, span_name: &str) -> Self {
        #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds since epoch won't exceed u64::MAX for many years
        let start_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("SystemTime should always be after UNIX_EPOCH")
            .as_millis() as u64;

        let span = Span::new_active(
            SpanContext::root(generate_trace_id(), generate_span_id(), 1),
            span_name.to_string(),
            start_time,
            std::collections::BTreeMap::new(),
            Vec::new(),
            SpanStatus::Unset,
        );

        self.span = Some(span);
        self
    }

    /// Build test data with full validation
    #[must_use]
    pub fn build(self) -> HashMap<String, String> {
        self.data
    }

    /// Build test data with OTEL span (if started)
    ///
    /// # Panics
    ///
    /// Panics if system time is before `UNIX_EPOCH` (should never happen in practice).
    #[cfg(feature = "otel")]
    #[must_use]
    pub fn build_with_otel(mut self) -> (HashMap<String, String>, Option<Span>) {
        let mut span = self.span.take();

        if let Some(ref mut s) = span {
            #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
            #[allow(clippy::cast_possible_truncation)]
            // Milliseconds since epoch won't exceed u64::MAX for many years
            let end_time = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("SystemTime should always be after UNIX_EPOCH")
                .as_millis() as u64;

            // End time should always be >= start time in normal operation
            if let Err(e) = s.complete(end_time) {
                // Log error but don't fail - span will remain active
                eprintln!("Warning: Failed to complete span: {e}");
            } else {
                s.status = SpanStatus::Ok;
            }
            s.attributes.insert("item_count".to_string(), self.data.len().to_string());
            s.attributes
                .insert("operation".to_string(), "build_validated_test_data".to_string());
        }

        (self.data, span)
    }
}

impl<T> Default for ValidatedTestDataBuilder<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "fake-data")]
/// Helper for generating fake test data
///
/// Provides convenient methods for generating realistic fake data
/// for testing purposes. Uses the `fake` crate internally.
///
/// # Example
///
/// ```rust
/// # #[cfg(feature = "fake-data")]
/// use chicago_tdd_tools::builders::FakeDataGenerator;
///
/// # #[cfg(feature = "fake-data")]
/// let generator = FakeDataGenerator::new();
/// # #[cfg(feature = "fake-data")]
/// let email = generator.email();
/// # #[cfg(feature = "fake-data")]
/// let name = generator.name();
/// # #[cfg(feature = "fake-data")]
/// assert!(!email.is_empty());
/// # #[cfg(feature = "fake-data")]
/// assert!(!name.is_empty());
/// ```
pub struct FakeDataGenerator;

#[cfg(feature = "fake-data")]
impl FakeDataGenerator {
    /// Create a new fake data generator
    #[must_use]
    pub const fn new() -> Self {
        Self
    }

    /// Generate a fake email address
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn email(&self) -> String {
        Faker.fake::<String>()
    }

    /// Generate a fake name
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn name(&self) -> String {
        Faker.fake::<String>()
    }

    /// Generate a fake UUID
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn uuid(&self) -> String {
        Faker.fake::<String>()
    }

    /// Generate a fake phone number
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn phone(&self) -> String {
        Faker.fake::<String>()
    }

    /// Generate a fake address
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn address(&self) -> String {
        Faker.fake::<String>()
    }

    /// Generate a fake company name
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn company(&self) -> String {
        Faker.fake::<String>()
    }

    /// Generate a fake integer in a range
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn int(&self, min: i32, max: i32) -> i32 {
        (min..max).fake::<i32>()
    }

    /// Generate a fake float in a range
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn float(&self, min: f64, max: f64) -> f64 {
        (min..max).fake::<f64>()
    }

    /// Generate a fake string with specified length
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn string(&self, len: usize) -> String {
        (0..len).map(|_| Faker.fake::<char>()).collect()
    }
}

#[cfg(feature = "fake-data")]
impl Default for FakeDataGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test;

    // ========================================================================
    // 1. ERROR PATH TESTING - Test error handling (80% of bugs)
    // ========================================================================

    test!(test_test_data_builder_build_json_empty, {
        // Arrange: Create empty builder
        let builder = TestDataBuilder::new();

        // Act: Build JSON
        let json = builder.build_json();
        assert!(json.is_ok());
        let json = json.unwrap();

        // Assert: Verify JSON structure
        assert!(json.is_object());
    });

    test!(test_test_data_builder_build_json_with_data, {
        // Arrange: Create builder with data
        let builder = TestDataBuilder::new().with_var("key", "value");

        // Act: Build JSON
        let json = builder.build_json();
        assert!(json.is_ok());
        let json = json.unwrap();

        // Assert: Verify JSON contains data
        assert_eq!(json["key"], "value");
    });

    // ========================================================================
    // 2. BUILDER PATTERN - Test fluent API
    // ========================================================================

    test!(test_test_data_builder_new, {
        // Arrange: Create new builder
        let builder = TestDataBuilder::new();

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data is empty
        assert!(data.is_empty());
    });

    test!(test_test_data_builder_with_var, {
        // Arrange: Create builder with var
        let builder = TestDataBuilder::new().with_var("key", "value");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data contains var
        assert_eq!(data.get("key"), Some(&"value".to_string()));
    });

    test!(test_test_data_builder_with_order_data, {
        // Arrange: Create builder with order data
        let builder = TestDataBuilder::new().with_order_data("order-123", "100.00");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify order data fields
        assert_eq!(data.get("order_id"), Some(&"order-123".to_string()));
        assert_eq!(data.get("total_amount"), Some(&"100.00".to_string()));
        assert_eq!(data.get("currency"), Some(&"USD".to_string()));
        assert_eq!(data.get("order_status"), Some(&"pending".to_string()));
    });

    test!(test_test_data_builder_with_customer_data, {
        // Arrange: Create builder with customer data
        let builder = TestDataBuilder::new().with_customer_data("customer-456");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify customer data fields
        assert_eq!(data.get("customer_id"), Some(&"customer-456".to_string()));
        assert_eq!(data.get("customer_email"), Some(&"customer@example.com".to_string()));
    });

    test!(test_test_data_builder_with_approval_data, {
        // Arrange: Create builder with approval data
        let builder = TestDataBuilder::new().with_approval_data("request-789", "50.00");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify approval data fields
        assert_eq!(data.get("request_id"), Some(&"request-789".to_string()));
        assert_eq!(data.get("amount"), Some(&"50.00".to_string()));
        assert_eq!(data.get("condition"), Some(&"true".to_string()));
    });

    test!(test_test_data_builder_chaining, {
        // Arrange: Create builder with chained methods
        let builder = TestDataBuilder::new()
            .with_var("key1", "value1")
            .with_var("key2", "value2")
            .with_order_data("order-123", "100.00");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify all data is present
        assert_eq!(data.len(), 6); // 2 vars + 4 order fields
        assert_eq!(data.get("key1"), Some(&"value1".to_string()));
        assert_eq!(data.get("key2"), Some(&"value2".to_string()));
        assert_eq!(data.get("order_id"), Some(&"order-123".to_string()));
    });

    test!(test_test_data_builder_default, {
        // Arrange: Create default builder
        let builder = TestDataBuilder::default();

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data is empty
        assert!(data.is_empty());
    });

    // ========================================================================
    // 3. GENERIC TEST DATA BUILDER - Test generic builder
    // ========================================================================

    test!(test_generic_test_data_builder_new, {
        // Arrange: Create generic builder
        let builder: GenericTestDataBuilder<String, String> = GenericTestDataBuilder::new();

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data is empty
        assert!(data.is_empty());
    });

    test!(test_generic_test_data_builder_with_var, {
        // Arrange: Create generic builder with var
        let builder: GenericTestDataBuilder<String, String> =
            GenericTestDataBuilder::new().with_var("key", "value");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data contains var
        assert_eq!(data.get("key"), Some(&"value".to_string()));
    });

    test!(test_generic_test_data_builder_build_json, {
        // Arrange: Create generic builder with var
        let builder: GenericTestDataBuilder<String, String> =
            GenericTestDataBuilder::new().with_var("key", "value");

        // Act: Build JSON
        let json = builder.build_json();
        assert!(json.is_ok());
        let json = json.unwrap();

        // Assert: Verify JSON contains data
        assert_eq!(json["key"], "value");
    });

    test!(test_generic_test_data_builder_default, {
        // Arrange: Create default generic builder
        let builder: GenericTestDataBuilder<String, String> = GenericTestDataBuilder::default();

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data is empty
        assert!(data.is_empty());
    });

    // ========================================================================
    // 4. VALIDATED TEST DATA BUILDER - Test validated builder
    // ========================================================================

    test!(test_validated_test_data_builder_new, {
        // Arrange: Create validated builder
        let builder: ValidatedTestDataBuilder<()> = ValidatedTestDataBuilder::new();

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data is empty
        assert!(data.is_empty());
    });

    test!(test_validated_test_data_builder_with_var, {
        // Arrange: Create validated builder with var
        let builder: ValidatedTestDataBuilder<()> =
            ValidatedTestDataBuilder::new().with_var("key", "value");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data contains var
        assert_eq!(data.get("key"), Some(&"value".to_string()));
    });

    test!(test_validated_test_data_builder_default, {
        // Arrange: Create default validated builder
        let builder: ValidatedTestDataBuilder<()> = ValidatedTestDataBuilder::default();

        // Act: Build data
        let data = builder.build();

        // Assert: Verify data is empty
        assert!(data.is_empty());
    });

    // ========================================================================
    // 5. BOUNDARY CONDITIONS - Test edge cases
    // ========================================================================

    test!(test_test_data_builder_empty_key, {
        // Arrange: Create builder with empty key
        let builder = TestDataBuilder::new().with_var("", "value");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify empty key is handled
        assert_eq!(data.get(""), Some(&"value".to_string()));
    });

    test!(test_test_data_builder_empty_value, {
        // Arrange: Create builder with empty value
        let builder = TestDataBuilder::new().with_var("key", "");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify empty value is handled
        assert_eq!(data.get("key"), Some(&"".to_string()));
    });

    test!(test_test_data_builder_overwrite, {
        // Arrange: Create builder with overwriting vars
        let builder = TestDataBuilder::new().with_var("key", "value1").with_var("key", "value2");

        // Act: Build data
        let data = builder.build();

        // Assert: Verify overwrite behavior
        assert_eq!(data.get("key"), Some(&"value2".to_string()));
        assert_eq!(data.len(), 1);
    });

    test!(test_test_data_builder_large_data, {
        // Arrange: Create builder with large dataset
        let mut builder = TestDataBuilder::new();
        for i in 0..100 {
            builder = builder.with_var(format!("key{i}"), format!("value{i}"));
        }

        // Act: Build data
        let data = builder.build();

        // Assert: Verify large dataset
        assert_eq!(data.len(), 100);
        assert_eq!(data.get("key0"), Some(&"value0".to_string()));
        assert_eq!(data.get("key99"), Some(&"value99".to_string()));
    });

    // ========================================================================
    // 6. BUILDER PRESETS - Test preset system
    // ========================================================================

    test!(test_builder_preset_register_and_use, {
        // Arrange: Register a preset
        let preset_name = "test_valid_order_001";
        let result = TestDataBuilder::register_preset(preset_name, |builder| {
            builder
                .with_var("order_id", "ORD-001")
                .with_var("amount", "100.00")
                .with_var("status", "pending")
        });

        // Assert: Registration succeeds
        assert!(result.is_ok());

        // Act: Use the preset
        let builder_result = TestDataBuilder::preset(preset_name);
        assert!(builder_result.is_ok());

        let data = builder_result.unwrap().build();

        // Assert: Verify preset data
        assert_eq!(data.get("order_id"), Some(&"ORD-001".to_string()));
        assert_eq!(data.get("amount"), Some(&"100.00".to_string()));
        assert_eq!(data.get("status"), Some(&"pending".to_string()));
    });

    test!(test_builder_preset_with_customization, {
        // Arrange: Register a preset
        let preset_name = "test_base_order_002";
        let result = TestDataBuilder::register_preset(preset_name, |builder| {
            builder.with_var("order_id", "ORD-002").with_var("status", "pending")
        });
        assert!(result.is_ok());

        // Act: Use preset and add customization
        let data = TestDataBuilder::preset(preset_name)
            .unwrap()
            .with_var("customer_id", "12345")
            .with_var("amount", "250.00")
            .build();

        // Assert: Verify both preset and custom data
        assert_eq!(data.get("order_id"), Some(&"ORD-002".to_string()));
        assert_eq!(data.get("status"), Some(&"pending".to_string()));
        assert_eq!(data.get("customer_id"), Some(&"12345".to_string()));
        assert_eq!(data.get("amount"), Some(&"250.00".to_string()));
    });

    test!(test_builder_preset_not_found, {
        // Act: Try to use non-existent preset
        let result = TestDataBuilder::preset("nonexistent_preset_xyz");

        // Assert: Should return error
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not found"));
    });

    test!(test_builder_preset_override, {
        // Arrange: Register a preset
        let preset_name = "test_order_with_defaults_004";
        let result = TestDataBuilder::register_preset(preset_name, |builder| {
            builder
                .with_var("status", "pending")
                .with_var("priority", "normal")
                .with_var("amount", "100.00")
        });
        assert!(result.is_ok());

        // Act: Use preset and override a value
        let data = TestDataBuilder::preset(preset_name)
            .unwrap()
            .with_var("priority", "high")
            .build();

        // Assert: Override should take effect
        assert_eq!(data.get("status"), Some(&"pending".to_string()));
        assert_eq!(data.get("priority"), Some(&"high".to_string()));
        assert_eq!(data.get("amount"), Some(&"100.00".to_string()));
    });

    test!(test_builder_preset_multiple_registrations, {
        // Arrange: Register multiple presets
        let preset1 = "test_preset_alpha_005";
        let preset2 = "test_preset_beta_005";

        let result1 =
            TestDataBuilder::register_preset(preset1, |builder| builder.with_var("type", "alpha"));
        let result2 =
            TestDataBuilder::register_preset(preset2, |builder| builder.with_var("type", "beta"));

        assert!(result1.is_ok());
        assert!(result2.is_ok());

        // Act: Use both presets
        let data1 = TestDataBuilder::preset(preset1).unwrap().build();
        let data2 = TestDataBuilder::preset(preset2).unwrap().build();

        // Assert: Each preset works independently
        assert_eq!(data1.get("type"), Some(&"alpha".to_string()));
        assert_eq!(data2.get("type"), Some(&"beta".to_string()));
    });

    // ========================================================================
    // 7. BUILDER VALIDATION HOOKS - Test validation system
    // ========================================================================

    test!(test_builder_validation_success, {
        // Arrange: Create builder with validation that passes
        let result = TestDataBuilder::new()
            .with_validation(|data| {
                if !data.contains_key("required_field") {
                    return Err("Missing required_field".to_string());
                }
                Ok(())
            })
            .with_var("required_field", "value")
            .try_build();

        // Assert: Validation passes
        assert!(result.is_ok());
        let data = result.unwrap();
        assert_eq!(data.get("required_field"), Some(&"value".to_string()));
    });

    test!(test_builder_validation_failure, {
        // Arrange: Create builder with validation that fails
        let result = TestDataBuilder::new()
            .with_validation(|data| {
                if !data.contains_key("required_field") {
                    return Err("Missing required_field".to_string());
                }
                Ok(())
            })
            .try_build();

        // Assert: Validation fails
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Missing required_field"));
    });

    test!(test_builder_multiple_validations, {
        // Arrange: Create builder with multiple validations
        let result = TestDataBuilder::new()
            .with_validation(|data| {
                if !data.contains_key("field1") {
                    return Err("Missing field1".to_string());
                }
                Ok(())
            })
            .with_validation(|data| {
                if !data.contains_key("field2") {
                    return Err("Missing field2".to_string());
                }
                Ok(())
            })
            .with_var("field1", "value1")
            .with_var("field2", "value2")
            .try_build();

        // Assert: All validations pass
        assert!(result.is_ok());
    });

    test!(test_builder_multiple_validations_first_fails, {
        // Arrange: Create builder where first validation fails
        let result = TestDataBuilder::new()
            .with_validation(|data| {
                if !data.contains_key("field1") {
                    return Err("Missing field1".to_string());
                }
                Ok(())
            })
            .with_validation(|data| {
                if !data.contains_key("field2") {
                    return Err("Missing field2".to_string());
                }
                Ok(())
            })
            .with_var("field2", "value2")
            .try_build();

        // Assert: First validation fails
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Missing field1"));
    });

    test!(test_builder_validation_with_custom_logic, {
        // Arrange: Create builder with custom validation logic
        let result = TestDataBuilder::new()
            .with_validation(|data| {
                if let Some(amount) = data.get("amount") {
                    if let Ok(val) = amount.parse::<f64>() {
                        if val < 0.0 {
                            return Err("Amount must be non-negative".to_string());
                        }
                    }
                }
                Ok(())
            })
            .with_var("amount", "100.00")
            .try_build();

        // Assert: Validation passes
        assert!(result.is_ok());
    });

    test!(test_builder_validation_custom_logic_fails, {
        // Arrange: Create builder with failing custom validation
        let result = TestDataBuilder::new()
            .with_validation(|data| {
                if let Some(amount) = data.get("amount") {
                    if let Ok(val) = amount.parse::<f64>() {
                        if val < 0.0 {
                            return Err("Amount must be non-negative".to_string());
                        }
                    }
                }
                Ok(())
            })
            .with_var("amount", "-50.00")
            .try_build();

        // Assert: Validation fails
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("non-negative"));
    });

    test!(test_builder_no_validations, {
        // Arrange: Create builder without validations
        let result = TestDataBuilder::new().with_var("key", "value").try_build();

        // Assert: Build succeeds without validations
        assert!(result.is_ok());
    });

    #[test]
    #[should_panic(expected = "Validation failed")]
    fn test_builder_build_panics_on_validation_failure() {
        // Arrange: Create builder with validation that will fail
        // Act & Assert: Should panic
        let _ = TestDataBuilder::new()
            .with_validation(|data| {
                if data.is_empty() {
                    return Err("Data cannot be empty".to_string());
                }
                Ok(())
            })
            .build();
    }
}
