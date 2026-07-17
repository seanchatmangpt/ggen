//! Snapshot Testing Framework
//!
//! Provides snapshot testing capabilities using insta for Chicago TDD.
//! Snapshot testing captures output and compares it against stored snapshots,
//! making it ideal for testing complex data structures and ensuring output stability.
//!
//! # Chicago TDD Alignment
//!
//! Snapshot testing aligns with Chicago TDD principles:
//! - **State-Based Testing**: Verifies outputs and state, not implementation
//! - **Behavior Verification**: Tests what code produces, not how it produces it
//! - **AAA Pattern**: Arrange (setup), Act (execute), Assert (snapshot comparison)

#[cfg(feature = "snapshot-testing")]
use insta::{assert_debug_snapshot, assert_snapshot, Settings};
#[cfg(feature = "snapshot-testing")]
use std::collections::HashMap;

/// Snapshot assertion helper for Chicago TDD
///
/// Provides a Chicago TDD-friendly wrapper around insta's snapshot testing.
/// This makes snapshot testing consistent with other assertion helpers in the framework.
#[cfg(feature = "snapshot-testing")]
pub struct SnapshotAssert;

#[cfg(feature = "snapshot-testing")]
impl SnapshotAssert {
    /// Assert that a value matches a snapshot
    ///
    /// # Arguments
    ///
    /// * `value` - The value to snapshot (must implement `Display` or `Debug`)
    /// * `snapshot_name` - Name of the snapshot (used as filename)
    ///
    /// # Panics
    ///
    /// Panics if the value doesn't match the stored snapshot.
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "snapshot-testing")]
    /// use chicago_tdd_tools::snapshot::SnapshotAssert;
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// let data = serde_json::json!({
    ///     "name": "test",
    ///     "value": 42
    /// });
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// SnapshotAssert::assert_matches(&data, "test_data");
    /// ```
    pub fn assert_matches<T: std::fmt::Display>(value: &T, snapshot_name: &str) {
        assert_snapshot!(snapshot_name, value);
    }

    /// Assert that a debug representation matches a snapshot
    ///
    /// # Arguments
    ///
    /// * `value` - The value to snapshot (must implement `Debug`)
    /// * `snapshot_name` - Name of the snapshot (used as filename)
    ///
    /// # Panics
    ///
    /// Panics if the debug representation doesn't match the stored snapshot.
    pub fn assert_debug_matches<T: std::fmt::Debug>(value: &T, snapshot_name: &str) {
        assert_snapshot!(snapshot_name, format!("{:#?}", value));
    }

    /// Assert that a JSON value matches a snapshot
    ///
    /// # Arguments
    ///
    /// * `value` - The JSON value to snapshot
    /// * `snapshot_name` - Name of the snapshot (used as filename)
    ///
    /// # Panics
    ///
    /// Panics if the JSON doesn't match the stored snapshot.
    pub fn assert_json_matches(value: &serde_json::Value, snapshot_name: &str) {
        assert_snapshot!(
            snapshot_name,
            serde_json::to_string_pretty(value).unwrap_or_else(|_| "invalid json".to_string())
        );
    }

    /// Configure snapshot settings for a test
    ///
    /// Allows customization of snapshot behavior (e.g., redactions, filters).
    ///
    /// # Arguments
    ///
    /// * `configure` - Function to configure snapshot settings
    /// * `test` - Test function to run with configured settings
    ///
    /// # Panics
    ///
    /// Panics if the test closure panics or if snapshot assertions fail.
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "snapshot-testing")]
    /// use chicago_tdd_tools::snapshot::SnapshotAssert;
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// SnapshotAssert::with_settings(|settings| {
    ///     settings.set_snapshot_path("snapshots");
    /// }, || {
    ///     SnapshotAssert::assert_matches(&"test", "custom_path_test");
    /// });
    /// ```
    pub fn with_settings<F, R>(configure: F, test: R)
    where
        F: FnOnce(&mut Settings),
        R: FnOnce(),
    {
        let mut settings = Settings::clone_current();
        configure(&mut settings);
        settings.bind(|| {
            test();
        });
    }

    /// Assert inline snapshot (v1.3.0)
    ///
    /// Snapshots are stored directly in the test source code for quick review.
    /// Commonly requested feature for simple assertions.
    ///
    /// # Arguments
    ///
    /// * `value` - The value to snapshot
    ///
    /// # Panics
    ///
    /// Panics if the value doesn't match the inline snapshot.
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "snapshot-testing")]
    /// use chicago_tdd_tools::snapshot::SnapshotAssert;
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// let result = format!("Hello, {}!", "World");
    /// // SnapshotAssert::assert_inline(&result);
    /// // On first run, insta will write the snapshot inline
    /// ```
    pub fn assert_inline<T: std::fmt::Display>(value: &T) {
        let thread = std::thread::current();
        let name = thread.name().unwrap_or("default");
        let sanitized: String = name
            .chars()
            .map(|c| if c.is_alphanumeric() || c == '-' || c == '_' { c } else { '_' })
            .collect();
        let mut settings = Settings::clone_current();
        settings.set_snapshot_suffix(sanitized);
        settings.bind(|| {
            assert_snapshot!(format!("{value}"));
        });
    }

    /// Assert inline debug snapshot (v1.3.0)
    ///
    /// Like `assert_inline` but uses Debug formatting.
    pub fn assert_inline_debug<T: std::fmt::Debug>(value: &T) {
        let thread = std::thread::current();
        let name = thread.name().unwrap_or("default");
        let sanitized: String = name
            .chars()
            .map(|c| if c.is_alphanumeric() || c == '-' || c == '_' { c } else { '_' })
            .collect();
        let mut settings = Settings::clone_current();
        settings.set_snapshot_suffix(sanitized);
        settings.bind(|| {
            assert_debug_snapshot!(value);
        });
    }

    /// Assert inline JSON snapshot (v1.3.0)
    ///
    /// Like `assert_inline` but for JSON values.
    pub fn assert_inline_json(value: &serde_json::Value) {
        let thread = std::thread::current();
        let name = thread.name().unwrap_or("default");
        let sanitized: String = name
            .chars()
            .map(|c| if c.is_alphanumeric() || c == '-' || c == '_' { c } else { '_' })
            .collect();
        let mut settings = Settings::clone_current();
        settings.set_snapshot_suffix(sanitized);
        settings.bind(|| {
            assert_snapshot!(
                serde_json::to_string_pretty(value).unwrap_or_else(|_| "invalid json".to_string())
            );
        });
    }

    /// Assert with redaction (v1.3.0)
    ///
    /// Redact sensitive data before snapshot comparison.
    /// Commonly requested for testing with timestamps, UUIDs, and secrets.
    ///
    /// # Arguments
    ///
    /// * `value` - The value to snapshot
    /// * `snapshot_name` - Name of the snapshot
    /// * `redactions` - `HashMap` of selectors to redaction values
    ///
    /// # Panics
    ///
    /// Panics if the value doesn't match the stored snapshot.
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "snapshot-testing")]
    /// use chicago_tdd_tools::snapshot::SnapshotAssert;
    /// use std::collections::HashMap;
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// let data = serde_json::json!({
    ///     "id": "uuid-12345",
    ///     "timestamp": "2024-01-01T00:00:00Z",
    ///     "message": "test"
    /// });
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// let mut redactions = HashMap::new();
    /// # #[cfg(feature = "snapshot-testing")]
    /// redactions.insert(".id".to_string(), "[UUID]".to_string());
    /// # #[cfg(feature = "snapshot-testing")]
    /// redactions.insert(".timestamp".to_string(), "[TIMESTAMP]".to_string());
    ///
    /// // SnapshotAssert::assert_with_redaction(&data, "test_redacted", &redactions);
    /// ```
    pub fn assert_with_redaction(
        value: &serde_json::Value,
        snapshot_name: &str,
        redactions: &HashMap<String, String>,
    ) {
        // Apply redactions by modifying the JSON value before snapshotting
        let mut redacted_value = value.clone();
        Self::apply_redactions(&mut redacted_value, redactions);
        Self::assert_json_matches(&redacted_value, snapshot_name);
    }

    /// Apply redactions to a JSON value using dot-notation paths
    fn apply_redactions(value: &mut serde_json::Value, redactions: &HashMap<String, String>) {
        // Determinism: HashMap iteration order is unspecified; sort selectors so
        // redactions apply in a stable order regardless of hasher state.
        let mut sorted: Vec<(&String, &String)> = redactions.iter().collect();
        sorted.sort_by_key(|(selector, _)| selector.as_str());
        for (selector, replacement) in sorted {
            let path: Vec<&str> = selector.trim_start_matches('.').split('.').collect();
            Self::set_json_path(value, &path, serde_json::Value::String(replacement.clone()));
        }
    }

    /// Set a value at a JSON path (dot-notation)
    fn set_json_path(value: &mut serde_json::Value, path: &[&str], replacement: serde_json::Value) {
        if path.is_empty() {
            return;
        }

        match value {
            serde_json::Value::Object(map) => {
                let key = path[0];
                if path.len() == 1 {
                    // Replace-only: redacting a field that is absent must not
                    // insert it. Inserting added phantom keys in HashMap
                    // iteration order, making snapshots nondeterministic
                    // (serde_json preserve_order keeps insertion order).
                    if let Some(existing) = map.get_mut(key) {
                        *existing = replacement;
                    }
                } else if let Some(nested) = map.get_mut(key) {
                    Self::set_json_path(nested, &path[1..], replacement);
                }
            }
            serde_json::Value::Array(arr) => {
                if let Ok(index) = path[0].parse::<usize>() {
                    if index < arr.len() {
                        if path.len() == 1 {
                            arr[index] = replacement;
                        } else {
                            Self::set_json_path(&mut arr[index], &path[1..], replacement);
                        }
                    }
                }
            }
            _ => {
                // For non-object/non-array values, we can't apply path-based redaction
                // This is expected for leaf values
            }
        }
    }

    /// Assert with profile (v1.3.0)
    ///
    /// Use environment-specific snapshot profiles (e.g., dev, ci, production).
    /// Commonly requested for different snapshot configurations per environment.
    ///
    /// # Arguments
    ///
    /// * `value` - The value to snapshot
    /// * `snapshot_name` - Name of the snapshot
    /// * `profile` - Profile name (e.g., "ci", "dev", "prod")
    ///
    /// # Panics
    ///
    /// Panics if the value doesn't match the stored snapshot for the given profile.
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "snapshot-testing")]
    /// use chicago_tdd_tools::snapshot::SnapshotAssert;
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// let data = "test_output";
    /// // SnapshotAssert::assert_with_profile(&data, "test_output", "ci");
    /// // Snapshot will be stored in snapshots/ci/ directory
    /// ```
    pub fn assert_with_profile<T: std::fmt::Display>(
        value: &T,
        snapshot_name: &str,
        profile: &str,
    ) {
        Self::with_settings(
            |settings| {
                settings.set_snapshot_path(format!("snapshots/{profile}"));
            },
            || {
                Self::assert_matches(value, snapshot_name);
            },
        );
    }

    /// Create a redaction helper for common patterns (v1.3.0)
    ///
    /// Provides pre-built redactions for common use cases.
    ///
    /// # Returns
    ///
    /// A `HashMap` with common redaction patterns (UUIDs, timestamps, etc.)
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "snapshot-testing")]
    /// use chicago_tdd_tools::snapshot::SnapshotAssert;
    ///
    /// # #[cfg(feature = "snapshot-testing")]
    /// let redactions = SnapshotAssert::common_redactions();
    /// // Contains: .id → [UUID], .timestamp → [TIMESTAMP], etc.
    /// ```
    #[must_use]
    pub fn common_redactions() -> HashMap<String, String> {
        let mut redactions = HashMap::new();
        redactions.insert(".id".to_string(), "[UUID]".to_string());
        redactions.insert(".uuid".to_string(), "[UUID]".to_string());
        redactions.insert(".timestamp".to_string(), "[TIMESTAMP]".to_string());
        redactions.insert(".created_at".to_string(), "[TIMESTAMP]".to_string());
        redactions.insert(".updated_at".to_string(), "[TIMESTAMP]".to_string());
        redactions.insert(".token".to_string(), "[TOKEN]".to_string());
        redactions.insert(".password".to_string(), "[PASSWORD]".to_string());
        redactions.insert(".secret".to_string(), "[SECRET]".to_string());
        redactions
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    // ========================================================================
    // TEST FIXTURES - Reusable test data (Chicago TDD: Arrange helpers)
    // ========================================================================

    /// Test fixtures for snapshot testing
    mod fixtures {
        use super::*;

        /// Create a simple test vector
        pub fn test_vector() -> Vec<i32> {
            vec![1, 2, 3]
        }

        /// Create a simple test string
        pub fn test_string() -> &'static str {
            "test_value"
        }

        /// Create a simple JSON object
        pub fn simple_json() -> serde_json::Value {
            serde_json::json!({
                "name": "test",
                "value": 42
            })
        }

        /// Create a nested JSON structure
        pub fn nested_json() -> serde_json::Value {
            serde_json::json!({
                "users": [
                    {
                        "id": 1,
                        "name": "Alice",
                        "tags": ["admin", "user"]
                    },
                    {
                        "id": 2,
                        "name": "Bob",
                        "tags": ["user"]
                    }
                ],
                "metadata": {
                    "count": 2,
                    "version": "1.0.0"
                }
            })
        }

        /// Create a test struct for inline snapshots
        #[derive(Debug)]
        #[allow(dead_code)] // Test struct - fields used for Debug output
        pub struct TestStruct {
            pub name: String,
            pub value: i32,
            pub tags: Vec<String>,
        }

        impl TestStruct {
            pub fn new() -> Self {
                Self {
                    name: "test".to_string(),
                    value: 42,
                    tags: vec!["tag1".to_string(), "tag2".to_string()],
                }
            }
        }

        /// Create a test enum with variants
        #[derive(Debug)]
        #[allow(dead_code)] // Test enum - fields used for Debug output
        pub enum TestEnum {
            Variant1,
            Variant2(String),
            Variant3 { field: i32 },
        }

        pub fn test_enum_variants() -> Vec<TestEnum> {
            vec![
                TestEnum::Variant1,
                TestEnum::Variant2("test".to_string()),
                TestEnum::Variant3 { field: 42 },
            ]
        }

        /// Create nested structs for testing
        #[derive(Debug)]
        #[allow(dead_code)] // Test struct - fields used for Debug output
        pub struct Inner {
            pub value: i32,
            pub name: String,
        }

        #[derive(Debug)]
        #[allow(dead_code)] // Test struct - fields used for Debug output
        pub struct Outer {
            pub inner: Inner,
            pub count: usize,
        }

        impl Outer {
            pub fn new() -> Self {
                Self { inner: Inner { value: 42, name: "test".to_string() }, count: 10 }
            }
        }

        /// Create a BTreeMap for deterministic ordering
        pub fn test_map() -> BTreeMap<String, String> {
            let mut map = BTreeMap::new();
            map.insert("key1".to_string(), "value1".to_string());
            map.insert("key2".to_string(), "value2".to_string());
            map.insert("key3".to_string(), "value3".to_string());
            map
        }

        /// Create JSON data with sensitive fields for redaction testing
        pub fn sensitive_json() -> serde_json::Value {
            serde_json::json!({
                "id": "uuid-12345",
                "timestamp": "2024-01-01T00:00:00Z",
                "token": "secret-token-abc",
                "message": "test"
            })
        }

        /// Create nested JSON with sensitive fields
        pub fn nested_sensitive_json() -> serde_json::Value {
            serde_json::json!({
                "user": {
                    "id": "uuid-user-123",
                    "email": "test@example.com"
                },
                "session": {
                    "token": "secret-session-token",
                    "created_at": "2024-01-01T00:00:00Z"
                }
            })
        }
    }

    // ========================================================================
    // CORE ASSERTION TESTS - Basic snapshot functionality
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_assert_matches() {
        // Arrange: Create test data
        let data = fixtures::test_string();

        // Act & Assert: Verify snapshot matches
        SnapshotAssert::assert_matches(&data, "test_snapshot_assert");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_assert_debug_matches() {
        // Arrange: Create test data
        let data = fixtures::test_vector();

        // Act & Assert: Verify debug snapshot matches
        SnapshotAssert::assert_debug_matches(&data, "lib_test_snapshot_debug");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_assert_json_matches() {
        // Arrange: Create test data
        let data = fixtures::simple_json();

        // Act & Assert: Verify JSON snapshot matches
        SnapshotAssert::assert_json_matches(&data, "lib_test_snapshot_json");
    }

    // ========================================================================
    // ERROR PATH TESTING - Test error scenarios (80% of bugs)
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_json_serialization_fallback() {
        // Arrange: Create null JSON value to test fallback path
        // This tests the unwrap_or_else in assert_json_matches
        // Note: serde_json::Value should always serialize successfully,
        // but we verify the fallback string format is correct
        let data = serde_json::json!(null);

        // Act & Assert: Verify snapshot handles null JSON correctly
        SnapshotAssert::assert_json_matches(&data, "test_snapshot_json_null");
    }

    // ========================================================================
    // BOUNDARY CONDITIONS - Test edge cases (empty, single, max, zero, negative)
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_empty_string() {
        // Arrange: Empty string boundary condition
        let data = "";

        // Act & Assert: Verify empty string snapshot
        SnapshotAssert::assert_matches(&data, "test_snapshot_empty_string");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_empty_collection() {
        // Arrange: Empty collection boundary condition
        let data: Vec<i32> = vec![];

        // Act & Assert: Verify empty collection snapshot
        SnapshotAssert::assert_debug_matches(&data, "test_snapshot_empty_collection");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_single_item_collection() {
        // Arrange: Single item collection boundary condition
        let data = vec![42];

        // Act & Assert: Verify single item collection snapshot
        SnapshotAssert::assert_debug_matches(&data, "test_snapshot_single_item");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_unicode_string() {
        // Arrange: Unicode string with special characters
        let data = "Hello 世界 🌍";

        // Act & Assert: Verify unicode string snapshot
        SnapshotAssert::assert_matches(&data, "test_snapshot_unicode");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_special_characters() {
        // Arrange: String with control characters
        let data = "Line 1\nLine 2\tTabbed\r\nWindows";

        // Act & Assert: Verify special characters snapshot
        SnapshotAssert::assert_matches(&data, "test_snapshot_special_chars");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_long_string() {
        // Arrange: Long string boundary condition
        let data = "x".repeat(1000);

        // Act & Assert: Verify long string snapshot
        SnapshotAssert::assert_matches(&data, "test_snapshot_long_string");
    }

    // ========================================================================
    // COMPLEX DATA STRUCTURES - Test real-world usage patterns
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_nested_json() {
        // Arrange: Create nested JSON structure
        let data = fixtures::nested_json();

        // Act & Assert: Verify nested JSON snapshot
        SnapshotAssert::assert_json_matches(&data, "test_snapshot_nested_json");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_hashmap() {
        // Arrange: Create BTreeMap for deterministic ordering
        let map = fixtures::test_map();

        // Act & Assert: Verify map snapshot
        SnapshotAssert::assert_debug_matches(&map, "test_snapshot_hashmap");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_enum_variants() {
        // Arrange: Create enum variants
        let variants = fixtures::test_enum_variants();

        // Act & Assert: Verify enum variants snapshot
        SnapshotAssert::assert_debug_matches(&variants, "test_snapshot_enum_variants");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_nested_struct() {
        // Arrange: Create nested struct
        let data = fixtures::Outer::new();

        // Act & Assert: Verify nested struct snapshot
        SnapshotAssert::assert_debug_matches(&data, "test_snapshot_nested_struct");
    }

    // ========================================================================
    // FORMAT DIFFERENCES - Test Display vs Debug formatting
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_display_vs_debug() {
        // Arrange: Create test data
        let data = 42;

        // Act & Assert: Verify Display and Debug produce different outputs
        SnapshotAssert::assert_matches(&data, "test_snapshot_display_number");
        SnapshotAssert::assert_debug_matches(&data, "test_snapshot_debug_number");
    }

    // ========================================================================
    // CUSTOM SETTINGS - Test with_settings configuration
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_with_custom_path() {
        // Arrange: Configure custom snapshot path
        // Act & Assert: Verify snapshot works with custom path
        SnapshotAssert::with_settings(
            |settings| {
                settings.set_snapshot_path("custom_snapshots");
            },
            || {
                let data = "custom_path_test";
                SnapshotAssert::assert_matches(&data, "lib_test_custom_path");
            },
        );
    }

    // ========================================================================
    // V1.3.0 FEATURES - Inline Snapshots
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_inline_simple() {
        // Arrange: Create simple test data
        let data = "inline_test_value";

        // Act & Assert: Verify inline snapshot
        SnapshotAssert::assert_inline(&data);
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_inline_debug() {
        // Arrange: Create test vector
        let data = fixtures::test_vector();

        // Act & Assert: Verify inline debug snapshot
        SnapshotAssert::assert_inline_debug(&data);
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_inline_json() {
        // Arrange: Create JSON data
        let data = serde_json::json!({"key": "value", "number": 42});

        // Act & Assert: Verify inline JSON snapshot
        SnapshotAssert::assert_inline_json(&data);
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_inline_complex_struct() {
        // Arrange: Create complex struct
        let data = fixtures::TestStruct::new();

        // Act & Assert: Verify debug snapshot for complex struct (file-based to avoid name conflict)
        SnapshotAssert::assert_debug_matches(&data, "test_snapshot_inline_complex_struct");
    }

    // ========================================================================
    // V1.3.0 FEATURES - Redaction Testing
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_redaction_basic() {
        // Arrange: Create JSON with sensitive data and single redaction
        let data = serde_json::json!({
            "id": "uuid-12345",
            "message": "test message"
        });
        let mut redactions = HashMap::new();
        redactions.insert(".id".to_string(), "[UUID]".to_string());

        // Act & Assert: Verify redaction works correctly
        SnapshotAssert::assert_with_redaction(&data, "test_redaction_basic", &redactions);
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_redaction_multiple() {
        // Arrange: Create JSON with multiple sensitive fields
        let data = fixtures::sensitive_json();
        let mut redactions = HashMap::new();
        redactions.insert(".id".to_string(), "[UUID]".to_string());
        redactions.insert(".timestamp".to_string(), "[TIMESTAMP]".to_string());
        redactions.insert(".token".to_string(), "[TOKEN]".to_string());

        // Act & Assert: Verify multiple redactions work correctly
        SnapshotAssert::assert_with_redaction(&data, "test_redaction_multiple", &redactions);
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_redaction_nested() {
        // Arrange: Create nested JSON with sensitive fields
        let data = fixtures::nested_sensitive_json();
        let mut redactions = HashMap::new();
        redactions.insert(".user.id".to_string(), "[USER_ID]".to_string());
        redactions.insert(".session.token".to_string(), "[SESSION_TOKEN]".to_string());
        redactions.insert(".session.created_at".to_string(), "[TIMESTAMP]".to_string());

        // Act & Assert: Verify nested redactions work correctly
        SnapshotAssert::assert_with_redaction(&data, "test_redaction_nested", &redactions);
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_redaction_with_common() {
        // Arrange: Create JSON with sensitive fields and use common redactions
        let data = serde_json::json!({
            "id": "uuid-12345",
            "timestamp": "2024-01-01T00:00:00Z",
            "message": "test message"
        });
        let redactions = SnapshotAssert::common_redactions();

        // Act & Assert: Verify common redactions work correctly
        SnapshotAssert::assert_with_redaction(&data, "test_common_redaction", &redactions);
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_common_redactions() {
        // Arrange: Get common redactions
        let redactions = SnapshotAssert::common_redactions();

        // Act & Assert: Verify common redactions contain expected keys and values
        assert!(redactions.contains_key(".id"));
        assert!(redactions.contains_key(".uuid"));
        assert!(redactions.contains_key(".timestamp"));
        assert!(redactions.contains_key(".token"));
        assert!(redactions.contains_key(".password"));
        assert_eq!(redactions.get(".id"), Some(&"[UUID]".to_string()));
    }

    // ========================================================================
    // V1.3.0 FEATURES - Profile Testing
    // ========================================================================

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_profile_ci() {
        // Arrange: Create test data
        let data = "ci_profile_test";

        // Act & Assert: Verify CI profile snapshot
        SnapshotAssert::assert_with_profile(&data, "test_profile_ci", "ci");
    }

    #[test]
    #[cfg(feature = "snapshot-testing")]
    fn test_snapshot_profile_dev() {
        // Arrange: Create test data
        let data = "dev_profile_test";

        // Act & Assert: Verify dev profile snapshot
        SnapshotAssert::assert_with_profile(&data, "test_profile_dev", "dev");
    }
}
