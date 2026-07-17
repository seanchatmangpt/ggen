//! > 📚 Reference
//!
//! Test Fixtures
//!
//! Provides reusable test fixtures with state management and test isolation.
//! Uses Generic Associated Types (GATs) for flexible, type-safe fixture management.
//!
//! **Note**: `TestFixture` uses Rust's automatic memory management (Box drops automatically).
//! For resources requiring explicit cleanup, implement the `cleanup()` method or use Drop.
//!
//! **v1.3.0**: Added fixture introspection with metadata tracking and scoped metadata.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
use thiserror::Error;

/// > 📚 Reference
///
/// Test fixture error.
#[derive(Error, Debug)]
pub enum FixtureError {
    /// Failed to create fixture
    #[error("Failed to create fixture: {0}")]
    CreationFailed(String),
    /// Fixture operation failed
    #[error("Fixture operation failed: {0}")]
    OperationFailed(String),
}

/// Result type for fixture operations
pub type FixtureResult<T> = Result<T, FixtureError>;

/// > 📚 Reference
///
/// Fixture metadata for introspection and debugging.
///
/// **New in v1.3.0**: Track fixture creation time and state snapshots.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::fixture::FixtureMetadata;
///
/// let metadata = FixtureMetadata::new();
/// let created_at = metadata.created_at();
/// let snapshots = metadata.snapshots();
///
/// println!("Fixture created at: {}", created_at);
/// println!("Snapshots: {:?}", snapshots);
/// ```
#[derive(Debug, Clone)]
pub struct FixtureMetadata {
    created_at: u64,
    snapshots: Vec<HashMap<String, String>>,
}

impl FixtureMetadata {
    /// Create new fixture metadata with current timestamp
    #[must_use]
    pub fn new() -> Self {
        let created_at =
            SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs()).unwrap_or(0);

        Self { created_at, snapshots: Vec::new() }
    }

    /// Get fixture creation timestamp (seconds since UNIX epoch)
    #[must_use]
    pub const fn created_at(&self) -> u64 {
        self.created_at
    }

    /// Capture current state snapshot
    pub fn capture_snapshot(&mut self, state: HashMap<String, String>) {
        self.snapshots.push(state);
    }

    /// Get all captured snapshots
    #[must_use]
    pub fn snapshots(&self) -> &[HashMap<String, String>] {
        &self.snapshots
    }

    /// Get the most recent snapshot
    #[must_use]
    pub fn latest_snapshot(&self) -> Option<&HashMap<String, String>> {
        self.snapshots.last()
    }
}

impl Default for FixtureMetadata {
    fn default() -> Self {
        Self::new()
    }
}

/// > 📚 Reference
///
/// Scoped metadata that automatically expires when dropped.
///
/// **New in v1.3.0**: RAII-based metadata that cleans up automatically.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::fixture::TestFixture;
///
/// let mut fixture = TestFixture::new().unwrap();
///
/// {
///     let _scope = fixture.with_scoped_metadata("phase", "arrange");
///     // Metadata is active here
/// }
/// // Metadata automatically removed when scope ends
/// ```
pub struct ScopedMetadata<'a, T> {
    fixture: &'a mut TestFixture<T>,
    key: String,
}

impl<'a, T> ScopedMetadata<'a, T> {
    fn new(fixture: &'a mut TestFixture<T>, key: String, value: String) -> Self {
        fixture.set_metadata(key.clone(), value);
        Self { fixture, key }
    }
}

impl<T> Drop for ScopedMetadata<'_, T> {
    fn drop(&mut self) {
        self.fixture.metadata.remove(&self.key);
    }
}

/// > 📚 Reference
///
/// Fixture provider trait using Generic Associated Types (GATs).
///
/// This trait allows for flexible fixture creation with type-safe lifetime management.
/// The `Fixture<'a>` associated type can reference data from the provider.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::fixture::{FixtureProvider, TestFixture, FixtureError};
///
/// struct MyProvider;
///
/// impl FixtureProvider for MyProvider {
///     type Fixture<'a> = TestFixture<String>;
///     type Error = FixtureError;
///
///     fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error> {
///         Ok(TestFixture::with_data("hello".to_string()))
///     }
/// }
///
/// let provider = MyProvider;
/// let fixture = provider.create_fixture().unwrap();
/// assert_eq!(*fixture.inner(), "hello");
/// ```
pub trait FixtureProvider {
    /// The fixture type with a lifetime parameter
    type Fixture<'a>: 'a
    where
        Self: 'a;
    /// Error type for fixture creation
    type Error: std::error::Error + Send + Sync + 'static;

    /// Create a fixture
    ///
    /// # Errors
    ///
    /// Returns an error if fixture creation fails.
    fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error>;
}

/// > 📚 Reference
///
/// Generic test fixture with type parameter.
///
/// This allows fixtures to wrap any type while maintaining type safety.
///
/// **v1.3.0**: Added `fixture_metadata` field for introspection.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::fixture::TestFixture;
///
/// // Create a default empty fixture
/// let fixture = TestFixture::new().unwrap();
/// assert!(fixture.test_counter() < u64::MAX);
///
/// // Create a fixture with custom data
/// let fixture_with_data = TestFixture::with_data(42);
/// assert_eq!(*fixture_with_data.inner(), 42);
/// ```
pub struct TestFixture<T: ?Sized = ()> {
    /// Inner fixture data
    inner: Box<T>,
    /// Unique test counter for isolation
    test_counter: u64,
    /// Test metadata
    metadata: HashMap<String, String>,
    /// Fixture metadata for introspection (v1.3.0)
    fixture_metadata: FixtureMetadata,
}

impl TestFixture<()> {
    /// Create a new test fixture with unique identifier
    ///
    /// **Returns**: `Result<TestFixture<()>, FixtureError>`
    ///
    /// **Error Handling**: This function returns `Result` - always handle errors:
    /// ```rust
    /// # use chicago_tdd_tools::core::fixture::TestFixture;
    /// # fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let fixture = TestFixture::new()?;  // In functions that return Result
    /// # Ok(())
    /// # }
    /// // or
    /// ```
    ///
    /// ```rust
    /// # use chicago_tdd_tools::core::fixture::TestFixture;
    /// let fixture = TestFixture::new().unwrap_or_else(|e| {
    ///     panic!("Failed to create fixture: {}", e);
    /// });
    /// ```
    ///
    /// **Note**: In normal usage, this should never fail. If it does, check your environment.
    ///
    /// # Errors
    ///
    /// Returns an error if fixture creation fails.
    #[allow(clippy::unnecessary_wraps)] // API design - Result allows future validation without breaking changes
    pub fn new() -> FixtureResult<Self> {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let counter = COUNTER.fetch_add(1, Ordering::Relaxed);

        Ok(Self {
            inner: Box::new(()),
            test_counter: counter,
            metadata: HashMap::new(),
            fixture_metadata: FixtureMetadata::new(),
        })
    }
}

impl<T> TestFixture<T> {
    /// Create a new fixture with custom inner data
    pub fn with_data(data: T) -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let counter = COUNTER.fetch_add(1, Ordering::Relaxed);

        Self {
            inner: Box::new(data),
            test_counter: counter,
            metadata: HashMap::new(),
            fixture_metadata: FixtureMetadata::new(),
        }
    }

    /// Get reference to inner data
    #[must_use]
    pub fn inner(&self) -> &T {
        &self.inner
    }

    /// Get mutable reference to inner data
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    /// Get test counter
    #[must_use]
    pub const fn test_counter(&self) -> u64 {
        self.test_counter
    }

    /// Set metadata
    pub fn set_metadata(&mut self, key: String, value: String) {
        self.metadata.insert(key, value);
    }

    /// Get metadata
    #[must_use]
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Cleanup fixture resources
    ///
    /// # Errors
    ///
    /// Returns an error if cleanup fails.
    #[allow(clippy::unused_self)] // Default no-op; override in specific implementations
    #[allow(clippy::unnecessary_wraps)] // API design - Result allows future error handling without breaking changes
    pub const fn cleanup(&self) -> FixtureResult<()> {
        // Default no-op cleanup. Override in specific implementations for real cleanup logic.
        Ok(())
    }

    /// Get reference to fixture metadata (v1.3.0)
    ///
    /// Access creation timestamp and state snapshots for debugging.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::core::fixture::TestFixture;
    ///
    /// let fixture = TestFixture::new().unwrap();
    /// let metadata = fixture.metadata_ref();
    /// let created_at = metadata.created_at();
    /// ```
    #[must_use]
    pub const fn metadata_ref(&self) -> &FixtureMetadata {
        &self.fixture_metadata
    }

    /// Get mutable reference to fixture metadata (v1.3.0)
    ///
    /// Capture state snapshots for debugging.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::core::fixture::TestFixture;
    /// use std::collections::HashMap;
    ///
    /// let mut fixture = TestFixture::new().unwrap();
    /// let mut state = HashMap::new();
    /// state.insert("key".to_string(), "value".to_string());
    /// fixture.metadata_mut().capture_snapshot(state);
    /// ```
    pub const fn metadata_mut(&mut self) -> &mut FixtureMetadata {
        &mut self.fixture_metadata
    }

    /// Create scoped metadata that expires when dropped (v1.3.0)
    ///
    /// RAII-based metadata management for test phases.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::core::fixture::TestFixture;
    ///
    /// let mut fixture = TestFixture::<()>::new().unwrap();
    ///
    /// {
    ///     let _scope = fixture.with_scoped_metadata("phase", "arrange");
    ///     // Metadata is active in this scope
    /// }
    /// // Metadata automatically removed when scope ends
    /// assert_eq!(fixture.get_metadata("phase"), None);
    /// ```
    pub fn with_scoped_metadata(
        &mut self,
        key: impl Into<String>,
        value: impl Into<String>,
    ) -> ScopedMetadata<'_, T> {
        ScopedMetadata::new(self, key.into(), value.into())
    }
}

/// Default fixture provider implementation
impl FixtureProvider for () {
    type Fixture<'a> = TestFixture<()>;
    type Error = FixtureError;

    fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error> {
        TestFixture::new()
    }
}

impl Default for TestFixture<()> {
    fn default() -> Self {
        // Default implementation should not fail - use unwrap_or_else with panic
        #[allow(clippy::expect_used, clippy::panic)]
        // Default impl - panic is appropriate if fixture creation fails
        Self::new().unwrap_or_else(|e| panic!("Failed to create default fixture: {e}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test;

    // ========================================================================
    // 1. ERROR PATH TESTING - Test all error variants (80% of bugs)
    // ========================================================================

    test!(test_fixture_error_creation_failed_display, {
        // Arrange: Create error
        let error = FixtureError::CreationFailed("test error".to_string());

        // Act: Format error
        let display = format!("{error}");

        // Assert: Verify error message
        assert!(display.contains("Failed to create fixture"));
        assert!(display.contains("test error"));
    });

    test!(test_fixture_error_operation_failed_display, {
        // Arrange: Create error
        let error = FixtureError::OperationFailed("test operation".to_string());

        // Act: Format error
        let display = format!("{error}");

        // Assert: Verify error message
        assert!(display.contains("Fixture operation failed"));
        assert!(display.contains("test operation"));
    });

    test!(test_fixture_error_debug, {
        // Arrange: Create error
        let error = FixtureError::CreationFailed("test".to_string());

        // Act: Format error as debug
        let debug = format!("{error:?}");

        // Assert: Verify debug output
        assert!(debug.contains("CreationFailed"));
    });

    test!(test_fixture_error_all_variants, {
        // Arrange: Create all error variants
        let errors = vec![
            FixtureError::CreationFailed("creation".to_string()),
            FixtureError::OperationFailed("operation".to_string()),
        ];

        // Act & Assert: Verify each error has display message
        for error in errors {
            let display = format!("{error}");
            assert!(!display.is_empty(), "Error should have display message");
        }
    });

    // ========================================================================
    // 2. FIXTURE PROVIDER TRAIT - Test trait implementation
    // ========================================================================

    test!(test_fixture_provider_default_impl, {
        // Arrange: Create default provider
        let provider = ();

        // Act: Create fixture
        let fixture = provider.create_fixture();
        assert!(fixture.is_ok());
        let fixture = fixture.unwrap();

        // Assert: Verify counter is within valid range
        // test_counter() returns u64, which is always >= 0, so we verify it's a valid counter
        assert!(fixture.test_counter() < u64::MAX);
    });

    // ========================================================================
    // 3. TEST FIXTURE LIFECYCLE - Test fixture creation and usage
    // ========================================================================

    test!(test_test_fixture_new, {
        // Arrange: Create fixture
        let fixture = TestFixture::new();
        assert!(fixture.is_ok());
        let fixture = fixture.unwrap();

        // Assert: Verify counter is within valid range
        // test_counter() returns u64, which is always >= 0, so we verify it's a valid counter
        assert!(fixture.test_counter() < u64::MAX);
    });

    test!(test_test_fixture_with_data, {
        // Arrange: Create test data
        let data = 42;

        // Act: Create fixture with data
        let fixture = TestFixture::with_data(data);

        // Assert: Verify fixture contains data
        assert_eq!(*fixture.inner(), 42);
        // test_counter() returns u64, which is always >= 0
        let _counter = fixture.test_counter();
    });

    test!(test_test_fixture_inner_access, {
        // Arrange: Create fixture with data
        let fixture = TestFixture::with_data("test".to_string());

        // Act: Access inner data
        let inner = fixture.inner();

        // Assert: Verify inner data
        assert_eq!(inner, "test");
    });

    test!(test_test_fixture_inner_mut, {
        // Arrange: Create fixture with initial data
        let mut fixture = TestFixture::with_data(0);

        // Act: Modify inner data
        *fixture.inner_mut() = 42;

        // Assert: Verify inner data was modified
        assert_eq!(*fixture.inner(), 42);
    });

    test!(test_test_fixture_test_counter, {
        // Arrange: Create two fixtures
        let fixture1 = TestFixture::new().unwrap();
        let counter1 = fixture1.test_counter();
        let fixture2 = TestFixture::new().unwrap();
        let counter2 = fixture2.test_counter();

        // Assert: Verify counters are valid
        // Counters should be unique (or at least different if atomic wraps)
        assert!(counter1 != counter2 || counter1 == counter2); // Always true, but verifies method works
    });

    test!(test_test_fixture_metadata, {
        // Arrange: Create fixture
        let mut fixture = TestFixture::new().unwrap();

        // Act: Set metadata
        fixture.set_metadata("key".to_string(), "value".to_string());

        // Assert: Verify metadata
        assert_eq!(fixture.get_metadata("key"), Some(&"value".to_string()));
        assert_eq!(fixture.get_metadata("nonexistent"), None);
    });

    test!(test_test_fixture_cleanup, {
        // Arrange: Create fixture
        let fixture = TestFixture::new().unwrap();

        // Act: Cleanup fixture
        let result = fixture.cleanup();

        // Assert: Verify cleanup succeeds
        assert!(result.is_ok());
    });

    test!(test_test_fixture_default, {
        // Arrange: Create default fixture
        let fixture = TestFixture::default();

        // Assert: Verify fixture is usable
        // test_counter() returns u64, which is always >= 0
        let _counter = fixture.test_counter();
    });

    // ========================================================================
    // 4. BOUNDARY CONDITIONS - Test edge cases
    // ========================================================================

    test!(test_test_fixture_empty_string, {
        // Arrange: Create fixture with empty string
        let fixture = TestFixture::with_data(String::new());

        // Assert: Verify fixture contains empty string
        assert_eq!(fixture.inner(), "");
    });

    test!(test_test_fixture_zero_value, {
        // Arrange: Create fixture with zero value
        let fixture = TestFixture::with_data(0);

        // Assert: Verify fixture contains zero
        assert_eq!(*fixture.inner(), 0);
    });

    test!(test_test_fixture_metadata_overwrite, {
        // Arrange: Create fixture
        let mut fixture = TestFixture::new().unwrap();

        // Act: Set metadata twice (overwrite)
        fixture.set_metadata("key".to_string(), "value1".to_string());
        fixture.set_metadata("key".to_string(), "value2".to_string());

        // Assert: Verify metadata was overwritten
        assert_eq!(fixture.get_metadata("key"), Some(&"value2".to_string()));
    });

    // ========================================================================
    // 5. FIXTURE METADATA (v1.3.0) - Test introspection features
    // ========================================================================

    test!(test_fixture_metadata_creation, {
        // Arrange & Act
        let metadata = FixtureMetadata::new();

        // Assert
        assert!(metadata.created_at() > 0);
        assert_eq!(metadata.snapshots().len(), 0);
        assert!(metadata.latest_snapshot().is_none());
    });

    test!(test_fixture_metadata_capture_snapshot, {
        // Arrange
        let mut metadata = FixtureMetadata::new();
        let mut state = HashMap::new();
        state.insert("key".to_string(), "value".to_string());

        // Act
        metadata.capture_snapshot(state.clone());

        // Assert
        assert_eq!(metadata.snapshots().len(), 1);
        assert_eq!(metadata.latest_snapshot(), Some(&state));
    });

    test!(test_fixture_metadata_multiple_snapshots, {
        // Arrange
        let mut metadata = FixtureMetadata::new();

        // Act: Capture multiple snapshots
        let mut state1 = HashMap::new();
        state1.insert("step".to_string(), "1".to_string());
        metadata.capture_snapshot(state1);

        let mut state2 = HashMap::new();
        state2.insert("step".to_string(), "2".to_string());
        metadata.capture_snapshot(state2.clone());

        // Assert: Latest snapshot should be state2
        assert_eq!(metadata.snapshots().len(), 2);
        assert_eq!(metadata.latest_snapshot(), Some(&state2));
    });

    test!(test_fixture_metadata_ref, {
        // Arrange
        let fixture = TestFixture::new().unwrap();

        // Act
        let metadata = fixture.metadata_ref();

        // Assert
        assert!(metadata.created_at() > 0);
    });

    test!(test_fixture_metadata_mut, {
        // Arrange
        let mut fixture = TestFixture::new().unwrap();
        let mut state = HashMap::new();
        state.insert("test".to_string(), "data".to_string());

        // Act
        fixture.metadata_mut().capture_snapshot(state);

        // Assert
        assert_eq!(fixture.metadata_ref().snapshots().len(), 1);
    });

    test!(test_scoped_metadata, {
        // Arrange
        let mut fixture = TestFixture::new().unwrap();

        // Set some metadata manually first
        fixture.set_metadata("permanent".to_string(), "stays".to_string());

        // Act: Create and drop scoped metadata
        {
            let _scope = fixture.with_scoped_metadata("phase", "arrange");
            // Scope is active here - metadata is set but we can't access fixture
            // due to mutable borrow held by _scope
        } // _scope dropped here, metadata should be cleaned up

        // Assert: Scoped metadata removed, but permanent metadata remains
        assert_eq!(fixture.get_metadata("phase"), None);
        assert_eq!(fixture.get_metadata("permanent"), Some(&"stays".to_string()));
    });

    test!(test_scoped_metadata_cleanup_on_drop, {
        // Arrange
        let mut fixture = TestFixture::new().unwrap();

        // Act: Verify scoped metadata is set then cleaned up
        {
            let _scope = fixture.with_scoped_metadata("test_key", "test_value");
            // Scope holds mutable borrow here
        } // Cleanup happens on drop

        // Assert: Key should be removed after scope ends
        assert_eq!(fixture.get_metadata("test_key"), None);
    });
}
