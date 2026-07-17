//! Async Fixture Providers
//!
//! Uses async traits (Rust 1.75+) for async fixture creation and management.
//! Provides type-safe async fixture lifecycle management with GATs.
//!
//! # Advanced Rust Features
//!
//! - **Async Traits**: Native async trait support (Rust 1.75+)
//! - **GATs**: Generic Associated Types for flexible lifetime management
//! - **Sealed Traits**: API safety and extensibility control
//!
//! # Note on Guarantees
//!
//! This module provides type-safe async fixture management, not compile-time lifecycle guarantees.
//! The type system ensures correct usage patterns, but fixture lifecycle is managed at runtime.
//!
//! # Requirements
//!
//! - **Rust Version**: 1.75 or later (for async traits support)
//! - **Feature Flag**: Requires `async` feature to be enabled
//!
//! # Example
//!
//! ```rust,ignore
//! #[cfg(feature = "async")]
//! use chicago_tdd_tools::core::async_fixture::{AsyncFixtureManager, AsyncFixtureProvider};
//! use chicago_tdd_tools::core::fixture::FixtureError;
//!
//! #[cfg(feature = "async")]
//! struct DatabaseFixture {
//!     connection: String,
//! }
//!
//! #[cfg(feature = "async")]
//! struct DatabaseProvider;
//!
//! #[cfg(feature = "async")]
//! impl chicago_tdd_tools::core::async_fixture::private::Sealed for DatabaseProvider {}
//!
//! #[cfg(feature = "async")]
//! impl AsyncFixtureProvider for DatabaseProvider {
//!     type Fixture<'a> = DatabaseFixture;
//!     type Error = FixtureError;
//!
//!     async fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error> {
//!         Ok(DatabaseFixture { connection: "connected".to_string() })
//!     }
//! }
//!
//! #[cfg(feature = "async")]
//! #[tokio::test]
//! async fn example() {
//!     let provider = DatabaseProvider;
//!     let manager = AsyncFixtureManager::new(provider);
//!     
//!     // Proper error handling
//!     match manager.setup().await {
//!         Ok(fixture) => println!("Fixture created: {}", fixture.connection),
//!         Err(e) => eprintln!("Failed to create fixture: {}", e),
//!     }
//! }
//! ```

#[cfg(feature = "async")]
use crate::core::fixture::{FixtureError, FixtureResult};
#[cfg(feature = "async")]
use std::future::Future;

/// Sealed trait pattern for API safety
///
/// This trait is sealed (only implementable within this crate) to prevent
/// external implementations that might violate invariants.
mod private {
    /// Sealed marker trait
    /// Note: This trait is used via the sealed trait pattern, not directly
    #[allow(dead_code)]
    pub trait Sealed {}
}

/// Async fixture provider trait using async traits (Rust 1.75+)
///
/// This trait allows for async fixture creation with type-safe lifetime management.
/// Uses Generic Associated Types (GATs) for flexible fixture types.
///
/// # Example
///
/// ```rust,ignore
/// use chicago_tdd_tools::core::async_fixture::AsyncFixtureProvider;
/// use chicago_tdd_tools::core::fixture::FixtureResult;
///
/// struct DatabaseFixture {
///     connection: String,
/// }
///
/// struct DatabaseProvider;
///
/// impl AsyncFixtureProvider for DatabaseProvider {
///     type Fixture<'a> = DatabaseFixture;
///     type Error = chicago_tdd_tools::core::fixture::FixtureError;
///
///     async fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error> {
///         // Async fixture creation
///         Ok(DatabaseFixture { connection: "connected".to_string() })
///     }
/// }
/// ```
#[cfg(feature = "async")]
pub trait AsyncFixtureProvider: private::Sealed {
    /// The fixture type with a lifetime parameter (GAT)
    type Fixture<'a>: 'a
    where
        Self: 'a;
    /// Error type for fixture creation
    type Error: std::error::Error + Send + Sync + 'static;

    /// Create a fixture asynchronously
    ///
    /// This method uses async traits (Rust 1.75+) for native async support.
    fn create_fixture(&self)
        -> impl Future<Output = Result<Self::Fixture<'_>, Self::Error>> + Send;
}

/// Async fixture manager for lifecycle management
///
/// Provides async setup/teardown operations with type-safe guarantees.
/// Note: Lifecycle is managed at runtime, not compile-time.
#[cfg(feature = "async")]
pub struct AsyncFixtureManager<P>
where
    P: AsyncFixtureProvider,
{
    provider: P,
}

#[cfg(feature = "async")]
impl<P> AsyncFixtureManager<P>
where
    P: AsyncFixtureProvider,
{
    /// Create a new async fixture manager
    pub const fn new(provider: P) -> Self {
        Self { provider }
    }

    /// Setup fixture asynchronously
    ///
    /// Creates the fixture and returns it for use in tests.
    ///
    /// # Errors
    ///
    /// Returns an error if fixture creation fails.
    #[allow(clippy::future_not_send)] // Trait design - Send bound is on trait, not implementation
    pub async fn setup(&self) -> Result<P::Fixture<'_>, P::Error> {
        self.provider.create_fixture().await
    }

    /// Teardown fixture asynchronously
    ///
    /// Performs cleanup operations. Override for custom cleanup logic.
    ///
    /// # Errors
    ///
    /// Returns an error if teardown fails.
    #[allow(clippy::unused_async)] // Part of async trait API - implementations may need async
    #[allow(clippy::future_not_send)] // Async trait API - Send bound not required for default implementation
    pub async fn teardown(&self) -> FixtureResult<()> {
        // Default: no-op cleanup
        // Override in implementations for custom cleanup
        Ok(())
    }
}

/// Default async fixture provider implementation
///
/// No-op fixture provider for tests that don't need fixture state.
/// `Fixture` is the unit type `()` — setup succeeds immediately with no allocations.
/// Use this as a stand-in when the test infrastructure requires an `AsyncFixtureProvider`
/// but the test itself has no external dependencies to set up.
#[cfg(feature = "async")]
pub struct DefaultAsyncFixtureProvider;

#[cfg(feature = "async")]
impl private::Sealed for DefaultAsyncFixtureProvider {}

#[cfg(feature = "async")]
impl AsyncFixtureProvider for DefaultAsyncFixtureProvider {
    type Fixture<'a> = ();
    type Error = FixtureError;

    async fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error> {
        Ok(())
    }
}

#[cfg(test)]
#[cfg(feature = "async")]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::{AsyncFixtureManager, AsyncFixtureProvider, DefaultAsyncFixtureProvider};
    use crate::assert_eq_msg;
    use crate::assert_err;
    use crate::assert_ok;
    use crate::assertions::assert_that_with_msg;
    use crate::async_test;
    use crate::core::fixture::FixtureError;

    #[derive(Debug)]
    struct TestAsyncFixture {
        data: String,
    }

    struct TestAsyncProvider;

    impl super::private::Sealed for TestAsyncProvider {}

    impl AsyncFixtureProvider for TestAsyncProvider {
        type Fixture<'a> = TestAsyncFixture;
        type Error = FixtureError;

        async fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error> {
            Ok(TestAsyncFixture {
                data: "test".to_string(),
            })
        }
    }

    async_test!(test_async_fixture_provider, {
        // Arrange: Create provider
        let provider = TestAsyncProvider;
        let manager = AsyncFixtureManager::new(provider);

        // Act: Setup fixture with proper error handling
        let fixture_result = manager.setup().await;
        assert_ok!(&fixture_result, "Fixture creation should succeed");
        let fixture = fixture_result.expect("Fixture should be Ok");

        // Assert: Verify fixture created
        assert_eq_msg!(
            &fixture.data,
            &"test".to_string(),
            "Fixture data should match"
        );
    });

    async_test!(test_default_async_fixture_provider, {
        // Arrange: Create default provider
        let provider = DefaultAsyncFixtureProvider;
        let manager = AsyncFixtureManager::new(provider);

        // Act: Setup fixture with proper error handling
        let fixture_result = manager.setup().await;
        assert_ok!(&fixture_result, "Default fixture creation should succeed");
        let fixture = fixture_result.expect("Fixture should be Ok");

        // Assert: Verify fixture created (unit type)
        let _ = fixture;
    });

    async_test!(test_async_fixture_error_handling, {
        // Arrange: Create provider that will fail
        struct FailingProvider;

        impl super::private::Sealed for FailingProvider {}

        impl AsyncFixtureProvider for FailingProvider {
            type Fixture<'a> = TestAsyncFixture;
            type Error = FixtureError;

            async fn create_fixture(&self) -> Result<Self::Fixture<'_>, Self::Error> {
                Err(FixtureError::CreationFailed(
                    "Intentional failure for testing".to_string(),
                ))
            }
        }

        let provider = FailingProvider;
        let manager = AsyncFixtureManager::new(provider);

        // Act: Attempt to setup fixture
        let fixture_result = manager.setup().await;

        // Assert: Verify error handling works correctly
        assert_err!(&fixture_result, "Failing provider should return error");
        match fixture_result.unwrap_err() {
            FixtureError::CreationFailed(msg) => {
                assert_that_with_msg(
                    &msg.contains("Intentional failure"),
                    |v| *v,
                    "Error message should be preserved",
                );
            }
            _ => panic!("Expected CreationFailed error"),
        }
    });
}
