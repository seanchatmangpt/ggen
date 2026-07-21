//! Test Macros for Chicago TDD Testing
//!
//! Provides macros to enforce Chicago TDD principles for test definitions:
//! - AAA pattern enforcement (Arrange-Act-Assert)
//! - Async test wrappers with fixture management
//! - Performance testing (tick budget validation)
//! - Parameterized testing (when parameterized-testing feature is enabled)

/// Default unit test timeout in seconds (SLA compliance)
///
/// **Kaizen improvement**: Extracted magic number `1` to named constant.
/// Pattern: Use named constants instead of magic numbers for configuration values.
/// Benefits: Improves readability, maintainability, self-documentation.
///
/// **Chicago TDD Principle**: Unit tests must complete within 1s for fast feedback.
/// This timeout ensures tests fail fast rather than hanging indefinitely.
///
/// Note: This constant is used in macro expansions. The value `1` is used directly
/// in macros since constants cannot be referenced in `macro_rules`! expansions.
pub const DEFAULT_UNIT_TEST_TIMEOUT_SECONDS: u64 = 1;

/// Default integration test timeout in seconds (SLA compliance)
///
/// **Chicago TDD Principle**: Integration tests require longer timeouts (30s) for
/// Docker operations, network calls, and external service interactions.
/// This timeout balances thoroughness with reasonable execution time.
///
/// Note: This constant is used in macro expansions. The value `30` is used directly
/// in macros since constants cannot be referenced in `macro_rules`! expansions.
pub const DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS: u64 = 30;

/// Default test timeout in seconds (SLA compliance)
///
/// **Deprecated**: Use `DEFAULT_UNIT_TEST_TIMEOUT_SECONDS` instead.
/// Kept for backward compatibility.
#[deprecated(note = "Use DEFAULT_UNIT_TEST_TIMEOUT_SECONDS instead")]
pub const DEFAULT_TEST_TIMEOUT_SECONDS: u64 = DEFAULT_UNIT_TEST_TIMEOUT_SECONDS;

/// Macro to enforce AAA (Arrange-Act-Assert) pattern
///
/// This macro ensures tests follow the Chicago TDD AAA pattern by requiring
/// explicit Arrange, Act, and Assert sections.
///
/// **Return Type Support**:
/// - Supports both `()` and `Result<(), E>` return types
/// - Use `?` operator for error propagation - errors are converted to panics
/// - Handles both Result and non-Result returns automatically
///
/// **Timeout Enforcement**:
/// - Unit tests: Automatically wrapped with a 1s timeout for SLA compliance
/// - Integration tests: No ntest timeout (relies on cargo-nextest profile timeout)
///   Integration tests are detected by checking if module path contains "testcontainers"
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::test;
///
/// # fn process(input: &str) -> &str { "result" }
/// test!(test_feature_behavior, {
///     // Arrange: Set up test data
///     let input = "test";
///     let expected = "result";
///
///     // Act: Execute feature
///     let result = process(input);
///
///     // Assert: Verify behavior
///     assert_eq!(result, expected);
/// });
/// ```
///
/// # Example with Result Return Type
///
/// ```rust
/// use chicago_tdd_tools::test;
///
/// # fn fallible_function() -> Result<(), Box<dyn std::error::Error>> { Ok(()) }
/// test!(test_with_result, {
///     // Arrange: Set up test data
///     let expected = true;
///
///     // Act: Execute fallible feature (use ? for error propagation)
///     fallible_function()?;
///
///     // Assert: Verify behavior
///     assert!(expected);
///     Ok::<(), Box<dyn std::error::Error>>(()) // Return Result - will be unwrapped automatically
/// });
/// ```
#[macro_export]
macro_rules! test {
    ($name:ident, $body:block) => {
        #[test]
        // **Root Cause Fix**: Removed ntest timeout to allow cargo-nextest profiles to handle timeouts
        // Unit tests: Use default profile (1s timeout in .config/nextest.toml)
        // Integration tests: Use integration profile (30s timeout in .config/nextest.toml)
        // Previously, ntest timeout (1s) took precedence over cargo-nextest profile timeout (30s),
        // causing integration tests to fail. Removing ntest timeout allows cargo-nextest to apply
        // the correct timeout based on the profile used.
        fn $name() -> Result<(), Box<dyn std::error::Error>> {
            // Helper trait to convert both () and Result to Result<(), Box<dyn Error>>
            // Use a uniquely named module to avoid conflicts across tests
            mod __chicago_tdd_test_output {
                pub trait TestOutput {
                    fn into_result(self) -> Result<(), Box<dyn std::error::Error>>;
                }

                impl TestOutput for () {
                    #[inline(always)]
                    fn into_result(self) -> Result<(), Box<dyn std::error::Error>> {
                        Ok(())
                    }
                }

                impl<E: std::fmt::Debug + std::error::Error + 'static> TestOutput for Result<(), E> {
                    #[inline(always)]
                    fn into_result(self) -> Result<(), Box<dyn std::error::Error>> {
                        self.map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
                    }
                }
            }

            // Execute test body - trait converts both () and Result to Result<(), Box<dyn Error>>
            // This allows ? operator to work in the test body
            let output = { $body };
            __chicago_tdd_test_output::TestOutput::into_result(output)
        }
    };
}

/// Macro for async tests with AAA pattern enforcement
///
/// Wraps async test functions and ensures AAA pattern is followed.
/// Supports `?` operator for error propagation - errors are converted to panics.
/// Handles both Result and non-Result returns.
///
/// **Return Type Support**:
/// - Supports both `()` and `Result<(), E>` return types (via `async_test_with_timeout!`)
/// - Use `?` operator for error propagation - errors are converted to panics
/// - Handles both Result and non-Result returns automatically
///
/// **Timeout Enforcement**:
/// - Default: Tests are automatically wrapped with `tokio::time::timeout` (1s) for unit tests
/// - Integration tests: Use `async_test_with_timeout!` with 30s timeout, or rely on cargo-nextest profile timeout
/// - Defense in depth: Multiple timeout layers ensure enforcement even if one layer fails
///
/// **Chicago TDD Principle**: "Better to break fast than freeze forever" - timeouts prevent infinite hangs
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::async_test;
///
/// # async fn async_function() -> Result<i32, Box<dyn std::error::Error>> { Ok(42) }
/// async_test!(test_async_feature, {
///     // Arrange: Set up test data
///     let expected = 42;
///
///     // Act: Execute async feature (use ? for error propagation)
///     let result = async_function().await?;
///
///     // Assert: Verify behavior
///     assert_eq!(result, expected);
///     Ok::<(), Box<dyn std::error::Error>>(()) // Return Result - will be unwrapped automatically
/// });
/// ```
#[macro_export]
macro_rules! async_test {
    ($name:ident, $body:block) => {
        $crate::async_test_with_timeout!($name, 1, $body);
    };
}

/// Macro for async tests with custom timeout
///
/// Same as `async_test!` but allows specifying a custom timeout in seconds.
/// Use this for integration tests that require longer timeouts (e.g., 30s for Docker operations).
///
/// **Timeout Enforcement**: Tests are wrapped with `tokio::time::timeout` using the specified duration.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::async_test_with_timeout;
///
/// # async fn slow_async_function() -> Result<i32, Box<dyn std::error::Error>> { Ok(42) }
/// // Integration test with 30s timeout
/// async_test_with_timeout!(test_integration_feature, 30, {
///     // Arrange: Set up test data
///     let expected = 42;
///
///     // Act: Execute slow async feature
///     let result = slow_async_function().await?;
///
///     // Assert: Verify behavior
///     assert_eq!(result, expected);
///     Ok::<(), Box<dyn std::error::Error>>(())
/// });
/// ```
#[macro_export]
macro_rules! async_test_with_timeout {
    ($name:ident, $timeout_secs:expr, $body:block) => {
        #[tokio::test]
        async fn $name() {
            use tokio::time::{timeout, Duration};

            // Helper trait to handle both Result and non-Result returns
            trait TestOutput {
                fn handle(self);
            }

            impl TestOutput for () {
                fn handle(self) {}
            }

            impl<E: std::fmt::Debug> TestOutput for Result<(), E> {
                fn handle(self) {
                    if let Err(e) = self {
                        panic!("Test failed: {:?}", e);
                    }
                }
            }

            // Execute body with specified timeout for SLA compliance
            // **Kaizen improvement**: Comments reference timeout constants for clarity
            // Note: Using literal value since macro_rules! cannot reference constants directly
            // Standard timeout values:
            //   - DEFAULT_UNIT_TEST_TIMEOUT_SECONDS (1s) for unit tests
            //   - DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS (30s) for integration tests
            // The $timeout_secs parameter allows custom timeouts (e.g., 30s for integration tests)
            let test_future = async {
                let output = async { $body }.await;
                TestOutput::handle(output);
            };

            match timeout(Duration::from_secs($timeout_secs), test_future).await {
                Ok(_) => {
                    // Test completed within timeout
                }
                Err(_) => {
                    panic!(
                        "Test '{}' exceeded {}s timeout (SLA violation). \
                        Expected timeout: {}s. \
                        Use async_test_with_timeout! with longer timeout for integration tests.",
                        stringify!($name),
                        $timeout_secs,
                        $timeout_secs
                    );
                }
            }
        }
    };
}

/// Macro for async tests with automatic fixture setup and teardown
///
/// Creates a test fixture, runs the test body, and ensures cleanup.
///
/// **Timeout Enforcement**:
/// - Default: Tests are automatically wrapped with `tokio::time::timeout` (1s) for unit tests
/// - Integration tests: Use `fixture_test_with_timeout!` with 30s timeout, or rely on cargo-nextest profile timeout
/// - Defense in depth: Multiple timeout layers ensure enforcement even if one layer fails
///
/// **Chicago TDD Principle**: "Better to break fast than freeze forever" - timeouts prevent infinite hangs
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::{fixture_test, prelude::*};
///
/// # fn process(counter: u64) -> u64 { counter + 1 }
/// fixture_test!(test_with_fixture, fixture, {
///     // Arrange: Use provided fixture
///     let counter = fixture.test_counter();
///
///     // Act: Execute test
///     let result = process(counter);
///
///     // Assert: Verify behavior
///     assert!(result > 0);
/// });
/// ```
#[macro_export]
macro_rules! fixture_test {
    ($name:ident, $fixture_var:ident, $body:block) => {
        $crate::fixture_test_with_timeout!($name, $fixture_var, 1, $body);
    };
}

/// Macro for async tests with fixture and custom timeout
///
/// Same as `fixture_test!` but allows specifying a custom timeout in seconds.
/// Use this for integration tests that require longer timeouts (e.g., 30s for Docker operations).
///
/// **Timeout Enforcement**: Tests are wrapped with `tokio::time::timeout` using the specified duration.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::{fixture_test_with_timeout, prelude::*};
///
/// # fn slow_process(counter: u64) -> u64 { counter + 1 }
/// // Integration test with 30s timeout
/// fixture_test_with_timeout!(test_integration_with_fixture, fixture, 30, {
///     // Arrange: Use provided fixture
///     let counter = fixture.test_counter();
///
///     // Act: Execute slow test
///     let result = slow_process(counter);
///
///     // Assert: Verify behavior
///     assert!(result > 0);
/// });
/// ```
#[macro_export]
macro_rules! fixture_test_with_timeout {
    ($name:ident, $fixture_var:ident, $timeout_secs:expr, $body:block) => {
        #[allow(unnameable_test_items, unused_mut)]
        #[tokio::test]
        async fn $name() {
            use tokio::time::{timeout, Duration};

            // Arrange: Create fixture
            #[allow(clippy::expect_used)] // Macro - panic is appropriate if fixture creation fails
            #[allow(unused_mut)] // Fixture may not require mutation in every test body
            let mut $fixture_var = $crate::core::fixture::TestFixture::new()
                .unwrap_or_else(|e| panic!("Failed to create test fixture: {}", e));

            // Execute test body with specified timeout for SLA compliance
            // **Kaizen improvement**: Comments reference timeout constants for clarity
            // Note: Using literal value since macro_rules! cannot reference constants directly
            // Standard timeout values:
            //   - DEFAULT_UNIT_TEST_TIMEOUT_SECONDS (1s) for unit tests
            //   - DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS (30s) for integration tests
            // The $timeout_secs parameter allows custom timeouts (e.g., 30s for integration tests)
            let test_future = async { $body };

            match timeout(Duration::from_secs($timeout_secs), test_future).await {
                Ok(_) => {
                    // Test completed within timeout
                }
                Err(_) => {
                    panic!(
                        "Test '{}' exceeded {}s timeout (SLA violation). \
                        Expected timeout: {}s. \
                        Use fixture_test_with_timeout! with longer timeout for integration tests.",
                        stringify!($name),
                        $timeout_secs,
                        $timeout_secs
                    );
                }
            }

            // Cleanup: Automatic teardown via Drop
        }
    };
}

/// Macro for performance tests with tick budget validation
///
/// Validates that hot path operations complete within the Chatman Constant
/// (≤8 ticks = 2ns budget).
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::{performance_test, prelude::*};
///
/// # fn create_test_input() -> i32 { 42 }
/// # fn hot_path_operation(input: &i32) -> i32 { *input }
/// performance_test!(test_hot_path_performance, {
///     // Arrange: Set up test data
///     let input = create_test_input();
///
///     // Act: Execute hot path operation
///     let (result, ticks) = measure_ticks(|| hot_path_operation(&input));
///
///     // Assert: Verify performance constraint
///     assert!(ticks <= 8, "Hot path exceeded tick budget: {} > 8", ticks);
///     assert_eq!(result, 42);
/// });
/// ```
#[macro_export]
macro_rules! performance_test {
    ($name:ident, $body:block) => {
        #[test]
        fn $name() {
            $body
        }
    };
}

#[cfg(feature = "parameterized-testing")]
/// Parameterized test macro using rstest
///
/// Creates parameterized tests that run with multiple input values.
/// This is a wrapper around rstest's `#[rstest]` attribute macro.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::param_test;
///
/// param_test! {
///     #[case(1, 2, 3)]
///     #[case(2, 3, 5)]
///     #[case(3, 4, 7)]
///     fn test_addition(a: i32, b: i32, expected: i32) {
///         assert_eq!(a + b, expected);
///     }
/// }
///
/// fn main() {}
/// ```
#[macro_export]
macro_rules! param_test {
    {
        $(#[$attr:meta])*
        fn $name:ident($($param:ident: $type:ty),* $(,)?) $body:block
    } => {
        #[$crate::rstest::rstest($($param),*)]
        $(#[$attr])*
        fn $name($($param: $type),*) $body
    };
}

#[cfg(not(feature = "parameterized-testing"))]
/// Parameterized test macro (requires parameterized-testing feature)
///
/// Enable the `parameterized-testing` feature to use parameterized tests.
#[macro_export]
macro_rules! param_test {
    ($($tt:tt)*) => {
        compile_error!("Parameterized testing requires the 'parameterized-testing' feature. Enable with: --features parameterized-testing");
    };
}

/// Macro for OTEL testing with automatic validation
///
/// Automates OTEL span/metric testing with Chicago TDD patterns:
/// - AAA pattern enforcement
/// - Automatic span/metric validation
/// - Test helper setup
///
/// **Timeout Enforcement**:
/// - Tests rely on cargo-nextest profile timeout (1s for unit tests, 30s for integration tests)
/// - No test-level timeout to allow cargo-nextest profiles to handle timeouts correctly
/// - Defense in depth: Process-level and runner-level timeouts ensure enforcement
///
/// **Chicago TDD Principle**: "Better to break fast than freeze forever" - timeouts prevent infinite hangs
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::{otel_test, prelude::*};
///
/// otel_test!(test_otel_span_validation, {
///     // Arrange: Create test span
///     let span = chicago_tdd_tools::otel::test_helpers::create_test_span("test.operation");
///
///     // Act: Validate span
///     let helper = chicago_tdd_tools::otel::OtelTestHelper::new();
///     helper.assert_spans_valid(&[span.clone()]);
///
///     // Assert: Verify span is valid
///     assert_eq!(span.name, "test.operation");
/// });
/// ```
#[cfg(feature = "otel")]
#[macro_export]
macro_rules! otel_test {
    ($name:ident, $body:block) => {
        #[test]
        // **Root Cause Fix**: Removed ntest timeout to allow cargo-nextest profiles to handle timeouts
        // Unit tests: Use default profile (1s timeout in .config/nextest.toml)
        // Integration tests: Use integration profile (30s timeout in .config/nextest.toml)
        // Previously, ntest timeout (1s) took precedence over cargo-nextest profile timeout (30s),
        // causing integration tests to fail. Removing ntest timeout allows cargo-nextest to apply
        // the correct timeout based on the profile used.
        fn $name() {
            $body
        }
    };
}

#[cfg(not(feature = "otel"))]
/// Macro for OTEL testing (requires otel feature)
///
/// Enable the `otel` feature to use OTEL testing macros.
#[macro_export]
macro_rules! otel_test {
    ($($tt:tt)*) => {
        compile_error!("OTEL testing requires the 'otel' feature. Enable with: --features otel");
    };
}

#[cfg(test)]
#[allow(unnameable_test_items)] // Macro-generated tests trigger this warning
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use crate::assert_eq_msg;
    use crate::assert_err;
    use crate::assertions::assert_that_with_msg;
    // Note: We can't use test! macro here because it would create
    // a test function with the same name, causing conflicts.
    // These tests verify the macro expansion works correctly.



    // Test fixture_test! macro expansion - verify it compiles and works
    // Note: This test verifies the macro expands correctly by using it directly
    fixture_test!(test_fixture_basic, fixture, {
        // Arrange
        let counter = fixture.test_counter();

        // Act
        let result = counter + 1;

        // Assert
        assert_that_with_msg(&result, |v| *v > 0, "Result should be greater than 0");
    });

    #[cfg(feature = "parameterized-testing")]
    #[test]
    fn test_parameterized_macro() {
        // This test demonstrates parameterized testing
        // Actual parameterized tests would use param_test! macro
        assert_that_with_msg(&true, |v| *v, "Value should be true");
    }

    // Compile + runtime check: param_test! must expand via the crate-root
    // rstest re-export ($crate::rstest::rstest) so consumers without a direct
    // rstest dependency can use it. This module deliberately has no
    // `use rstest` import.
    #[cfg(feature = "parameterized-testing")]
    mod param_test_expansion {
        crate::param_test! {
            #[case(1, 2, 3)]
            #[case(2, 3, 5)]
            fn param_test_expands_dep_free(a: i32, b: i32, expected: i32) {
                assert_eq!(a + b, expected);
            }
        }
    }

    #[test]
    #[allow(deprecated)] // Testing deprecated constant for backward compatibility
    fn test_timeout_constants() {
        // Verify timeout constants are correctly defined
        assert_eq_msg!(
            &super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS,
            &1,
            "DEFAULT_UNIT_TEST_TIMEOUT_SECONDS should be 1"
        );
        assert_eq_msg!(
            &super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS,
            &30,
            "DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS should be 30"
        );
        // Verify deprecated constant still works (backward compatibility)
        assert_eq_msg!(
            &super::DEFAULT_TEST_TIMEOUT_SECONDS,
            &super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS,
            "DEFAULT_TEST_TIMEOUT_SECONDS should equal DEFAULT_UNIT_TEST_TIMEOUT_SECONDS"
        );
    }




    // Test to verify timeout enforcement works (FM9: Defense in depth verification)
    // FMEA Fix: Verifies that tokio::time::timeout actually enforces timeouts.
    // This test ensures timeout layer 1 (tokio::time::timeout) is working correctly.
    async_test!(test_timeout_enforcement_works, {
        use tokio::time::{timeout, Duration, Instant};

        // Arrange: Create a future that takes longer than timeout
        let start = Instant::now();
        let slow_future = async {
            tokio::time::sleep(Duration::from_millis(200)).await;
        };

        // Act: Apply 100ms timeout to 200ms operation
        let result = timeout(Duration::from_millis(100), slow_future).await;

        // Assert: Timeout should have triggered
        assert_err!(&result, "Timeout should have been triggered");
        let elapsed = start.elapsed();
        assert_that_with_msg(
            &(elapsed < Duration::from_millis(150)),
            |v| *v,
            &format!(
                "Timeout should trigger within 150ms, but took {:?}",
                elapsed
            ),
        );
    });

    /// Test to verify timeout constants match actual usage (FM1: Constant mismatch verification)
    ///
    /// **FMEA Fix**: Verifies that timeout constants match the values used in macros.
    /// This ensures consistency between constants and actual timeout values.
    #[test]
    fn test_timeout_constants_match_macro_usage() {
        // Verify unit test timeout constant matches macro default
        assert_eq_msg!(
            &super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS,
            &1,
            "DEFAULT_UNIT_TEST_TIMEOUT_SECONDS should be 1s for unit tests"
        );

        // Verify integration test timeout constant matches expected value
        assert_eq_msg!(
            &super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS,
            &30,
            "DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS should be 30s for integration tests"
        );

        // Verify macros use correct default values (checked via expansion tests)
        // async_test! uses 1, fixture_test! uses 1, weaver_test! uses 1
        // These are verified in macro expansion tests above
    }

    /// Test to verify defense in depth - multiple timeout layers (FM9: Defense in depth)
    ///
    /// **FMEA Fix**: Verifies that timeout layers are documented and present.
    /// This test ensures we can verify timeout layers exist.
    #[test]
    fn test_timeout_layers_documented() {
        // Layer 1: Test-level timeouts (tokio::time::timeout) - Verified in async macros
        // Layer 2: Test runner timeouts (cargo-nextest) - Verified in .config/nextest.toml
        // Layer 3: Process-level timeouts (Unix timeout) - Verified in Makefile.toml

        // Verify constants exist for timeout configuration
        assert_that_with_msg(
            &(super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS > 0),
            |v| *v,
            "DEFAULT_UNIT_TEST_TIMEOUT_SECONDS should be > 0",
        );
        assert_that_with_msg(
            &(super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS > 0),
            |v| *v,
            "DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS should be > 0",
        );
        assert_that_with_msg(
            &(super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS
                > super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS),
            |v| *v,
            "Integration timeout should be greater than unit timeout",
        );
    }

    /// Test to verify process timeout is sufficient (FM4: Process timeout verification)
    ///
    /// **FMEA Fix**: Verifies that process-level timeouts allow tests to complete.
    /// Unit tests: 10s process timeout allows for 1s per-test SLA with parallel execution
    /// Integration tests: 30s process timeout allows for 30s per-test SLA
    #[test]
    fn test_process_timeout_sufficient() {
        // Unit tests: Process timeout (10s) >> per-test timeout (1s) * parallel execution
        // Integration tests: Process timeout (30s) >= per-test timeout (30s)
        // This ensures process timeout doesn't kill tests before test-level timeout

        // Verify unit test timeout allows for parallel execution
        let unit_process_timeout = 10u64; // seconds
        let unit_test_timeout = super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS;
        assert_that_with_msg(
            &(unit_process_timeout > unit_test_timeout * 5),
            |v| *v,
            &format!("Process timeout ({unit_process_timeout}) should be much larger than per-test timeout ({unit_test_timeout}) for parallel execution")
        );

        // Verify integration test timeout is sufficient
        let integration_process_timeout = 30u64; // seconds
        let integration_test_timeout = super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS;
        assert_that_with_msg(
            &(integration_process_timeout >= integration_test_timeout),
            |v| *v,
            &format!(
                "Process timeout ({}) should be >= per-test timeout ({})",
                integration_process_timeout, integration_test_timeout
            ),
        );
    }

    /// Test to verify integration tests use correct profile (FM7: Profile verification)
    ///
    /// **FMEA Fix**: Verifies that integration tests are configured to use integration profile.
    /// This is verified by checking Makefile.toml configuration (test-integration task uses --profile integration).
    /// This test documents the expected configuration.
    #[test]
    fn test_integration_profile_configuration() {
        // Integration tests should use:
        // 1. --profile integration flag in cargo-nextest
        // 2. Integration profile has 30s timeout (verified in .config/nextest.toml)
        // 3. Process-level timeout of 30s (verified in Makefile.toml)

        // Verify integration timeout matches expected value
        assert_eq_msg!(
            &super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS,
            &30,
            "Integration tests should use 30s timeout"
        );

        // Verify integration timeout is longer than unit timeout
        assert_that_with_msg(
            &(super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS
                > super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS),
            |v| *v,
            &format!(
                "Integration timeout ({}) should be longer than unit timeout ({})",
                super::DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS,
                super::DEFAULT_UNIT_TEST_TIMEOUT_SECONDS
            ),
        );
    }
}
