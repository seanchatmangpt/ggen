//! Test Utilities
//!
//! Common testing utilities that address frequently requested features from the Rust testing community.

use std::path::PathBuf;
use std::time::{Duration, Instant};

/// Test retry configuration for handling flaky tests
///
/// Commonly requested feature for dealing with non-deterministic test failures.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::test_utils::RetryConfig;
///
/// let config = RetryConfig::default()
///     .with_max_attempts(3)
///     .with_delay(std::time::Duration::from_millis(100));
///
/// let result = config.retry(|| {
///     // Potentially flaky operation
///     Ok::<_, String>(42)
/// });
///
/// assert!(result.is_ok());
/// ```
#[derive(Debug, Clone)]
pub struct RetryConfig {
    max_attempts: usize,
    delay: Duration,
    exponential_backoff: bool,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self { max_attempts: 3, delay: Duration::from_millis(100), exponential_backoff: false }
    }
}

impl RetryConfig {
    /// Create a new retry configuration
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set maximum number of retry attempts
    #[must_use]
    pub const fn with_max_attempts(mut self, attempts: usize) -> Self {
        self.max_attempts = attempts;
        self
    }

    /// Set delay between retry attempts
    #[must_use]
    pub const fn with_delay(mut self, delay: Duration) -> Self {
        self.delay = delay;
        self
    }

    /// Enable exponential backoff for retries
    #[must_use]
    pub const fn with_exponential_backoff(mut self) -> Self {
        self.exponential_backoff = true;
        self
    }

    /// Retry a function until it succeeds or max attempts reached
    ///
    /// # Errors
    ///
    /// Returns the last error if all retry attempts fail.
    ///
    /// # Panics
    ///
    /// Panics if `max_attempts` is 0 (should never happen with default configuration).
    pub fn retry<F, T, E>(&self, mut f: F) -> Result<T, E>
    where
        F: FnMut() -> Result<T, E>,
    {
        let mut last_error = None;

        for attempt in 0..self.max_attempts {
            match f() {
                Ok(value) => return Ok(value),
                Err(e) => {
                    last_error = Some(e);

                    if attempt < self.max_attempts - 1 {
                        #[allow(clippy::cast_possible_truncation)]
                        // Attempt count won't exceed u32::MAX
                        let delay = if self.exponential_backoff {
                            self.delay * 2_u32.pow(attempt as u32)
                        } else {
                            self.delay
                        };
                        std::thread::sleep(delay);
                    }
                }
            }
        }

        // Safe to unwrap: if max_attempts > 0, last_error will be Some
        last_error.map_or_else(|| unreachable!("max_attempts should be > 0"), Err)
    }
}

/// Temporary directory helper for tests
///
/// Automatically cleaned up when dropped. Common need in integration tests.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::test_utils::TempDir;
///
/// let temp = TempDir::new("my_test").unwrap();
/// let path = temp.path();
///
/// // Use the temporary directory
/// std::fs::write(path.join("test.txt"), "hello").unwrap();
///
/// // Automatically cleaned up when temp goes out of scope
/// ```
#[derive(Debug)]
pub struct TempDir {
    path: PathBuf,
}

impl TempDir {
    /// Create a new temporary directory with a prefix
    ///
    /// # Errors
    ///
    /// Returns an error if the temporary directory cannot be created.
    pub fn new(prefix: &str) -> Result<Self, std::io::Error> {
        let temp_dir = std::env::temp_dir();
        let uuid = uuid::Uuid::new_v4();
        let unique_name = format!("{prefix}-{uuid}");
        let path = temp_dir.join(unique_name);

        std::fs::create_dir_all(&path)?;

        Ok(Self { path })
    }

    /// Get the path to the temporary directory
    #[must_use]
    pub const fn path(&self) -> &PathBuf {
        &self.path
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
}

/// Test timing helper
///
/// Measure execution time of test operations. Commonly used for performance validation.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::test_utils::TestTimer;
///
/// let timer = TestTimer::start();
///
/// // Perform operation
/// std::thread::sleep(std::time::Duration::from_millis(10));
///
/// let elapsed = timer.elapsed();
/// assert!(elapsed.as_millis() >= 10);
/// ```
#[derive(Debug)]
pub struct TestTimer {
    start: Instant,
}

impl TestTimer {
    /// Start a new timer
    #[must_use]
    pub fn start() -> Self {
        Self { start: Instant::now() }
    }

    /// Get elapsed time since timer started
    #[must_use]
    pub fn elapsed(&self) -> Duration {
        self.start.elapsed()
    }

    /// Check if elapsed time exceeds a threshold
    #[must_use]
    pub fn exceeds(&self, threshold: Duration) -> bool {
        self.elapsed() > threshold
    }

    /// Reset the timer
    pub fn reset(&mut self) {
        self.start = Instant::now();
    }
}

/// Test data generator for common types
///
/// Quick data generation for tests without external dependencies.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::test_utils::TestData;
///
/// let email = TestData::email("john");
/// assert_eq!(email, "john@example.com");
///
/// let phone = TestData::phone("555", "1234");
/// assert_eq!(phone, "555-1234");
/// ```
pub struct TestData;

impl TestData {
    /// Generate a test email address
    #[must_use]
    pub fn email(name: &str) -> String {
        format!("{name}@example.com")
    }

    /// Generate a test phone number
    #[must_use]
    pub fn phone(area: &str, number: &str) -> String {
        format!("{area}-{number}")
    }

    /// Generate a test UUID string
    #[must_use]
    pub fn uuid() -> String {
        uuid::Uuid::new_v4().to_string()
    }

    /// Generate a test timestamp (current time)
    #[must_use]
    pub fn timestamp() -> String {
        chrono::Utc::now().to_rfc3339()
    }

    /// Generate a sequence of integers
    #[must_use]
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)] // Count is typically small for test data
    pub fn sequence(start: i32, count: usize) -> Vec<i32> {
        (start..start + count as i32).collect()
    }

    /// Generate a test string with a pattern
    #[must_use]
    pub fn string_with_pattern(pattern: &str, count: usize) -> String {
        pattern.repeat(count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test;

    test!(test_retry_config_success_first_attempt, {
        // Arrange
        let config = RetryConfig::default();
        let mut attempts = 0;

        // Act
        let result = config.retry(|| {
            attempts += 1;
            Ok::<_, String>(42)
        });

        // Assert
        assert_eq!(result, Ok(42));
        assert_eq!(attempts, 1);
    });

    test!(test_retry_config_success_after_retries, {
        // Arrange
        let config = RetryConfig::default().with_delay(Duration::from_millis(1));
        let mut attempts = 0;

        // Act
        let result = config.retry(|| {
            attempts += 1;
            if attempts < 3 {
                Err("Not yet")
            } else {
                Ok(42)
            }
        });

        // Assert
        assert_eq!(result, Ok(42));
        assert_eq!(attempts, 3);
    });

    test!(test_retry_config_all_failures, {
        // Arrange
        let config =
            RetryConfig::default().with_max_attempts(2).with_delay(Duration::from_millis(1));
        let mut attempts = 0;

        // Act
        let result = config.retry(|| {
            attempts += 1;
            Err::<i32, _>("Always fails")
        });

        // Assert
        assert_eq!(result, Err("Always fails"));
        assert_eq!(attempts, 2);
    });

    test!(test_temp_dir_creation, {
        // Arrange & Act
        let temp = TempDir::new("test").unwrap();
        let path = temp.path();

        // Assert
        assert!(path.exists());
        assert!(path.is_dir());
    });

    test!(test_temp_dir_cleanup, {
        // Arrange
        let path = {
            let temp = TempDir::new("test").unwrap();
            temp.path().clone()
        };

        // Act & Assert: Directory should be cleaned up after drop
        assert!(!path.exists());
    });

    test!(test_test_timer_elapsed, {
        // Arrange
        let timer = TestTimer::start();

        // Act
        std::thread::sleep(Duration::from_millis(10));

        // Assert
        assert!(timer.elapsed().as_millis() >= 10);
    });

    test!(test_test_timer_exceeds, {
        // Arrange
        let timer = TestTimer::start();

        // Act
        std::thread::sleep(Duration::from_millis(20));

        // Assert
        assert!(timer.exceeds(Duration::from_millis(10)));
        assert!(!timer.exceeds(Duration::from_millis(100)));
    });

    test!(test_test_data_email, {
        // Act
        let email = TestData::email("john");

        // Assert
        assert_eq!(email, "john@example.com");
    });

    test!(test_test_data_phone, {
        // Act
        let phone = TestData::phone("555", "1234");

        // Assert
        assert_eq!(phone, "555-1234");
    });

    test!(test_test_data_uuid, {
        // Act
        let uuid1 = TestData::uuid();
        let uuid2 = TestData::uuid();

        // Assert
        assert_ne!(uuid1, uuid2);
        assert_eq!(uuid1.len(), 36); // UUID format length
    });

    test!(test_test_data_sequence, {
        // Act
        let seq = TestData::sequence(10, 5);

        // Assert
        assert_eq!(seq, vec![10, 11, 12, 13, 14]);
    });

    test!(test_test_data_string_pattern, {
        // Act
        let result = TestData::string_with_pattern("ab", 3);

        // Assert
        assert_eq!(result, "ababab");
    });
}
