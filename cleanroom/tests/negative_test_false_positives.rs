//! Negative Testing: False Positive Detection
//!
//! This test suite is designed to detect false positives - methods that
//! return success when they should fail without Docker.
//!
//! **CRITICAL**: These tests should FAIL if mock implementations are present.
//!
//! Run these tests WITHOUT Docker running to verify proper error handling.

use cleanroom::containers::{GenericContainer, PostgresContainer, RedisContainer};
use cleanroom::error::Result;

/// Test that postgres methods fail properly without Docker
///
/// **Expected Behavior WITHOUT Docker**:
/// - Container creation should fail at `.start()`
/// - If creation somehow succeeds (bug), connection tests should fail
#[tokio::test]
#[ignore] // Run manually with `cargo test --test negative_test_false_positives -- --ignored`
async fn negative_test_postgres_false_positives() {
    println!("\n🔍 NEGATIVE TEST: Testing PostgreSQL false positives...");
    println!("⚠️  This test should be run WITHOUT Docker running!");

    // Step 1: Try to create container (should fail without Docker)
    let postgres_result = PostgresContainer::new("testdb", "testuser", "testpass");

    match postgres_result {
        Err(e) => {
            println!("✅ GOOD: Container creation failed as expected: {}", e);
            // This is the correct behavior - test passes
            return;
        }
        Ok(postgres) => {
            println!("⚠️  WARNING: Container creation succeeded (Docker might be running)");
            println!("   Now testing for false positives in method implementations...");

            // Step 2: Test connection (should fail without real database)
            let connection_result = postgres.test_connection().await;
            match connection_result {
                Ok(()) => {
                    panic!(
                        "❌ FALSE POSITIVE DETECTED: test_connection() returned Ok(()) \
                        without real database connection! This is a mock implementation."
                    );
                }
                Err(e) => {
                    println!("✅ GOOD: test_connection() properly failed: {}", e);
                }
            }

            // Step 3: Test SQL execution (should fail without real database)
            let sql_result = postgres.execute_sql("SELECT 1").await;
            match sql_result {
                Ok(result) => {
                    if result.contains("Mock") {
                        panic!(
                            "❌ FALSE POSITIVE DETECTED: execute_sql() returned mock data: '{}' \
                            without real database connection!",
                            result
                        );
                    } else {
                        println!("✅ SQL execution returned: {}", result);
                    }
                }
                Err(e) => {
                    println!("✅ GOOD: execute_sql() properly failed: {}", e);
                }
            }
        }
    }
}

/// Test that redis methods fail properly without Docker
///
/// **Expected Behavior WITHOUT Docker**:
/// - Container creation should fail at `.start()`
/// - If creation somehow succeeds (bug), command execution should fail
#[tokio::test]
#[ignore] // Run manually
async fn negative_test_redis_false_positives() {
    println!("\n🔍 NEGATIVE TEST: Testing Redis false positives...");
    println!("⚠️  This test should be run WITHOUT Docker running!");

    // Step 1: Try to create container (should fail without Docker)
    let redis_result = RedisContainer::new(None);

    match redis_result {
        Err(e) => {
            println!("✅ GOOD: Container creation failed as expected: {}", e);
            return;
        }
        Ok(redis) => {
            println!("⚠️  WARNING: Container creation succeeded (Docker might be running)");
            println!("   Now testing for false positives in method implementations...");

            // Step 2: Test connection (should fail without real Redis)
            let connection_result = redis.test_connection().await;
            match connection_result {
                Ok(()) => {
                    panic!(
                        "❌ FALSE POSITIVE DETECTED: test_connection() returned Ok(()) \
                        without real Redis connection! This is a mock implementation."
                    );
                }
                Err(e) => {
                    println!("✅ GOOD: test_connection() properly failed: {}", e);
                }
            }

            // Step 3: Test command execution (should fail without real Redis)
            let cmd_result = redis.execute_command("PING").await;
            match cmd_result {
                Ok(result) => {
                    if result.contains("Mock") {
                        panic!(
                            "❌ FALSE POSITIVE DETECTED: execute_command() returned mock data: '{}' \
                            without real Redis connection!",
                            result
                        );
                    } else {
                        println!("✅ Command execution returned: {}", result);
                    }
                }
                Err(e) => {
                    println!("✅ GOOD: execute_command() properly failed: {}", e);
                }
            }

            // Step 4: Test SET operation (should fail without real Redis)
            let set_result = redis.set("key", "value").await;
            match set_result {
                Ok(result) => {
                    if result.contains("Mock") {
                        panic!(
                            "❌ FALSE POSITIVE DETECTED: set() returned mock data: '{}' \
                            without real Redis connection!",
                            result
                        );
                    }
                }
                Err(e) => {
                    println!("✅ GOOD: set() properly failed: {}", e);
                }
            }
        }
    }
}

/// Test that generic container methods fail properly without Docker
#[tokio::test]
#[ignore] // Run manually
async fn negative_test_generic_container_false_positives() {
    println!("\n🔍 NEGATIVE TEST: Testing GenericContainer false positives...");
    println!("⚠️  This test should be run WITHOUT Docker running!");

    // Step 1: Try to create container (should fail without Docker)
    let container_result = GenericContainer::new("test", "alpine", "latest");

    match container_result {
        Err(e) => {
            println!("✅ GOOD: Container creation failed as expected: {}", e);
            return;
        }
        Ok(container) => {
            println!("⚠️  WARNING: Container creation succeeded (Docker might be running)");
            println!("   Now testing for false positives in method implementations...");

            // Step 2: Test command execution (should fail without real container)
            let cmd_result = container
                .execute_command(vec!["echo".into(), "test".into()])
                .await;
            match cmd_result {
                Ok(result) => {
                    if result.contains("Mock") {
                        panic!(
                            "❌ FALSE POSITIVE DETECTED: execute_command() returned mock data: '{}' \
                            without real container!",
                            result
                        );
                    } else {
                        println!("✅ Command execution returned: {}", result);
                    }
                }
                Err(e) => {
                    println!("✅ GOOD: execute_command() properly failed: {}", e);
                }
            }
        }
    }
}

/// Test that container status is accurate
///
/// **Expected Behavior**:
/// - Status should NOT always return "Running"
/// - Status should reflect actual Docker state
#[tokio::test]
#[ignore] // Run manually
async fn negative_test_container_status_accuracy() {
    println!("\n🔍 NEGATIVE TEST: Testing container status accuracy...");

    // This test verifies that status() doesn't always return Running
    // Run this WITHOUT Docker and status should return Error or Stopped

    use cleanroom::cleanroom::{ContainerStatus, ContainerWrapper};

    let postgres_result = PostgresContainer::new("testdb", "testuser", "testpass");

    if let Ok(postgres) = postgres_result {
        let status = postgres.status();

        if status == ContainerStatus::Running {
            println!("⚠️  WARNING: Container status is 'Running'");
            println!("   This might be accurate if Docker is running,");
            println!("   or it might be a false positive (hardcoded status).");
            println!("   Verify by stopping Docker and running this test again.");
        } else {
            println!("✅ GOOD: Container status is: {:?}", status);
        }
    } else {
        println!("✅ Container creation failed without Docker (expected)");
    }
}

/// Test that metrics are not hardcoded
///
/// **Expected Behavior**:
/// - Metrics should reflect actual container state
/// - Metrics should fail or return zeros without Docker
#[tokio::test]
#[ignore] // Run manually
async fn negative_test_metrics_accuracy() {
    println!("\n🔍 NEGATIVE TEST: Testing metrics accuracy...");

    use cleanroom::cleanroom::ContainerWrapper;

    let postgres_result = PostgresContainer::new("testdb", "testuser", "testpass");

    if let Ok(postgres) = postgres_result {
        let metrics = postgres.metrics();

        // Check for suspiciously static values
        if metrics.cpu_usage_percent == 5.0
            && metrics.memory_usage_bytes == 128 * 1024 * 1024
            && metrics.network_bytes_sent == 0
            && metrics.network_bytes_received == 0
        {
            println!(
                "⚠️  WARNING: Metrics appear to be hardcoded values:\n\
                CPU: {}%, Memory: {} bytes, Network: {}/{} bytes",
                metrics.cpu_usage_percent,
                metrics.memory_usage_bytes,
                metrics.network_bytes_sent,
                metrics.network_bytes_received
            );
            println!("   These should vary based on actual container usage.");
        } else {
            println!("✅ GOOD: Metrics appear to be dynamic: {:?}", metrics);
        }
    } else {
        println!("✅ Container creation failed without Docker (expected)");
    }
}

/// Integration test: Full workflow should fail without Docker
///
/// This test simulates a typical workflow and ensures all steps
/// fail appropriately when Docker is not available.
#[tokio::test]
#[ignore] // Run manually with Docker stopped
async fn negative_test_full_workflow_without_docker() {
    println!("\n🔍 NEGATIVE TEST: Full workflow without Docker...");
    println!("⚠️  Docker should be STOPPED for this test!");

    let mut failures = Vec::new();

    // Test 1: PostgreSQL creation should fail
    match PostgresContainer::new("testdb", "testuser", "testpass") {
        Ok(_) => failures.push("PostgreSQL container creation succeeded (should fail)"),
        Err(_) => println!("✅ PostgreSQL creation failed (expected)"),
    }

    // Test 2: Redis creation should fail
    match RedisContainer::new(None) {
        Ok(_) => failures.push("Redis container creation succeeded (should fail)"),
        Err(_) => println!("✅ Redis creation failed (expected)"),
    }

    // Test 3: Generic container creation should fail
    match GenericContainer::new("test", "alpine", "latest") {
        Ok(_) => failures.push("Generic container creation succeeded (should fail)"),
        Err(_) => println!("✅ Generic container creation failed (expected)"),
    }

    if !failures.is_empty() {
        panic!(
            "❌ FALSE POSITIVES DETECTED:\n{}",
            failures
                .iter()
                .map(|f| format!("  - {}", f))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    println!("✅ All operations properly failed without Docker!");
}

#[cfg(test)]
mod documentation {
    //! HOW TO USE THESE TESTS
    //!
    //! These are negative tests designed to detect false positives.
    //!
    //! ## Running the tests:
    //!
    //! 1. Stop Docker Desktop completely
    //! 2. Run: `cargo test --test negative_test_false_positives -- --ignored --nocapture`
    //! 3. Verify all tests PASS (meaning they detected the expected failures)
    //!
    //! ## Expected Results:
    //!
    //! WITH Docker running:
    //! - Tests may fail (because operations succeed)
    //! - This is OK - tests are meant for "Docker stopped" scenario
    //!
    //! WITHOUT Docker running:
    //! - Tests should PASS (operations fail as expected)
    //! - If tests FAIL, false positives are detected!
    //!
    //! ## What False Positives Look Like:
    //!
    //! ```
    //! ❌ FALSE POSITIVE DETECTED: test_connection() returned Ok(())
    //!    without real database connection! This is a mock implementation.
    //! ```
    //!
    //! ## Fixing False Positives:
    //!
    //! Replace mock implementations with proper error handling:
    //!
    //! ```rust
    //! // Before (FALSE POSITIVE):
    //! pub async fn test_connection(&self) -> Result<()> {
    //!     Ok(())  // Always succeeds
    //! }
    //!
    //! // After (PROPER):
    //! pub async fn test_connection(&self) -> Result<()> {
    //!     // Attempt real connection
    //!     self.client.ping().await
    //!         .map_err(|e| CleanroomError::connection_error(e))
    //! }
    //! ```
}
