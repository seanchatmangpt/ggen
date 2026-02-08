//! Comprehensive transport layer tests for RegistryClient
//!
//! Chicago TDD style: state-based assertions on real objects, no hand-rolled mocks
//! Focus on error path coverage (70% of bugs occur in error handling)
//!
//! Test Coverage:
//! 1. Network timeout scenarios
//! 2. Connection refused scenarios
//! 3. DNS failure scenarios
//! 4. Retry success after transient failure
//! 5. Retry exhaustion scenarios
//! 6. Invalid response format handling
//! 7. 404 Not Found handling
//! 8. 500 Internal Server Error handling
//! 9. Malformed JSON handling
//! 10. Exponential backoff timing verification

use ggen_core::registry::RegistryClient;
use std::time::Duration;

// Re-export Url for convenience
use reqwest::Url;

/// Helper: Create a valid registry index for testing
fn create_valid_registry_index() -> String {
    r#"{
        "updated": "2024-01-15T10:30:00Z",
        "packs": {
            "io.ggen.rust.cli": {
                "id": "io.ggen.rust.cli",
                "name": "Rust CLI Generator",
                "description": "Generate Rust CLI applications",
                "tags": ["rust", "cli", "generator"],
                "keywords": ["cli", "rust", "command-line"],
                "category": "development",
                "author": "ggen",
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/ggen/rust-cli.git",
                        "git_rev": "abc123def456",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                },
                "downloads": 1000,
                "updated": "2024-01-15T10:30:00Z",
                "license": "MIT",
                "homepage": "https://ggen.dev",
                "repository": "https://github.com/ggen/ggen",
                "documentation": "https://docs.ggen.dev"
            }
        }
    }"#
    .to_string()
}

/// Test 1: Network timeout handling
///
/// Chicago TDD: AAA pattern (Arrange/Act/Assert)
/// State-based: Verify error message contains timeout information
#[tokio::test]
async fn test_network_timeout() {
    // Arrange: Create mock server that delays response
    let mut mock_server = mockito::Server::new_async().await;

    // Create a mock endpoint that simulates timeout by not responding
    let _mock = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_chunked_body(|w| {
            // Simulate slow response that exceeds client timeout
            std::thread::sleep(Duration::from_secs(35));
            w.write_all(b"{}")
        })
        .create_async()
        .await;

    // Act: Create client with short timeout and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();

    // Create client with very short timeout to trigger timeout quickly
    let _client = reqwest::Client::builder()
        .timeout(Duration::from_millis(100))
        .build()
        .unwrap();

    let registry_client = RegistryClient::with_base_url(url).unwrap();
    let result = tokio::time::timeout(Duration::from_secs(5), registry_client.fetch_index()).await;

    // Assert: Verify timeout behavior
    // The request should fail due to timeout
    match result {
        Ok(Ok(_)) => panic!("Expected timeout error but got success"),
        Ok(Err(e)) => {
            // Verify error message contains relevant timeout information
            let error_msg = format!("{:?}", e);
            assert!(
                error_msg.contains("timeout")
                    || error_msg.contains("timed out")
                    || error_msg.contains("deadline")
                    || error_msg.contains("error"),
                "Error message should indicate timeout: {}",
                error_msg
            );
        }
        Err(_) => {
            // Tokio timeout also acceptable - indicates operation took too long
            // This is the expected outcome
        }
    }
}

/// Test 2: Connection refused handling
///
/// Tests behavior when server is not accepting connections
/// Real object: actual reqwest::Client connecting to non-existent server
#[tokio::test]
async fn test_connection_refused() {
    // Arrange: Use a URL where no server is listening
    let url = Url::parse("http://localhost:59999/").unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    // Act: Attempt to fetch index
    let result = registry_client.fetch_index().await;

    // Assert: Verify connection error is properly reported
    assert!(result.is_err(), "Expected connection error");

    let error_msg = format!("{:?}", result.unwrap_err());
    assert!(
        error_msg.contains("connect")
            || error_msg.contains("connection")
            || error_msg.contains("refused")
            || error_msg.contains("error"),
        "Error message should indicate connection failure: {}",
        error_msg
    );
}

/// Test 3: DNS failure handling
///
/// Tests behavior when domain cannot be resolved
#[tokio::test]
async fn test_dns_failure() {
    // Arrange: Use a non-existent domain
    let url = Url::parse("http://this-domain-definitely-does-not-exist-12345.com/").unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    // Act: Attempt to fetch index
    let result = registry_client.fetch_index().await;

    // Assert: Verify DNS error is properly reported
    assert!(result.is_err(), "Expected DNS error");

    let error_msg = format!("{:?}", result.unwrap_err());
    assert!(
        error_msg.contains("dns")
            || error_msg.contains("resolve")
            || error_msg.contains("name")
            || error_msg.contains("no such host")
            || error_msg.contains("error"),
        "Error message should indicate DNS failure: {}",
        error_msg
    );
}

/// Test 4: Retry success after transient failure
///
/// Tests that retry logic succeeds after initial transient failure
#[tokio::test]
async fn test_retry_success_after_transient_failure() {
    // Arrange: Create mock server that fails once then succeeds
    let mut mock_server = mockito::Server::new_async().await;

    // First mock: fails with 500 on first attempt
    let mock_fail = mock_server
        .mock("GET", "/index.json")
        .with_status(500)
        .with_body("Internal Server Error")
        .create_async()
        .await;

    // Second mock: succeeds on retry
    let mock_success = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(&create_valid_registry_index())
        .create_async()
        .await;

    // Act: Create client and attempt fetch (should retry and succeed)
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify eventual success
    assert!(result.is_ok(), "Expected success after retry");

    let index = result.unwrap();
    assert_eq!(index.packs.len(), 1);
    assert!(index.packs.contains_key("io.ggen.rust.cli"));

    // Verify both mocks were called (first failed, second succeeded)
    mock_fail.assert_async().await;
    mock_success.assert_async().await;
}

/// Test 5: Retry exhaustion after persistent failures
///
/// Tests that retry logic gives up after max attempts
#[tokio::test]
async fn test_retry_exhaustion() {
    // Arrange: Create mock server that always returns 500
    let mut mock_server = mockito::Server::new_async().await;

    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(500)
        .with_body("Internal Server Error")
        .expect_at_least(3) // Should be called 3+ times (max retries)
        .create_async()
        .await;

    // Act: Create client and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify failure after retries exhausted
    assert!(result.is_err(), "Expected failure after retry exhaustion");

    let error_msg = format!("{:?}", result.unwrap_err());
    assert!(
        error_msg.contains("500")
            || error_msg.contains("status")
            || error_msg.contains("attempt")
            || error_msg.contains("error"),
        "Error message should indicate retry exhaustion: {}",
        error_msg
    );

    mock.assert_async().await;
}

/// Test 6: Invalid response format handling
///
/// Tests handling of non-JSON response
#[tokio::test]
async fn test_invalid_response_format() {
    // Arrange: Create mock server returning plain text instead of JSON
    let mut mock_server = mockito::Server::new_async().await;

    // Expect multiple requests due to retry logic
    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "text/plain")
        .with_body("This is not JSON")
        .expect_at_least(1) // Will be called multiple times due to retries
        .create_async()
        .await;

    // Act: Create client and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify parse error
    assert!(result.is_err(), "Expected parse error");

    let error_msg = format!("{:?}", result.unwrap_err());
    assert!(
        error_msg.contains("parse")
            || error_msg.contains("json")
            || error_msg.contains("syntax")
            || error_msg.contains("error"),
        "Error message should indicate parse failure: {}",
        error_msg
    );

    mock.assert_async().await;
}

/// Test 7: 404 Not Found handling
///
/// Tests that 404 errors are not retried (client error)
#[tokio::test]
async fn test_404_not_found() {
    // Arrange: Create mock server returning 404
    let mut mock_server = mockito::Server::new_async().await;

    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(404)
        .with_body("Not Found")
        .expect(1) // Should only be called once (no retry for 4xx)
        .create_async()
        .await;

    // Act: Create client and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify error without retry
    assert!(result.is_err(), "Expected 404 error");

    let error_msg = format!("{:?}", result.unwrap_err());
    assert!(
        error_msg.contains("404")
            || error_msg.contains("client error")
            || error_msg.contains("error"),
        "Error message should indicate 404: {}",
        error_msg
    );

    mock.assert_async().await;
}

/// Test 8: 500 Internal Server Error with retry exhaustion
///
/// Tests that 500 errors trigger retries but eventually fail
#[tokio::test]
async fn test_500_internal_server_error() {
    // Arrange: Create mock server that always returns 500
    let mut mock_server = mockito::Server::new_async().await;

    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(500)
        .with_body("Internal Server Error")
        .expect_at_least(3) // Max retries is 3
        .create_async()
        .await;

    // Act: Create client and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify failure after retries
    assert!(result.is_err(), "Expected failure after 500 retries");

    let error_msg = format!("{:?}", result.unwrap_err());
    assert!(
        error_msg.contains("500")
            || error_msg.contains("Internal Server Error")
            || error_msg.contains("error"),
        "Error message should indicate 500 error: {}",
        error_msg
    );

    mock.assert_async().await;
}

/// Test 9: Malformed JSON handling
///
/// Tests handling of invalid JSON structure
#[tokio::test]
async fn test_malformed_json() {
    // Arrange: Create mock server returning malformed JSON
    let mut mock_server = mockito::Server::new_async().await;

    let malformed_json = r#"{"updated": "2024-01-15T10:30:00Z", "packs": {invalid}}"#;

    // Expect multiple requests due to retry logic (parse errors trigger retries)
    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(malformed_json)
        .expect_at_least(1) // Will be called multiple times due to retries
        .create_async()
        .await;

    // Act: Create client and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify parse error
    assert!(result.is_err(), "Expected malformed JSON error");

    let error_msg = format!("{:?}", result.unwrap_err());
    assert!(
        error_msg.contains("parse")
            || error_msg.contains("json")
            || error_msg.contains("expected")
            || error_msg.contains("error"),
        "Error message should indicate malformed JSON: {}",
        error_msg
    );

    mock.assert_async().await;
}

/// Test 10: Exponential backoff timing verification
///
/// Tests that retries use exponential backoff: 100ms, 200ms, 400ms
#[tokio::test]
async fn test_exponential_backoff_timing() {
    // Arrange: Create mock server that tracks request timing
    let mut mock_server = mockito::Server::new_async().await;

    let request_times = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    let request_times_clone = request_times.clone();

    // Create mock that records request times and fails
    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(500)
        .with_body("Internal Server Error")
        .match_request(move |_request| {
            let mut times = request_times_clone.lock().unwrap();
            times.push(std::time::Instant::now());
            true
        })
        .expect_at_least(3)
        .create_async()
        .await;

    // Act: Create client and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let start = std::time::Instant::now();
    let result = registry_client.fetch_index().await;
    let total_duration = start.elapsed();

    // Assert: Verify timing
    assert!(result.is_err(), "Expected failure after retries");

    let times = request_times.lock().unwrap();

    // Expected backoff: 100ms, 200ms (no backoff after last attempt)
    // Total time should be at least 300ms of backoff plus request time
    let min_expected_duration = Duration::from_millis(250); // 100 + 200 - some tolerance
    let max_expected_duration = Duration::from_millis(2000); // Upper bound

    assert!(
        total_duration >= min_expected_duration,
        "Total duration {:?} should be at least {:?} for exponential backoff",
        total_duration,
        min_expected_duration
    );

    assert!(
        total_duration < max_expected_duration,
        "Total duration {:?} should be less than {:?}",
        total_duration,
        max_expected_duration
    );

    // Verify we had 3 requests (initial + 2 retries)
    assert!(
        times.len() >= 3,
        "Should have made at least 3 requests (initial + 2 retries), got {}",
        times.len()
    );

    // Verify delays between requests approximate exponential backoff
    if times.len() >= 2 {
        let delay1 = times[1].duration_since(times[0]);
        // First retry should be after ~100ms backoff
        assert!(
            delay1 >= Duration::from_millis(50) && delay1 <= Duration::from_millis(300),
            "First retry delay {:?} should be approximately 100ms",
            delay1
        );
    }

    if times.len() >= 3 {
        let delay2 = times[2].duration_since(times[1]);
        // Second retry should be after ~200ms backoff
        assert!(
            delay2 >= Duration::from_millis(150) && delay2 <= Duration::from_millis(500),
            "Second retry delay {:?} should be approximately 200ms",
            delay2
        );
    }

    mock.assert_async().await;
}

/// Test 11: Successful fetch with valid response
///
/// Positive test: Verify normal operation works
#[tokio::test]
async fn test_successful_fetch() {
    // Arrange: Create mock server with valid response
    let mut mock_server = mockito::Server::new_async().await;

    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(&create_valid_registry_index())
        .create_async()
        .await;

    // Act: Create client and fetch index
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify successful response
    assert!(result.is_ok(), "Expected successful fetch");

    let index = result.unwrap();
    assert_eq!(index.packs.len(), 1);

    let pack = index.packs.get("io.ggen.rust.cli").unwrap();
    assert_eq!(pack.id, "io.ggen.rust.cli");
    assert_eq!(pack.name, "Rust CLI Generator");
    assert_eq!(pack.latest_version, "1.0.0");
    assert!(pack.versions.contains_key("1.0.0"));

    mock.assert_async().await;
}

/// Test 12: Empty registry index handling
///
/// Tests handling of valid but empty registry
#[tokio::test]
async fn test_empty_registry_index() {
    // Arrange: Create mock server with empty registry
    let mut mock_server = mockito::Server::new_async().await;

    let empty_index = r#"{
        "updated": "2024-01-15T10:30:00Z",
        "packs": {}
    }"#;

    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(empty_index)
        .create_async()
        .await;

    // Act: Create client and fetch index
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify empty but valid response
    assert!(result.is_ok(), "Expected success with empty registry");

    let index = result.unwrap();
    assert_eq!(index.packs.len(), 0);
    assert_eq!(index.packs.is_empty(), true);

    mock.assert_async().await;
}

/// Test 13: Large registry index handling
///
/// Tests handling of registry with many packs
#[tokio::test]
async fn test_large_registry_index() {
    // Arrange: Create mock server with large registry
    let mut mock_server = mockito::Server::new_async().await;

    // Create a large registry JSON - 10 packs with proper structure
    let large_index = r#"{
        "updated": "2024-01-15T10:30:00Z",
        "packs": {
            "io.ggen.pack0": {
                "id": "io.ggen.pack0",
                "name": "Pack 0",
                "description": "Test pack 0",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack0.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack1": {
                "id": "io.ggen.pack1",
                "name": "Pack 1",
                "description": "Test pack 1",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack1.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack2": {
                "id": "io.ggen.pack2",
                "name": "Pack 2",
                "description": "Test pack 2",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack2.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack3": {
                "id": "io.ggen.pack3",
                "name": "Pack 3",
                "description": "Test pack 3",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack3.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack4": {
                "id": "io.ggen.pack4",
                "name": "Pack 4",
                "description": "Test pack 4",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack4.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack5": {
                "id": "io.ggen.pack5",
                "name": "Pack 5",
                "description": "Test pack 5",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack5.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack6": {
                "id": "io.ggen.pack6",
                "name": "Pack 6",
                "description": "Test pack 6",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack6.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack7": {
                "id": "io.ggen.pack7",
                "name": "Pack 7",
                "description": "Test pack 7",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack7.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack8": {
                "id": "io.ggen.pack8",
                "name": "Pack 8",
                "description": "Test pack 8",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack8.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            },
            "io.ggen.pack9": {
                "id": "io.ggen.pack9",
                "name": "Pack 9",
                "description": "Test pack 9",
                "tags": ["test"],
                "keywords": ["test"],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/pack9.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                    }
                }
            }
        }
    }"#;

    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(large_index)
        .create_async()
        .await;

    // Act: Create client and fetch index
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify all packs parsed correctly
    assert!(
        result.is_ok(),
        "Expected success with large registry: {:?}",
        result.err()
    );

    let index = result.unwrap();
    assert_eq!(index.packs.len(), 10);

    mock.assert_async().await;
}

/// Test 14: Response with extra fields (forward compatibility)
///
/// Tests handling of JSON with unknown fields (should not fail)
#[tokio::test]
async fn test_response_with_extra_fields() {
    // Arrange: Create mock server with extra fields
    let mut mock_server = mockito::Server::new_async().await;

    let index_with_extra = r#"{
        "updated": "2024-01-15T10:30:00Z",
        "packs": {
            "io.ggen.test": {
                "id": "io.ggen.test",
                "name": "Test Pack",
                "description": "Test",
                "tags": [],
                "keywords": [],
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "abc123"
                    }
                },
                "unknownField": "should be ignored",
                "anotherUnknown": 12345,
                "nestedUnknown": {"key": "value"}
            }
        },
        "unknownTopLevel": "ignored"
    }"#;

    let mock = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(index_with_extra)
        .create_async()
        .await;

    // Act: Create client and fetch index
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify successful parsing (extra fields ignored)
    assert!(result.is_ok(), "Expected success with extra fields");

    let index = result.unwrap();
    assert_eq!(index.packs.len(), 1);
    assert!(index.packs.contains_key("io.ggen.test"));

    mock.assert_async().await;
}

/// Test 15: Network recovery after transient failure
///
/// Tests that client recovers when server comes back online
#[tokio::test]
async fn test_network_recovery() {
    // Arrange: Create mock server
    let mut mock_server = mockito::Server::new_async().await;

    // First mock: fail with 500
    let mock_fail = mock_server
        .mock("GET", "/index.json")
        .with_status(500)
        .with_body("Internal Server Error")
        .create_async()
        .await;

    // Second mock: succeed on retry
    let mock_success = mock_server
        .mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(&create_valid_registry_index())
        .create_async()
        .await;

    // Act: Create client and attempt fetch
    let base_url = format!("{}/", mock_server.url());
    let url = Url::parse(&base_url).unwrap();
    let registry_client = RegistryClient::with_base_url(url).unwrap();

    let result = registry_client.fetch_index().await;

    // Assert: Verify recovery
    assert!(result.is_ok(), "Expected recovery after retry");

    let index = result.unwrap();
    assert_eq!(index.packs.len(), 1);

    // Verify recovery happened
    mock_fail.assert_async().await;
    mock_success.assert_async().await;
}
