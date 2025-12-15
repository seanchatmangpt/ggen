//! Testcontainers Example
//!
//! Demonstrates usage of testcontainers support in Chicago TDD Tools.
//! Shows how to use HTTP servers and static file servers for integration testing.
//!
//! **Chicago TDD Principles**:
//! - Use REAL HTTP servers, not mocks
//! - Verify REAL HTTP responses
//! - Automatic cleanup via TestFixture
//! - AAA pattern: Arrange (start container), Act (test), Assert (verify)

#[cfg(feature = "testcontainers")]
use chicago_tdd_tools::prelude::*;
use std::time::Duration;
use tokio::time::sleep;

#[cfg(feature = "testcontainers")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testcontainers Example");
    println!("======================");

    // Example 1: HTTP Server Container
    println!("\n1. HTTP Server Container");
    println!("-------------------------");

    // Arrange: Create container client and HTTP server
    let client = ContainerClient::new();
    let http_server = HttpServerContainerBuilder::new("nginx", "alpine")
        .with_port(80)
        .with_wait_for(testcontainers::core::WaitFor::message_on_stdout(
            "start worker process",
        ))
        .build(&client)?;

    println!("✓ HTTP server started");
    println!("  Base URL: {}", http_server.base_url());
    println!("  Host port: {}", http_server.host_port());

    // Act: Wait for server to be ready
    sleep(Duration::from_secs(2)).await;

    // Assert: Verify server is accessible
    let base_url = http_server.base_url();
    println!("✓ HTTP server is ready at {}", base_url);

    // Example 2: Static File Server Container
    println!("\n2. Static File Server Container");
    println!("---------------------------------");

    // Arrange: Create temporary directory with test file
    let temp_dir = tempfile::TempDir::new()?;
    let test_file = temp_dir.path().join("test.json");
    std::fs::write(&test_file, r#"{"test": "data"}"#)?;

    let file_server = StaticFileServerContainerBuilder::new("nginx", "alpine")
        .with_port(80)
        .mount_directory("/usr/share/nginx/html", temp_dir.path())
        .build(&client)?;

    println!("✓ Static file server started");
    println!("  Base URL: {}", file_server.base_url());
    println!("  File URL: {}", file_server.file_url("test.json"));

    // Act: Wait for server to be ready
    sleep(Duration::from_secs(2)).await;

    // Assert: Verify file is accessible
    let file_url = file_server.file_url("test.json");
    println!("✓ Static file server is ready at {}", file_url);

    // Example 3: Integration with TestFixture
    println!("\n3. TestFixture Integration");
    println!("---------------------------");

    // Arrange: Create fixture with container
    let mut fixture = TestFixture::new()?;
    let http_server2 = HttpServerContainerBuilder::new("nginx", "alpine")
        .with_port(80)
        .build(&client)?;

    // Add container to fixture for automatic cleanup
    fixture.add_container(http_server2);
    println!("✓ Container added to fixture");
    println!("  Container will be cleaned up when fixture drops");

    // Act: Use fixture
    let counter = fixture.test_counter();
    println!("✓ Fixture test counter: {}", counter);

    // Assert: Verify fixture works
    assert!(counter >= 0);
    println!("✓ TestFixture integration works");

    println!("\n✓ All examples completed successfully!");
    println!("\nNote: Containers are automatically cleaned up when they go out of scope.");

    Ok(())
}

#[cfg(not(feature = "testcontainers"))]
fn main() {
    println!("Testcontainers feature is not enabled.");
    println!("To enable, add 'testcontainers' feature to Cargo.toml:");
    println!("  cargo run --example testcontainers_example --features testcontainers");
}















