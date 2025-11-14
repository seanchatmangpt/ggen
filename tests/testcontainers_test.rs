use testcontainers::{Container, GenericImage, RunnableImage};

// Example: Using testcontainers with config-based timeouts
// In actual tests, import test_config module:
// #[path = "common/mod.rs"]
// mod test_config;
// use test_config::{container_wait_timeout, http_connection_timeout};

fn main() {
    println!("Testing testcontainers API");

    // Try to create a simple container
    let docker = testcontainers::Cli::default();
    let image = RunnableImage::from(GenericImage::new("alpine", "latest"));
    let container = docker.run(image);

    println!("Container created successfully!");

    // Example: Use config-based timeout for container operations
    // let timeout = test_config::container_wait_timeout();
    // Use timeout in wait conditions, HTTP connections, etc.
}
