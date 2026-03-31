use ggen_ai::client::GenAiClient;
use ggen_ai::config::LlmConfig;
use tokio::runtime::Runtime;

#[tokio::main]
async fn main() {
    // Initialize tracing
    ggen_ai::init_logging();

    println!("Testing OTEL spans with direct GenAiClient...");

    // Test 1: Direct async call (should have spans)
    println!("\n=== Test 1: Direct async call ===");
    let config = LlmConfig::default();
    let client = GenAiClient::new(config).expect("Failed to create client");

    let response = client.complete("Hello, world!").await.expect("Failed to complete");
    println!("Response: {}", response.content);
    println!("Tokens: {:?}", response.usage);

    // Test 2: Sync via tokio runtime (should preserve spans)
    println!("\n=== Test 2: Sync via runtime ===");
    let rt = Runtime::new().expect("Failed to create runtime");
    let response2 = rt.block_on(async {
        client.complete("Hello, universe!").await.expect("Failed to complete")
    });
    println!("Response: {}", response2.content);
    println!("Tokens: {:?}", response2.usage);
}