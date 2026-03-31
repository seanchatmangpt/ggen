#[cfg(test)]
mod tests {
    use ggen_ai::client::GenAiClient;
    use ggen_ai::config::LlmConfig;
    use tokio::runtime::Runtime;

    #[test]
    fn test_otel_spans_direct() {
        ggen_ai::init_logging();

        println!("Testing OTEL spans with direct GenAiClient...");

        // Test 1: Direct async call (should have spans)
        println!("\n=== Test 1: Direct async call ===");
        let config = LlmConfig::default();
        let client = GenAiClient::new(config).expect("Failed to create client");

        let rt = Runtime::new().expect("Failed to create runtime");
        let response = rt.block_on(async {
            client.complete("Hello, world!").await.expect("Failed to complete")
        });
        println!("Response: {}", response.content);
        println!("Tokens: {:?}", response.usage);
    }
}