//! Test to detect real vs mock API calls in ggen-ai

use ggen_ai::config::GlobalLlmConfig;

#[tokio::test]
async fn test_real_api_call_detection() {
    let config = GlobalLlmConfig::default();
    println!("Default provider: {:?}", config.provider);
    println!("Is test mode: {}", config.is_test_mode());

    // Check what client would be created
    let client_result = config.create_client();
    match client_result {
        Ok(client) => {
            let client_type = std::any::type_name_of_val(client.as_ref());
            let model = client.get_config().model.clone();
            println!("Client type: {}", client_type);
            println!("Client model: {}", model);

            // Determine if this is mock or real
            if client_type.contains("MockClient") {
                println!("⚠️  DETECTED: Mock client - NOT making real API calls");
                println!("Mock client always returns predefined responses");
            } else if client_type.contains("GenAiClient") {
                if model.starts_with("groq::")
                    || model.starts_with("openai/")
                    || model.starts_with("claude-")
                {
                    println!("✅ DETECTED: Real GenAiClient configured for {}", model);
                    println!("This would make REAL API calls if GROQ_API_KEY is set");
                } else {
                    println!("🔄 DETECTED: GenAiClient with local model - may or may not make real calls");
                    println!("Model: {}", model);
                }
            } else {
                println!("❓ UNKNOWN client type: {}", client_type);
            }
        }
        Err(e) => {
            println!("❌ Failed to create client: {}", e);
            println!("This indicates missing configuration or API keys");

            // Try creating a mock client
            let mock_client = config.create_mock_client("test response");
            println!(
                "Mock client created: {}",
                std::any::type_name_of_val(mock_client.as_ref())
            );
        }
    }
}
