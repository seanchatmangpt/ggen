use ggen_ai::{client::GenAiClient, LlmConfig, LlmClient};

#[tokio::test]
async fn test_groq_real_api_call() {
    // Check if GROQ_API_KEY is set
    match std::env::var("GROQ_API_KEY") {
        Ok(key) => {
            println!("✅ GROQ_API_KEY is set: {}", if key.len() > 10 { &key[..10] + "..." } else { &key });

            // Create a GenAiClient with Groq configuration
            let config = LlmConfig {
                model: "groq::openai/gpt-oss-20b".to_string(),
                max_tokens: Some(100),
                temperature: Some(0.7),
                top_p: Some(0.9),
                stop: None,
                extra: std::collections::HashMap::new(),
            };

            match GenAiClient::new(config) {
                Ok(client) => {
                    println!("✅ GenAiClient created successfully");

                    // Try a simple completion
                    match client.complete("Hello, world!").await {
                        Ok(response) => {
                            println!("✅ Real API call successful!");
                            println!("Response: {}", response.content);
                            println!("Model: {}", response.model);
                            println!("Tokens used: {:?}", response.usage);

                            // Check if this looks like a real response (not mock)
                            if response.content.len() > 10 && !response.content.contains("Mock") {
                                println!("✅ Response appears to be from real Groq API");
                            } else {
                                println!("⚠️  Response might be mock/synthetic");
                            }
                        }
                        Err(e) => {
                            println!("❌ API call failed: {}", e);
                            println!("This could be due to network issues, invalid API key, or other problems");
                        }
                    }
                }
                Err(e) => {
                    println!("❌ Failed to create GenAiClient: {}", e);
                }
            }
        }
        Err(_) => {
            println!("❌ GROQ_API_KEY is not set - cannot test real API calls");
        }
    }
}