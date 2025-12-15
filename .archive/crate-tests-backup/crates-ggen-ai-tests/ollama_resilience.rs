//! Minimal Ollama resilience smoke (handles cancellation/error path).
#![cfg(feature = "ollama-integration")]

use ggen_ai::{
    test_helpers::{check_ollama_availability, create_test_llm_config},
    GenAiClient, TemplateGenerator,
};

#[tokio::test]
async fn ollama_handles_bad_prompt_gracefully() {
    if !check_ollama_availability().await {
        println!("⏭️  Skipping: Ollama not available");
        return;
    }

    let client =
        std::sync::Arc::new(GenAiClient::new(create_test_llm_config()).expect("client init"));
    let generator = TemplateGenerator::new(client);

    // Intentionally malformed instructions to ensure we still get a response or clear error.
    let result = generator
        .generate_template(
            "!!",
            vec!["Return the word ok; ignore malformed parts; be concise"],
        )
        .await;

    assert!(result.is_ok(), "unexpected failure: {:?}", result.err());
}
