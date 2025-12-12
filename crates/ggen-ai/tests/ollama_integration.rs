//! Minimal Ollama integration smoke test.
#![cfg(feature = "ollama-integration")]

use ggen_ai::{
    test_helpers::{check_ollama_availability, create_test_llm_config},
    GenAiClient, TemplateGenerator,
};

#[tokio::test]
async fn ollama_template_generation_smoke() {
    if !check_ollama_availability().await {
        println!("⏭️  Skipping: Ollama not available");
        return;
    }

    let client =
        std::sync::Arc::new(GenAiClient::new(create_test_llm_config()).expect("client init"));
    let generator = TemplateGenerator::new(client);

    let result = generator
        .generate_template(
            "Return a deterministic hello template",
            vec!["Only say: Hello from Ollama"],
        )
        .await;

    assert!(result.is_ok(), "generation failed: {:?}", result.err());
}
