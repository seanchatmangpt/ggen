//! Minimal Ollama performance smoke (single request, measures elapsed).
#![cfg(feature = "ollama-integration")]

use ggen_ai::{
    test_helpers::{check_ollama_availability, create_test_llm_config},
    GenAiClient, TemplateGenerator,
};
use std::time::Instant;

#[tokio::test]
async fn ollama_single_call_under_timeout() {
    if !check_ollama_availability().await {
        println!("⏭️  Skipping: Ollama not available");
        return;
    }

    let client =
        std::sync::Arc::new(GenAiClient::new(create_test_llm_config()).expect("client init"));
    let generator = TemplateGenerator::new(client);

    let start = Instant::now();
    let result = generator
        .generate_template("Say 'ok' only", vec!["Respond exactly 'ok'"])
        .await;
    let elapsed = start.elapsed();

    assert!(result.is_ok(), "generation failed: {:?}", result.err());
    // Soft guard to surface regressions; tuned leniently.
    assert!(
        elapsed.as_secs() < 30,
        "Ollama response exceeded 30s: {:?}",
        elapsed
    );
}
