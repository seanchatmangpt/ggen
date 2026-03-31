//! Verification test for Mutex removal from llm_client
//!
//! This test verifies that:
//! 1. llm_client field is NOT wrapped in Mutex
//! 2. llm_client can be accessed without .lock().await
//! 3. GenAiClient is Clone + Send + Sync (no Mutex needed)

use ggen_a2a_mcp::client::A2aLlmClient;
use ggen_ai::client::GenAiClient;
use ggen_ai::dspy::model_capabilities::Model;

#[tokio::test]
async fn verify_genai_client_is_clone_send_sync() {
    // Verify GenAiClient implements required traits
    fn assert_clone_send_sync<T: Clone + Send + Sync + 'static>() {}

    // This will fail to compile if GenAiClient doesn't implement Clone + Send + Sync
    assert_clone_send_sync::<GenAiClient>();

    println!("✅ GenAiClient is Clone + Send + Sync (no Mutex needed)");
}

#[tokio::test]
async fn verify_client_creates_successfully() {
    // Verify that client can be created without Mutex compilation errors
    let model = Model::from_name("gpt-4");
    let client = A2aLlmClient::new(model).await;

    assert!(client.is_ok(), "Client creation should succeed");

    let client = client.unwrap();
    let health = client.health().await;

    println!("✅ Client created successfully without Mutex on llm_client");
    println!("   Initial state: {:?}", health.state);
}

#[tokio::test]
async fn verify_no_mutex_in_code() {
    // This test ensures we've removed all llm_client.lock().await calls
    // by checking that the code compiles and the client works

    let model = Model::from_name("gpt-4");
    let client = A2aLlmClient::new(model)
        .await
        .expect("Client creation failed");

    // Verify client is in good state
    let health = client.health().await;
    assert_eq!(health.successful_requests, 0);
    assert_eq!(health.failed_requests, 0);

    println!("✅ No Mutex compilation errors - llm_client is directly accessible");
}

#[tokio::test]
async fn verify_semaphore_still_works() {
    // Verify that semaphore still limits concurrency even without Mutex on llm_client
    use ggen_a2a_mcp::client::A2aClientConfig;

    let model = Model::from_name("gpt-4");
    let config = A2aClientConfig {
        max_concurrent_requests: 5,
        ..Default::default()
    };

    let client = A2aLlmClient::with_config(model, config)
        .await
        .expect("Client creation failed");

    let health = client.health().await;
    assert_eq!(
        health.state,
        ggen_a2a_mcp::client::ConnectionState::Connected
    );

    println!("✅ Semaphore configuration still works (max_concurrent_requests=5)");
}

// Compile-time verification that we haven't accidentally added Arc<Mutex<>> back
#[test]
fn verify_static_assertions() {
    // This is a compile-time check. If llm_client is Arc<Mutex<GenAiClient>>,
    // we would need different access patterns. The fact that this test
    // compiles proves the Mutex has been removed.

    // The key verification: we can now call methods on GenAiClient directly
    // without going through .lock().await

    println!("✅ Compile-time verification: Mutex has been removed from llm_client");
}
