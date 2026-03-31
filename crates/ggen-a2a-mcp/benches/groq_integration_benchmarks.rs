// NOTE: These benchmarks require GROQ_API_KEY env var and make real API calls.
// Run with: GROQ_API_KEY=sk-xxx cargo bench -p ggen-a2a-mcp --bench groq_integration_benchmarks
//
// Benchmarks measure real Groq API latency and throughput through ggen's ggen-ai crate.
// Call chain: ggen_ai::GenAiClient -> genai::Client -> Groq API

use criterion::{criterion_group, criterion_main, Criterion};
use futures::StreamExt;
use ggen_ai::{GenAiClient, LlmClient, LlmConfig};
use std::hint::black_box;

/// Create a Groq client using ggen-ai wrapper.
/// Requires GROQ_API_KEY env var (auto-detected by LlmConfig::default()).
fn create_groq_client() -> GenAiClient {
    GenAiClient::new(LlmConfig::default()).expect("GROQ_API_KEY must be set in environment")
}

/// Warm up the client with a single request to amortize TLS/connection setup.
async fn warm_up(client: &GenAiClient) {
    let _ = client.complete("ping").await;
}

// ---------------------------------------------------------------------------
// Synchronous completion benchmarks
// ---------------------------------------------------------------------------

/// Single short completion -- measures baseline Groq API round-trip latency.
fn bench_groq_simple_completion(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let client = create_groq_client();
    rt.block_on(warm_up(&client));

    c.bench_function("groq_simple_completion", |b| {
        b.to_async(&rt).iter(|| {
            let client = client.clone();
            async move {
                let resp = client.complete("Say hello in Rust").await;
                black_box(&resp);
            }
        });
    });
}

/// Code generation prompt -- measures latency for structured output generation.
fn bench_groq_code_generation(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let client = create_groq_client();
    rt.block_on(warm_up(&client));

    c.bench_function("groq_code_generation", |b| {
        b.to_async(&rt).iter(|| {
            let client = client.clone();
            async move {
                let prompt = "Generate a Rust struct called Config with fields: name (String), port (u16), debug (bool). Derive Debug and Clone.";
                let resp = client.complete(prompt).await;
                black_box(&resp);
            }
        });
    });
}

/// Reasoning prompt -- measures latency for concept explanation.
fn bench_groq_reasoning(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let client = create_groq_client();
    rt.block_on(warm_up(&client));

    c.bench_function("groq_reasoning", |b| {
        b.to_async(&rt).iter(|| {
            let client = client.clone();
            async move {
                let prompt = "Explain the difference between Rust's ownership model and garbage collection in 3 sentences.";
                let resp = client.complete(prompt).await;
                black_box(&resp);
            }
        });
    });
}

// ---------------------------------------------------------------------------
// Streaming benchmarks
// ---------------------------------------------------------------------------

/// Time-to-first-token via complete_stream() -- measures server processing + first byte.
fn bench_groq_streaming_first_token(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let client = create_groq_client();
    rt.block_on(warm_up(&client));

    c.bench_function("groq_streaming_first_token", |b| {
        b.to_async(&rt).iter(|| {
            let client = client.clone();
            async move {
                let mut stream = client
                    .complete_stream("Say hello in Rust")
                    .await
                    .expect("stream creation failed");
                // Pull first non-empty chunk to measure time-to-first-token
                while let Some(chunk) = stream.next().await {
                    if !chunk.content.is_empty() {
                        black_box(&chunk);
                        break;
                    }
                }
            }
        });
    });
}

/// Full streaming response -- measures end-to-end streaming latency including all chunks.
fn bench_groq_streaming_full_response(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let client = create_groq_client();
    rt.block_on(warm_up(&client));

    c.bench_function("groq_streaming_full_response", |b| {
        b.to_async(&rt).iter(|| {
            let client = client.clone();
            async move {
                let mut stream = client
                    .complete_stream("Say hello in Rust")
                    .await
                    .expect("stream creation failed");
                while let Some(chunk) = stream.next().await {
                    black_box(&chunk);
                }
            }
        });
    });
}

// ---------------------------------------------------------------------------
// Concurrency and throughput benchmarks
// ---------------------------------------------------------------------------

/// 3 concurrent completion requests -- measures parallel API throughput.
fn bench_groq_concurrent_3(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let client = create_groq_client();
    rt.block_on(warm_up(&client));

    c.bench_function("groq_concurrent_3", |b| {
        b.to_async(&rt).iter(|| {
            let c1 = client.clone();
            let c2 = client.clone();
            let c3 = client.clone();
            async move {
                let (r1, r2, r3) = tokio::join!(
                    c1.complete("Say hi"),
                    c2.complete("Say hello"),
                    c3.complete("Say greetings"),
                );
                black_box(&r1);
                black_box(&r2);
                black_box(&r3);
            }
        });
    });
}

/// 5 sequential completions -- measures sustained throughput (requests/sec).
fn bench_groq_batch_5_sequential(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let client = create_groq_client();
    rt.block_on(warm_up(&client));

    c.bench_function("groq_batch_5_sequential", |b| {
        b.to_async(&rt).iter(|| {
            let client = client.clone();
            async move {
                for i in 0..5u32 {
                    let prompt = format!("Respond with the number {}", i);
                    let resp = client.complete(&prompt).await;
                    black_box(&resp);
                }
            }
        });
    });
}

// ---------------------------------------------------------------------------
// Criterion groups
// ---------------------------------------------------------------------------

criterion_group!(
    benches,
    // Synchronous completion
    bench_groq_simple_completion,
    bench_groq_code_generation,
    bench_groq_reasoning,
    // Streaming
    bench_groq_streaming_first_token,
    bench_groq_streaming_full_response,
    // Concurrency and throughput
    bench_groq_concurrent_3,
    bench_groq_batch_5_sequential,
);
criterion_main!(benches);
