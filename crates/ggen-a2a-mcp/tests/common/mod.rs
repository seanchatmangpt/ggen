//! Shared test helpers for ggen-a2a-mcp integration tests.
//!
//! Provides tracing initialization with optional OTLP export.
//! When the `otel` feature is enabled and `OTEL_EXPORTER_OTLP_ENDPOINT` is set,
//! traces are exported via gRPC in addition to stdout.
//!
//! **Export strategy**: Uses `BatchSpanProcessor` (200ms scheduled delay) with a
//! pre-warmed tonic gRPC channel. The channel is connected on a dedicated thread
//! (to avoid "Cannot start a runtime from within a runtime" when called from
//! `#[tokio::test]`), then passed to the exporter via `with_channel()`. This
//! eliminates the "Service was not ready: transport error" that occurs with the
//! default `connect_lazy()` channel.
//!
//! **Safety net**: An `atexit()` handler calls `force_flush()` as a final
//! safety net before process exit.

use std::sync::OnceLock;

/// Initialize tracing subscriber (once per process).
///
/// When the `otel` feature is enabled:
///   - If `OTEL_EXPORTER_OTLP_ENDPOINT` is set, initializes OTLP gRPC exporter.
///   - If the endpoint is unreachable, falls back to stdout-only logging.
///   - If the env var is not set, falls back to stdout-only logging.
///
/// When the `otel` feature is disabled: behaves identically to the original
/// `init_tracing()` with stdout test writer.
pub fn init_tracing() {
    static TRACING: OnceLock<()> = OnceLock::new();
    TRACING.get_or_init(|| {
        #[cfg(feature = "otel")]
        {
            init_tracing_with_otel();
        }
        #[cfg(not(feature = "otel"))]
        {
            tracing_subscriber::fmt()
                .with_env_filter("info")
                .with_test_writer()
                .init();
        }
    });
}

#[cfg(feature = "otel")]
fn init_tracing_with_otel() {
    let endpoint = std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
        .unwrap_or_else(|_| "http://localhost:4317".to_string());

    // Pre-flight TCP check: avoid hanging if nothing is listening.
    if !can_connect_tcp(&endpoint) {
        eprintln!(
            "[otel] OTLP endpoint {} unreachable, falling back to stdout",
            endpoint
        );
        tracing_subscriber::fmt()
            .with_env_filter("info")
            .with_test_writer()
            .init();
        return;
    }

    match init_otlp(&endpoint) {
        Ok(()) => {
            eprintln!("[otel] Telemetry initialized — exporting to {}", endpoint);
        }
        Err(e) => {
            eprintln!(
                "[otel] OTLP export to {} failed ({}), falling back to stdout",
                endpoint, e
            );
            tracing_subscriber::fmt()
                .with_env_filter("info")
                .with_test_writer()
                .init();
        }
    }
}

/// Quick TCP connectivity check with a 2-second timeout.
#[cfg(feature = "otel")]
fn can_connect_tcp(endpoint: &str) -> bool {
    let without_scheme = endpoint
        .strip_prefix("https://")
        .or_else(|| endpoint.strip_prefix("http://"))
        .unwrap_or(endpoint);

    let (host, port) = match without_scheme.rsplit_once(':') {
        Some((h, p)) => (h, p),
        None => return false,
    };

    let port: u16 = match port.parse() {
        Ok(p) => p,
        Err(_) => return false,
    };

    std::net::TcpStream::connect_timeout(
        &format!("{}:{}", host, port)
            .parse()
            .unwrap_or_else(|_| std::net::SocketAddr::from(([127, 0, 0, 1], port))),
        std::time::Duration::from_secs(2),
    )
    .is_ok()
}

/// Global handle to the SdkTracerProvider for flushing.
#[cfg(feature = "otel")]
static TRACER_PROVIDER: OnceLock<opentelemetry_sdk::trace::SdkTracerProvider> = OnceLock::new();

/// Register `atexit` flush handler.
///
/// Rust's test harness calls `std::process::exit()` which does NOT run
/// static destructors. `atexit()` handlers DO run before `exit()` returns,
/// while the BatchSpanProcessor's background thread is still alive.
#[cfg(feature = "otel")]
fn register_atexit_flush() {
    extern "system" {
        fn atexit(cb: extern "system" fn());
    }

    extern "system" fn flush_on_exit() {
        if let Some(provider) = TRACER_PROVIDER.get() {
            eprintln!("[otel] atexit: force_flush...");
            // force_flush() in opentelemetry_sdk 0.31 is synchronous.
            // The warmup phase ensures the gRPC channel is ready.
            let _ = provider.force_flush();
        }
    }

    unsafe {
        atexit(flush_on_exit);
    }
}

#[cfg(feature = "otel")]
fn init_otlp(endpoint: &str) -> Result<(), Box<dyn std::error::Error>> {
    use opentelemetry::KeyValue;
    use opentelemetry_otlp::WithTonicConfig;
    use opentelemetry_sdk::trace::{BatchConfigBuilder, BatchSpanProcessor, SdkTracerProvider};
    use opentelemetry_sdk::Resource;
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Registry};

    // Pre-warm the tonic gRPC channel on a dedicated thread.
    // This avoids two problems:
    //   1. "Cannot start a runtime from within a runtime" — init_tracing()
    //      is called from #[tokio::test], so we can't create a runtime there.
    //   2. "Service was not ready: transport error" — the default connect_lazy()
    //      means the channel isn't ready when BatchSpanProcessor exports.
    //
    // The dedicated thread owns a tokio runtime, connects the channel, sends it
    // back via mpsc, then parks itself to keep the runtime alive.
    let channel = {
        let endpoint_owned = endpoint.to_string();
        let (tx, rx) = std::sync::mpsc::channel::<Result<tonic::transport::Channel, String>>();

        std::thread::spawn(move || {
            let rt = match tokio::runtime::Runtime::new() {
                Ok(rt) => rt,
                Err(e) => {
                    let _ = tx.send(Err(format!("runtime creation: {e}")));
                    return;
                }
            };

            let ep = endpoint_owned
                .strip_prefix("https://")
                .or_else(|| endpoint_owned.strip_prefix("http://"))
                .unwrap_or(&endpoint_owned);
            let addr = format!("http://{}", ep);

            let ch = rt.block_on(async {
                tonic::transport::Endpoint::from_shared(addr.clone())
                    .map_err(|e| format!("endpoint parse: {e}"))?
                    .timeout(std::time::Duration::from_secs(5))
                    .connect()
                    .await
                    .map_err(|e| format!("channel connect: {e}"))
            });

            match ch {
                Ok(ch) => {
                    let _ = tx.send(Ok(ch));
                    // Keep the runtime alive so the channel stays connected.
                    // This thread blocks here until process exit.
                    std::thread::park();
                }
                Err(e) => {
                    let _ = tx.send(Err(e));
                }
            }
        });

        rx.recv()
            .map_err(|_| "channel warmup thread died".to_string())?
            .map_err(|e| -> Box<dyn std::error::Error> { e.into() })?
    };

    let exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_tonic()
        .with_channel(channel)
        .build()?;

    let resource = Resource::builder_empty()
        .with_attributes([
            KeyValue::new("service.name", "ggen-a2a-mcp-test"),
            KeyValue::new("test.session", "integration"),
        ])
        .build();

    // BatchSpanProcessor with 200ms scheduled delay.
    // Short delay ensures spans are flushed quickly during tests.
    let batch_config = BatchConfigBuilder::default()
        .with_scheduled_delay(std::time::Duration::from_millis(200))
        .build();

    let batch_processor = BatchSpanProcessor::builder(exporter)
        .with_batch_config(batch_config)
        .build();

    let provider = SdkTracerProvider::builder()
        .with_span_processor(batch_processor)
        .with_resource(resource)
        .build();

    opentelemetry::global::set_tracer_provider(provider.clone());

    let tracer = opentelemetry::global::tracer("ggen-a2a-mcp-test");
    let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

    let subscriber = Registry::default()
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")))
        .with(telemetry_layer)
        .with(tracing_subscriber::fmt::layer().with_test_writer());

    subscriber.init();

    // Store provider globally for flush on exit.
    let _ = TRACER_PROVIDER.set(provider);

    // Register atexit handler (runs before exit(), while background thread alive).
    register_atexit_flush();

    Ok(())
}

/// Flush pending OTEL spans and shut down the tracer provider.
///
/// Call at the end of the last test in a process to ensure all spans are sent.
/// After calling this, no more spans can be exported.
#[allow(dead_code)]
pub fn shutdown_tracing() {
    #[cfg(feature = "otel")]
    {
        if let Some(provider) = TRACER_PROVIDER.get() {
            eprintln!("[otel] Explicit flush requested...");
            match provider.force_flush() {
                Ok(_) => eprintln!("[otel] Flush complete"),
                Err(e) => eprintln!("[otel] Flush failed: {}", e),
            }
        }
    }
    #[cfg(not(feature = "otel"))]
    {
        // no-op
    }
}
