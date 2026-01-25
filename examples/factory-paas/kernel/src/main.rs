//! Kernel - Stable Runtime Host
//!
//! This is the ONLY human-editable Rust code in the system.
//! All domain logic is generated from ontology.
//!
//! The kernel's job:
//! - Bootstrap the runtime (Tokio)
//! - Load configuration
//! - Initialize OpenTelemetry (tracing, metrics, logging)
//! - Wire up generated world modules
//! - Handle graceful shutdown
//!
//! DO NOT PUT DOMAIN LOGIC HERE.
//! Edit ontology/*.ttl and run `ggen sync` to regenerate world/.

use std::net::SocketAddr;
use tokio::signal;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize OpenTelemetry tracing pipeline
    // This must happen BEFORE any tracing calls
    let otel_config = create_otel_config();
    init_otel_tracing(&otel_config)?;

    tracing::info!("🏭 TCPS: Booting Attribution Context with OpenTelemetry");
    tracing::info!("📍 Working directory: {:?}", std::env::current_dir()?);

    // Initialize OpenTelemetry metrics pipeline
    let metrics_config = create_metrics_config();
    let (meter_provider, metrics_registry) = init_otel_metrics(metrics_config)?;

    tracing::info!("📊 OpenTelemetry metrics initialized");

    // Load generated world
    tracing::info!("📦 Loading generated world modules...");

    // Check if world library exists
    let world_lib = std::path::Path::new("../world/Cargo.toml");
    if !world_lib.exists() {
        tracing::error!("❌ Generated world not found!");
        tracing::error!("   Run: ggen sync");
        tracing::error!("   Expected: ../world/Cargo.toml");
        return Err("Generated world not found. Run `ggen sync` first.".into());
    }

    tracing::info!("✅ World modules found");

    // Start HTTP server
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    tracing::info!("🚀 Starting HTTP server on {}", addr);
    tracing::info!("🔗 Health: http://{}/health", addr);
    tracing::info!("📊 Metrics: http://{}/metrics", addr);
    tracing::info!("📋 Receipts: http://{}/receipts", addr);

    // Note: This will fail until world/ is generated
    // Uncomment after running `ggen sync`:
    //
    // let app = world::routes::build_router_with_metrics(metrics_registry);
    // let listener = tokio::net::TcpListener::bind(addr).await?;
    // tracing::info!("✅ Server listening on http://{}", addr);
    // axum::serve(listener, app)
    //     .with_graceful_shutdown(shutdown_signal())
    //     .await?;

    tracing::info!("🏭 TCPS: Attribution Context ready");
    tracing::info!("⏳ Waiting for world generation...");
    tracing::info!("   Run: cd .. && ggen sync");

    // Keep process alive
    shutdown_signal().await;
    tracing::info!("👋 Shutting down gracefully...");

    // Shutdown OpenTelemetry
    shutdown_otel_tracing().await?;
    shutdown_otel_metrics(meter_provider).await?;

    Ok(())
}

async fn shutdown_signal() {
    let ctrl_c = async {
        signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }

    tracing::info!("Shutdown signal received");
}

/// Create OpenTelemetry tracing configuration
///
/// GCP Cloud Trace endpoint format: https://cloudtrace.googleapis.com/v1/projects/{PROJECT_ID}/traces
/// For local development, use: http://localhost:4317 (OTLP gRPC)
fn create_otel_config() -> OtelConfig {
    let gcp_project = std::env::var("GCP_PROJECT_ID").ok();

    let otlp_endpoint = if let Some(_project_id) = gcp_project {
        // Production: Use GCP Cloud Trace
        "https://cloudtrace.googleapis.com:443".to_string()
    } else {
        // Development: Use local OTLP collector
        std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
            .unwrap_or_else(|_| "http://localhost:4317".to_string())
    };

    OtelConfig {
        service_name: "factory-paas-attribution".to_string(),
        service_version: env!("CARGO_PKG_VERSION").to_string(),
        environment: std::env::var("ENVIRONMENT").unwrap_or_else(|_| "dev".to_string()),
        otlp_endpoint,
        sampling_ratio: std::env::var("OTEL_SAMPLING_RATIO")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(1.0), // 100% sampling in dev, adjust for prod
    }
}

/// Create OpenTelemetry metrics configuration
///
/// GCP Cloud Monitoring endpoint format: https://monitoring.googleapis.com/v3/projects/{PROJECT_ID}
/// For local development, use: http://localhost:4317 (OTLP gRPC)
fn create_metrics_config() -> MetricsConfig {
    let gcp_project = std::env::var("GCP_PROJECT_ID").ok();

    let otlp_endpoint = if let Some(_project_id) = gcp_project {
        // Production: Use GCP Cloud Monitoring
        "https://monitoring.googleapis.com:443".to_string()
    } else {
        // Development: Use local OTLP collector
        std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
            .unwrap_or_else(|_| "http://localhost:4317".to_string())
    };

    MetricsConfig {
        service_name: "factory-paas-attribution".to_string(),
        otlp_endpoint,
        export_interval_secs: 60, // Export metrics every 60 seconds
    }
}

// Import generated OpenTelemetry modules
// These will be available after running `ggen sync`
// use world::otel_tracing::{init_otel_tracing, shutdown_otel_tracing, OtelConfig};
// use world::otel_metrics::{init_otel_metrics, shutdown_otel_metrics, MetricsConfig};

// Stub implementations for compilation before world/ is generated
struct OtelConfig {
    service_name: String,
    service_version: String,
    environment: String,
    otlp_endpoint: String,
    sampling_ratio: f64,
}

struct MetricsConfig {
    service_name: String,
    otlp_endpoint: String,
    export_interval_secs: u64,
}

struct MetricsRegistry;

fn init_otel_tracing(_config: &OtelConfig) -> Result<(), Box<dyn std::error::Error>> {
    // Stub - replaced by world::otel_tracing::init_otel_tracing after generation
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info".into()),
        )
        .json()
        .init();
    Ok(())
}

async fn shutdown_otel_tracing() -> Result<(), Box<dyn std::error::Error>> {
    // Stub - replaced by world::otel_tracing::shutdown_otel_tracing after generation
    Ok(())
}

fn init_otel_metrics(
    _config: MetricsConfig,
) -> Result<(SdkMeterProvider, MetricsRegistry), Box<dyn std::error::Error>> {
    // Stub - replaced by world::otel_metrics::init_otel_metrics after generation
    Ok((SdkMeterProvider::builder().build(), MetricsRegistry))
}

// Type alias to avoid conflicts
type SdkMeterProvider = opentelemetry_sdk::metrics::MeterProvider;

async fn shutdown_otel_metrics(
    _provider: SdkMeterProvider,
) -> Result<(), Box<dyn std::error::Error>> {
    // Stub - replaced by world::otel_metrics::shutdown_otel_metrics after generation
    Ok(())
}
