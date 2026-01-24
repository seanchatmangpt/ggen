//! Kernel - Stable Runtime Host
//!
//! This is the ONLY human-editable Rust code in the system.
//! All domain logic is generated from ontology.
//!
//! The kernel's job:
//! - Bootstrap the runtime (Tokio)
//! - Load configuration
//! - Wire up generated world modules
//! - Handle graceful shutdown
//!
//! DO NOT PUT DOMAIN LOGIC HERE.
//! Edit ontology/*.ttl and run `ggen sync` to regenerate world/.

use std::net::SocketAddr;
use tokio::signal;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    tracing::info!("🏭 TCPS: Booting Attribution Context");
    tracing::info!("📍 Working directory: {:?}", std::env::current_dir()?);

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
    // let app = world::routes::build_router();
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
