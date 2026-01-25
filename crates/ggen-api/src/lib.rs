//! REST API layer for ggen monetization
//!
//! This crate provides HTTP endpoints for:
//! - Marketplace operations (search, install, purchase)
//! - User authentication (OAuth, JWT, API keys)
//! - Billing and usage tracking
//! - SaaS tier management and quota enforcement

pub mod handlers;
pub mod middleware;
pub mod models;
pub mod routes;
pub mod state;
pub mod error;
pub mod network;

pub use routes::create_router;
pub use state::AppState;
pub use error::ApiError;

use axum::Router;
use std::sync::Arc;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;

/// Initialize the API router with all handlers
pub async fn init_api(state: AppState) -> Router {
    let cors = CorsLayer::permissive();
    let trace = TraceLayer::new_for_http();

    Router::new()
        .nest("/api/v1", create_router(state))
        .layer(cors)
        .layer(trace)
}
