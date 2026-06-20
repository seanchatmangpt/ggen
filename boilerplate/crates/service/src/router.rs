use axum::{Router, routing::{get, post}};
use tower_http::{cors::CorsLayer, trace::TraceLayer};
use crate::handlers;

pub fn build() -> Router {
    Router::new()
        .route("/health", get(handlers::health))
        .route("/items", post(handlers::create_item))
        .route("/items/:id", get(handlers::get_item))
        .layer(TraceLayer::new_for_http())
        .layer(CorsLayer::permissive())
}
