use crate::handlers;
use axum::{
    extract::Request,
    middleware::Next,
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
use tower_http::{cors::CorsLayer, trace::TraceLayer};

static REQUEST_COUNTER: AtomicU64 = AtomicU64::new(0);

fn generate_request_id() -> String {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_micros();
    let seq = REQUEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{ts:x}-{seq:x}")
}

async fn add_request_id(req: Request, next: Next) -> impl IntoResponse {
    let id = generate_request_id();
    let mut response = next.run(req).await;
    if let Ok(val) = id.parse() {
        response.headers_mut().insert("x-request-id", val);
    }
    response
}

pub fn build() -> Router {
    Router::new()
        .route("/health", get(handlers::health))
        .route("/items", post(handlers::create_item))
        .route("/items/:id", get(handlers::get_item))
        .layer(axum::middleware::from_fn(add_request_id))
        .layer(TraceLayer::new_for_http())
        .layer(CorsLayer::permissive())
}
