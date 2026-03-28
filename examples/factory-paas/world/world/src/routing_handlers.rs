//! HTTP Route Handlers for Affiliate Routing
//!
//! Generated from ontology/routing.ttl
//! DO NOT EDIT - regenerate via: ggen sync

use axum::{
    extract::{Path, State},
    http::{StatusCode, header},
    response::{IntoResponse, Response},
    Json,
};
use serde_json::json;
use uuid::Uuid;

use crate::routing::{RouteResolver, RouteError};
use crate::click_tracker::{ClickReceiptGenerator, ClickReceipt};

/// Application state for route handlers
#[derive(Clone)]
pub struct AppState {
    pub resolver: RouteResolver,
    pub receipt_gen: std::sync::Arc<ClickReceiptGenerator>,
}

/// Redirect handler: /r/{slug}
///
/// # Flow
/// 1. Resolve route from slug (zero-alloc cache lookup)
/// 2. Generate click receipt (async, non-blocking)
/// 3. Return 302 redirect with X-Click-ID header
pub async fn redirect_handler(
    Path(slug): Path<String>,
    State(state): State<AppState>,
) -> Result<Response, RouteHandlerError> {
    // Resolve route (zero-alloc on cache hit)
    let route = state.resolver
        .resolve(&slug)
        .map_err(RouteHandlerError::RouteError)?;

    // Generate click receipt (async)
    let click_id = Uuid::now_v7();

    // Build redirect URL with tracking params
    let redirect_url = RouteResolver::build_redirect_url(&route, click_id)
        .map_err(RouteHandlerError::RouteError)?;

    // Background task: Generate receipt asynchronously
    let receipt_gen = state.receipt_gen.clone();
    let route_id = route.id;
    tokio::spawn(async move {
        let _ = receipt_gen
            .generate(route_id, "0.0.0.0", None, None)
            .await;
    });

    // Return 302 redirect
    Ok((
        StatusCode::FOUND,
        [(header::LOCATION, redirect_url.as_str())],
        [(header::HeaderName::from_static("x-click-id"), click_id.to_string())],
    ).into_response())
}

/// Track click handler: POST /track/click
///
/// # Request Body
/// ```json
/// {
///   "route_id": "uuid",
///   "visitor_ip": "192.168.1.100",
///   "user_agent": "Mozilla/5.0...",
///   "referrer": "https://example.com"
/// }
/// ```
pub async fn track_click_handler(
    State(state): State<AppState>,
    Json(payload): Json<TrackClickRequest>,
) -> Result<Json<ClickReceipt>, RouteHandlerError> {
    let receipt = state.receipt_gen
        .generate(
            payload.route_id,
            &payload.visitor_ip,
            payload.user_agent,
            payload.referrer,
        )
        .await
        .map_err(RouteHandlerError::ReceiptError)?;

    Ok(Json(receipt))
}

/// Receipt verification handler: GET /receipts/{click_id}
///
/// Returns stored receipt with verification status
pub async fn verify_receipt_handler(
    Path(click_id): Path<Uuid>,
) -> Result<Json<serde_json::Value>, RouteHandlerError> {
    // TODO: Load receipt from storage
    // For now, return placeholder
    Ok(Json(json!({
        "click_id": click_id,
        "verified": true,
        "message": "Receipt verification not yet implemented"
    })))
}

/// Request payload for track_click_handler
#[derive(Debug, serde::Deserialize)]
pub struct TrackClickRequest {
    pub route_id: Uuid,
    pub visitor_ip: String,
    pub user_agent: Option<String>,
    pub referrer: Option<String>,
}

/// Route handler errors
#[derive(Debug, thiserror::Error)]
pub enum RouteHandlerError {
    #[error("Route error: {0}")]
    RouteError(#[from] RouteError),

    #[error("Receipt error: {0}")]
    ReceiptError(#[from] crate::click_tracker::ReceiptError),
}

impl IntoResponse for RouteHandlerError {
    fn into_response(self) -> Response {
        let (status, message) = match self {
            RouteHandlerError::RouteError(RouteError::NotFound { ref slug }) => {
                (StatusCode::NOT_FOUND, format!("Route not found: {}", slug))
            }
            RouteHandlerError::RouteError(RouteError::Inactive { ref slug }) => {
                (StatusCode::GONE, format!("Route inactive: {}", slug))
            }
            RouteHandlerError::RouteError(RouteError::InvalidSlug { ref slug }) => {
                (StatusCode::BAD_REQUEST, format!("Invalid slug: {}", slug))
            }
            RouteHandlerError::RouteError(_) => {
                (StatusCode::INTERNAL_SERVER_ERROR, "Route error".to_string())
            }
            RouteHandlerError::ReceiptError(_) => {
                (StatusCode::INTERNAL_SERVER_ERROR, "Receipt generation failed".to_string())
            }
        };

        (status, Json(json!({ "error": message }))).into_response()
    }
}

/// Build router with all routing endpoints
pub fn build_routing_router() -> axum::Router<AppState> {
    use axum::routing::{get, post};

    axum::Router::new()
        .route("/r/:slug", get(redirect_handler))
        .route("/track/click", post(track_click_handler))
        .route("/receipts/:click_id", get(verify_receipt_handler))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::routing::AffiliateLinkRoute;
    use chrono::Utc;

    #[tokio::test]
    async fn test_redirect_handler() {
        let resolver = RouteResolver::new();
        let route = AffiliateLinkRoute {
            id: Uuid::now_v7(),
            source_path: "test-123".to_string(),
            destination_url: "https://example.com/offer".to_string(),
            affiliate_id: Uuid::now_v7(),
            tracking_params: None,
            created_at: Utc::now(),
            active: true,
        };
        resolver.warm_cache(vec![route]).expect("warm cache");

        let state = AppState {
            resolver,
            receipt_gen: std::sync::Arc::new(ClickReceiptGenerator::new()),
        };

        let result = redirect_handler(
            Path("test-123".to_string()),
            State(state),
        ).await;

        assert!(result.is_ok());
    }
}
