//! Health check and status handlers

use axum::{extract::State, Json};
use chrono::Utc;

use crate::{error::ApiResult, models::HealthResponse, state::AppState};

/// Health check endpoint
pub async fn health(
    State(_state): State<AppState>,
) -> ApiResult<Json<HealthResponse>> {
    Ok(Json(HealthResponse {
        status: "healthy".to_string(),
        timestamp: Utc::now(),
        version: env!("CARGO_PKG_VERSION").to_string(),
    }))
}

/// Ready check (dependencies up)
pub async fn ready(
    State(_state): State<AppState>,
) -> ApiResult<Json<serde_json::Value>> {
    // TODO: Check database connectivity
    // TODO: Check Stripe connectivity
    // TODO: Check cache connectivity

    Ok(Json(serde_json::json!({
        "ready": true,
    })))
}
