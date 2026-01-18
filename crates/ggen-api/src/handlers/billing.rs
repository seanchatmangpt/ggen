//! Billing and usage tracking handlers

use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    Json,
};
use chrono::{Duration, Utc};

use crate::{
    error::{ApiError, ApiResult},
    models::{InvoiceItem, InvoiceResponse, UpgradeRequest, UsageRequest, UsageStatsResponse},
    state::AppState,
};

/// Get user's current usage statistics
pub async fn get_usage_stats(
    State(_state): State<AppState>,
) -> ApiResult<Json<UsageStatsResponse>> {
    // TODO: Get user from auth context
    // TODO: Fetch usage stats from metrics database
    // TODO: Get tier limits from tenant config

    let now = Utc::now();
    let month_start = now - Duration::days(30);

    Ok(Json(UsageStatsResponse {
        user_id: "user123".to_string(),
        tier: "free".to_string(),
        period_start: month_start,
        period_end: now,
        api_calls_used: 425,
        api_calls_limit: 1000,
        templates_used: 3,
        templates_limit: 5,
        total_credits_used: 425,
        total_credits_available: 1000,
    }))
}

/// Record a usage event
pub async fn record_usage(
    State(_state): State<AppState>,
    Json(req): Json<UsageRequest>,
) -> ApiResult<StatusCode> {
    // Validate request
    if req.operation.is_empty() {
        return Err(ApiError::BadRequest("Operation required".to_string()));
    }

    // TODO: Get user from auth context
    // TODO: Check if usage would exceed quota
    // TODO: Record usage event in database
    // TODO: Update metrics
    // TODO: Update cached usage stats

    Ok(StatusCode::ACCEPTED)
}

/// Get user's invoices
pub async fn get_invoices(
    State(_state): State<AppState>,
    Query(params): Query<std::collections::HashMap<String, String>>,
) -> ApiResult<Json<Vec<InvoiceResponse>>> {
    // TODO: Get user from auth context
    // TODO: Fetch invoices from database
    // TODO: Support pagination via limit/offset

    let limit = params
        .get("limit")
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(20);

    Ok(Json(vec![]))
}

/// Get a specific invoice
pub async fn get_invoice(
    State(_state): State<AppState>,
    Path(invoice_id): Path<String>,
) -> ApiResult<Json<InvoiceResponse>> {
    // Validate request
    if invoice_id.is_empty() {
        return Err(ApiError::NotFound("Invoice not found".to_string()));
    }

    // TODO: Get user from auth context
    // TODO: Verify invoice ownership
    // TODO: Fetch invoice with line items

    Err(ApiError::NotFound("Invoice not found".to_string()))
}

/// Upgrade or downgrade subscription
pub async fn change_tier(
    State(_state): State<AppState>,
    Json(req): Json<UpgradeRequest>,
) -> ApiResult<Json<serde_json::Value>> {
    // Validate request
    let valid_tiers = vec!["free", "pro", "enterprise"];
    if !valid_tiers.contains(&req.new_tier.as_str()) {
        return Err(ApiError::BadRequest(format!(
            "Invalid tier: {}",
            req.new_tier
        )));
    }

    // TODO: Get user from auth context
    // TODO: Verify new tier is different from current
    // TODO: Calculate prorated charges if upgrading
    // TODO: Create Stripe subscription change
    // TODO: Update user's tier in database
    // TODO: Send confirmation email

    Ok(Json(serde_json::json!({
        "tier": req.new_tier,
        "effective_date": Utc::now(),
        "next_billing_date": Utc::now() + Duration::days(30),
    })))
}

/// Get billing history
pub async fn get_billing_history(
    State(_state): State<AppState>,
    Query(params): Query<std::collections::HashMap<String, String>>,
) -> ApiResult<Json<serde_json::Value>> {
    // TODO: Get user from auth context
    // TODO: Fetch billing events from database
    // TODO: Include subscriptions, charges, refunds

    Ok(Json(serde_json::json!({
        "events": []
    })))
}

/// Get current subscription details
pub async fn get_subscription(
    State(_state): State<AppState>,
) -> ApiResult<Json<serde_json::Value>> {
    // TODO: Get user from auth context
    // TODO: Fetch current subscription from database
    // TODO: Get Stripe subscription details if exists

    Ok(Json(serde_json::json!({
        "tier": "free",
        "status": "active",
        "current_period_start": Utc::now(),
        "current_period_end": Utc::now() + Duration::days(30),
        "cancel_at_period_end": false,
        "price": 0.0,
    })))
}

/// Cancel subscription
pub async fn cancel_subscription(
    State(_state): State<AppState>,
) -> ApiResult<StatusCode> {
    // TODO: Get user from auth context
    // TODO: Mark subscription as cancelling (at period end)
    // TODO: Update Stripe subscription
    // TODO: Send cancellation confirmation email

    Ok(StatusCode::NO_CONTENT)
}

/// Get payment methods
pub async fn get_payment_methods(
    State(_state): State<AppState>,
) -> ApiResult<Json<serde_json::Value>> {
    // TODO: Get user from auth context
    // TODO: Fetch customer from Stripe
    // TODO: List payment methods from Stripe

    Ok(Json(serde_json::json!({
        "payment_methods": []
    })))
}

/// Add a payment method
pub async fn add_payment_method(
    State(_state): State<AppState>,
    Json(payload): Json<serde_json::Value>,
) -> ApiResult<(StatusCode, Json<serde_json::Value>)> {
    // TODO: Get user from auth context
    // TODO: Create Stripe SetupIntent
    // TODO: Return client secret for frontend to complete

    Ok((
        StatusCode::CREATED,
        Json(serde_json::json!({
            "setup_intent_secret": "seti_1234567890",
        })),
    ))
}

/// Remove a payment method
pub async fn remove_payment_method(
    State(_state): State<AppState>,
    Path(method_id): Path<String>,
) -> ApiResult<StatusCode> {
    if method_id.is_empty() {
        return Err(ApiError::BadRequest("Method ID required".to_string()));
    }

    // TODO: Get user from auth context
    // TODO: Verify ownership of payment method
    // TODO: Delete from Stripe

    Ok(StatusCode::NO_CONTENT)
}

/// Get available pricing tiers
pub async fn get_pricing(
    State(_state): State<AppState>,
) -> ApiResult<Json<serde_json::Value>> {
    Ok(Json(serde_json::json!({
        "tiers": [
            {
                "tier": "free",
                "price": 0.0,
                "api_calls": 1000,
                "templates": 5,
            },
            {
                "tier": "pro",
                "price": 29.0,
                "billing_period": "monthly",
                "api_calls": 100000,
                "templates": 50,
            },
            {
                "tier": "enterprise",
                "price": "custom",
                "api_calls": "unlimited",
                "templates": "unlimited",
            },
        ]
    })))
}
