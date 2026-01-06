//! API route definitions

use axum::{
    routing::{get, post, put, delete},
    Router,
};

use crate::{
    handlers::{
        auth::*, billing::*, health::*, marketplace::*,
    },
    state::AppState,
};

/// Create the main API router
pub fn create_router(state: AppState) -> Router {
    Router::new()
        .route("/health", get(health))
        .route("/ready", get(ready))
        .nest("/marketplace", marketplace_routes())
        .nest("/auth", auth_routes())
        .nest("/billing", billing_routes())
        .with_state(state)
}

/// Marketplace routes
fn marketplace_routes() -> Router<AppState> {
    Router::new()
        .route("/search", post(search_packages))
        .route("/packages/:id", get(get_package))
        .route("/packages/:id/purchase", post(purchase_package))
        .route("/packages/:id/:version/download", get(download_package))
        .route("/purchases", get(list_purchases))
}

/// Authentication routes
fn auth_routes() -> Router<AppState> {
    Router::new()
        .route("/register", post(register))
        .route("/login", post(login))
        .route("/validate", post(validate_token))
        .route("/keys", post(create_api_key))
        .route("/keys", get(list_api_keys))
        .route("/keys/:id/revoke", post(revoke_api_key))
}

/// Billing routes
fn billing_routes() -> Router<AppState> {
    Router::new()
        .route("/usage", get(get_usage_stats))
        .route("/usage", post(record_usage))
        .route("/invoices", get(get_invoices))
        .route("/invoices/:id", get(get_invoice))
        .route("/subscription", get(get_subscription))
        .route("/subscription", put(change_tier))
        .route("/subscription", delete(cancel_subscription))
        .route("/pricing", get(get_pricing))
        .route("/payment-methods", get(get_payment_methods))
        .route("/payment-methods", post(add_payment_method))
        .route("/payment-methods/:id", delete(remove_payment_method))
        .route("/history", get(get_billing_history))
}
